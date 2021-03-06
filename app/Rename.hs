{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified System.Environment
import qualified System.Directory
import qualified System.FilePath
import qualified System.Process
import qualified System.IO
import qualified System.IO.Temp
import qualified System.Exit
import qualified Algorithms.NaturalSort as NaturalSort

import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Bits as Bits
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Data.Semigroup ((<>))
import Data.Foldable (forM_)

import qualified Options.Applicative as Opt

import Control.Applicative ((<**>), (<|>))
import Control.Monad (foldM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import System.IO.Error (catchIOError)

import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Except as Except

import qualified Version
import Open
import qualified Development.GitRev as Git


type Pair = (FilePath, FilePath)
type Pairs = [Pair]

type Reason = String

-- TODO
-- maybe replace list of pairs with list of integer ids, for efficiency
data Trace = Trace
    -- files that are successfully renamed.
    -- this exists (other than just using allFiles-errorFiles) because that
    -- we want to record which ones have been touched
  { successFiles :: Pairs
    -- an error occurred when renaming these files.
    -- like nonexistent files, permission errors, etc.
  , errorFiles :: [(Pair, Reason)]
  }

type RenameAction a = Except.ExceptT Reason (State.StateT Trace IO) a


indent = ("    " ++)

delimiter = "------ Above is the original file names, put new file names below ------"

data ExitCode
  = HasActiveStop Reason
  | HasErrorFiles Reason
  | HasSuccessFiles Reason

extractReason (HasActiveStop x) = x
extractReason (HasErrorFiles x) = x
extractReason (HasSuccessFiles x) = x

newtype Index = Index Int

setErrorBit code (Index i) = Bits.setBit code i

-- 1 is reserved for command line parsing failure
exitCodeIndexParseFailure = Index 1

toIndex (HasErrorFiles _) = Index 2
toIndex (HasSuccessFiles _) = Index 3
toIndex (HasActiveStop _) = Index 4

toExitCode :: [ExitCode] -> Int
toExitCode =
    List.foldl' (\acc code -> setErrorBit acc (toIndex code)) 0

main = do
  option <- Opt.execParser optionParser
  case option of
    Rename renameOption -> do
      -- runExceptT :: ExceptT e m a -> m (Either e a)
      let stateM = Except.runExceptT (rename renameOption)

      let initTrace = Trace {successFiles = [], errorFiles = []}

      -- runStateT :: StateT s m a -> s -> m (a, s)
      let ioM = State.runStateT stateM initTrace

      (either, Trace {errorFiles, successFiles}) <- ioM
      let eStop =
            case either of
              Right () -> Nothing
              Left reason -> Just $ HasActiveStop reason
      let eErrorFiles =
            HasErrorFiles <$> unlineErrors errorFiles
      let eSuccessFiles =
            HasSuccessFiles <$> unlineSuccesses successFiles
      let exitCode =
            [eStop, eErrorFiles, eSuccessFiles] |> collectJust |> toExitCode
      let printError maybeE =
            forM_ maybeE (System.IO.hPutStrLn System.IO.stderr . extractReason)
      if exitCode == 0
         || exitCode == setErrorBit 0 (toIndex $ HasSuccessFiles "")
        then
          System.Exit.exitSuccess
        else do
          case eStop of
            Nothing ->
              case (eErrorFiles, eSuccessFiles) of
                (Nothing, _) ->
                  -- if there are no error files,
                  -- be a quiet man and don't print success files
                  return ()
                (a@(Just _), Nothing) ->
                  printError a
                (a@(Just _), b@(Just _)) -> do
                  printError a
                  printError b
            Just _ -> do
              printError eStop
              printError eErrorFiles
              printError eSuccessFiles

          System.Exit.exitWith (System.Exit.ExitFailure exitCode)

unlineErrors :: [(Pair, Reason)] -> Maybe String
unlineErrors [] = Nothing
unlineErrors xs@(_:_) =
  Just . unlines $ "Error occurred when renaming these files:" : map toLine xs
  where
    toLine ((old, _new), reason) = indent $ unwords [old, ":", reason]

unlineSuccesses :: Pairs -> Maybe String
unlineSuccesses [] = Nothing
unlineSuccesses xs@(_:_) =
  Just . unlines $ "These files are renamed successfully:" : map toLine xs
  where
    toLine (old, _new) = indent old


rename :: RenameOption -> RenameAction ()
rename option = do
  editor <- getEditor option
  fileNames <- getFileNames option
  pairsE <-
    liftIO $
    System.IO.Temp.withSystemTempFile
      "rename-.txt"
      (waitPairs
         editor
         (renameOptionEditorOptions option)
         fileNames
         (renameOptionSort option))
  case pairsE of
    Left e -> Except.throwE e
    Right pairs -> renamePairs option pairs

renamePairs :: RenameOption -> Pairs -> RenameAction ()
renamePairs option pairs = do
  when
    (renameOptionMissingDir option == Abort)
    (do let paths = Set.fromList $ map (getDirname . snd) pairs
        err <- liftIO $ checkMissingDir paths
        case err of
          Just e -> Except.throwE e
          Nothing -> return ())
  mapM_ renameOne pairs
  where
    renameOne pair@(old, new) = do
      let doit = do
            System.Directory.createDirectoryIfMissing True (getDirname new)
            System.Directory.renamePath old new
            return Nothing
      err <- liftIO $ catchIOError doit (return . Just . show)
      case err of
        Nothing ->
          lift $ addSuccessFiles pair
        Just e -> do
          lift $ addErrorFiles (pair, e)
          when
            (renameOptionStopOnError option)
            (Except.throwE "Renaming stopped because of an error.")


checkMissingDir :: Set.Set FilePath -> IO (Maybe Reason)
checkMissingDir dirNames =
  catchIOError
    (checkMissingDir' dirNames)
    (return . Just . show)

checkMissingDir' dirNames =
  fmap missingsToMaybe mMissings
  where
    mMissings = foldM check Set.empty dirNames
    check acc dirName = do
      exist <- System.Directory.doesDirectoryExist dirName
      if not exist
        then return $ Set.insert dirName acc
        else return acc
    missingsToMaybe missings
      | Set.null missings = Nothing
      | otherwise =
        let indented = map indent $ Set.toList missings
            header = "Nothing is renamed, these directories are missing:"
        in Just $ unlines (header : indented)

waitPairs :: String -> [String]
          -> [FilePath] -> SortMethod
          -> FilePath -> System.IO.Handle
          -> IO (Either Reason Pairs)
waitPairs editor editorOptions
          fileNames sortMethod
          tempFile tempHandle = do
  -- write file names to the temp file
  fileNamesStr <- unlines <$> sort sortMethod fileNames
  System.IO.hPutStr tempHandle fileNamesStr
  System.IO.hPutStr tempHandle $ unlines ["", delimiter, ""]
  System.IO.hPutStr tempHandle fileNamesStr
  System.IO.hFlush tempHandle

  let createProcess' = System.Process.proc editor $ editorOptions ++ [tempFile]
  let createProcess = createProcess' {System.Process.std_in = System.Process.CreatePipe}
  (_, _, _, processHandle) <- System.Process.createProcess createProcess

  exitCode <- System.Process.waitForProcess processHandle
  if exitCode /= System.Exit.ExitSuccess
    then let msg =
               "Editor terminated with a non-zero exit code: " ++ show exitCode
         in return $ Left msg
    else fileToPairs tempHandle


sort :: SortMethod -> [String] -> IO [String]
sort NoSort xs = return xs
sort NaturalSort xs = return $ List.sortBy NaturalSort.compare xs
sort DepthFirstSort xs = do
  absolutes <- sequence $ map System.Directory.makeAbsolute xs
  let sorted = reverse $ List.sortBy cmp absolutes
  sequence $ map System.Directory.makeRelativeToCurrentDirectory sorted
  where
    cmp :: FilePath -> FilePath -> Ordering
    cmp a b =
      compare (length . System.FilePath.splitPath $ a) (length . System.FilePath.splitPath $ b)

fileToPairs :: System.IO.Handle -> IO (Either Reason Pairs)
fileToPairs handle = do
  System.IO.hSeek handle System.IO.AbsoluteSeek 0
  fileNames <- lines <$> System.IO.hGetContents handle
  let result = foldM splitLines ([], [], True) fileNames
        where
          splitLines acc@(accOlds, accNews, prependOld) line'
            | line == delimiter =
              if prependOld
                then Right (accOlds, accNews, False)
                else Left "Multiple delimiters found."
            | line == "" =
              Right acc
            | otherwise =
              if prependOld
                then Right (line : accOlds, accNews, prependOld)
                else Right (accOlds, line : accNews, prependOld)
            where
              line = strip line'
  case result of
    Left reason ->
      return $ Left reason
    Right (olds, news, _) ->
      if length olds == length news
        then let pairs = reverse $ zip olds news
             in return $
                if null pairs
                  then Left "No files in editor."
                  else Right pairs
        else return $
             Left
               "Number of old file names does not equal to number of new file names."

strip :: String -> String
strip =
  let stripPrefix = dropWhile Char.isSpace
  in reverse . stripPrefix . reverse . stripPrefix

getDirname = fst . System.FilePath.splitFileName

addErrorFiles x = State.modify (\s -> s {errorFiles = x : errorFiles s})

addSuccessFiles x = State.modify (\s -> s {successFiles = x : successFiles s})


getEditor :: RenameOption -> RenameAction String
getEditor option =
  case renameOptionEditor option of
    Just editor ->
      return editor
    Nothing -> do
      maybeEditor <- liftIO $ System.Environment.lookupEnv "EDITOR"
      case maybeEditor of
        Nothing -> Except.throwE "No editor found."
        Just editor -> return editor


getFileNames :: RenameOption -> RenameAction [FilePath]
getFileNames option =
  let ioLines =
        case renameOptionSource option of
          Dir ->
            System.Directory.getCurrentDirectory >>=
            System.Directory.listDirectory
          Stdin -> lines <$> getContents
  in do lines <- liftIO ioLines
        if not $ null lines
          then return lines
          else case renameOptionSource option of
                 Dir -> Except.throwE "No files found."
                 Stdin -> Except.throwE "No files specified."


optionParser :: Opt.ParserInfo Option
optionParser =
  Opt.info
    (optionParser' <**> versionParser <**> Opt.helper)
    (  Opt.progDesc "Rename files using text editor"
    <> Opt.failureCode (setErrorBit 0 exitCodeIndexParseFailure)
    <> Opt.footerDoc (Just exitCodeExplaination))
  where
    versionParser = Version.parser "0.6.0" $(Git.gitHash) $(Git.gitDirty)
    optionParser' = Rename <$> renameOptionParser

exitCodeExplaination =
  PP.vsep $ List.intersperse PP.empty [okCase, errorCase, bitInfo]
  where
    okCase = paragraph
        ["This program exits with exit code 0 if everything goes well."]
    errorCase = paragraph
        [ "If there are errors, then the error messages are printed to stderr,"
        , "and bit fields in the error code are used to indicate errors:"
        , "if a bit at a given index is set,"
        , "then the error corresponding to that bit has happened."]
    bitInfo = PP.vsep
        [ paragraph [ "Bit at a given index and the error it represents:"
                    , "(index starts from 0, but the zeroth bit is never set)"]
        , bitAtIndex exitCodeIndexParseFailure "Cannot parse command line arguments"
        , bitAtIndex (toIndex $ HasErrorFiles "") "Some files cannot be renamed"
        , bitAtIndex (toIndex $ HasSuccessFiles "") "Some files are renamed"
        , bitAtIndex (toIndex $ HasActiveStop "") "Renaming is aborted due to some errors"]
    paragraph sentences =
        foldr
          (\word acc -> PP.text word PP.</> acc)
          PP.empty
          (words $ unwords sentences)
    bitAtIndex (Index index) str =
        PP.text $ show index ++ ": " ++ str

newtype Option
  = Rename RenameOption

data Source
  = Dir
  | Stdin

data MissingDir
  = Create
  | Abort
  deriving (Eq)

instance Show MissingDir where
  show Create = "create"
  show Abort = "abort"


data SortMethod
  = NoSort
  | NaturalSort
  | DepthFirstSort

instance Show SortMethod where
  show NoSort = "no-sort"
  show NaturalSort = "natural"
  show DepthFirstSort = "depth-first"

data RenameOption = RenameOption
  { renameOptionSource        :: Source
  , renameOptionEditor        :: Maybe String
  , renameOptionEditorOptions :: [String]
  , renameOptionSort          :: SortMethod
  , renameOptionMissingDir    :: MissingDir
  , renameOptionStopOnError   :: Bool
  }

renameOptionParser :: Opt.Parser RenameOption
renameOptionParser = do
  let dirParser =
        Opt.flag' Dir
          (  Opt.long "dir"
          <> Opt.help "Rename files in the current directory. This is the default behaviour")

  let stdinParser =
        Opt.flag' Stdin
          (  Opt.long "stdin"
          <> Opt.help "Rename files read from stdin")

  let sourceParser =
        dirParser <|> stdinParser <|> pure Dir

  renameOptionSource <- sourceParser

  renameOptionEditor <-
    Opt.optional (Opt.strOption
      (  Opt.long "editor"
      <> Opt.metavar "EDITOR"
      <> Opt.help "Use this editor instead of $EDITOR"))

  renameOptionEditorOptions <-
    fmap words (Opt.strOption
      (  Opt.long "editor-options"
      <> Opt.metavar "OPTIONS"
      <> Opt.value ""
      <> Opt.showDefault
      <> Opt.help "Additional options to pass to editor. \
                  \NOTE: the passed string will simply be split on spaces, \
                  \special characters are NOT taken care of"))

  renameOptionSort <-
    Opt.option parseSortMethod
      (  Opt.long "sort-method"
      <> Opt.metavar "SORT_METHOD"
      <> Opt.value NaturalSort
      <> Opt.showDefault
      <> Opt.help "Files will be sorted according to this method when they are putting to the editor. \
                  \SORT_METHOD is one of no-sort, natural, depth-first")

  renameOptionMissingDir <-
    Opt.option parseMissingDir
      (  Opt.long "missing-dir"
      <> Opt.metavar "ACTION"
      <> Opt.value Abort
      <> Opt.showDefault
      <> Opt.help "Do ACTION if the directory in destination does not exist. \
                  \ACTION is one of: create, abort")

  renameOptionStopOnError <-
    fmap not (Opt.switch
      (  Opt.long "continue-on-error"
      <> Opt.help "Continue renaming the rest of the files when a previous one has failed"))

  pure RenameOption
         { renameOptionSource
         , renameOptionEditor
         , renameOptionEditorOptions
         , renameOptionSort
         , renameOptionMissingDir
         , renameOptionStopOnError}

-- TODO incomplete: add an "ask" option
parseMissingDir :: Opt.ReadM MissingDir
parseMissingDir =
  Opt.eitherReader rd
  where
    rd action
      | action == "create" = Right Create
      | action == "abort" = Right Abort
      | otherwise = Left $ "Unrecognized action \"" ++ action ++ "\""

parseSortMethod :: Opt.ReadM SortMethod
parseSortMethod =
  Opt.eitherReader rd
  where
    rd method
      | method == "no-sort" = Right NoSort
      | method == "natural" = Right NaturalSort
      | method == "depth-first" = Right DepthFirstSort
      | otherwise = Left $ "Unrecognized sort method \"" ++ method ++ "\""
