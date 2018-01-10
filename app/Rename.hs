{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

import qualified System.Environment
import qualified System.Directory
import qualified System.FilePath
import qualified System.Process
import qualified System.IO
import qualified System.IO.Temp
import qualified System.Exit
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Algorithms.NaturalSort as NaturalSort

import System.IO.Error (catchIOError)

import qualified Options.Applicative as Opt

import Data.Semigroup ((<>))
import Control.Applicative ((<**>), (<|>))
import Control.Monad (forM_, foldM, when)

import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as State

import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as Except

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromJust)

type Pair = (FilePath, FilePath)
type Pairs = [Pair]

-- reason of error
type Reason = String

-- TODO incomplete
-- maybe replace list of pairs with list of integer ids, for efficiency
data Trace = Trace
    {
    -- files that are renamed successfully.
    -- this exists (other than just using allFiles-errorFiles) because that
    -- we want to record that if there are files are touched, and if so, which ones
      successFiles :: Pairs

    -- an error occurred when renaming these files.
    -- like nonexistent files, permission errors, etc.
    , errorFiles :: [(Pair, Reason)]
    }

type RenameAction a = ExceptT Reason (StateT Trace IO) a

main = do
    option <- Opt.execParser optionParser
    case option of
      Rename renameOption -> do
          let initTrace = Trace
                { successFiles = []
                , errorFiles = []
                }

          -- runExceptT :: ExceptT e m a -> m (Either e a)
          -- stateM :: StateT Reason IO (Either Reason a)
          let stateM = Except.runExceptT (rename renameOption)

          -- runStateT :: StateT s m a -> s -> m (a, s)
          let ioM = State.runStateT stateM initTrace
          (either, Trace {errorFiles, successFiles}) <- ioM
          let traceMsg = unlineErrors errorFiles <+ unlineSuccesses successFiles
          case either of
            Right () ->
                case traceMsg of
                  Nothing ->
                      System.Exit.exitSuccess
                  Just msg -> do
                      System.IO.hPutStrLn System.IO.stderr msg
                      System.Exit.exitWith (System.Exit.ExitFailure 1)
            Left reason -> do
                let msg = fromJust (Just reason <+ traceMsg)
                System.IO.hPutStrLn System.IO.stderr msg
                System.Exit.exitWith (System.Exit.ExitFailure 2)

Nothing  <+ _ = Nothing
(Just x) <+ Nothing = Just x
(Just x) <+ (Just y) = Just $ unlines [x, y]

unlineErrors :: [(Pair, Reason)] -> Maybe String
unlineErrors [] = Nothing
unlineErrors xs@(_:_) =
    Just . unlines $ "Errors occurred when renaming these files:" : map f xs
      where
        f ((old, _new), reason) =
            indentString ++ unwords [old, ":", reason]

unlineSuccesses :: Pairs -> Maybe String
unlineSuccesses [] = Nothing
unlineSuccesses xs@(_:_) =
    Just . unlines $ "These files are renamed successfully:" : map f xs
      where
        f (old, new) =
            indentString ++ old


addErrorFiles pr = lift $ State.modify (\s -> s {errorFiles = pr : errorFiles s})

addSuccessFiles fp = lift $ State.modify (\s -> s {successFiles = fp : successFiles s})

rename :: RenameOption -> RenameAction ()
rename option = do
    editor <- getEditor option
    fileNames <- getFilenames option
    pairsE <- liftIO $ System.IO.Temp.withSystemTempFile "rename-.txt"
               (waitPairs
                   editor (renameOptionEditorOptions option)
                   fileNames (renameOptionSort option))
    case pairsE of
      Left e ->
          Except.throwE e
      Right pairs ->
          if null pairs
             then Except.throwE "Nothing to rename."
             else renamePairs option pairs

getDirname = fst . System.FilePath.splitFileName

renamePairs :: RenameOption -> Pairs -> RenameAction ()
renamePairs option pairs = do
    when (renameOptionMissingDir option == Abort)
         (do missingEither <- liftIO $ checkMissingDir (Set.fromList $ map (getDirname . snd) pairs)
             case missingEither of
               Left e ->
                   Except.throwE e
               Right () ->
                   return ())
    mapM_ renameOne pairs
  where
    renameOne pair@(old, new) = do
        result <- liftIO $ catchIOError
                      (System.Directory.createDirectoryIfMissing True (getDirname new) >>
                       System.Directory.renamePath old new >>
                       return Nothing)
                      (return . Just . show)
        case result of
          Nothing ->
              addSuccessFiles pair
          Just e -> do
              addErrorFiles (pair, e)
              when (renameOptionStopOnError option)
                   (Except.throwE "Error encountered while renaming.")



checkMissingDir :: Set.Set String -> IO (Either Reason ())
checkMissingDir dirNames =
    catchIOError
        (checkMissingDir' dirNames)
        (return . Left . show)

checkMissingDir' dirNames =
    fmap missingsToEither mMissings
      where
        mMissings =
            foldM check Set.empty dirNames
              where
                check acc dirName = do
                    exist <- System.Directory.doesDirectoryExist dirName
                    if not exist
                       then return $ Set.insert dirName acc
                       else return acc
        missingsToEither missings
          | Set.null missings = Right ()
          | otherwise =
              let indentLine x = indentString ++ x
                  indented = map indentLine $ Set.toList missings
                  headed = "Nothing is renamed, these directories are missing:" : indented
              in Left $ unlines headed

waitPairs :: String -> [String]
          -> [String] -> Bool
          -> FilePath -> System.IO.Handle -> IO (Either Reason Pairs)
waitPairs editor editorOptions
          filenames isSort
          tempFile tempHandle = do
    let filenamesStr = unlines $ sort isSort filenames
    System.IO.hPutStr tempHandle filenamesStr
    System.IO.hPutStr tempHandle $ unlines ["", delimiter, ""]
    System.IO.hPutStr tempHandle filenamesStr
    System.IO.hFlush tempHandle

    let createProcess = System.Process.proc editor $ editorOptions ++ [tempFile]
    let createProcess' = createProcess {System.Process.std_in = System.Process.CreatePipe}
    (_, _, _, processHandle) <- System.Process.createProcess createProcess'

    exitCode <- System.Process.waitForProcess processHandle
    if exitCode /= System.Exit.ExitSuccess
       then
           let msg = "Editor terminated with a non-zero exit code: " ++ show exitCode
           in return $ Left msg
       else
           fileToPairs tempHandle

    where
      sort :: Bool -> [String] -> [String]
      sort False xs = xs
      sort True xs = List.sortBy NaturalSort.compare xs


-- TODO incomplete: strip white spaces
fileToPairs :: System.IO.Handle -> IO (Either Reason Pairs)
fileToPairs handle = do
    System.IO.hSeek handle System.IO.AbsoluteSeek 0
    filenames <- lines <$> System.IO.hGetContents handle
    let result = foldM splitLines ([], [], True) filenames
          where
            splitLines acc@(accOlds, accNew, prependOld) line
              | line == delimiter =
                  if prependOld
                  then Right (accOlds, accNew, False)
                  else Left "Multiple delimiters found."
              | line == "" =
                  Right acc
              | otherwise =
                  if prependOld
                  then Right (line:accOlds, accNew, prependOld)
                  else Right (accOlds, line:accNew, prependOld)
    case result of
      Left reason ->
          return $ Left reason
      Right (olds, news, _) ->
          if length olds == length news
             then return $ Right (zip olds news)
             else return $ Left "Number of old file names does not equal to number of new file names."


-- TODO incomplete: use a better delimiter?
delimiter = "------ Above is the original file names, put new file names below ------"

getEditor option =
    case renameOptionEditor option of
      Just editor ->
          return editor
      Nothing -> do
          maybeEditor <- liftIO $ System.Environment.lookupEnv "EDITOR"
          case maybeEditor of
            Nothing ->
                Except.throwE "No editor found."
            Just editor ->
                return editor


getFilenames option =
    let mLines = case renameOptionSource option of
                   Dir ->
                       System.Directory.getCurrentDirectory >>=
                       System.Directory.listDirectory
                   Stdin ->
                       lines <$> getContents
    in do
        lines <- liftIO mLines
        if not $ null lines
           then
               return lines
           else
               case renameOptionSource option of
                 Dir -> Except.throwE "No files found."
                 Stdin -> Except.throwE "No files specified."


optionParser :: Opt.ParserInfo Option
optionParser =
    Opt.info
        (optionParser' <**> Opt.helper)
        (Opt.progDesc "Rename files using text editor")
    where
        optionParser' = Rename <$> renameOptionParser


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

data RenameOption = RenameOption
    { renameOptionSource :: Source
    , renameOptionEditor :: Maybe String
    , renameOptionEditorOptions :: [String]
    , renameOptionSort :: Bool
    , renameOptionMissingDir :: MissingDir
    , renameOptionStopOnError :: Bool
    }

renameOptionParser :: Opt.Parser RenameOption
renameOptionParser =
    let dirParser =
          Opt.flag' Dir
            (  Opt.long "dir"
            <> Opt.help "Rename files in the current directory. This is the default behaviour")

        stdinParser =
          Opt.flag' Stdin
            (  Opt.long "stdin"
            <> Opt.help "Rename files read from stdin")

        sourceParser =
            (dirParser <|> stdinParser <|> pure Dir)
    in do
        renameOptionSource <- sourceParser

        renameOptionEditor <- Opt.optional (Opt.strOption
          (  Opt.long "editor"
          <> Opt.metavar "EDITOR"
          <> Opt.help "Use this editor instead of $EDITOR"))

        renameOptionEditorOptions <- fmap words (Opt.strOption
          (  Opt.long "editor-options"
          <> Opt.metavar "OPTIONS"
          <> Opt.value ""
          <> Opt.showDefault
          <> Opt.help (unwords [ "Additional options to pass to editor."
                               , "NOTE: the passed string will simply be split on spaces,"
                               , "special characters are NOT taken care of"
                               ])))

        renameOptionSort <- fmap not (Opt.switch
          (  Opt.long "no-sort"
          <> Opt.help "Don't sort files when putting them to editor"))

        renameOptionMissingDir <- Opt.option parseMissingDir
          (  Opt.long "missing-dir"
          <> Opt.metavar "ACTION"
          <> Opt.value Abort
          <> Opt.showDefault
          <> Opt.help (unwords [ "Do ACTION if the directory in destination does not exist."
                               , "ACTION is one of: create, abort"]))

        renameOptionStopOnError <- fmap not (Opt.switch
          (  Opt.long "continue-on-error"
          <> Opt.help "Continue renaming the rest of the files when a previous one has failed"))

        pure RenameOption
                { renameOptionSource
                , renameOptionEditor
                , renameOptionEditorOptions
                , renameOptionSort
                , renameOptionMissingDir
                , renameOptionStopOnError
                }

-- TODO incomplete: add an "ask" option
parseMissingDir :: Opt.ReadM MissingDir
parseMissingDir =
    Opt.eitherReader r
      where
        r action
          | action == "create" = Right Create
          | action == "abort" = Right Abort
          | otherwise = Left $ "Unrecognized action \"" ++ action ++ "\""

indentString = "    "
