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

import qualified Options.Applicative as Opt

import Data.Semigroup ((<>))
import Control.Applicative ((<**>), (<|>))
import Control.Monad (forM_, foldM)


main = do
    option <- Opt.execParser optionParser
    case option of
      Rename renameOption ->
          rename renameOption


rename :: RenameOption -> IO ()
rename option = do
    editorO <- getEditor (renameOptionEditor option)
    case editorO of
      Nothing ->
          System.Exit.die "No editor found."
      Just editor -> do
          fileNames <- getFilenames option
          if null fileNames
             then
                 System.Exit.die "No files found."
             else do
                 pairsR <- System.IO.Temp.withSystemTempFile "rename-.txt"
                           (waitPairs
                               editor (renameOptionEditorOptions option)
                               fileNames (renameOptionSort option))
                 case pairsR of
                   Left reason ->
                       System.Exit.die reason
                   Right pairs ->
                       if null pairs
                          then System.Exit.die "Nothing to rename."
                          else renamePairs (renameOptionMissingDir option) pairs


waitPairs :: String -> [String]
          -> [String] -> Bool
          -> FilePath -> System.IO.Handle -> IO (Either String [(String, String)])
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
fileToPairs :: System.IO.Handle -> IO (Either String [(String, String)])
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


-- TODO incomplete: better handling of non-existent and already existed files
renamePairs :: MissingDir -> [(String, String)] -> IO ()
renamePairs missingDir pairs = do
    maybeE <- checkMissingDir missingDir (Set.fromList $ map (getDirname . snd) pairs)
    case maybeE of
      Nothing ->
          forM_ pairs (uncurry System.Directory.renamePath)
      Just e ->
          System.Exit.die e
    where
      getDirname = fst . System.FilePath.splitFileName

checkMissingDir :: MissingDir -> Set.Set String -> IO (Maybe String)
checkMissingDir Abort dirNames =
    fmap missingsToMaybe mMissings
      where
        missingsToMaybe missings
          | Set.null missings = Nothing
          | otherwise =
              let indentLine x = "    " ++ x
                  indented = map indentLine $ Set.toList missings
                  headed = "Nothing is renamed, these directories are missing:" : indented
              in Just $ unlines headed
        mMissings =
            foldM check Set.empty dirNames
              where
                check acc dirName = do
                    exist <- System.Directory.doesDirectoryExist dirName
                    if not exist
                       then return $ Set.insert dirName acc
                       else return acc

checkMissingDir Create dirNames = do
    mapM_ create (Set.toList dirNames)
    return Nothing
      where
        create = System.Directory.createDirectoryIfMissing True


-- TODO incomplete: use a better delimiter?
delimiter = "------ Above is the original file names, put new file names below ------"


getEditor :: Maybe String -> IO (Maybe String)
getEditor (Just editor) =
    return $ Just editor
getEditor Nothing =
    System.Environment.lookupEnv "EDITOR"


getFilenames :: RenameOption -> IO [FilePath]
getFilenames option =
  case renameOptionSource option of
    Dir ->
        System.Directory.getCurrentDirectory >>=
        System.Directory.listDirectory
    Stdin ->
        lines <$> getContents


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

instance Show MissingDir where
    show Create = "create"
    show Abort = "abort"

data RenameOption = RenameOption
    { renameOptionSource :: Source
    , renameOptionEditor :: Maybe String
    , renameOptionEditorOptions :: [String]
    , renameOptionSort :: Bool
    , renameOptionMissingDir :: MissingDir
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

        pure RenameOption
                { renameOptionSource
                , renameOptionEditor
                , renameOptionEditorOptions
                , renameOptionSort
                , renameOptionMissingDir
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
