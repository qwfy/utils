import qualified System.Environment
import qualified System.Directory
import qualified System.FilePath
import qualified System.Process
import qualified System.IO
import qualified System.IO.Temp
import qualified System.Exit

import qualified Options.Applicative as Opt

import Data.Semigroup ((<>))
import Control.Applicative ((<**>))
import Control.Monad (forM_, foldM)

main = do
    option <- Opt.execParser optionParser
    case optionCommand option of
      FromDir fromDirOption ->
          renameFromDir fromDirOption

renameFromDir :: FromDirOption -> IO ()
renameFromDir option = do
    editorO <- getEditor option
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
                      (waitPairs editor (fromDirOptionEditorOptions option) fileNames)
            case pairsR of
              Left reason ->
                  System.Exit.die reason
              Right pairs ->
                  if null pairs
                  then System.Exit.die "Nothing to rename."
                  else rename (fromDirOptionDir option) pairs

waitPairs :: String -> [String] -> [String] -> FilePath -> System.IO.Handle -> IO (Either String [(String, String)])
waitPairs editor editorOptions filenames tempFile tempHandle = do
    let filenameStr = unlines filenames
    System.IO.hPutStr tempHandle filenameStr
    System.IO.hPutStr tempHandle $ unlines ["", delimiter, ""]
    System.IO.hPutStr tempHandle filenameStr
    System.IO.hFlush tempHandle
    (_, _, _, processHandle) <- System.Process.createProcess
                                (System.Process.proc editor $ editorOptions ++ [tempFile])
    exitCode <- System.Process.waitForProcess processHandle
    if exitCode /= System.Exit.ExitSuccess
    then
      return $ Left $ "Editor terminated with a non-zero exit code: " ++ show exitCode
    else
      fileToPairs tempHandle

-- TODO incomplete: strip white spaces
fileToPairs :: System.IO.Handle -> IO (Either String [(String, String)])
fileToPairs handle = do
    System.IO.hSeek handle System.IO.AbsoluteSeek 0
    lines <- lines <$> System.IO.hGetContents handle
    let result = foldM splitLines ([], [], True) lines
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
rename :: String -> [(String, String)] -> IO ()
rename relativeTo pairs = do
    let addDir (old, new) = ( System.FilePath.combine relativeTo old
                            , System.FilePath.combine relativeTo new)
    let pairs' = map addDir pairs
    forM_ pairs' (uncurry System.Directory.renamePath)

getEditor :: FromDirOption -> IO (Maybe String)
getEditor option =
    case fromDirOptionEditor option of
      Just editor ->
          return $ Just editor
      Nothing ->
          System.Environment.lookupEnv "EDITOR"

getFilenames :: FromDirOption -> IO [FilePath]
getFilenames option = do
    let dir = fromDirOptionDir option
    System.Directory.listDirectory dir

delimiter = "------ Above is the original file names, put new file names below ------"

optionParser :: Opt.ParserInfo Option
optionParser =
    Opt.info
      (optionParser' <**> Opt.helper)
      (Opt.progDesc "Rename files using text editor")
    where
      optionParser' = Option <$> Opt.hsubparser
          ( Opt.command "dir"
              (Opt.info
                (FromDir <$> fromDirOptionParser)
                (Opt.progDesc "Rename files in a directory"))
          )

data Option = Option
    { optionCommand :: Command
    }

data Command
    = FromDir FromDirOption

data FromDirOption = FromDirOption
    { fromDirOptionDir :: String
    , fromDirOptionEditor :: Maybe String
    , fromDirOptionEditorOptions :: [String]
    }

fromDirOptionParser :: Opt.Parser FromDirOption
fromDirOptionParser = FromDirOption
    <$> Opt.strArgument
        ( Opt.metavar "DIRECTORY"
       <> Opt.value "./"
       <> Opt.showDefault
       <> Opt.help "Rename files in this directory")
    <*> Opt.optional (Opt.strOption
        ( Opt.long "editor"
       <> Opt.metavar "EDITOR"
       <> Opt.help "Use this editor instead of $EDITOR"))
    <*> fmap words (Opt.strOption
        ( Opt.long "editor-options"
       <> Opt.metavar "EDITOR_OPTIONS"
       <> Opt.value ""
       <> Opt.showDefault
       <> Opt.help ( unwords [ "Additional options to pass to editor."
                             , "NOTE: the passed string will simply be split on spaces,"
                             , "no extra handling are added"])))
