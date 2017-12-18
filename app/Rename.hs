import qualified System.Environment
import qualified System.Directory
import qualified System.FilePath
import qualified System.Process
import qualified System.IO
import qualified System.IO.Temp
import qualified System.Exit
import qualified Data.List as List
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
          fileNames <- getFilenames (renameOptionSource option)
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
                  else renamePairs (renameOptionSource option) pairs


waitPairs :: String -> [String]
          -> [String] -> Bool
          -> FilePath -> System.IO.Handle -> IO (Either String [(String, String)])
waitPairs editor editorOptions
          filenames isSort
          tempFile tempHandle = do
    let filenameStr = unlines $ sort isSort filenames
    System.IO.hPutStr tempHandle filenameStr
    System.IO.hPutStr tempHandle $ unlines ["", delimiter, ""]
    System.IO.hPutStr tempHandle filenameStr
    System.IO.hFlush tempHandle

    let createProcess = (System.Process.proc editor $ editorOptions ++ [tempFile])
    let createProcess' = createProcess {System.Process.std_in = System.Process.CreatePipe}
    (_, _, _, processHandle) <- System.Process.createProcess createProcess'

    exitCode <- System.Process.waitForProcess processHandle
    if exitCode /= System.Exit.ExitSuccess
    then
      return $ Left $ "Editor terminated with a non-zero exit code: " ++ show exitCode
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
renamePairs :: Source -> [(String, String)] -> IO ()
renamePairs source pairs =
  forM_ pairs' (uncurry System.Directory.renamePath)
    where
      pairs' = map addDir pairs
      addDir (old, new) = ( System.FilePath.combine cd old
                          , System.FilePath.combine cd new)
      cd =
        case source of
          Dir d -> d
          Stdin d -> d


-- TODO incomplete: use a better delimiter?
delimiter = "------ Above is the original file names, put new file names below ------"


getEditor :: Maybe String -> IO (Maybe String)
getEditor (Just editor) =
    return $ Just editor
getEditor Nothing =
    System.Environment.lookupEnv "EDITOR"


getFilenames :: Source -> IO [FilePath]
getFilenames (Dir dir) =
    System.Directory.listDirectory dir
getFilenames (Stdin _cd) =
    lines <$> getContents


optionParser :: Opt.ParserInfo Option
optionParser =
    Opt.info
      (optionParser' <**> Opt.helper)
      (Opt.progDesc "Rename files using text editor")
    where
      optionParser' =
        Rename <$> renameOptionParser

data Option
    = Rename RenameOption

data Source
    = Dir String
    | Stdin String

data RenameOption = RenameOption
    { renameOptionSource :: Source
    , renameOptionEditor :: Maybe String
    , renameOptionEditorOptions :: [String]
    , renameOptionSort :: Bool
    }

renameOptionParser :: Opt.Parser RenameOption
renameOptionParser =
    let defaultDir = "./"

        dirParser = Dir <$>
          Opt.strOption
             ( Opt.long "dir"
            <> Opt.metavar "DIR"
            <> Opt.value defaultDir
            <> Opt.showDefault
            <> Opt.help "Rename files in this directory")

        stdinParser = Stdin <$>
          Opt.strOption
             ( Opt.long "stdin"
            <> Opt.metavar "CD"
            <> Opt.value defaultDir
            <> Opt.showDefault
            <> Opt.help (unlines [ "Rename files read from stdin."
                                 , "When actually renaming,"
                                 , "prepend CD to the file names"
                                 ]))

        sourceParser =
          (dirParser <|> stdinParser <|> pure (Dir defaultDir))
    in
    RenameOption
    <$> sourceParser
    <*> Opt.optional (Opt.strOption
        ( Opt.long "editor"
       <> Opt.metavar "EDITOR"
       <> Opt.help "Use this editor instead of $EDITOR"))
    <*> fmap words (Opt.strOption
        ( Opt.long "editor-options"
       <> Opt.metavar "OPTIONS"
       <> Opt.value ""
       <> Opt.showDefault
       <> Opt.help ( unwords [ "Additional options to pass to editor."
                             , "NOTE: the passed string will simply be split on spaces,"
                             , "special characters are NOT taken care of"])))
    <*> fmap not (Opt.switch
        ( Opt.long "no-sort"
       <> Opt.help "Don't sort files when putting them to editor"))
