{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.Time.Clock as Clock
import qualified System.Console.Terminal.Size as TermSize

import qualified Options.Applicative as Opt
import Control.Applicative ((<**>), pure)
import qualified Version
import qualified Development.GitRev as Git

main = do
  () <- Opt.execParser optionParser
  termSize <- TermSize.size
  let termWidth = maybe 80 TermSize.width termSize
  crntTime <- fmap LocalTime.zonedTimeToUTC LocalTime.getZonedTime
  let lines = hr termWidth crntTime
  TIO.putStrLn $ T.unlines lines

hr :: Int -> Clock.UTCTime -> [T.Text]
hr n crntTime =
  [blocks, time, blocks]
  where
    blocks = T.replicate n $ T.pack ['▀']
    time = T.pack $ show crntTime

optionParser :: Opt.ParserInfo ()
optionParser =
  Opt.info
    (optionParser' <**> versionParser <**> Opt.helper)
    (Opt.progDesc "Print a horizontal rule and the current UTC time")
  where
    versionParser = Version.parser "1.0.0" $(Git.gitHash) $(Git.gitDirty)
    optionParser' = pure ()
