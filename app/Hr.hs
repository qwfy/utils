{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Shelly
import qualified Data.Text as T
import qualified Data.Text.IO
import qualified Data.Time.LocalTime as Lt
import qualified Data.Time.Clock as Clock
import qualified System.IO

main = do
    System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering
    shelly $ do
      crntTime <- liftIO $ fmap Lt.zonedTimeToUTC Lt.getZonedTime
      widthStr <- silently $ run "tput" ["cols"]
      let width = read . T.unpack $ widthStr :: Int
      let lines = hr width crntTime
      liftIO $ Data.Text.IO.putStrLn (T.intercalate "\n" lines)

hr :: Int -> Clock.UTCTime -> [T.Text]
hr n crntTime =
    [blocks, time, blocks]
    where
      blocks = T.replicate n $ T.pack ['▀']
      time = T.pack $ show crntTime
