{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle
import Data.Text (pack, unpack)

hr :: Int -> UTCTime -> String
hr n crntTime = top ++ middle ++ bottom
  where top = replicate n '▀'
        bottom = replicate n '▄'
        middle = crntTimeStr ++ trailingSpaces ++ "\n"
          where crntTimeStr = show crntTime
                trailingSpaces = replicate (max 0 $ n - length crntTimeStr) ' '

main = sh $ do
  crntTime <- date
  widthStr <- inshell "tput cols" empty
  let width = read . unpack $ widthStr :: Int
  stdout $ return . pack $ hr width crntTime
