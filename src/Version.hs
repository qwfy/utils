module Version
  ( parser
  ) where

import qualified Options.Applicative as Opt
import Data.Semigroup ((<>))
import qualified Data.List as List
import Open


parser :: String -> String -> Bool -> Opt.Parser (a -> a)
parser semanticVersion gitHash gitDirty =
  Opt.infoOption
    version
    (  Opt.long "version"
    <> Opt.help "Show version")
  where
    version =
      [ Just semanticVersion
      , Just $ "git commit: " ++ gitHash
      , if gitDirty
          then Just "development build"
          else Nothing
      ] |> collectJust
        |> List.intercalate ", "
