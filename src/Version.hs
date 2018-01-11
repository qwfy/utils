{-# LANGUAGE TemplateHaskell #-}

module Version
    ( parser
    ) where

import qualified Options.Applicative as Opt
import qualified Development.GitRev as GitRev
import Data.Semigroup ((<>))
import qualified Data.List as List
import Open

addSemanticVersion str =
    [ Just str
    , Just $ "git commit: " ++ $(GitRev.gitHash)
    , if $(GitRev.gitDirty)
         then Just "development build"
         else Nothing
    ] |> collectJust
      |> List.intercalate ", "

parser :: String -> Opt.Parser (a -> a)
parser semanticVersion =
    Opt.infoOption
        (addSemanticVersion semanticVersion)
        (  Opt.long "version"
        <> Opt.help "Show version"
        )
