{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.VersionParser where

import Text.Megaparsec
import Unison.Codebase.Editor.RemoteRepo
import Text.Megaparsec.Char
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Unison.Codebase.Path as Path
import Data.Void (Void)

-- |"release/M1j.2" -> "releases._M1j"
--  "latest-*" -> "trunk"
defaultBaseLib :: Parsec Void Text ReadRemoteNamespace
defaultBaseLib = fmap makeNS $ latest <|> release
  where
  latest, release, version :: Parsec Void Text Text
  latest = "latest-" *> many anyChar *> eof $> "trunk"
  release = fmap ("releases._" <>) $ "release/" *> version <* eof
  version = fmap Text.pack $
              try (someTill anyChar "." <* many anyChar) <|> many anyChar
  makeNS :: Text -> ReadRemoteNamespace
  makeNS t = ( ReadGitRepo "https://github.com/unisonweb/base"
             , Nothing
             , Path.fromText t)
