{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.VersionParser where

import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Unison.Codebase.Editor.RemoteRepo
import qualified Unison.Codebase.Path as Path

-- | "release/M1j.2" -> "releases._M1j"
--   "devel/*" -> "trunk"
defaultBaseLib :: Parsec Void Text RemoteNamespace
defaultBaseLib = fmap makeNS $ devel <|> release
  where
    devel, release, version :: Parsec Void Text Text
    devel = "devel/" *> many anyChar *> eof $> "trunk"
    release = fmap ("releases._" <>) $ "release/" *> version <* eof
    version =
      fmap Text.pack $
        try (someTill anyChar "." <* many anyChar) <|> many anyChar
    makeNS :: Text -> RemoteNamespace
    makeNS t =
      ( GitRepo "https://github.com/unisonweb/base" Nothing,
        Nothing,
        Path.fromText t
      )
