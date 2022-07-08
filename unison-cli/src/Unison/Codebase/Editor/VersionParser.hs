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

-- | Parse git version strings into valid unison namespaces.
--
-- >>> parseMaybe defaultBaseLib "release/M1j"
-- Just (ReadShareRemoteNamespace {server = DefaultCodeserver, repo = "unison", path = public.base.releases._M1j})
--
-- >>> parseMaybe defaultBaseLib "release/M1j.2"
-- Just (ReadShareRemoteNamespace {server = DefaultCodeserver, repo = "unison", path = public.base.releases._M1j_2})
--
-- >>> parseMaybe defaultBaseLib "latest-1234"
-- Just (ReadShareRemoteNamespace {server = DefaultCodeserver, repo = "unison", path = public.base.latest})
defaultBaseLib :: Parsec Void Text ReadShareRemoteNamespace
defaultBaseLib = fmap makeNS $ latest <|> release
  where
    latest, release, version :: Parsec Void Text Text
    latest = "latest-" *> many anySingle *> eof $> "latest"
    release = fmap ("releases._" <>) $ "release/" *> version <* eof
    version = do
      Text.pack <$> some (alphaNumChar <|> ('_' <$ oneOf ['.', '_', '-']))
    makeNS :: Text -> ReadShareRemoteNamespace
    makeNS t =
      ReadShareRemoteNamespace
        { server = DefaultCodeserver,
          repo = "unison",
          path = "public" Path.:< "base" Path.:< Path.fromText t
        }
