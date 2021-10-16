{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Unison.Codebase.Editor.VersionParser where

import Text.Megaparsec
import Unison.Codebase.Editor.RemoteRepo
import Text.Megaparsec.Char
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.Path (Path, PathType(..))
import Data.Void (Void)

-- | Parse git version strings into valid unison namespaces.
--   "release/M1j" -> "releases._M1j"
--   "release/M1j.2" -> "releases._M1j_2"
--   "latest-*" -> "trunk"
defaultBaseLib :: Parsec Void Text ReadRemoteNamespace
defaultBaseLib = do
  versionText <- latest <|> release
  basePath <- Path.matchUnknown (Path.unknownPathFromText versionText) \case
    -- The version we parse doesn't start with a '.', but it represents an absolute path on
    -- the remote.
    Path.RelativePath p -> pure (Path.AbsolutePath p)
    absPath@(Path.AbsolutePath{}) -> pure absPath
  pure $ makeReadRemote basePath
  where
  latest, release, version :: Parsec Void Text Text
  latest = "latest-" *> many anyChar *> eof $> "trunk"
  release = fmap ("releases._" <>) $ "release/" *> version <* eof
  version = do
    Text.pack <$> some (alphaNumChar <|> ('_' <$ oneOf ['.', '_', '-']))
  makeReadRemote :: Path 'Absolute -> ReadRemoteNamespace
  makeReadRemote p = ( ReadGitRepo "https://github.com/unisonweb/base"
                    , Nothing
                    , p)
