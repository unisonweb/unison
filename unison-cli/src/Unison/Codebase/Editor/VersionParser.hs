{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.VersionParser where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Unison.Codebase.Editor.RemoteRepo
import qualified Unison.Codebase.Path as Path

-- | Parse git version strings into valid unison namespaces.
--
-- >>> parseMaybe defaultBaseLib "release/M4"
-- >>> parseMaybe defaultBaseLib "release/M4b"
-- >>> parseMaybe defaultBaseLib "release/M4c.2"
-- Just (ReadShareRemoteNamespace {server = DefaultCodeserver, repo = "unison", path = public.base.releases.M4})
-- Just (ReadShareRemoteNamespace {server = DefaultCodeserver, repo = "unison", path = public.base.releases.M4b})
-- Just (ReadShareRemoteNamespace {server = DefaultCodeserver, repo = "unison", path = public.base.releases.M4c_2})

-- >>> parseMaybe defaultBaseLib "dev/M4-1-g22ccb0b3b"
-- Just (ReadShareRemoteNamespace {server = DefaultCodeserver, repo = "unison", path = public.base.main})

-- A version with the 'dirty' flag
-- >>> parseMaybe defaultBaseLib "dev/M3-409-gbcdf68db3'"
-- Just (ReadShareRemoteNamespace {server = DefaultCodeserver, repo = "unison", path = public.base.main})
defaultBaseLib :: Parsec Void Text ReadShareRemoteNamespace
defaultBaseLib = fmap makeNS $ release <|> unknown
  where
    unknown, release, milestoneVersion :: Parsec Void Text Text
    unknown = pure "main" <* takeWhileP Nothing (const True) <* eof
    release = fmap ("releases." <>) $ "release/" *> milestoneVersion <* eof
    -- Parses the milestone of the current version; e.g. M4a -> M4
    milestoneVersion = do
      m <- char 'M'
      milestoneVersion <- some digitChar
      _minor <- many (alphaNumChar <|> ('_' <$ oneOf ['.', '_', '-']))
      _dirty <- optional (char '\'')
      pure . Text.pack $ m : milestoneVersion
    makeNS :: Text -> ReadShareRemoteNamespace
    makeNS t =
      ReadShareRemoteNamespace
        { server = DefaultCodeserver,
          repo = ShareUserHandle "unison",
          path = "public" Path.:< "base" Path.:< Path.fromText t
        }
