{-# LANGUAGE DeriveAnyClass #-}

module Unison.Share.Types where

import Data.Aeson
import qualified Data.Configurator.Types as Configurator
import Data.Text
import qualified Data.Text as Text
import Network.URI
import Unison.Prelude

newtype CodeserverURI = CodeserverURI URI
  deriving newtype (Show, Eq, Ord)

instance Configurator.Configured CodeserverURI where
  convert = \case
    Configurator.String txt ->
      CodeserverURI <$> parseURI (Text.unpack txt)
    _ -> Nothing

-- foldMap Just . fromJSON

-- | This is distinct from the codeserver URI in that we store credentials by host, since it's
-- much easier to look up that way than from an arbitrary path.
-- We may wish to use explicitly named configurations in the future.
newtype CodeserverHost = CodeserverHost URIAuth
  deriving newtype (Show, Eq, Ord)
  deriving anyclass (ToJSONKey, FromJSONKey)

instance ToJSON CodeserverHost where
  toJSON (CodeserverHost uri) = toJSON $ show uri

instance FromJSON CodeserverHost where
  parseJSON =
    withText "CodeserverHost" $ \txt ->
      either (fail . Text.unpack) pure $ parseCodeserverHost txt

parseCodeserverHost :: Text -> Either Text CodeserverHost
parseCodeserverHost txt =
  case parseURI (Text.unpack txt) of
    Nothing -> Left $ "Invalid URI" <> txt
    Just uri -> case uriAuthority uri of
      Nothing -> Left $ "Invalid URI Authority" <> txt
      Just ua -> pure (CodeserverHost ua)

codeserverHostFromURI :: CodeserverURI -> Either Text CodeserverHost
codeserverHostFromURI (CodeserverURI uri) =
  case uriAuthority uri of
    Nothing -> Left $ "No URI Authority for URI " <> tShow uri
    Just ua -> pure (CodeserverHost ua)
