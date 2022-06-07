{-# LANGUAGE RecordWildCards #-}

module Unison.Codeserver.Discovery where

import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Network.URI (URI)
import qualified Network.URI as URI
import Unison.Prelude

-- | The latest API version a given codeserver supports.
newtype CodeserverVersion = CodeserverVersion Int
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

-- | Document describing the location of various APIs.
data CodeserverDescription = CodeserverDescription
  { syncAPIRoot :: URI,
    openIDConnectDiscoveryLocation :: URI,
    codeserverVersion :: Int
  }
  deriving stock (Show, Eq, Ord)

instance FromJSON CodeserverDescription where
  parseJSON = withObject "CodeserverDescription" \obj -> do
    syncAPIRoot <- (obj .: "sync_api_root") >>= jsonURI
    openIDConnectDiscoveryLocation <- (obj .: "open_id_connect_discovery_location") >>= jsonURI
    codeserverVersion <- obj .: "codeserver_version"
    pure $ CodeserverDescription {..}
    where
      jsonURI :: MonadFail m => String -> m URI
      jsonURI txt =
        case URI.parseURI txt of
          Nothing -> fail $ "Invalid URI: " <> txt
          Just uri -> pure uri

instance ToJSON CodeserverDescription where
  toJSON (CodeserverDescription {..}) =
    object
      [ "sync_api_root" .= uriJSON syncAPIRoot,
        "open_id_connect_discovery_location" .= uriJSON openIDConnectDiscoveryLocation,
        "codeserver_version" .= codeserverVersion
      ]
    where
      uriJSON :: URI -> Value
      uriJSON = toJSON . show

newtype CodeserverDescriptionURI = CodeserverDescriptionURI
  {codeserverDescriptionURI :: URI}

data CodeserverError = InvalidCodeserverDescription CodeserverDescriptionURI Text

fetchCodeServerDescription :: MonadIO m => CodeserverDescriptionURI -> m (Either CodeserverError CodeserverDescription)
fetchCodeServerDescription cdu@(CodeserverDescriptionURI discoveryURI) = liftIO $ do
  httpClient <- HTTP.getGlobalManager
  req <- HTTP.requestFromURI discoveryURI
  resp <- HTTP.httpLbs req httpClient
  case Aeson.eitherDecode (HTTP.responseBody $ resp) of
    Left err -> pure . Left $ InvalidCodeserverDescription cdu (Text.pack err)
    Right doc -> pure . Right $ doc
