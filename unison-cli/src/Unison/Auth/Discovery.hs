module Unison.Auth.Discovery where

import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP
import Network.URI
import qualified Network.URI as URI
import Unison.Auth.Types

-- TODO: use the correct enpdpoint based on the environment.
discoveryURI :: Audience -> URI
discoveryURI aud = fromJust . URI.parseURI $ case aud of
  Share -> "https://enlil.unison-lang.org/.well-known/openid-configuration"
  ShareStaging -> "https://enlil-staging.unison-lang.org/.well-known/openid-configuration"

discoveryForAudience :: HTTP.Manager -> Audience -> IO (Either CredentialFailure DiscoveryDoc)
discoveryForAudience httpClient aud = do
  let uri = discoveryURI aud
  req <- HTTP.requestFromURI uri
  resp <- HTTP.httpLbs req httpClient
  case Aeson.eitherDecode (HTTP.responseBody $ resp) of
    Left err -> pure . Left $ InvalidDiscoveryDocument uri (Text.pack err)
    Right doc -> pure $ Right doc
