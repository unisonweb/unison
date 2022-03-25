module Unison.Auth.Discovery where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP
import Network.URI
import qualified Network.URI as URI
import Unison.Auth.Types
import Unison.Prelude
import qualified UnliftIO

discoveryURI :: Host -> Either CredentialFailure URI
discoveryURI (Host host) =
  maybeToEither (InvalidHost (Host host) "Invalid URI") (URI.parseURI (Text.unpack host)) <&> \host ->
    host {uriPath = "/.well-known/openid-configuration"}

discoveryForHost :: MonadIO m => HTTP.Manager -> Host -> m (Either CredentialFailure DiscoveryDoc)
discoveryForHost httpClient host = liftIO . UnliftIO.try @_ @CredentialFailure $ do
  uri <- UnliftIO.fromEither $ discoveryURI host
  req <- HTTP.requestFromURI uri
  resp <- HTTP.httpLbs req httpClient
  case Aeson.eitherDecode (HTTP.responseBody $ resp) of
    Left err -> UnliftIO.throwIO $ InvalidDiscoveryDocument uri (Text.pack err)
    Right doc -> pure doc
