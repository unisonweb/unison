module Unison.Auth.Discovery where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP
import Network.URI
import Unison.Auth.Types
import Unison.Prelude
import Unison.Share.Types (CodeserverURI (..))
import qualified UnliftIO

discoveryURI :: CodeserverURI -> URI
discoveryURI (CodeserverURI uri) =
  uri {uriPath = uriPath uri <> "/.well-known/openid-configuration"}

discoveryForCodeserver :: MonadIO m => HTTP.Manager -> CodeserverURI -> m (Either CredentialFailure DiscoveryDoc)
discoveryForCodeserver httpClient host = liftIO . UnliftIO.try @_ @CredentialFailure $ do
  let uri = discoveryURI host
  req <- HTTP.requestFromURI uri
  resp <- HTTP.httpLbs req httpClient
  case Aeson.eitherDecode (HTTP.responseBody $ resp) of
    Left err -> UnliftIO.throwIO $ InvalidDiscoveryDocument uri (Text.pack err)
    Right doc -> pure doc
