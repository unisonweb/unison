module Unison.Auth.Discovery where

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Network.URI
import Unison.Auth.Types
import Unison.Prelude
import Unison.Share.Types (CodeserverURI (..), codeserverToURI)
import UnliftIO qualified

discoveryURIForCodeserver :: CodeserverURI -> URI
discoveryURIForCodeserver cs =
  let uri = codeserverToURI cs
   in uri {uriPath = uriPath uri <> "/.well-known/openid-configuration"}

fetchDiscoveryDoc :: (MonadIO m) => URI -> m (Either CredentialFailure DiscoveryDoc)
fetchDiscoveryDoc discoveryURI = liftIO . UnliftIO.try @_ @CredentialFailure $ do
  unauthenticatedHttpClient <- HTTP.getGlobalManager
  req <- HTTP.requestFromURI discoveryURI
  resp <- HTTP.httpLbs req unauthenticatedHttpClient
  case Aeson.eitherDecode (HTTP.responseBody $ resp) of
    Left err -> UnliftIO.throwIO $ InvalidDiscoveryDocument discoveryURI (Text.pack err)
    Right doc -> pure doc
