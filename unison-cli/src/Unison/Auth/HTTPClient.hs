module Unison.Auth.HTTPClient (newAuthorizedHTTPClient, AuthorizedHttpClient (..)) where

import qualified Data.Text.Encoding as Text
import Network.HTTP.Client (Request)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Unison.Auth.CredentialManager (CredentialManager)
import Unison.Auth.Tokens (TokenProvider, newTokenProvider)
import Unison.Auth.Types
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import Unison.Prelude
import qualified Unison.Util.HTTP as HTTP

-- | Newtype to delineate HTTP Managers with access-token logic.
newtype AuthorizedHttpClient = AuthorizedHttpClient HTTP.Manager

-- | Returns a new http manager which applies the appropriate Authorization header to
-- any hosts our UCM is authenticated with.
newAuthorizedHTTPClient :: MonadIO m => CredentialManager -> UCMVersion -> m AuthorizedHttpClient
newAuthorizedHTTPClient credsMan ucmVersion = liftIO $ do
  let tokenProvider = newTokenProvider credsMan
  let managerSettings =
        HTTP.tlsManagerSettings
          & HTTP.addRequestMiddleware (authMiddleware tokenProvider)
          & HTTP.setUserAgent (HTTP.ucmUserAgent ucmVersion)
  AuthorizedHttpClient <$> HTTP.newTlsManagerWith managerSettings

-- | Adds Bearer tokens to requests according to their host.
-- If a CredentialFailure occurs (failure to refresh a token), auth is simply omitted,
-- and the request is likely to trigger a 401 response which the caller can detect and initiate a re-auth.
--
-- If a host isn't associated with any credentials auth is omitted.
authMiddleware :: TokenProvider -> (Request -> IO Request)
authMiddleware tokenProvider req = do
  result <- tokenProvider (Host . Text.decodeUtf8 $ HTTP.host req)
  case result of
    Right token -> pure $ HTTP.applyBearerAuth (Text.encodeUtf8 token) req
    Left _ -> pure req
