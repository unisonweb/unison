module Unison.Auth.HTTPClient where

import qualified Data.Text.Encoding as Text
import Network.HTTP.Client (Request)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Unison.Auth.CredentialManager (CredentialManager, getHostAudience, newCredentialManager)
import Unison.Auth.Tokens (TokenProvider, newTokenProvider)
import Unison.Auth.Types
import Unison.Codebase.Editor.Command (UCMVersion)
import Unison.Prelude
import qualified Unison.Util.HTTP as HTTP

-- | Returns a new http manager which applies the appropriate Authorization header to
-- any hosts our UCM is authenticated with.
newAuthorizedHTTPClient :: MonadIO m => UCMVersion -> m HTTP.Manager
newAuthorizedHTTPClient ucmVersion = liftIO $ do
  credManager <- newCredentialManager
  let tokenProvider = newTokenProvider credManager
  let managerSettings =
        HTTP.tlsManagerSettings
          & HTTP.addRequestMiddleware (authMiddleware credManager tokenProvider)
          & HTTP.setUserAgent (HTTP.ucmUserAgent ucmVersion)
  HTTP.newTlsManagerWith managerSettings

-- | Adds Bearer tokens to requests according to their host.
-- If a CredentialFailure occurs (failure to refresh a token), auth is simply omitted,
-- and the request is likely to trigger a 401 response which the caller can detect and initiate a re-auth.
--
-- If a host isn't associated with any credentials auth is omitted.
authMiddleware :: CredentialManager -> TokenProvider -> (Request -> IO Request)
authMiddleware credMan tokenProvider req = do
  mayAud <- getHostAudience credMan (Host . Text.decodeUtf8 $ HTTP.host req)
  case mayAud of
    Nothing -> pure req
    Just aud -> do
      tokenProvider aud >>= \case
        Right token -> pure $ HTTP.applyBearerAuth (Text.encodeUtf8 token) req
        Left _ -> pure req
