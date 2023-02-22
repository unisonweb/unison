module Unison.Auth.HTTPClient (newAuthenticatedHTTPClient, AuthenticatedHttpClient (..)) where

import qualified Data.Text.Encoding as Text
import Network.HTTP.Client (Request)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Unison.Auth.Tokens (TokenProvider)
import Unison.Codebase.Editor.UCMVersion (UCMVersion)
import Unison.Prelude
import Unison.Share.Types (codeserverIdFromURI)
import qualified Unison.Util.HTTP as HTTP

-- | Newtype to delineate HTTP Managers with access-token logic.
newtype AuthenticatedHttpClient = AuthenticatedHttpClient HTTP.Manager

-- | Returns a new http manager which applies the appropriate Authorization header to
-- any hosts our UCM is authenticated with.
newAuthenticatedHTTPClient :: (MonadIO m) => TokenProvider -> UCMVersion -> m AuthenticatedHttpClient
newAuthenticatedHTTPClient tokenProvider ucmVersion = liftIO $ do
  let managerSettings =
        HTTP.tlsManagerSettings
          & HTTP.addRequestMiddleware (authMiddleware tokenProvider)
          & HTTP.setUserAgent (HTTP.ucmUserAgent ucmVersion)
  AuthenticatedHttpClient <$> HTTP.newTlsManagerWith managerSettings

-- | Adds Bearer tokens to requests according to their host.
-- If a CredentialFailure occurs (failure to refresh a token), auth is simply omitted,
-- and the request is likely to trigger a 401 response which the caller can detect and initiate a re-auth.
--
-- If a host isn't associated with any credentials auth is omitted.
authMiddleware :: TokenProvider -> (Request -> IO Request)
authMiddleware tokenProvider req = do
  -- The http manager "may run this function multiple times" when preparing a request.
  -- We may wish to look into a better way to attach auth to our requests in middleware, but
  -- this is a simple fix that works for now.
  -- https://github.com/snoyberg/http-client/issues/350
  case Prelude.lookup ("Authorization") (HTTP.requestHeaders req) of
    Just _ -> pure req
    Nothing -> do
      case codeserverIdFromURI $ (HTTP.getUri req) of
        -- If we can't identify an appropriate codeserver we pass it through without any auth.
        Left _ -> pure req
        Right codeserverHost -> do
          tokenProvider codeserverHost >>= \case
            Right token -> do
              let newReq = HTTP.applyBearerAuth (Text.encodeUtf8 token) req
              pure newReq
            Left _err -> do
              pure req
