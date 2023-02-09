module Unison.Auth.UserInfo where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Unison.Auth.Types
import Unison.Prelude

-- | Get user info for an authenticated user.
getUserInfo :: MonadIO m => DiscoveryDoc -> AccessToken -> m (Either CredentialFailure UserInfo)
getUserInfo (DiscoveryDoc {userInfoEndpoint}) accessToken = liftIO $ do
  unauthenticatedHttpClient <- HTTP.getGlobalManager
  req <- HTTP.requestFromURI userInfoEndpoint <&> HTTP.applyBearerAuth (Text.encodeUtf8 accessToken)
  resp <- HTTP.httpLbs req unauthenticatedHttpClient
  case Aeson.eitherDecode (HTTP.responseBody resp) of
    Left err -> pure . Left $ FailedToFetchUserInfo userInfoEndpoint (Text.pack err)
    Right userInfo -> pure . Right $ userInfo
