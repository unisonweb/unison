module Unison.Auth.UserInfo where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Unison.Auth.Types
import Unison.Prelude

-- | Get user info for an authenticated user.
getUserInfo :: (MonadIO m) => DiscoveryDoc -> AccessToken -> m (Either CredentialFailure UserInfo)
getUserInfo (DiscoveryDoc {userInfoEndpoint}) accessToken = liftIO $ do
  unauthenticatedHttpClient <- HTTP.getGlobalManager
  req <- HTTP.requestFromURI userInfoEndpoint <&> HTTP.applyBearerAuth (Text.encodeUtf8 accessToken)
  resp <- HTTP.httpLbs req unauthenticatedHttpClient
  case decodeUserInfo (HTTP.responseBody resp) of
    Left err -> pure . Left $ FailedToFetchUserInfo userInfoEndpoint (Text.pack err)
    Right userInfo -> pure . Right $ userInfo

decodeUserInfo :: BL.ByteString -> Either String UserInfo
decodeUserInfo bs = do
  obj <- Aeson.eitherDecode bs
  flip Aeson.parseEither obj $
    Aeson.withObject "UserInfo" $ \o -> do
      userId <- o Aeson..: "sub"
      name <- o Aeson..:? "name"
      handle <- o Aeson..: "handle"
      pure
        UserInfo
          { userId,
            name,
            handle
          }
