module Unison.Auth.Tokens where

import Control.Monad.Except
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BSC
import Data.Text qualified as Text
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Network.HTTP.Types qualified as Network
import Unison.Auth.CredentialManager
import Unison.Auth.Discovery (fetchDiscoveryDoc)
import Unison.Auth.Types
import Unison.Auth.UserInfo (getUserInfo)
import Unison.Prelude
import Unison.Share.Types (CodeserverId)
import UnliftIO qualified

-- | Given a 'CodeserverId', provide a valid 'AccessToken' for the associated host.
-- The TokenProvider may automatically refresh access tokens if we have a refresh token.
type TokenProvider = CodeserverId -> IO (Either CredentialFailure AccessToken)

-- | Creates a 'TokenProvider' using the given 'CredentialManager'
newTokenProvider :: CredentialManager -> TokenProvider
newTokenProvider manager host = UnliftIO.try @_ @CredentialFailure $ do
  creds@CodeserverCredentials {tokens, discoveryURI} <- throwEitherM $ getCredentials manager host
  let Tokens {accessToken = currentAccessToken} = tokens
  expired <- isExpired creds
  if expired
    then do
      discoveryDoc <- throwEitherM $ fetchDiscoveryDoc discoveryURI
      fetchTime <- getCurrentTime
      newTokens@(Tokens {accessToken = newAccessToken}) <- throwEitherM $ performTokenRefresh discoveryDoc tokens
      userInfo <- throwEitherM $ getUserInfo discoveryDoc newAccessToken
      saveCredentials manager host (codeserverCredentials discoveryURI newTokens fetchTime userInfo)
      pure $ newAccessToken
    else pure currentAccessToken

-- | Don't yet support automatically refreshing tokens.
--
-- Specification: https://datatracker.ietf.org/doc/html/rfc6749#section-6
performTokenRefresh :: (MonadIO m) => DiscoveryDoc -> Tokens -> m (Either CredentialFailure Tokens)
performTokenRefresh DiscoveryDoc {tokenEndpoint} (Tokens {refreshToken = currentRefreshToken}) = runExceptT $
  case currentRefreshToken of
    Nothing ->
      throwError $ (RefreshFailure . Text.pack $ "Unable to refresh authentication, please run auth.login and try again.")
    Just rt -> do
      req <- liftIO $ HTTP.requestFromURI tokenEndpoint
      let addFormData =
            HTTP.urlEncodedBody
              [ ("grant_type", "refresh_token"),
                ("refresh_token", BSC.pack . Text.unpack $ rt)
              ]
      let fullReq = addFormData $ req {HTTP.method = "POST", HTTP.requestHeaders = [("Accept", "application/json")]}
      unauthenticatedHttpClient <- liftIO $ HTTP.getGlobalManager
      resp <- liftIO $ HTTP.httpLbs fullReq unauthenticatedHttpClient
      newTokens <- case HTTP.responseStatus resp of
        status
          | status < Network.status300 -> do
              let respBytes = HTTP.responseBody resp
              case Aeson.eitherDecode @Tokens respBytes of
                Left err -> throwError (InvalidTokenResponse tokenEndpoint (Text.pack err))
                Right a -> pure a
          | otherwise -> throwError $ (InvalidTokenResponse tokenEndpoint $ "Received " <> tShow status <> " response from token endpoint")
      -- According to the spec, servers may or may not update the refresh token itself.
      -- If updated we need to replace it, if not updated we keep the existing one.
      pure $ newTokens {refreshToken = refreshToken newTokens <|> currentRefreshToken}
