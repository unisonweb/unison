module Unison.Auth.Tokens where

import Control.Monad.Except
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types as Network
import Network.URI (URI)
import Unison.Auth.CredentialManager
import Unison.Auth.Discovery (fetchDiscoveryDoc)
import Unison.Auth.Types
import Unison.CommandLine.InputPattern (patternName)
import qualified Unison.CommandLine.InputPatterns as IP
import Unison.Prelude
import Unison.Share.Types (CodeserverId)
import qualified UnliftIO
import UnliftIO.Exception
import Web.JWT
import qualified Web.JWT as JWT

-- | Checks whether a JWT access token is expired.
isExpired :: MonadIO m => AccessToken -> m Bool
isExpired accessToken = liftIO do
  jwt <- JWT.decode accessToken `whenNothing` (throwIO $ InvalidJWT "Failed to decode JWT")
  now <- getPOSIXTime
  expDate <- JWT.exp (claims jwt) `whenNothing` (throwIO $ InvalidJWT "Missing exp claim on JWT")
  let expiry = JWT.secondsSinceEpoch expDate
  pure (now >= expiry)

-- | Given a 'CodeserverId', provide a valid 'AccessToken' for the associated host.
-- The TokenProvider may automatically refresh access tokens if we have a refresh token.
type TokenProvider = CodeserverId -> IO (Either CredentialFailure AccessToken)

-- | Creates a 'TokenProvider' using the given 'CredentialManager'
newTokenProvider :: CredentialManager -> TokenProvider
newTokenProvider manager host = UnliftIO.try @_ @CredentialFailure $ do
  CodeserverCredentials {tokens, discoveryURI} <- throwEitherM $ getCredentials manager host
  let Tokens {accessToken = currentAccessToken} = tokens
  expired <- isExpired currentAccessToken
  if expired
    then do
      newTokens@(Tokens {accessToken = newAccessToken}) <- throwEitherM $ performTokenRefresh discoveryURI tokens
      saveCredentials manager host (codeserverCredentials discoveryURI newTokens)
      pure $ newAccessToken
    else pure currentAccessToken

-- | Don't yet support automatically refreshing tokens.
--
-- Specification: https://datatracker.ietf.org/doc/html/rfc6749#section-6
performTokenRefresh :: MonadIO m => URI -> Tokens -> m (Either CredentialFailure Tokens)
performTokenRefresh discoveryURI (Tokens {refreshToken = currentRefreshToken}) = runExceptT $
  case currentRefreshToken of
    Nothing ->
      throwError $ (RefreshFailure . Text.pack $ "Unable to refresh authentication, please run " <> patternName IP.authLogin <> " and try again.")
    Just rt -> do
      DiscoveryDoc {tokenEndpoint} <- ExceptT $ fetchDiscoveryDoc discoveryURI
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
