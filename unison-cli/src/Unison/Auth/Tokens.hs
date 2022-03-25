module Unison.Auth.Tokens where

import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Unison.Auth.CredentialManager
import Unison.Auth.Types
import Unison.CommandLine.InputPattern (patternName)
import qualified Unison.CommandLine.InputPatterns as IP
import Unison.Prelude
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

-- | Given a 'Host', provide a valid 'AccessToken' for the associated host.
-- The TokenProvider may automatically refresh access tokens if we have a refresh token.
type TokenProvider = Host -> IO (Either CredentialFailure AccessToken)

-- | Creates a 'TokenProvider' using the given 'CredentialManager'
newTokenProvider :: CredentialManager -> TokenProvider
newTokenProvider manager host = UnliftIO.try @_ @CredentialFailure $ do
  tokens@(Tokens {accessToken}) <- throwEitherM $ getTokens manager host
  expired <- isExpired accessToken
  if expired
    then do
      newTokens@(Tokens {accessToken = newAccessToken}) <- throwEitherM $ refreshTokens manager host tokens
      saveTokens manager host newTokens
      pure $ newAccessToken
    else pure accessToken

-- | Don't yet support automatically refreshing tokens.
refreshTokens :: MonadIO m => CredentialManager -> Host -> Tokens -> m (Either CredentialFailure Tokens)
refreshTokens _manager _host _tokens =
  -- Refreshing tokens is currently unsupported.
  pure (Left (RefreshFailure . Text.pack $ "Unable to refresh authentication, please run " <> patternName IP.authLogin <> " and try again."))
