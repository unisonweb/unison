module Unison.Auth.Tokens where

import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Unison.Auth.Storage
import Unison.Auth.Types
import Unison.CommandLine.InputPattern (patternName)
import qualified Unison.CommandLine.InputPatterns as IP
import Unison.Prelude
import qualified UnliftIO
import UnliftIO.Exception
import Web.JWT
import qualified Web.JWT as JWT

-- | Checks whether a JWT access token is expired.
isExpired :: AccessToken -> IO Bool
isExpired accessToken = do
  jwt <- JWT.decode accessToken `whenNothing` (throwIO $ InvalidJWT "Failed to decode JWT")
  now <- getPOSIXTime
  expDate <- JWT.exp (claims jwt) `whenNothing` (throwIO $ InvalidJWT "Missing exp claim on JWT")
  let expiry = JWT.secondsSinceEpoch expDate
  pure (now >= expiry)

type TokenProvider = Audience -> IO (Either CredentialFailure AccessToken)

newTokenProvider :: CredentialManager -> TokenProvider
newTokenProvider manager aud = UnliftIO.try @_ @CredentialFailure $ do
  tokens@(Tokens {accessToken}) <- throwEitherM $ getTokens manager aud
  expired <- isExpired accessToken
  if expired
    then do
      newTokens@(Tokens {accessToken = newAccessToken}) <- throwEitherM $ refreshTokens manager aud tokens
      saveTokens manager aud newTokens
      pure $ newAccessToken
    else pure accessToken

-- | Currently don't support refreshing tokens.
refreshTokens :: CredentialManager -> Audience -> Tokens -> IO (Either CredentialFailure Tokens)
refreshTokens _manager _aud _tokens =
  -- Refreshing tokens is currently unsupported.
  pure (Left (RefreshFailure . Text.pack $ "Unable to refresh authentication, please run " <> patternName IP.authLogin <> " and try again."))
