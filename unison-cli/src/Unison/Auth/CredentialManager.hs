{-# LANGUAGE DeriveAnyClass #-}

module Unison.Auth.CredentialManager
  ( saveCredentials,
    CredentialManager,
    newCredentialManager,
    getCredentials,
    isExpired,
  )
where

import Control.Monad.Trans.Except
import Data.Time.Clock (addUTCTime, diffUTCTime, getCurrentTime)
import Unison.Auth.CredentialFile
import Unison.Auth.Types
import Unison.Prelude
import Unison.Share.Types (CodeserverId)
import UnliftIO qualified

-- | A 'CredentialManager' knows how to load, save, and cache credentials.
-- It's thread-safe and safe for use across multiple UCM clients.
-- Note: Currently the in-memory cache is _not_ updated if a different UCM updates
-- the credentials file, however this shouldn't pose any problems, since auth will still
-- be refreshed if we encounter any auth failures on requests.
newtype CredentialManager = CredentialManager (UnliftIO.MVar Credentials)

-- | Saves credentials to the active profile.
saveCredentials :: (UnliftIO.MonadUnliftIO m) => CredentialManager -> CodeserverId -> CodeserverCredentials -> m ()
saveCredentials credManager aud creds = do
  void . modifyCredentials credManager $ setCodeserverCredentials aud creds

-- | Atomically update the credential storage file, and update the in-memory cache.
modifyCredentials :: (UnliftIO.MonadUnliftIO m) => CredentialManager -> (Credentials -> Credentials) -> m Credentials
modifyCredentials (CredentialManager credsVar) f = do
  UnliftIO.modifyMVar credsVar $ \_ -> do
    newCreds <- atomicallyModifyCredentialsFile f
    pure (newCreds, newCreds)

getCredentials :: (MonadIO m) => CredentialManager -> CodeserverId -> m (Either CredentialFailure CodeserverCredentials)
getCredentials (CredentialManager credsVar) aud = runExceptT do
  creds <- lift (UnliftIO.readMVar credsVar)
  codeserverCreds <- except (getCodeserverCredentials aud creds)
  lift (isExpired codeserverCreds) >>= \case
    True -> throwE (ReauthRequired aud)
    False -> pure codeserverCreds

newCredentialManager :: (MonadIO m) => m CredentialManager
newCredentialManager = do
  credentials <- atomicallyModifyCredentialsFile id
  credentialsVar <- UnliftIO.newMVar credentials
  pure (CredentialManager credentialsVar)

-- | Checks whether CodeserverCredentials are expired.
isExpired :: (MonadIO m) => CodeserverCredentials -> m Bool
isExpired CodeserverCredentials {fetchTime, tokens = Tokens {expiresIn}} = liftIO do
  now <- getCurrentTime
  let expTime = addUTCTime expiresIn fetchTime
  let remainingTime = diffUTCTime expTime now
  let threshold = expiresIn * 0.1
  pure (threshold >= remainingTime)
