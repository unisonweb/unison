{-# LANGUAGE DeriveAnyClass #-}

module Unison.Auth.CredentialManager
  ( saveCredentials,
    CredentialManager,
    newCredentialManager,
    getCredentials,
  )
where

import Unison.Auth.CredentialFile
import Unison.Auth.Types
import Unison.Prelude
import Unison.Share.Types (CodeserverId)
import qualified UnliftIO

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
getCredentials (CredentialManager credsVar) aud = do
  creds <- UnliftIO.readMVar credsVar
  pure $ getCodeserverCredentials aud creds

newCredentialManager :: (MonadIO m) => m CredentialManager
newCredentialManager = do
  credentials <- atomicallyModifyCredentialsFile id
  credentialsVar <- UnliftIO.newMVar credentials
  pure (CredentialManager credentialsVar)
