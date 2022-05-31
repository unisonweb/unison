{-# LANGUAGE DeriveAnyClass #-}

module Unison.Auth.CredentialManager
  ( saveTokens,
    CredentialManager,
    newCredentialManager,
    getTokens,
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
saveTokens :: UnliftIO.MonadUnliftIO m => CredentialManager -> CodeserverId -> Tokens -> m ()
saveTokens credManager aud tokens = do
  void . modifyCredentials credManager $ setActiveTokens aud tokens

-- | Atomically update the credential storage file, and update the in-memory cache.
modifyCredentials :: UnliftIO.MonadUnliftIO m => CredentialManager -> (Credentials -> Credentials) -> m Credentials
modifyCredentials (CredentialManager credsVar) f = do
  UnliftIO.modifyMVar credsVar $ \_ -> do
    newCreds <- atomicallyModifyCredentialsFile f
    pure (newCreds, newCreds)

getTokens :: MonadIO m => CredentialManager -> CodeserverId -> m (Either CredentialFailure Tokens)
getTokens (CredentialManager credsVar) aud = do
  creds <- UnliftIO.readMVar credsVar
  pure $ getActiveTokens aud creds

newCredentialManager :: MonadIO m => m CredentialManager
newCredentialManager = do
  credentials <- atomicallyModifyCredentialsFile id
  credentialsVar <- UnliftIO.newMVar credentials
  pure (CredentialManager credentialsVar)
