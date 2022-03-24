{-# LANGUAGE DeriveAnyClass #-}

module Unison.Auth.CredentialManager
  ( saveTokens,
    CredentialManager,
    newCredentialManager,
    getTokens,
    getHostAudience,
  )
where

import qualified Data.Map as Map
import Unison.Auth.CredentialFile
import Unison.Auth.Types
import Unison.Prelude
import qualified UnliftIO

-- | A 'CredentialManager' knows how to load, save, and cache credentials.
newtype CredentialManager = CredentialManager (UnliftIO.MVar Credentials)

-- | Saves credentials for profile and sets that to the active profile.
saveTokens :: UnliftIO.MonadUnliftIO m => CredentialManager -> Audience -> Tokens -> m ()
saveTokens credManager aud tokens = do
  void . modifyCredentials credManager $ setActiveTokens aud tokens

-- | Atomically update the credential storage file, and update the in-memory cache.
modifyCredentials :: UnliftIO.MonadUnliftIO m => CredentialManager -> (Credentials -> Credentials) -> m Credentials
modifyCredentials (CredentialManager credsVar) f = do
  UnliftIO.modifyMVar credsVar $ \_ -> do
    newCreds <- atomicallyModifyCredentialsFile f
    pure (newCreds, newCreds)

getTokens :: MonadIO m => CredentialManager -> Audience -> m (Either CredentialFailure Tokens)
getTokens (CredentialManager credsVar) aud = do
  creds <- UnliftIO.readMVar credsVar
  pure $ getActiveTokens aud creds

getHostAudience :: MonadIO m => CredentialManager -> Host -> m (Maybe Audience)
getHostAudience (CredentialManager credsVar) host = do
  creds <- UnliftIO.readMVar credsVar
  pure $ Map.lookup host (hostMap creds)

newCredentialManager :: MonadIO m => m CredentialManager
newCredentialManager = do
  credentials <- atomicallyModifyCredentialsFile id
  credentialsVar <- UnliftIO.newMVar credentials
  pure (CredentialManager credentialsVar)
