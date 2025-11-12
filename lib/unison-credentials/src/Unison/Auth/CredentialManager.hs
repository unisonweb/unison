{-# LANGUAGE DeriveAnyClass #-}

module Unison.Auth.CredentialManager
  ( saveCredentials,
    CredentialManager,
    newCredentialManager,
    getCodeserverCredentials,
    getOrCreatePersonalKey,
    isExpired,
  )
where

import Control.Monad.Trans.Except
import Data.Map qualified as Map
import Data.Time.Clock (addUTCTime, diffUTCTime, getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)
import Unison.Auth.CredentialFile qualified as CF
import Unison.Auth.PersonalKey (PersonalPrivateKey, generatePersonalKey)
import Unison.Auth.Types hiding (getCodeserverCredentials)
import Unison.Auth.Types qualified as Auth
import Unison.Prelude
import Unison.Share.Types (CodeserverId)
import UnliftIO qualified

-- | A 'CredentialManager' knows how to load, save, and cache credentials.
-- It's thread-safe and safe for use across multiple UCM clients.
-- Note: Currently the in-memory cache is _not_ updated if a different UCM updates
-- the credentials file, however this shouldn't pose any problems, since auth will still
-- be refreshed if we encounter any auth failures on requests.
newtype CredentialManager = CredentialManager (UnliftIO.MVar (Maybe Credentials {- Credentials may or may not be initialized -}))

-- | A global CredentialManager instance/singleton.
globalCredentialsManager :: CredentialManager
globalCredentialsManager = unsafePerformIO do
  CredentialManager <$> UnliftIO.newMVar Nothing
{-# NOINLINE globalCredentialsManager #-}

-- | Fetches the user's personal key from the active profile, if it exists.
-- Otherwise it creates a new personal key, saves it to the active profile, and returns it.
getOrCreatePersonalKey :: (MonadUnliftIO m) => CredentialManager -> m PersonalPrivateKey
getOrCreatePersonalKey credMan = do
  modifyCredentials credMan \creds@(Credentials {activeProfile, personalKeys}) -> do
    case Map.lookup activeProfile personalKeys of
      Just pk -> pure (creds, pk)
      Nothing -> do
        pk <- generatePersonalKey
        pure (creds {personalKeys = Map.insert activeProfile pk personalKeys}, pk)

-- | Saves credentials to the active profile.
saveCredentials :: (UnliftIO.MonadUnliftIO m) => CredentialManager -> CodeserverId -> CodeserverCredentials -> m ()
saveCredentials credManager aud creds = do
  void . modifyCredentials credManager $ \cf -> pure (setCodeserverCredentials aud creds cf, ())

-- | Atomically update the credential storage file, and update the in-memory cache.
modifyCredentials :: (UnliftIO.MonadUnliftIO m) => CredentialManager -> (Credentials -> m (Credentials, r)) -> m r
modifyCredentials (CredentialManager credsVar) f = do
  UnliftIO.modifyMVar credsVar $ \_ -> do
    (creds, r) <- CF.atomicallyModifyCredentialsFile (f >=> \(creds', r') -> pure (creds', (creds', r')))
    pure (Just creds, r)

readCredentials :: (UnliftIO.MonadUnliftIO m) => CredentialManager -> m Credentials
readCredentials (CredentialManager credsVar) = do
  UnliftIO.modifyMVar credsVar $ \mayCreds -> case mayCreds of
    Just creds -> pure (mayCreds, creds)
    Nothing -> do
      creds <- CF.atomicallyModifyCredentialsFile \c -> pure (c, c)
      pure (Just creds, creds)

getCodeserverCredentials :: (MonadIO m) => CredentialManager -> CodeserverId -> m (Either CredentialFailure CodeserverCredentials)
getCodeserverCredentials credMan aud = runExceptT do
  creds <- liftIO $ readCredentials credMan
  codeserverCreds <- except (Auth.getCodeserverCredentials aud creds)
  lift (isExpired codeserverCreds) >>= \case
    True -> throwE (ReauthRequired aud)
    False -> pure codeserverCreds

newCredentialManager :: CredentialManager
newCredentialManager = globalCredentialsManager

-- | Checks whether CodeserverCredentials are expired.
isExpired :: (MonadIO m) => CodeserverCredentials -> m Bool
isExpired CodeserverCredentials {fetchTime, tokens = Tokens {expiresIn}} = liftIO do
  now <- getCurrentTime
  let expTime = addUTCTime expiresIn fetchTime
  let remainingTime = diffUTCTime expTime now
  let threshold = expiresIn * 0.1
  pure (threshold >= remainingTime)
