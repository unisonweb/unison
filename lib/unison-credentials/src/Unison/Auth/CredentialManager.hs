module Unison.Auth.CredentialManager
  ( saveCredentials,
    CredentialManager,
    globalCredentialManager,
    newCredentialManager,
    getCodeserverCredentials,
    getOrCreatePersonalKey,
    isExpired,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Monad.Catch (MonadMask)
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
--
-- It's thread-safe and safe for use across multiple UCM clients.
--
-- __Note__: Currently the in-memory cache is _not_ updated if a different UCM updates the credentials file, however
--           this shouldn't pose any problems, since auth will still be refreshed if we encounter any auth failures on
--           requests.
data CredentialManager = CredentialManager
  { credsVar :: MVar (Maybe Credentials),
    file :: FilePath
  }

-- | A global CredentialManager instance/singleton.
globalCredentialManager :: CredentialManager
globalCredentialManager = unsafePerformIO $ newCredentialManager Nothing
{-# NOINLINE globalCredentialManager #-}

-- | Fetches the user's personal key from the active profile, if it exists.
-- Otherwise it creates a new personal key, saves it to the active profile, and returns it.
getOrCreatePersonalKey :: CredentialManager -> IO PersonalPrivateKey
getOrCreatePersonalKey credMan = do
  modifyCredentials credMan \creds@(Credentials {activeProfile, personalKeys}) ->
    case Map.lookup activeProfile personalKeys of
      Just pk -> pure (creds, pk)
      Nothing -> do
        pk <- generatePersonalKey
        pure (creds {personalKeys = Map.insert activeProfile pk personalKeys}, pk)

-- | Saves credentials to the active profile.
saveCredentials :: CredentialManager -> CodeserverId -> CodeserverCredentials -> IO ()
saveCredentials credManager aud creds = do
  modifyCredentials credManager $ pure . (,()) . setCodeserverCredentials aud creds

-- | Atomically update the credential storage file, and update the in-memory cache.
modifyCredentials :: (MonadMask m, UnliftIO.MonadUnliftIO m) => CredentialManager -> (Credentials -> m (Credentials, r)) -> m r
modifyCredentials (CredentialManager {credsVar, file}) f =
  UnliftIO.modifyMVar credsVar . const $
    first pure <$> CF.atomicallyModifyCredentialsFile (fmap (\(creds', r') -> (creds', (creds', r'))) . f) file

readCredentials :: CredentialManager -> IO Credentials
readCredentials (CredentialManager {credsVar, file}) =
  modifyMVar credsVar $ \case
    Just creds -> pure (pure creds, creds)
    Nothing -> do
      creds <- CF.atomicallyModifyCredentialsFile (\c -> pure (c, c)) file
      pure (pure creds, creds)

getCodeserverCredentials :: CredentialManager -> CodeserverId -> IO (Either CredentialFailure CodeserverCredentials)
getCodeserverCredentials credMan aud = runExceptT do
  creds <- lift $ readCredentials credMan
  codeserverCreds <- except (Auth.getCodeserverCredentials aud creds)
  lift (isExpired codeserverCreds) >>= \case
    True -> throwE (ReauthRequired aud)
    False -> pure codeserverCreds

newCredentialManager :: Maybe FilePath -> IO CredentialManager
newCredentialManager mfile = do
  file <- maybe CF.getCredentialJSONFilePath pure mfile
  credentials <- CF.atomicallyModifyCredentialsFile (\c -> pure (c, c)) file
  credsVar <- newMVar $ pure credentials
  pure CredentialManager {credsVar, file}

-- | Checks whether CodeserverCredentials are expired.
isExpired :: CodeserverCredentials -> IO Bool
isExpired CodeserverCredentials {fetchTime, tokens = Tokens {expiresIn}} = do
  now <- getCurrentTime
  let expTime = addUTCTime expiresIn fetchTime
  let remainingTime = diffUTCTime expTime now
  let threshold = expiresIn * 0.1
  pure (threshold >= remainingTime)
