{-# LANGUAGE NumericUnderscores #-}

module Unison.Auth.CredentialFile (atomicallyModifyCredentialsFile) where

import qualified Data.Aeson as Aeson
import System.FilePath (takeDirectory, (</>))
import System.IO.LockFile
import Unison.Auth.Types
import qualified Unison.Debug as Debug
import Unison.Prelude
import UnliftIO.Directory

lockfileConfig :: LockingParameters
lockfileConfig =
  LockingParameters
    { retryToAcquireLock = NumberOfTimes 3,
      sleepBetweenRetries = sleepTimeMicros
    }
  where
    sleepTimeMicros = 100_000 -- 100ms

getCredentialJSONFilePath :: (MonadIO m) => m FilePath
getCredentialJSONFilePath = do
  unisonDataDir <- getXdgDirectory XdgData "unisonlanguage"
  pure (unisonDataDir </> "credentials.json")

-- | Atomically update the credential storage file.
-- Creates an empty file automatically if one doesn't exist.
atomicallyModifyCredentialsFile :: (MonadIO m) => (Credentials -> Credentials) -> m Credentials
atomicallyModifyCredentialsFile f = liftIO $ do
  credentialJSONPath <- getCredentialJSONFilePath
  doesFileExist credentialJSONPath >>= \case
    True -> pure ()
    False -> do
      createDirectoryIfMissing True $ takeDirectory credentialJSONPath
      Aeson.encodeFile credentialJSONPath emptyCredentials

  withLockFile lockfileConfig (withLockExt credentialJSONPath) $ do
    credentials <-
      Aeson.eitherDecodeFileStrict credentialJSONPath >>= \case
        -- If something goes wrong, just wipe the credentials file so we're in a clean slate.
        -- In the worst case the user will simply need to log in again.
        Left err -> do
          Debug.debugM Debug.Auth "Error decoding credentials file" err
          Aeson.encodeFile credentialJSONPath emptyCredentials
          pure emptyCredentials
        Right creds -> pure creds
    let newCredentials = f credentials
    when (newCredentials /= credentials) $ do
      Aeson.encodeFile credentialJSONPath $ newCredentials
    pure newCredentials
