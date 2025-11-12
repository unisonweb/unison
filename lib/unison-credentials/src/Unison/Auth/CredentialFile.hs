{-# LANGUAGE NumericUnderscores #-}

module Unison.Auth.CredentialFile (atomicallyModifyCredentialsFile) where

import Data.Aeson qualified as Aeson
import System.FilePath (takeDirectory, (</>))
import System.IO.LockFile
import Unison.Auth.Types
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
atomicallyModifyCredentialsFile :: (MonadUnliftIO m) => (Credentials -> m (Credentials, r)) -> m r
atomicallyModifyCredentialsFile f = do
  credentialJSONPath <- liftIO $ getCredentialJSONFilePath
  liftIO (doesFileExist credentialJSONPath) >>= \case
    True -> pure ()
    False -> liftIO $ do
      createDirectoryIfMissing True $ takeDirectory credentialJSONPath
      Aeson.encodeFile credentialJSONPath emptyCredentials

  toIO <- askRunInIO
  liftIO $ withLockFile lockfileConfig (withLockExt credentialJSONPath) $ toIO $ do
    credentials <-
      liftIO (Aeson.eitherDecodeFileStrict credentialJSONPath) >>= \case
        -- If something goes wrong, just wipe the credentials file so we're in a clean slate.
        -- In the worst case the user will simply need to log in again.
        Left _err -> do
          liftIO $ Aeson.encodeFile credentialJSONPath emptyCredentials
          pure emptyCredentials
        Right creds -> pure creds
    (newCredentials, r) <- f credentials
    when (newCredentials /= credentials) $ do
      liftIO $ Aeson.encodeFile credentialJSONPath newCredentials
    pure r
