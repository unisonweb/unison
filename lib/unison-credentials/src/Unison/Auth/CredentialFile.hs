{-# LANGUAGE NumericUnderscores #-}

module Unison.Auth.CredentialFile
  ( atomicallyModifyCredentialsFile,
    getCredentialJSONFilePath,
  )
where

import Control.Monad.Catch (MonadMask)
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

getCredentialJSONFilePath :: IO FilePath
getCredentialJSONFilePath = do
  unisonDataDir <- getXdgDirectory XdgData "unisonlanguage"
  pure (unisonDataDir </> "credentials.json")

-- | Atomically update the credential storage file.
-- Creates an empty file automatically if one doesn't exist.
atomicallyModifyCredentialsFile :: (MonadMask m, MonadIO m) => (Credentials -> m (Credentials, r)) -> FilePath -> m r
atomicallyModifyCredentialsFile f credentialJSONPath = do
  liftIO $
    doesFileExist credentialJSONPath >>= \case
      True -> pure ()
      False -> do
        createDirectoryIfMissing True $ takeDirectory credentialJSONPath
        Aeson.encodeFile credentialJSONPath emptyCredentials

  withLockFile lockfileConfig (withLockExt credentialJSONPath) do
    credentials <-
      liftIO $
        Aeson.eitherDecodeFileStrict credentialJSONPath >>= \case
          -- If something goes wrong, just wipe the credentials file so we're in a clean slate.
          -- In the worst case the user will simply need to log in again.
          Left _err -> do
            Aeson.encodeFile credentialJSONPath emptyCredentials
            pure emptyCredentials
          Right creds -> pure creds
    (newCredentials, r) <- f credentials
    liftIO . when (newCredentials /= credentials) $
      Aeson.encodeFile credentialJSONPath newCredentials
    pure r
