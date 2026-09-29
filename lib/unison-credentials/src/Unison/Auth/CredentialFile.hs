{-# LANGUAGE CPP #-}
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

#if !defined(mingw32_HOST_OS)
import System.Posix.Files qualified as Posix
#endif

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

-- | Restrict the credentials file to be readable/writable by the owner only.
-- The file contains sensitive data, so other users on the system should not
-- be able to read it. No-op on Windows, where POSIX modes don't apply.
restrictCredentialsFilePermissions :: FilePath -> IO ()
#if defined(mingw32_HOST_OS)
restrictCredentialsFilePermissions _ = pure ()
#else
restrictCredentialsFilePermissions path = Posix.setFileMode path 0o600
#endif

writeCredentialsFile :: FilePath -> Credentials -> IO ()
writeCredentialsFile path creds = do
  Aeson.encodeFile path creds
  restrictCredentialsFilePermissions path

-- | Atomically update the credential storage file.
-- Creates an empty file automatically if one doesn't exist.
atomicallyModifyCredentialsFile :: (MonadMask m, MonadIO m) => (Credentials -> m (Credentials, r)) -> FilePath -> m r
atomicallyModifyCredentialsFile f credentialJSONPath = do
  liftIO $
    doesFileExist credentialJSONPath >>= \case
      -- Re-apply permissions in case a pre-existing file was created with the
      -- old behavior (world-readable) before this safeguard was in place.
      True -> restrictCredentialsFilePermissions credentialJSONPath
      False -> do
        createDirectoryIfMissing True $ takeDirectory credentialJSONPath
        writeCredentialsFile credentialJSONPath emptyCredentials

  withLockFile lockfileConfig (withLockExt credentialJSONPath) do
    credentials <-
      liftIO $
        Aeson.eitherDecodeFileStrict credentialJSONPath >>= \case
          -- If something goes wrong, just wipe the credentials file so we're in a clean slate.
          -- In the worst case the user will simply need to log in again.
          Left _err -> do
            writeCredentialsFile credentialJSONPath emptyCredentials
            pure emptyCredentials
          Right creds -> pure creds
    (newCredentials, r) <- f credentials
    liftIO . when (newCredentials /= credentials) $
      writeCredentialsFile credentialJSONPath newCredentials
    pure r
