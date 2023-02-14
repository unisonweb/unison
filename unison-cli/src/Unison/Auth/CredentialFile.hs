{-# LANGUAGE NumericUnderscores #-}

module Unison.Auth.CredentialFile (atomicallyModifyCredentialsFile) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import System.FilePath (takeDirectory, (</>))
import System.IO.LockFile
import Unison.Auth.Types
import Unison.Prelude
import qualified UnliftIO
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
        Left err -> UnliftIO.throwIO $ CredentialParseFailure credentialJSONPath (Text.pack err)
        Right creds -> pure creds
    let newCredentials = f credentials
    when (newCredentials /= credentials) $ do
      Aeson.encodeFile credentialJSONPath $ newCredentials
    pure newCredentials
