{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NumericUnderscores #-}

module Unison.Auth.Storage
  ( saveTokens,
    CredentialManager,
    newCredentialManager,
    getTokens,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import System.FilePath (takeDirectory, (</>))
import System.IO.LockFile
import Unison.Auth.Types
import Unison.Prelude
import qualified UnliftIO
import UnliftIO.Directory
import UnliftIO.STM

newtype CredentialManager = CredentialManager (TVar Credentials)

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

-- | Saves credentials for profile and sets that to the active profile.
saveTokens :: CredentialManager -> Audience -> Tokens -> IO ()
saveTokens credManager aud tokens = do
  void . modifyCredentials credManager $ setActiveTokens aud tokens

-- | Atomically update the credential storage file, and update the in-memory cache.
modifyCredentials :: CredentialManager -> (Credentials -> Credentials) -> IO Credentials
modifyCredentials (CredentialManager credsVar) f = do
  newCreds <- atomicModifyCredentialsFile f
  atomically $ writeTVar credsVar newCreds
  pure newCreds

-- | Atomically update the credential storage file.
-- Creates an empty file automatically if one doesn't exist.
atomicModifyCredentialsFile :: (Credentials -> Credentials) -> IO Credentials
atomicModifyCredentialsFile f = do
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

getTokens :: CredentialManager -> Audience -> IO (Either CredentialFailure Tokens)
getTokens (CredentialManager credsVar) aud = do
  creds <- readTVarIO credsVar
  pure $ getActiveTokens aud creds

-- getTokenMap :: IO (Maybe (Map Audience Tokens))
-- getTokenMap = do
--   credentialJSONPath <- getCredentialJSONFilePath
--   Aeson.eitherDecodeFileStrict credentialJSONPath >>= \case
--     Left err -> do
--       putStrLn $ "Failed to load " <> credentialJSONPath <> ": " err
--       pure Nothing
--     Right (Credentials {credentials, activeProfile}) -> do
--       pure $ credentials ^? ix activeProfile

newCredentialManager :: IO CredentialManager
newCredentialManager = do
  credentials <- atomicModifyCredentialsFile id
  credentialsVar <- newTVarIO credentials
  pure (CredentialManager credentialsVar)
