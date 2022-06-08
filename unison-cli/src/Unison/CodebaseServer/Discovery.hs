{-# LANGUAGE RecordWildCards #-}

module Unison.CodebaseServer.Discovery where

import Control.Monad.Except
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Types (parseQuery)
import Network.URI
import System.IO.Unsafe (unsafePerformIO)
import qualified Unison.Codebase.Editor.RemoteRepo as RemoteRepo
import Unison.Prelude
import Unison.Share.Types
import qualified Unison.Share.Types as Share
import UnliftIO
import UnliftIO.Environment (lookupEnv)

data CodeserverError
  = InvalidCodeserverDescription CodeserverRoot Text

-- | Fetches codeserver description doc from (<codeserverRoot>/.codeserver)
fetchCodeServerDescription :: MonadIO m => CodeserverRoot -> m (Either CodeserverError CodeserverDescription)
fetchCodeServerDescription cdu = liftIO $ do
  (Map.lookup cdu <$> readTVarIO codeserverCache) >>= \case
    Just description -> pure $ Right description
    Nothing -> do
      httpClient <- HTTP.getGlobalManager
      let uri =
            codeserverToURI cdu
              & \uri ->
                uri {uriPath = uriPath uri <> "/.codeserver"}
      req <-
        HTTP.requestFromURI uri
          <&> ( HTTP.setQueryString $
                  parseQuery (BSC.pack $ uriQuery uri) <> [("client_version", Just (BSC.pack $ show requestedCodeserverVersion))]
              )
      resp <- HTTP.httpLbs req httpClient
      case Aeson.eitherDecode (HTTP.responseBody $ resp) of
        Left err -> pure . Left $ InvalidCodeserverDescription cdu (Text.pack err)
        Right doc -> do
          atomically $ modifyTVar codeserverCache (Map.insert cdu doc)
          pure . Right $ doc
  where
    requestedCodeserverVersion :: ByteString
    requestedCodeserverVersion = "1"

-- | Ephemeral in memory cache for codeserver descriptions.
codeserverCache :: TVar (Map CodeserverRoot CodeserverDescription)
codeserverCache = unsafePerformIO $ newTVarIO mempty
{-# NOINLINE codeserverCache #-}

-- | This is the URI where the share API is based.
defaultCodeserverRoot :: CodeserverRoot
defaultCodeserverRoot = unsafePerformIO $ do
  lookupEnv "UNISON_SHARE_HOST" <&> \case
    -- TODO: swap to production share before release.
    Nothing ->
      CodeserverRoot
        { codeserverScheme = Share.Https,
          codeserverUserInfo = "",
          codeserverRegName = "share-next.us-west-2.unison-lang.org",
          codeserverPort = Just 443,
          codeserverPath = ["api"]
        }
    Just shareHost ->
      fromMaybe (error $ "Share Host is not a valid URI: " <> shareHost) $ do
        uri <- parseURI shareHost
        codeserverFromURI uri
{-# NOINLINE defaultCodeserverRoot #-}

resolveCodeserver :: MonadIO m => RemoteRepo.CodeserverLocation -> m (Either CodeserverError Codeserver)
resolveCodeserver cs = runExceptT $ do
  let (codeserverProvenance, codeserverRoot) = case cs of
        RemoteRepo.DefaultShare -> (DefaultCodeserver, defaultCodeserverRoot)
        RemoteRepo.CustomShare cs -> (CustomCodeserver, cs)
  codeserverDescription <- ExceptT (fetchCodeServerDescription codeserverRoot)
  let codeserverId = codeserverIdFromCodeserverRoot codeserverRoot
  pure (Codeserver {codeserverDescription, codeserverProvenance, codeserverId, codeserverRoot})
