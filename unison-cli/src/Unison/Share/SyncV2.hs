{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Unison.Share.SyncV2
  ( -- ** Get causal hash by path
    getCausalHash,
    GetCausalHashByPathError (..),

    -- ** Pull/Download
    pull,
    PullError (..),
    downloadEntities,
  )
where

import Conduit (ConduitT)
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Reader qualified as Reader
import Data.Conduit ((.|))
import Data.Conduit qualified as Conduit
import Data.Conduit.Combinators qualified as Conduit
import Data.Map qualified as Map
import Data.Proxy
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy
import Network.HTTP.Client qualified as Http.Client
import Network.HTTP.Types qualified as HTTP
import Servant.API qualified as Servant
import Servant.Client (BaseUrl)
import Servant.Client qualified as Servant
import Servant.Client.Streaming qualified as ServantStreaming
import Servant.Conduit ()
import Servant.Types.SourceT qualified as Servant
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.TempEntity (TempEntity)
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import Unison.Auth.HTTPClient qualified as Auth
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Debug qualified as Debug
import Unison.Hash32 (Hash32)
import Unison.Prelude
import Unison.Share.API.Hash qualified as Share
import Unison.Share.ExpectedHashMismatches (expectedCausalHashMismatches, expectedComponentHashMismatches)
import Unison.Share.Sync.Types
import Unison.Sqlite qualified as Sqlite
import Unison.Sync.Common (hash32ToCausalHash, tempEntityToEntity)
import Unison.Sync.EntityValidation qualified as EV
import Unison.Sync.Types qualified as Share
import Unison.SyncV2.API qualified as SyncV2
import Unison.SyncV2.Types qualified as CBOR
import Unison.SyncV2.Types qualified as SyncV2
import Unison.Util.Monoid (foldMapM)

------------------------------------------------------------------------------------------------------------------------
-- Pull

pull ::
  -- | The Unison Share URL.
  BaseUrl ->
  -- | The repo+path to pull from.
  Share.Path ->
  SyncV2.BranchRef ->
  -- | Callback that's given a number of entities we just downloaded.
  (Int -> IO ()) ->
  Cli (Either (SyncError SyncV2.PullError) CausalHash)
pull unisonShareUrl repoPath branchRef downloadedCallback =
  getCausalHash unisonShareUrl branchRef >>= \case
    Left err -> pure (Left (SyncV2.PullError'GetCausalHash <$> err))
    -- There's nothing at the remote path, so there's no causal to pull.
    Right hashJwt -> do
      -- TODO: include known hashes
      let knownHashes = Set.empty
      downloadEntities unisonShareUrl (Share.pathRepoInfo repoPath) hashJwt knownHashes downloadedCallback <&> \case
        Left err -> Left err
        Right () -> Right (hash32ToCausalHash (Share.hashJWTHash hashJwt))

------------------------------------------------------------------------------------------------------------------------
-- Download entities

downloadEntities ::
  -- | The Unison Share URL.
  BaseUrl ->
  -- | The repo to download from.
  Share.RepoInfo ->
  -- | The hash to download.
  Share.HashJWT ->
  Set Hash32 ->
  -- | Callback that's given a number of entities we just downloaded.
  (Int -> IO ()) ->
  Cli (Either (SyncError SyncV2.PullError) ())
downloadEntities unisonShareUrl repoInfo hashJwt knownHashes downloadedCallback = do
  Cli.Env {authHTTPClient, codebase} <- ask

  Cli.label \done -> do
    let failed :: SyncError SyncV2.PullError -> Cli void
        failed = done . Left

    let hash = Share.hashJWTHash hashJwt

    Cli.runTransaction (Q.entityLocation hash) >>= \case
      Just Q.EntityInMainStorage -> pure ()
      -- Just Q.EntityInTempStorage -> error "TODO: implement temp storage handler"
      _ -> do
        let request =
              httpDownloadEntities
                authHTTPClient
                unisonShareUrl
                SyncV2.DownloadEntitiesRequest {repoInfo, causalHash = hashJwt, knownHashes}
        liftIO request >>= \case
          Left err -> failed (TransportError err)
          Right source -> do
            conduit <- liftIO $ Servant.fromSourceIO source
            let entityPipeline :: ConduitT () c (ExceptT SyncV2.PullError Cli) ()
                entityPipeline = conduit .| unpackEntities downloadedCallback .| entityValidator .| entityInserter
            runExceptT (Conduit.runConduit entityPipeline) >>= \case
              Left err -> failed (SyncError err)
              Right () -> pure ()
    didCausalSuccessfullyImport codebase hash >>= \case
      False -> do
        failed (SyncError (SyncV2.PullError'Sync . SyncV2.SyncErrorExpectedResultNotInMain . hash32ToCausalHash $ hash))
      True -> pure ()
    -- we'll try vacuuming again next pull.
    _success <- liftIO (Codebase.withConnection codebase Sqlite.vacuum)
    pure (Right ())
  where
    -- Verify that the expected hash made it into main storage.
    didCausalSuccessfullyImport :: Codebase.Codebase IO v a -> Hash32 -> Cli Bool
    didCausalSuccessfullyImport codebase hash = do
      let expectedHash = hash32ToCausalHash hash
      isJust <$> liftIO (Codebase.runTransaction codebase $ Q.loadCausalByCausalHash expectedHash)

entityValidator :: (MonadError SyncV2.PullError m) => ConduitT (Hash32, TempEntity) (Hash32, TempEntity) m ()
entityValidator = Conduit.iterM $ \(hash, entity) ->
  -- TODO: We can investigate batching or running this in parallel if it becomes a bottleneck.
  case EV.validateTempEntity hash entity of
    Nothing -> pure ()
    Just err@(Share.EntityHashMismatch et (Share.HashMismatchForEntity {supplied, computed})) ->
      let expectedMismatches = case et of
            Share.TermComponentType -> expectedComponentHashMismatches
            Share.DeclComponentType -> expectedComponentHashMismatches
            Share.CausalType -> expectedCausalHashMismatches
            _ -> mempty
       in case Map.lookup supplied expectedMismatches of
            Just expected
              | expected == computed -> pure ()
            _ -> do
              throwError . SyncV2.PullError'DownloadEntities . SyncV2.DownloadEntitiesEntityValidationFailure $ err
    Just err -> do
      throwError . SyncV2.PullError'DownloadEntities . SyncV2.DownloadEntitiesEntityValidationFailure $ err

entityInserter :: (MonadIO m, MonadReader Cli.Env m) => ConduitT (Hash32, TempEntity) o m ()
entityInserter = Conduit.mapM_ \(hash, entity) -> do
  Cli.Env {codebase} <- ask
  liftIO . Codebase.runTransaction codebase $ upsertEntitySomewhere hash entity
  pure ()

unpackEntities :: (MonadError SyncV2.PullError m, MonadIO m) => (Int -> IO ()) -> ConduitT SyncV2.DownloadEntitiesChunk (Hash32, TempEntity) m ()
unpackEntities downloadedCallback = Conduit.mapM $ \case
  SyncV2.EntityChunk {hash, entityCBOR = entityBytes} ->
    case CBOR.deserialiseOrFailCBORBytes entityBytes of
      Left err -> do
        throwError (SyncV2.PullError'Sync $ SyncV2.SyncErrorDeserializationFailure err)
      Right entity -> do
        liftIO (downloadedCallback 1)
        pure (hash, entity)
  SyncV2.ErrorChunk {err} -> do
    throwError (SyncV2.PullError'DownloadEntities err)

------------------------------------------------------------------------------------------------------------------------
-- Get causal hash by path

-- | Get the causal hash of a path hosted on Unison Share.
getCausalHash ::
  -- | The Unison Share URL.
  BaseUrl ->
  SyncV2.BranchRef ->
  Cli (Either (SyncError SyncV2.GetCausalHashError) Share.HashJWT)
getCausalHash unisonShareUrl branchRef = do
  Cli.Env {authHTTPClient} <- ask
  liftIO (httpGetCausalHash authHTTPClient unisonShareUrl (SyncV2.GetCausalHashRequest branchRef)) <&> \case
    Left err -> Left (TransportError err)
    Right (SyncV2.GetCausalHashSuccess hashJWT) -> Right hashJWT
    Right (SyncV2.GetCausalHashError err) ->
      case err of
        SyncV2.GetCausalHashNoReadPermission repoInfo ->
          Left (SyncError (SyncV2.GetCausalHashNoReadPermission repoInfo))
        SyncV2.GetCausalHashInvalidRepoInfo err repoInfo ->
          Left (SyncError (SyncV2.GetCausalHashInvalidRepoInfo err repoInfo))
        SyncV2.GetCausalHashUserNotFound ->
          Left (SyncError SyncV2.GetCausalHashUserNotFound)

------------------------------------------------------------------------------------------------------------------------
-- Upload entities

-- | Upload a set of entities to Unison Share. If the server responds that it cannot yet store any hash(es) due to
-- missing dependencies, send those dependencies too, and on and on, until the server stops responding that it's missing
-- anything.
--
-- Returns true on success, false on failure (because the user does not have write permission).
_uploadEntities ::
  BaseUrl ->
  Share.RepoInfo ->
  NESet Hash32 ->
  (Int -> IO ()) ->
  Cli (Either (SyncError Share.UploadEntitiesError) ())
_uploadEntities _unisonShareUrl _repoInfo _hashes0 _uploadedCallback = do
  error "syncv2 uploadEntities not implemented"

------------------------------------------------------------------------------------------------------------------------
-- Database operations

-- | Upsert a downloaded entity "somewhere" -
--
--   1. Nowhere if we already had the entity (in main or temp storage).
--   2. In main storage if we already have all of its dependencies in main storage.
--   3. In temp storage otherwise.
upsertEntitySomewhere ::
  Hash32 ->
  TempEntity ->
  Sqlite.Transaction Q.EntityLocation
upsertEntitySomewhere hash entity =
  Q.entityLocation hash >>= \case
    Just location -> pure location
    Nothing -> do
      missingDependencies1 :: Set Hash32 <-
        Share.entityDependencies (tempEntityToEntity entity)
          & foldMapM
            ( \depHash -> do
                Q.entityExists depHash <&> \case
                  True -> mempty
                  False -> Set.singleton depHash
            )
      case NESet.nonEmptySet missingDependencies1 of
        Nothing -> do
          _id <- Q.saveTempEntityInMain v2HashHandle hash entity
          pure Q.EntityInMainStorage
        Just missingDependencies -> do
          Q.insertTempEntityV2
            hash
            entity
            missingDependencies
          pure Q.EntityInTempStorage

------------------------------------------------------------------------------------------------------------------------
-- HTTP calls

-- syncV2Client :: SyncV2.Routes (Servant.AsClientT ServantStreaming.ClientM)
-- syncV2Client = ServantStreaming.client SyncV2.api -- remember: api :: Proxy MovieCatalogAPI

-- syncV2Client :: SyncV2.Routes (Servant.AsClientT ServantStreaming.ClientM)
-- Routes {} = ServantStreaming.client SyncV2.api

httpGetCausalHash ::
  Auth.AuthenticatedHttpClient ->
  BaseUrl ->
  SyncV2.GetCausalHashRequest ->
  IO (Either CodeserverTransportError SyncV2.GetCausalHashResponse)
httpDownloadEntities ::
  Auth.AuthenticatedHttpClient ->
  BaseUrl ->
  SyncV2.DownloadEntitiesRequest ->
  (IO (Either CodeserverTransportError (Servant.SourceT IO SyncV2.DownloadEntitiesChunk)))
_httpUploadEntities ::
  Auth.AuthenticatedHttpClient ->
  BaseUrl ->
  SyncV2.UploadEntitiesRequest ->
  IO (Either CodeserverTransportError (Servant.SourceT IO ByteString))
( httpGetCausalHash,
  httpDownloadEntities,
  _httpUploadEntities
  ) =
    let SyncV2.Routes
          { getCausalHash,
            downloadEntitiesStream,
            uploadEntitiesStream
          } =
            let pp :: Proxy ("ucm" Servant.:> "v1" Servant.:> "sync" Servant.:> SyncV2.API)
                pp = Proxy
             in ServantStreaming.hoistClient pp hoist (ServantStreaming.client SyncV2.api)
     in ( go getCausalHash,
          go downloadEntitiesStream,
          go uploadEntitiesStream
        )
    where
      hoist :: ServantStreaming.ClientM a -> ReaderT Servant.ClientEnv (ExceptT CodeserverTransportError IO) a
      hoist m = do
        clientEnv <- Reader.ask
        (lift . lift $ ServantStreaming.withClientM m clientEnv pure) >>= \case
          Right a -> pure a
          Left err -> do
            Debug.debugLogM Debug.Sync (show err)
            throwError case err of
              Servant.FailureResponse _req resp ->
                case HTTP.statusCode $ Servant.responseStatusCode resp of
                  401 -> Unauthenticated (Servant.baseUrl clientEnv)
                  -- The server should provide semantically relevant permission-denied messages
                  -- when possible, but this should catch any we miss.
                  403 -> PermissionDenied (Text.Lazy.toStrict . Text.Lazy.decodeUtf8 $ Servant.responseBody resp)
                  408 -> Timeout
                  429 -> RateLimitExceeded
                  504 -> Timeout
                  _ -> UnexpectedResponse resp
              Servant.DecodeFailure msg resp -> DecodeFailure msg resp
              Servant.UnsupportedContentType _ct resp -> UnexpectedResponse resp
              Servant.InvalidContentTypeHeader resp -> UnexpectedResponse resp
              Servant.ConnectionError _ -> UnreachableCodeserver (Servant.baseUrl clientEnv)

      go ::
        (req -> ReaderT Servant.ClientEnv (ExceptT CodeserverTransportError IO) resp) ->
        Auth.AuthenticatedHttpClient ->
        BaseUrl ->
        req ->
        IO (Either CodeserverTransportError resp)
      go f (Auth.AuthenticatedHttpClient httpClient) unisonShareUrl req =
        (Servant.mkClientEnv httpClient unisonShareUrl)
          { Servant.makeClientRequest = \url request ->
              -- Disable client-side timeouts
              (Servant.defaultMakeClientRequest url request)
                <&> \r ->
                  r
                    { Http.Client.responseTimeout = Http.Client.responseTimeoutNone
                    }
          }
          & runReaderT (f req)
          & runExceptT
