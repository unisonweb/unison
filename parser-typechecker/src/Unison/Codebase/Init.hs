{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Init
  ( Init (..),
    DebugName,
    InitError (..),
    CodebaseInitOptions (..),
    InitResult (..),
    SpecifiedCodebase (..),
    MigrationStrategy (..),
    Pretty,
    createCodebase,
    initCodebaseAndExit,
    withOpenOrCreateCodebase,
    withNewUcmCodebaseOrExit,
    withTemporaryUcmCodebase,
  )
where

import System.Exit (exitFailure)
import Unison.Codebase (Codebase, CodebasePath)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.FileCodebase as FCC
import Unison.Codebase.Init.CreateCodebaseError
import Unison.Codebase.Init.OpenCodebaseError
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import qualified Unison.PrettyTerminal as PT
import Unison.Symbol (Symbol)
import qualified Unison.Util.Pretty as P
import qualified UnliftIO
import UnliftIO.Directory (canonicalizePath)

-- CodebaseInitOptions is used to help pass around a Home directory that isn't the
-- actual home directory of the user. Useful in tests.
data CodebaseInitOptions
  = Home CodebasePath
  | Specified SpecifiedCodebase

data SpecifiedCodebase
  = CreateWhenMissing CodebasePath
  | DontCreateWhenMissing CodebasePath

data MigrationStrategy
  = -- | Perform a migration immediately if one is required.
    MigrateAutomatically
  | -- | Prompt the user that a migration is about to occur, continue after acknownledgment
    MigrateAfterPrompt
  | -- | Triggers an 'OpenCodebaseRequiresMigration' error instead of migrating
    DontMigrate
  deriving stock (Show, Eq, Ord)

initOptionsToDir :: CodebaseInitOptions -> CodebasePath
initOptionsToDir (Home dir) = dir
initOptionsToDir (Specified (CreateWhenMissing dir)) = dir
initOptionsToDir (Specified (DontCreateWhenMissing dir)) = dir

type DebugName = String

data Init m v a = Init
  { -- | open an existing codebase
    withOpenCodebase :: forall r. DebugName -> CodebasePath -> MigrationStrategy -> (Codebase m v a -> m r) -> m (Either OpenCodebaseError r),
    -- | create a new codebase
    withCreatedCodebase :: forall r. DebugName -> CodebasePath -> (Codebase m v a -> m r) -> m (Either CreateCodebaseError r),
    -- | given a codebase root, and given that the codebase root may have other junk in it,
    -- give the path to the "actual" files; e.g. what a forked transcript should clone.
    codebasePath :: CodebasePath -> CodebasePath
  }

-- | An error that occurred while initializing a codebase.
data InitError
  = FoundV1Codebase
  | InitErrorOpen OpenCodebaseError
  | CouldntCreateCodebase Pretty
  deriving (Show, Eq)

data InitResult
  = OpenedCodebase
  | CreatedCodebase
  deriving (Show, Eq)

createCodebaseWithResult ::
  MonadIO m =>
  Init m v a ->
  DebugName ->
  CodebasePath ->
  (Codebase m v a -> m r) ->
  m (Either (CodebasePath, InitError) r)
createCodebaseWithResult cbInit debugName dir action =
  createCodebase cbInit debugName dir action <&> mapLeft \case
    errorMessage -> (dir, (CouldntCreateCodebase errorMessage))

withOpenOrCreateCodebase ::
  MonadIO m =>
  Init m v a ->
  DebugName ->
  CodebaseInitOptions ->
  MigrationStrategy ->
  ((InitResult, CodebasePath, Codebase m v a) -> m r) ->
  m (Either (CodebasePath, InitError) r)
withOpenOrCreateCodebase cbInit debugName initOptions migrationStrategy action = do
  let resolvedPath = initOptionsToDir initOptions
  result <- withOpenCodebase cbInit debugName resolvedPath migrationStrategy \codebase -> do
    action (OpenedCodebase, resolvedPath, codebase)
  case result of
    Right r -> pure $ Right r
    Left err -> case err of
      OpenCodebaseDoesntExist ->
        case initOptions of
          Home homeDir -> do
            ifM
              (FCC.codebaseExists homeDir)
              (do pure (Left (homeDir, FoundV1Codebase)))
              ( do
                  -- Create V2 codebase if neither a V1 or V2 exists
                  createCodebaseWithResult cbInit debugName homeDir (\codebase -> action (CreatedCodebase, homeDir, codebase))
              )
          Specified specified ->
            ifM
              (FCC.codebaseExists resolvedPath)
              (pure $ Left (resolvedPath, FoundV1Codebase))
              case specified of
                DontCreateWhenMissing dir ->
                  pure (Left (dir, (InitErrorOpen OpenCodebaseDoesntExist)))
                CreateWhenMissing dir ->
                  createCodebaseWithResult cbInit debugName dir (\codebase -> action (CreatedCodebase, dir, codebase))
      OpenCodebaseUnknownSchemaVersion {} -> pure (Left (resolvedPath, InitErrorOpen err))
      OpenCodebaseRequiresMigration {} -> pure (Left (resolvedPath, InitErrorOpen err))

createCodebase :: MonadIO m => Init m v a -> DebugName -> CodebasePath -> (Codebase m v a -> m r) -> m (Either Pretty r)
createCodebase cbInit debugName path action = do
  prettyDir <- P.string <$> canonicalizePath path
  withCreatedCodebase cbInit debugName path action <&> mapLeft \case
    CreateCodebaseAlreadyExists ->
      P.wrap $
        "It looks like there's already a codebase in: "
          <> prettyDir

-- * compatibility stuff

-- previously: initCodebaseOrExit :: CodebasePath -> m (m (), Codebase m v a)
-- previously: FileCodebase.initCodebase :: CodebasePath -> m (m (), Codebase m v a)
withNewUcmCodebaseOrExit :: MonadIO m => Init m Symbol Ann -> DebugName -> CodebasePath -> (Codebase m Symbol Ann -> m r) -> m r
withNewUcmCodebaseOrExit cbInit debugName path action = do
  prettyDir <- P.string <$> canonicalizePath path
  let codebaseSetup codebase = do
        liftIO $ PT.putPrettyLn' . P.wrap $ "Initializing a new codebase in: " <> prettyDir
        Codebase.runTransaction codebase (Codebase.installUcmDependencies codebase)
  createCodebase cbInit debugName path (\cb -> codebaseSetup cb *> action cb)
    >>= \case
      Left error -> liftIO $ PT.putPrettyLn' error >> exitFailure
      Right result -> pure result

-- | try to init a codebase where none exists and then exit regardless (i.e. `ucm --codebase dir init`)
initCodebaseAndExit :: MonadIO m => Init m Symbol Ann -> DebugName -> Maybe CodebasePath -> m ()
initCodebaseAndExit i debugName mdir = do
  codebaseDir <- Codebase.getCodebaseDir mdir
  withNewUcmCodebaseOrExit i debugName codebaseDir (const $ pure ())

withTemporaryUcmCodebase ::
  MonadUnliftIO m =>
  Init m Symbol Ann ->
  DebugName ->
  ((CodebasePath, Codebase m Symbol Ann) -> m r) ->
  m r
withTemporaryUcmCodebase cbInit debugName action = do
  UnliftIO.withSystemTempDirectory debugName $ \tempDir -> do
    withNewUcmCodebaseOrExit cbInit debugName tempDir $ \codebase -> do
      action (tempDir, codebase)
