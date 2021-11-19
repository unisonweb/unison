{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Init
  ( Init (..),
    DebugName,
    InitError (..),
    CodebaseInitOptions (..),
    InitResult (..),
    SpecifiedCodebase (..),
    Pretty,
    createCodebase,
    initCodebaseAndExit,
    withOpenOrCreateCodebase,
    openNewUcmCodebaseOrExit,
  )
where

import System.Exit (exitFailure)
import Unison.Codebase (Codebase, CodebasePath)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.FileCodebase as FCC
import Unison.Parser.Ann (Ann(..))
import Unison.Prelude
import qualified Unison.PrettyTerminal as PT
import Unison.Symbol (Symbol)
import qualified Unison.Util.Pretty as P
import UnliftIO.Directory (canonicalizePath)
import Unison.Codebase.Init.CreateCodebaseError

-- CodebaseInitOptions is used to help pass around a Home directory that isn't the
-- actual home directory of the user. Useful in tests.
data CodebaseInitOptions
  = Home CodebasePath
  | Specified SpecifiedCodebase

data SpecifiedCodebase
  = CreateWhenMissing CodebasePath
  | DontCreateWhenMissing CodebasePath

initOptionsToDir :: CodebaseInitOptions -> CodebasePath
initOptionsToDir (Home dir ) = dir
initOptionsToDir (Specified (CreateWhenMissing dir)) = dir
initOptionsToDir (Specified (DontCreateWhenMissing dir)) = dir

type DebugName = String

data Init m v a = Init
  { -- | open an existing codebase
    withOpenCodebase :: forall r. DebugName -> CodebasePath -> (Codebase m v a -> m r) -> m (Either Pretty r),
    -- | create a new codebase
    withCreatedCodebase' :: forall r. DebugName -> CodebasePath -> (Codebase m v a -> m r) -> m (Either CreateCodebaseError r),
    -- | given a codebase root, and given that the codebase root may have other junk in it,
    -- give the path to the "actual" files; e.g. what a forked transcript should clone.
    codebasePath :: CodebasePath -> CodebasePath
  }

data InitError
  = NoCodebaseFoundAtSpecifiedDir
  | FoundV1Codebase
  | CouldntCreateCodebase Pretty

data InitResult m v a
  = OpenedCodebase CodebasePath (Codebase m v a)
  | CreatedCodebase CodebasePath (Codebase m v a)

createCodebaseWithResult :: MonadIO m => Init m v a -> DebugName -> CodebasePath -> m (InitResult m v a)
createCodebaseWithResult cbInit debugName dir =
  createCodebase cbInit debugName dir >>= \case
    Left errorMessage -> do
      pure (Error dir (CouldntCreateCodebase errorMessage))
    Right cb -> do
      pure (CreatedCodebase dir cb)

whenNoV1Codebase :: MonadIO m => CodebasePath -> m (InitResult m v a) -> m (InitResult m v a )
whenNoV1Codebase dir initResult =
  ifM (FCC.codebaseExists dir)
    (pure (Error dir FoundV1Codebase))
    initResult

withOpenOrCreateCodebase :: MonadIO m => Init m v a -> DebugName -> CodebaseInitOptions -> (InitResult m v a -> m r) -> m (Either (CodebasePath, InitError) r)
withOpenOrCreateCodebase cbInit debugName initOptions = do
  let resolvedPath = initOptionsToDir initOptions
  result <- withOpenCodebase cbInit debugName resolvedPath $ \case
    Right cb -> pure (OpenedCodebase resolvedPath cb)
  case result of
    Right _ -> _
    Left _ ->
      case initOptions of
        Home homeDir -> do
          ifM (FCC.codebaseExists homeDir)
            (do pure (Error homeDir FoundV1Codebase))
            (do
              -- Create V2 codebase if neither a V1 or V2 exists
              createCodebaseWithResult cbInit debugName homeDir
            )

        Specified specified ->
          whenNoV1Codebase resolvedPath $ do
            case specified of
              DontCreateWhenMissing dir ->
                pure (Error dir NoCodebaseFoundAtSpecifiedDir)
              CreateWhenMissing dir ->
                createCodebaseWithResult cbInit debugName dir

createCodebase :: MonadIO m => Init m v a -> DebugName -> CodebasePath -> m (Either Pretty (m (), Codebase m v a))
createCodebase cbInit debugName path = do
  prettyDir <- P.string <$> canonicalizePath path
  createCodebase' cbInit debugName path <&> mapLeft \case
    CreateCodebaseAlreadyExists ->
      P.wrap $
        "It looks like there's already a codebase in: "
          <> prettyDir
    CreateCodebaseOther message ->
      P.wrap ("I ran into an error when creating the codebase in: " <> prettyDir)
        <> P.newline
        <> P.newline
        <> "The error was:"
        <> P.newline
        <> P.indentN 2 message

-- * compatibility stuff

-- previously: initCodebaseOrExit :: CodebasePath -> m (m (), Codebase m v a)
-- previously: FileCodebase.initCodebase :: CodebasePath -> m (m (), Codebase m v a)
openNewUcmCodebaseOrExit :: MonadIO m => Init m Symbol Ann -> DebugName -> CodebasePath -> m (m (), Codebase m Symbol Ann)
openNewUcmCodebaseOrExit cbInit debugName path = do
  prettyDir <- P.string <$> canonicalizePath path
  createCodebase cbInit debugName path >>= \case
    Left error -> liftIO $ PT.putPrettyLn' error >> exitFailure
    Right x@(_, codebase) -> do
      liftIO $
        PT.putPrettyLn'
          . P.wrap
          $ "Initializing a new codebase in: "
            <> prettyDir
      Codebase.installUcmDependencies codebase
      pure x

-- | try to init a codebase where none exists and then exit regardless (i.e. `ucm --codebase dir init`)
initCodebaseAndExit :: MonadIO m => Init m Symbol Ann -> DebugName -> Maybe CodebasePath -> m ()
initCodebaseAndExit i debugName mdir =
  void $ openNewUcmCodebaseOrExit i debugName =<< Codebase.getCodebaseDir mdir
