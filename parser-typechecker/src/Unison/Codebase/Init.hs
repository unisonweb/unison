{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Init where

import System.Exit (exitFailure)
import Unison.Codebase (Codebase, CodebasePath)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.FileCodebase.Common as FCC
import Unison.Parser (Ann)
import Unison.Prelude
import qualified Unison.PrettyTerminal as PT
import Unison.Symbol (Symbol)
import qualified Unison.Util.Pretty as P
import UnliftIO.Directory (canonicalizePath, getHomeDirectory)

type Pretty = P.Pretty P.ColorText

-- CodebaseDir is used to help pass around a Home directory that isn't the
-- actual home directory of the user. Useful in tests.
data CodebaseDir = Home CodebasePath | Specified CodebasePath

homeOrSpecifiedDir :: Maybe CodebasePath -> IO CodebaseDir
homeOrSpecifiedDir specifiedDir = do
  homeDir <- getHomeDirectory
  pure $ maybe (Home homeDir) Specified specifiedDir

codebaseDirToCodebasePath :: CodebaseDir -> CodebasePath
codebaseDirToCodebasePath (Home dir) = dir
codebaseDirToCodebasePath (Specified dir) = dir

data CreateCodebaseError
  = CreateCodebaseAlreadyExists
  | CreateCodebaseOther Pretty

type DebugName = String

data Init m v a = Init
  { -- | open an existing codebase
    openCodebase :: DebugName -> CodebasePath -> m (Either Pretty (m (), Codebase m v a)),
    -- | create a new codebase
    createCodebase' :: DebugName -> CodebasePath -> m (Either CreateCodebaseError (m (), Codebase m v a)),
    -- | given a codebase root, and given that the codebase root may have other junk in it,
    -- give the path to the "actual" files; e.g. what a forked transcript should clone.
    codebasePath :: CodebasePath -> CodebasePath
  }

type FinalizerAndCodebase m v a = (m (), Codebase m v a) 

data InitError 
  = NoCodebaseFoundAtSpecifiedDir
  | FoundV1Codebase
  | CouldntCreateCodebase Pretty

data InitResult m v a 
  = OpenedCodebase CodebasePath (FinalizerAndCodebase m v a) 
  | CreatedCodebase CodebasePath (FinalizerAndCodebase m v a) 
  | Error CodebasePath InitError 


openOrCreateCodebase :: MonadIO m => Init m v a -> DebugName -> CodebaseDir -> m (InitResult m v a)
openOrCreateCodebase cbInit debugName codebaseDir = do
  let resolvedPath = (codebaseDirToCodebasePath codebaseDir)  
  openCodebase cbInit debugName resolvedPath >>= \case 
    Right cb -> pure (OpenedCodebase resolvedPath cb)
    Left _ ->
      case codebaseDir of
        Home homeDir -> do
          ifM (FCC.codebaseExists homeDir)
            (do pure (Error homeDir FoundV1Codebase))
            (do
              -- Create V2 codebase if neither a V1 or V2 exists
              createCodebase cbInit debugName homeDir >>= \case
                Left errorMessage -> do
                  pure (Error homeDir (CouldntCreateCodebase errorMessage))
                Right cb -> do
                  pure (CreatedCodebase homeDir cb)
            )
        Specified specifiedDir -> do
          ifM (FCC.codebaseExists specifiedDir)
            (pure (Error specifiedDir FoundV1Codebase))
            (pure (Error specifiedDir NoCodebaseFoundAtSpecifiedDir))

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

-- | try to init a codebase where none exists and then exit regardless (i.e. `ucm -codebase dir init`)
initCodebaseAndExit :: MonadIO m => Init m Symbol Ann -> DebugName -> Maybe CodebasePath -> m ()
initCodebaseAndExit i debugName mdir =
  void $ openNewUcmCodebaseOrExit i debugName =<< Codebase.getCodebaseDir mdir
