{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Init
  ( Init (..),
    DebugName,
    Pretty,
    createCodebase,
    initCodebaseAndExit,
    openNewUcmCodebaseOrExit,
  )
where

import Unison.Codebase.Init.Type
import System.Exit (exitFailure)
import Unison.Codebase (Codebase, CodebasePath)
import qualified Unison.Codebase as Codebase
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.PrettyTerminal as PT
import Unison.Symbol (Symbol)
import qualified Unison.Util.Pretty as P
import UnliftIO.Directory (canonicalizePath)
import qualified Unison.Codebase.Init.CreateCodebaseError as E
import Unison.Codebase.Init.CreateCodebaseError (Pretty)

type DebugName = String

createCodebase :: MonadIO m => Init m v a -> DebugName -> CodebasePath -> m (Either Pretty (m (), Codebase m v a))
createCodebase debugName cbInit path = do
  prettyDir <- P.string <$> canonicalizePath path
  createCodebase' debugName cbInit path <&> mapLeft \case
    E.CreateCodebaseAlreadyExists ->
      P.wrap $
        "It looks like there's already a codebase in: "
          <> prettyDir
    E.CreateCodebaseOther message ->
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
openNewUcmCodebaseOrExit debugName cbInit path = do
  prettyDir <- P.string <$> canonicalizePath path
  createCodebase debugName cbInit path >>= \case
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
