{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Helper for rendering docs within a given namespace
module Unison.Server.Share.RenderDoc where

import Control.Monad.Except
import Data.Set qualified as Set
import Servant.OpenApi ()
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.NameLookups (PathSegments (..))
import U.Codebase.Sqlite.Operations qualified as Ops
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Runtime qualified as Rt
import Unison.LabeledDependency qualified as LD
import Unison.NameSegment (NameSegment (..))
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnvDecl.Sqlite qualified as PPESqlite
import Unison.Server.Backend
import Unison.Server.Backend qualified as Backend
import Unison.Server.Doc (Doc)
import Unison.Server.Doc qualified as Doc
import Unison.Symbol (Symbol)
import Unison.Util.Pretty (Width)

-- | Find, eval, and render the first doc we find with any of the provided names within the given namespace
-- If no doc is found, return Nothing
--
-- Requires Name Lookups, currently only usable on Share.
findAndRenderDoc ::
  Set NameSegment ->
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  Path.Path ->
  CausalHash ->
  Maybe Width ->
  Backend IO (Maybe Doc)
findAndRenderDoc docNames runtime codebase namespacePath rootCausalHash _mayWidth = do
  (shallowBranchAtNamespace, namesPerspective) <-
    liftIO . (Codebase.runTransaction codebase) $ do
      rootCausal <- Backend.resolveCausalHashV2 (Just rootCausalHash)
      let rootBranchHash = V2Causal.valueHash rootCausal
      namespaceCausal <- Codebase.getShallowCausalAtPath namespacePath (Just rootCausal)
      shallowBranchAtNamespace <- V2Causal.value namespaceCausal
      namesPerspective <- Ops.namesPerspectiveForRootAndPath rootBranchHash (coerce . Path.toList $ namespacePath)
      pure (shallowBranchAtNamespace, namesPerspective)
  let mayDocRef = Backend.findDocInBranch docNames shallowBranchAtNamespace
  for mayDocRef \docRef -> do
    eDoc <- liftIO $ evalDocRef runtime codebase docRef
    let docDeps = Doc.dependencies eDoc <> Set.singleton (LD.TermReference docRef)
    docPPE <- liftIO $ Codebase.runTransaction codebase $ PPESqlite.ppedForReferences namesPerspective docDeps
    pure $ Doc.renderDoc docPPE eDoc
