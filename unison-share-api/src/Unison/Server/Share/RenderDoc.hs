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
import U.Codebase.Projects qualified as Projects
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Runtime qualified as Rt
import Unison.LabeledDependency qualified as LD
import Unison.NameSegment (NameSegment)
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
  (rootCausal, shallowBranch) <-
    Backend.hoistBackend (Codebase.runTransaction codebase) do
      rootCausal <- lift $ Backend.resolveCausalHashV2 (Just rootCausalHash)
      namespaceCausal <- lift $ Codebase.getShallowCausalAtPath namespacePath (Just rootCausal)
      shallowBranch <- lift $ V2Causal.value namespaceCausal
      pure (rootCausal, shallowBranch)
  namesRoot <- fmap (fromMaybe namespacePath) . liftIO . Codebase.runTransaction codebase $ Projects.inferNamesRoot namespacePath shallowBranch
  let rootBranchHash = V2Causal.valueHash rootCausal
  let mayDocRef = Backend.findDocInBranch docNames shallowBranch
  for mayDocRef \docRef -> do
    eDoc <- liftIO $ evalDocRef runtime codebase docRef
    let docDeps = Doc.dependencies eDoc <> Set.singleton (LD.TermReference docRef)
    docPPE <- liftIO $ Codebase.runTransaction codebase $ PPESqlite.ppedForReferences rootBranchHash namesRoot docDeps
    let renderedDoc = Doc.renderDoc docPPE eDoc
    pure renderedDoc
