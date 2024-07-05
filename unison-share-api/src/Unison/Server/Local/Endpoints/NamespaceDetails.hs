{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Local.Endpoints.NamespaceDetails where

import Data.Set qualified as Set
import Servant (Capture, QueryParam, (:>))
import Servant.Docs (DocCapture (..), ToCapture (..))
import Servant.OpenApi ()
import U.Codebase.Causal qualified as V2Causal
import U.Codebase.HashTags (CausalHash)
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.Runtime qualified as Rt
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Server.Backend
import Unison.Server.Backend qualified as Backend
import Unison.Server.Doc qualified as Doc
import Unison.Server.Types
  ( APIGet,
    NamespaceDetails (..),
    v2CausalBranchToUnisonHash,
  )
import Unison.Symbol (Symbol)
import Unison.Util.Pretty (Width)

type NamespaceDetailsAPI =
  "namespaces"
    :> Capture "namespace" Path.Path
    :> QueryParam "renderWidth" Width
    :> APIGet NamespaceDetails

instance ToCapture (Capture "namespace" Text) where
  toCapture _ =
    DocCapture
      "namespace"
      "The fully qualified name of a namespace. The leading `.` is optional."

namespaceDetails ::
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  Path.Path ->
  Either ShortCausalHash CausalHash ->
  Maybe Width ->
  Backend IO NamespaceDetails
namespaceDetails runtime codebase namespacePath root _mayWidth = do
  (rootCausal, namespaceCausal, shallowBranch) <-
    Backend.hoistBackend (Codebase.runTransaction codebase) do
      rootCausalHash <-
        case root of
          (Left sch) -> Backend.resolveRootBranchHashV2 sch
          (Right ch) -> lift $ Codebase.expectCausalBranchByCausalHash ch
      namespaceCausal <- lift $ Codebase.getShallowCausalAtPath namespacePath rootCausalHash
      shallowBranch <- lift $ V2Causal.value namespaceCausal
      pure (rootCausalHash, namespaceCausal, shallowBranch)
  namespaceDetails <- do
    (_localNamesOnly, ppe) <- Backend.namesAtPathFromRootBranchHash codebase rootCausal namespacePath
    let mayReadmeRef = Backend.findDocInBranch readmeNames shallowBranch
    renderedReadme <- for mayReadmeRef \readmeRef -> do
      -- Local server currently ignores eval errors.
      (eDoc, _evalErrs) <- liftIO $ evalDocRef runtime codebase readmeRef
      pure $ Doc.renderDoc ppe eDoc
    let causalHash = v2CausalBranchToUnisonHash namespaceCausal
    pure $ NamespaceDetails namespacePath causalHash renderedReadme
  pure $ namespaceDetails
  where
    readmeNames =
      Set.fromList $ NameSegment <$> ["README", "Readme", "ReadMe", "readme"]
