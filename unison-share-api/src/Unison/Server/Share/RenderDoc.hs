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
import Data.Aeson
import Data.OpenApi (ToSchema)
import Servant (Capture, QueryParam, (:>))
import Servant.Docs (DocCapture (..), ToCapture (..), ToSample (..))
import Servant.OpenApi ()
import qualified U.Codebase.Causal as V2Causal
import U.Codebase.HashTags (CausalHash)
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Runtime as Rt
import Unison.Codebase.ShortCausalHash (ShortCausalHash)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.Server.Backend
import qualified Unison.Server.Backend as Backend
import Unison.Server.Doc (Doc)
import Unison.Server.Types
  ( APIGet,
    UnisonHash,
    mayDefaultWidth,
    v2CausalBranchToUnisonHash,
  )
import Unison.Symbol (Symbol)
import Unison.Util.Pretty (Width)
import Unison.NameSegment (NameSegment)

renderDoc ::
  Set NameSegment ->
  Rt.Runtime Symbol ->
  Codebase IO Symbol Ann ->
  Path.Path ->
  Maybe (Either ShortCausalHash CausalHash) ->
  Maybe Width ->
  Backend IO (Maybe Doc)
renderDoc docNames runtime codebase namespacePath mayRoot mayWidth =
  let width = mayDefaultWidth mayWidth
   in do
        (rootCausal, namespaceCausal, shallowBranch) <-
          Backend.hoistBackend (Codebase.runTransaction codebase) do
            rootCausalHash <-
              case mayRoot of
                Nothing -> Backend.resolveRootBranchHashV2 Nothing
                Just (Left sch) -> Backend.resolveRootBranchHashV2 (Just sch)
                Just (Right ch) -> lift $ Backend.resolveCausalHashV2 (Just ch)
            -- lift (Backend.resolveCausalHashV2 rootCausalHash)
            namespaceCausal <- lift $ Codebase.getShallowCausalAtPath namespacePath (Just rootCausalHash)
            shallowBranch <- lift $ V2Causal.value namespaceCausal
            pure (rootCausalHash, namespaceCausal, shallowBranch)
        (_localNamesOnly, ppe) <- Backend.scopedNamesForBranchHash codebase (Just rootCausal) namespacePath
        renderedDoc <-
          Backend.findDocInBranchAndRender
            docNames
            width
            runtime
            codebase
            ppe
            shallowBranch
        pure renderedDoc
