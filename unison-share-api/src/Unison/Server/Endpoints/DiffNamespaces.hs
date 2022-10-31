{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Endpoints.DiffNamespaces where

import Control.Monad.Except
import Data.Aeson (ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Data.OpenApi (ToSchema)
import qualified Data.Set as Set
import Servant
  ( QueryParam,
    (:>),
  )
import Servant.Docs
  ( DocQueryParam (..),
    ParamKind (Normal),
    ToParam (..),
    ToSample (..),
    noSamples,
  )
import Servant.OpenApi ()
import qualified Text.FuzzyFind as FZF
import U.Codebase.Branch (Branch)
import qualified U.Codebase.Branch.Diff as BranchDiff
import qualified U.Codebase.Causal as V2Causal
import U.Codebase.Reference (Reference)
import U.Codebase.Referent (Referent)
import qualified U.Codebase.ShortHash as SBH
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Editor.DisplayObject
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import Unison.Name (Name)
import Unison.NameSegment
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPE
import qualified Unison.Server.Backend as Backend
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Types
  ( APIGet,
    ExactName (..),
    HashQualifiedName,
    NamedTerm,
    NamedType,
    UnisonName,
    mayDefaultWidth,
  )
import Unison.Symbol (Symbol)
import qualified Unison.Util.List as ListUtils
import Unison.Util.Pretty (Width)

-- | All the terms which exist _only_ in one side or the other of the diff.
-- Terms which are unchanged between from<->to are omitted.
data NamespaceDiffResponse = NamespaceDiffResponse
  { onlyFrom :: DiffContents,
    onlyTo :: DiffContents
  }

data DiffContents = DiffContents
  { terms :: Map Name [Referent],
    types :: Map Name [Reference]
  }

serveDiffNamespaces ::
  forall m.
  MonadIO m =>
  Codebase m Symbol Ann ->
  SBH.ShortBranchHash ->
  SBH.ShortBranchHash ->
  Backend.Backend m NamespaceDiffResponse
serveDiffNamespaces codebase fromNamespaceHash toNamespaceHash = do
  fromNamespace <- resolveNamespaceFromHash fromNamespaceHash
  toNamespace <- resolveNamespaceFromHash toNamespaceHash
  treeDiff <- lift $ BranchDiff.diffBranches fromNamespace toNamespace
  let BranchDiff.NameChanges {termNameAdds, termNameRemovals, typeNameAdds, typeNameRemovals} = BranchDiff.nameChanges Nothing treeDiff
  let onlyFrom = DiffContents {terms = ListUtils.multimap termNameRemovals, types = ListUtils.multimap typeNameRemovals}
  let onlyTo = DiffContents {terms = ListUtils.multimap termNameAdds, types = ListUtils.multimap typeNameAdds}
  pure $ NamespaceDiffResponse {onlyFrom, onlyTo}
  where
    resolveNamespaceFromHash ::
      ( SBH.ShortBranchHash ->
        t m (Branch m)
      )
    resolveNamespaceFromHash sbh = do
      let v1SBH = Cv.sbh2to1 sbh
      hashMatches <- lift $ Codebase.branchHashesByPrefix codebase v1SBH
      case Set.toList hashMatches of
        [] -> throwError $ Backend.CouldntExpandBranchHash v1SBH
        [nsh] -> lift $ Codebase.getShallowBranchForHash codebase nsh `whenNothingM` throwError (Backend.NoNamespaceForHash nsh)
        _ -> throwError . Backend.AmbiguousBranchHash v1SBH $ Set.map (Cv.sbh2to1 . SBH.ShortBranchHash) hashMatches
