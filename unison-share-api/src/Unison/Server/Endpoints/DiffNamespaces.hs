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
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Set as Set
import Servant.OpenApi ()
import U.Codebase.Branch (Branch)
import qualified U.Codebase.Branch.Diff as BranchDiff
import U.Codebase.Reference (Reference)
import U.Codebase.Referent (Referent)
import qualified U.Codebase.ShortHash as SBH
import qualified U.Codebase.Sqlite.Operations as Ops
import Unison.Codebase (Codebase)
import qualified Unison.Codebase as Codebase
import Unison.Name (Name)
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import qualified Unison.Server.Backend as Backend
import Unison.Symbol (Symbol)
import qualified Unison.Util.List as ListUtils

serveDiffNamespaces ::
  forall m.
  MonadIO m =>
  Codebase m Symbol Ann ->
  SBH.ShortNamespaceHash ->
  SBH.ShortNamespaceHash ->
  Backend.Backend m (NamespaceDiffResponse Referent Reference)
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
      ( SBH.ShortNamespaceHash ->
        Backend.Backend m (Branch m)
      )
    resolveNamespaceFromHash snh = do
      hashMatches <- lift $ Codebase.runTransaction codebase $ Ops.namespaceHashesByPrefix snh
      case Set.toList hashMatches of
        [] -> throwError $ Backend.CouldntExpandNamespaceHash snh
        [nsh] -> (lift $ Codebase.getShallowBranchForHash codebase nsh) `whenNothingM` throwError (Backend.NoNamespaceForHash nsh)
        _ -> throwError . Backend.AmbiguousNamespaceHash snh $ hashMatches

-- | All the terms which exist _only_ in one side or the other of the diff.
-- Terms which are unchanged between from<->to are omitted.
data NamespaceDiffResponse termRef typeRef = NamespaceDiffResponse
  { onlyFrom :: DiffContents termRef typeRef,
    onlyTo :: DiffContents termRef typeRef
  }

instance (ToJSON termRef, ToJSON typeRef) => ToJSON (NamespaceDiffResponse termRef typeRef) where
  toJSON (NamespaceDiffResponse {onlyFrom, onlyTo}) =
    Aeson.object
      [ "onlyFrom" .= onlyFrom,
        "onlyTo" .= onlyTo
      ]

data DiffContents termRef typeRef = DiffContents
  { terms :: Map Name [termRef],
    types :: Map Name [typeRef]
  }

instance (ToJSON termRef, ToJSON typeRef) => ToJSON (DiffContents termRef typeRef) where
  toJSON (DiffContents {terms, types}) =
    Aeson.object
      [ "terms" .= terms,
        "types" .= types
      ]
