{-# LANGUAGE OverloadedRecordDot #-}

module Unison.Merge.Diff
  ( nameBasedNamespaceDiff,
  )
where

import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith)
import Data.Set qualified as Set
import Data.These (These (..))
import U.Codebase.Reference (TypeReference)
import Unison.Hash (Hash)
import Unison.HashQualified' qualified as HQ'
import Unison.Merge.Database (MergeDatabase (..))
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.Synhash qualified as Synhash
import Unison.Merge.ThreeWay (ThreeWay (..))
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Name (Name)
import Unison.Prelude hiding (catMaybes)
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.PrettyPrintEnv qualified as Ppe
import Unison.Referent (Referent)
import Unison.Sqlite (Transaction)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..))

type Diff =
  Defns (Map Name (DiffOp Hash)) (Map Name (DiffOp Hash))

-- | @nameBasedNamespaceDiff loadDecl loadTerm maybeLcaDefns aliceDefns bobDefns@ returns Alice's and Bob's name-based
-- namespace diffs, each in the form:
--
-- > decls :: Map Name (DiffOp Hash)
-- > terms :: Map Name (DiffOp Hash)
--
-- where each name is paired with its diff-op (added, deleted, or updated), relative to the LCA between Alice and Bob's
-- branches. If the hash of a name did not change, it will not appear in the map.
--
-- If there is no LCA (i.e. @maybeLcaDefns@ is @Nothing@), we fall back to a two-way diff, where every name in each of
-- Alice and Bob's branches is considered an add.
nameBasedNamespaceDiff ::
  MergeDatabase ->
  ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Transaction (TwoWay Diff)
nameBasedNamespaceDiff db defns = do
  lca <- synhashDefns defns.lca
  alice <- synhashDefns defns.alice
  bob <- synhashDefns defns.bob
  pure (diffNamespaceDefns lca <$> TwoWay {alice, bob})
  where
    synhashDefns ::
      Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
      Transaction (Defns (Map Name Hash) (Map Name Hash))
    synhashDefns =
      -- FIXME: use cache so we only synhash each thing once
      synhashDefnsWith (Synhash.hashTerm db.loadV1Term ppe) (Synhash.hashDecl db.loadV1Decl ppe)
      where
        ppe :: PrettyPrintEnv
        ppe =
          -- The order isn't important here for syntactic hashing
          (deepNamespaceDefinitionsToPpe defns.alice `Ppe.addFallback` deepNamespaceDefinitionsToPpe defns.bob)

diffNamespaceDefns ::
  Defns (Map Name Hash) (Map Name Hash) ->
  Defns (Map Name Hash) (Map Name Hash) ->
  Defns (Map Name (DiffOp Hash)) (Map Name (DiffOp Hash))
diffNamespaceDefns oldDefns newDefns =
  Defns
    { terms = go oldDefns.terms newDefns.terms,
      types = go oldDefns.types newDefns.types
    }
  where
    go :: Map Name Hash -> Map Name Hash -> Map Name (DiffOp Hash)
    go old new =
      Map.mapMaybe id (alignWith f old new)
      where
        f :: These Hash Hash -> Maybe (DiffOp Hash)
        f = \case
          This x -> Just (Deleted x)
          That y -> Just (Added y)
          These x y
            | x == y -> Nothing
            | otherwise -> Just (Updated x y)

------------------------------------------------------------------------------------------------------------------------
-- Pretty-print env helpers

deepNamespaceDefinitionsToPpe :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) -> PrettyPrintEnv
deepNamespaceDefinitionsToPpe Defns {terms, types} =
  PrettyPrintEnv
    (\ref -> arbitraryName ref terms)
    (\ref -> arbitraryName ref types)
  where
    arbitraryName :: Ord ref => ref -> BiMultimap ref Name -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
    arbitraryName ref names =
      BiMultimap.lookupDom ref names
        & Set.lookupMin
        & maybe [] \name -> [(HQ'.NameOnly name, HQ'.NameOnly name)]

------------------------------------------------------------------------------------------------------------------------
-- Syntactic hashing helpers

synhashDefnsWith ::
  Monad m =>
  (term -> m Hash) ->
  (typ -> m Hash) ->
  Defns (BiMultimap term Name) (BiMultimap typ Name) ->
  m (Defns (Map Name Hash) (Map Name Hash))
synhashDefnsWith hashReferent hashDecl defns = do
  terms <- BiMultimap.range <$> BiMultimap.unsafeTraverseDom hashReferent defns.terms
  types <- BiMultimap.range <$> BiMultimap.unsafeTraverseDom hashDecl defns.types
  pure Defns {terms, types}
