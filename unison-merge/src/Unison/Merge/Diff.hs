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
import Unison.Merge.Synhashed (Synhashed (..))
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

-- | @nameBasedNamespaceDiff db defns@ returns Alice's and Bob's name-based namespace diffs, each in the form:
--
-- > decls :: Map Name (DiffOp (Synhashed TypeReference))
-- > terms :: Map Name (DiffOp (Synhashed Referent))
--
-- where each name is paired with its diff-op (added, deleted, or updated), relative to the LCA between Alice and Bob's
-- branches. If the hash of a name did not change, it will not appear in the map.
--
-- If there is no LCA, this operation is equivalent to a two-way diff, where every name in each of Alice and Bob's
-- branches is considered an add.
nameBasedNamespaceDiff ::
  MergeDatabase ->
  ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Transaction
    ( TwoWay
        ( Defns
            (Map Name (DiffOp (Synhashed Referent)))
            (Map Name (DiffOp (Synhashed TypeReference)))
        )
    )
nameBasedNamespaceDiff db defns = do
  lca <- synhashDefns defns.lca
  alice <- synhashDefns defns.alice
  bob <- synhashDefns defns.bob
  pure (diffNamespaceDefns lca <$> TwoWay {alice, bob})
  where
    synhashDefns ::
      Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
      Transaction (Defns (Map Name (Synhashed Referent)) (Map Name (Synhashed TypeReference)))
    synhashDefns =
      -- FIXME: use cache so we only synhash each thing once
      synhashDefnsWith (Synhash.hashTerm db.loadV1Term ppe) (Synhash.hashDecl db.loadV1Decl ppe)
      where
        ppe :: PrettyPrintEnv
        ppe =
          -- The order isn't important here for syntactic hashing
          (deepNamespaceDefinitionsToPpe defns.alice `Ppe.addFallback` deepNamespaceDefinitionsToPpe defns.bob)

diffNamespaceDefns ::
  Defns (Map Name (Synhashed Referent)) (Map Name (Synhashed TypeReference)) ->
  Defns (Map Name (Synhashed Referent)) (Map Name (Synhashed TypeReference)) ->
  Defns (Map Name (DiffOp (Synhashed Referent))) (Map Name (DiffOp (Synhashed TypeReference)))
diffNamespaceDefns oldDefns newDefns =
  Defns
    { terms = go oldDefns.terms newDefns.terms,
      types = go oldDefns.types newDefns.types
    }
  where
    go :: Map Name (Synhashed ref) -> Map Name (Synhashed ref) -> Map Name (DiffOp (Synhashed ref))
    go old new =
      Map.mapMaybe id (alignWith f old new)
      where
        f :: Eq x => These x x -> Maybe (DiffOp x)
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
  m (Defns (Map Name (Synhashed term)) (Map Name (Synhashed typ)))
synhashDefnsWith hashTerm hashType defns = do
  terms <- BiMultimap.range <$> BiMultimap.unsafeTraverseDom hashTerm1 defns.terms
  types <- BiMultimap.range <$> BiMultimap.unsafeTraverseDom hashType1 defns.types
  pure Defns {terms, types}
  where
    hashTerm1 term = do
      hash <- hashTerm term
      pure (Synhashed hash term)

    hashType1 typ = do
      hash <- hashType typ
      pure (Synhashed hash typ)
