{-# LANGUAGE OverloadedRecordDot #-}

module Unison.Merge.Diff
  ( nameBasedNamespaceDiff,
  )
where

import Control.Lens (over)
import Data.Bitraversable (bitraverse)
import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith)
import Data.Set qualified as Set
import Data.These (These (..))
import U.Codebase.Reference (TypeReference)
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DD
import Unison.Hash (Hash)
import Unison.HashQualified' qualified as HQ'
import Unison.Merge.Database (MergeDatabase (..))
import Unison.Merge.DeclNameLookup (DeclNameLookup, expectConstructorNames)
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.Synhash qualified as Synhash
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.ThreeWay (ThreeWay (..))
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Name (Name)
import Unison.Prelude hiding (catMaybes)
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.PrettyPrintEnv qualified as Ppe
import Unison.Reference (TypeReferenceId)
import Unison.Referent (Referent)
import Unison.Sqlite (Transaction)
import Unison.Syntax.Name qualified as Name
import Unison.Type (Type)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Var (Var)

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
  ThreeWay DeclNameLookup ->
  ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Transaction
    ( TwoWay
        ( DefnsF
            (Map Name)
            (DiffOp (Synhashed Referent))
            (DiffOp (Synhashed TypeReference))
        )
    )
nameBasedNamespaceDiff db declNameLookups defns = do
  diffs <- sequence (synhashDefns <$> declNameLookups <*> defns)
  pure (diffNamespaceDefns diffs.lca <$> TwoWay {alice = diffs.alice, bob = diffs.bob})
  where
    synhashDefns ::
      DeclNameLookup ->
      Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
      Transaction (DefnsF (Map Name) (Synhashed Referent) (Synhashed TypeReference))
    synhashDefns declNameLookup =
      -- FIXME: use cache so we only synhash each thing once
      synhashDefnsWith
        (Synhash.hashTerm db.loadV1Term ppe)
        ( \name ->
            Synhash.hashDecl
              (withAccurateConstructorNames db.loadV1Decl declNameLookup name)
              ppe
              name
        )
      where
        ppe :: PrettyPrintEnv
        ppe =
          -- The order isn't important here for syntactic hashing
          (deepNamespaceDefinitionsToPpe defns.alice `Ppe.addFallback` deepNamespaceDefinitionsToPpe defns.bob)

withAccurateConstructorNames ::
  forall a v.
  Var v =>
  (TypeReferenceId -> Transaction (Decl v a)) ->
  DeclNameLookup ->
  Name ->
  TypeReferenceId ->
  Transaction (Decl v a)
withAccurateConstructorNames load declNameLookup name ref = do
  load ref <&> over (DD.declAsDataDecl_ . DD.constructors_) setConstructorNames
  where
    setConstructorNames :: [(a, v, Type v a)] -> [(a, v, Type v a)]
    setConstructorNames =
      zipWith
        (\realConName (ann, _junkConName, typ) -> (ann, Name.toVar realConName, typ))
        (expectConstructorNames declNameLookup name)

diffNamespaceDefns ::
  DefnsF (Map Name) (Synhashed Referent) (Synhashed TypeReference) ->
  DefnsF (Map Name) (Synhashed Referent) (Synhashed TypeReference) ->
  DefnsF (Map Name) (DiffOp (Synhashed Referent)) (DiffOp (Synhashed TypeReference))
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
          This x -> Just (DiffOp'Delete x)
          That y -> Just (DiffOp'Add y)
          These x y
            | x == y -> Nothing
            | otherwise -> Just (DiffOp'Update x y)

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
  (Name -> typ -> m Hash) ->
  Defns (BiMultimap term Name) (BiMultimap typ Name) ->
  m (DefnsF (Map Name) (Synhashed term) (Synhashed typ))
synhashDefnsWith hashTerm hashType = do
  bitraverse
    (traverse hashTerm1 . BiMultimap.range)
    (Map.traverseWithKey hashType1 . BiMultimap.range)
  where
    hashTerm1 term = do
      hash <- hashTerm term
      pure (Synhashed hash term)

    hashType1 name typ = do
      hash <- hashType name typ
      pure (Synhashed hash typ)
