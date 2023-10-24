module Unison.Merge.Diff
  ( TwoWay (..),
    TwoOrThreeWay (..),
    nameBasedNamespaceDiff,
  )
where

import Control.Lens ((^.))
import Data.ByteString.Short (ShortByteString)
import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith)
import Data.Set qualified as Set
import Data.These (These (..))
import U.Codebase.Reference (TermReferenceId, TypeReference, TypeReferenceId)
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration qualified as V1 (Decl)
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.HashQualified' qualified as HQ'
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.NamespaceTypes (Defns (..))
import Unison.Merge.Synhash qualified as Synhash
import Unison.Name (Name)
import Unison.Prelude hiding (catMaybes)
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.PrettyPrintEnv qualified as Ppe
import Unison.Referent qualified as V1 (Referent)
import Unison.Referent qualified as V1.Referent
import Unison.Term qualified as V1 (Term)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Var (Var)

-- A couple of internal types and type aliases of questionable utility.

type Synhashes =
  Defns (Map Name Hash) (Map Name Hash)

type Diff =
  Defns (Map Name (DiffOp Hash)) (Map Name (DiffOp Hash))

data TwoWay a = TwoWay
  { alice :: !a,
    bob :: !a
  }
  deriving stock (Functor, Generic)

instance Semigroup a => Semigroup (TwoWay a) where
  TwoWay ax bx <> TwoWay ay by = TwoWay (ax <> ay) (bx <> by)

data TwoOrThreeWay a = TwoOrThreeWay
  { lca :: !(Maybe a),
    alice :: !a,
    bob :: !a
  }
  deriving stock (Functor, Generic)

data ThreeWay a = ThreeWay
  { lca :: !a,
    alice :: !a,
    bob :: !a
  }
  deriving stock (Functor, Generic)

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
  forall a m v.
  (Monad m, Var v) =>
  (TypeReferenceId -> m (V1.Decl v a)) ->
  (TermReferenceId -> m (V1.Term v a)) ->
  TwoOrThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  m (TwoWay Diff)
nameBasedNamespaceDiff loadDecl loadTerm (TwoOrThreeWay maybeLcaDefns aliceDefns bobDefns) = do
  aliceSynhashes <- synhashDefns aliceDefns
  bobSynhashes <- synhashDefns bobDefns
  case maybeLcaDefns of
    Nothing -> pure (twoWayDiff TwoWay {alice = aliceSynhashes, bob = bobSynhashes})
    Just lcaDefns -> do
      lcaSynhashes <- synhashDefns lcaDefns
      pure (threeWayDiff ThreeWay {lca = lcaSynhashes, alice = aliceSynhashes, bob = bobSynhashes})
  where
    synhashDefns :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) -> m Synhashes
    synhashDefns =
      synhashDefnsWith
        loadDecl
        loadTerm
        -- The order isn't important here for syntactic hashing
        (deepNamespaceDefinitionsToPpe aliceDefns `Ppe.addFallback` deepNamespaceDefinitionsToPpe bobDefns)

twoWayDiff :: TwoWay Synhashes -> TwoWay Diff
twoWayDiff synhashes =
  TwoWay
    { alice = synhashesToAdds (synhashes ^. #alice),
      bob = synhashesToAdds (synhashes ^. #bob)
    }
  where
    synhashesToAdds :: Synhashes -> Defns (Map Name (DiffOp Hash)) (Map Name (DiffOp Hash))
    synhashesToAdds Defns {terms, types} =
      Defns
        { terms = Map.map Added terms,
          types = Map.map Added types
        }

threeWayDiff :: ThreeWay Synhashes -> TwoWay Diff
threeWayDiff ThreeWay {lca, alice, bob} =
  TwoWay
    { alice = diffNamespaceDefns lca alice,
      bob = diffNamespaceDefns lca bob
    }

diffNamespaceDefns ::
  Defns (Map Name Hash) (Map Name Hash) ->
  Defns (Map Name Hash) (Map Name Hash) ->
  Defns (Map Name (DiffOp Hash)) (Map Name (DiffOp Hash))
diffNamespaceDefns oldDefns newDefns =
  Defns
    { terms = go (oldDefns ^. #terms) (newDefns ^. #terms),
      types = go (oldDefns ^. #types) (newDefns ^. #types)
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
    (\ref -> arbitraryName (referent1to2 ref) terms)
    (\ref -> arbitraryName ref types)
  where
    arbitraryName :: Ord ref => ref -> BiMultimap ref Name -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
    arbitraryName ref names =
      BiMultimap.lookupDom ref names
        & Set.lookupMin
        & maybe [] \name -> [(HQ'.NameOnly name, HQ'.NameOnly name)]

    -- Our pretty-print env takes V1 referents, which have constructor types, but we can safely throw those constructor
    -- types away, because the constructor reference is all we need to look up in our map.
    referent1to2 :: V1.Referent -> Referent
    referent1to2 = \case
      V1.Referent.Con (ConstructorReference typeRef conId) _conTy -> Referent.Con typeRef conId
      V1.Referent.Ref termRef -> Referent.Ref termRef

------------------------------------------------------------------------------------------------------------------------
-- Syntactic hashing helpers

synhashDefnsWith ::
  (Monad m, Var v) =>
  (TypeReferenceId -> m (V1.Decl v a)) ->
  (TermReferenceId -> m (V1.Term v a)) ->
  PrettyPrintEnv ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  m (Defns (Map Name Hash) (Map Name Hash))
synhashDefnsWith loadDecl loadTerm ppe defns = do
  terms <- BiMultimap.range <$> BiMultimap.unsafeTraverseDom (synhashReferent loadTerm ppe) (defns ^. #terms)
  types <- BiMultimap.range <$> BiMultimap.unsafeTraverseDom (Synhash.hashDecl loadDecl ppe) (defns ^. #types)
  pure Defns {terms, types}

synhashReferent ::
  (Monad m, Var v) =>
  (TermReferenceId -> m (V1.Term v a)) ->
  PrettyPrintEnv ->
  Referent ->
  m Hash
synhashReferent loadTerm ppe = \case
  Referent.Con _ _ -> pure hashThatIsDistinctFromAllTermHashes
  Referent.Ref ref -> Synhash.hashTerm loadTerm ppe ref
  where
    -- TODO explain better why it's fine to give all data constructors the same syntactic hash, so long as it's
    -- different than any term hash. the skinny:
    --   * want datacon->term or vice versa to look like a conflict
    --   * datacon->datacon is fine to consider not-conflicted because a conflict, if any, would appear on the decl
    hashThatIsDistinctFromAllTermHashes :: Hash
    hashThatIsDistinctFromAllTermHashes =
      Hash.Hash (mempty :: ShortByteString)
