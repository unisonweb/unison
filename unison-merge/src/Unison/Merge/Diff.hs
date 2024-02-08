module Unison.Merge.Diff
  ( TwoWay (..),
    TwoOrThreeWay (..),
    nameBasedNamespaceDiff,
  )
where

import Control.Lens ((^.))
import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith)
import Data.Set qualified as Set
import Data.These (These (..))
import U.Codebase.Reference (TypeReference)
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.Hash (Hash)
import Unison.HashQualified' qualified as HQ'
import Unison.Merge.Database (MergeDatabase (..), referent2to1)
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.Synhash qualified as Synhash
import Unison.Name (Name)
import Unison.Prelude hiding (catMaybes)
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.PrettyPrintEnv qualified as Ppe
import Unison.Referent qualified as V1 (Referent)
import Unison.Referent qualified as V1.Referent
import Unison.Sqlite (Transaction)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Nametree (Defns (..))

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

instance Applicative TwoWay where
  pure x = TwoWay x x
  TwoWay f g <*> TwoWay x y = TwoWay (f x) (g y)

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
  MergeDatabase ->
  TwoOrThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  Transaction (TwoWay Diff)
nameBasedNamespaceDiff db@MergeDatabase {loadV1Term, loadV1Decl} (TwoOrThreeWay maybeLcaDefns aliceDefns bobDefns) = do
  aliceSynhashes <- synhashDefns aliceDefns
  bobSynhashes <- synhashDefns bobDefns
  case maybeLcaDefns of
    Nothing -> pure (twoWayDiff TwoWay {alice = aliceSynhashes, bob = bobSynhashes})
    Just lcaDefns -> do
      lcaSynhashes <- synhashDefns lcaDefns
      pure (threeWayDiff ThreeWay {lca = lcaSynhashes, alice = aliceSynhashes, bob = bobSynhashes})
  where
    synhashDefns :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) -> Transaction Synhashes
    synhashDefns =
      -- FIXME: use cache so we only synhash each thing once
      synhashDefnsWith (Synhash.hashTerm loadV1Term ppe <=< referent2to1 db) (Synhash.hashDecl loadV1Decl ppe)
      where
        ppe :: PrettyPrintEnv
        ppe =
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
  Monad m =>
  (Referent -> m Hash) ->
  (TypeReference -> m Hash) ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  m (Defns (Map Name Hash) (Map Name Hash))
synhashDefnsWith hashReferent hashDecl defns = do
  terms <- BiMultimap.range <$> BiMultimap.unsafeTraverseDom hashReferent (defns ^. #terms)
  types <- BiMultimap.range <$> BiMultimap.unsafeTraverseDom hashDecl (defns ^. #types)
  pure Defns {terms, types}
