module Unison.Merge.Diff
  ( NamespaceDefns (..),
    nameBasedNamespaceDiff,
  )
where

import Control.Lens ((^.))
import Data.ByteString.Short (ShortByteString)
-- TODO remove me once prelude PR lands
import Data.Generics.Labels ()
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

-- | A generic data structure that holds the (deep) "definitions" (i.e., decls and terms, sans library dependencies) in
-- a namespace.
--
-- FIXME this should be defined somewhere else, or tweaked, or deleted - it was just invented to write this particular
-- module and may not be generally useful enough to require callers of this code to use.
data NamespaceDefns f dk dv tk tv = NamespaceDefns
  { decls :: !(f dk dv),
    terms :: !(f tk tv)
  }
  deriving stock (Generic)

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
  Maybe (NamespaceDefns BiMultimap TypeReference Name Referent Name) ->
  NamespaceDefns BiMultimap TypeReference Name Referent Name ->
  NamespaceDefns BiMultimap TypeReference Name Referent Name ->
  m
    ( NamespaceDefns Map Name (DiffOp Hash) Name (DiffOp Hash),
      NamespaceDefns Map Name (DiffOp Hash) Name (DiffOp Hash)
    )
nameBasedNamespaceDiff loadDecl loadTerm maybeLcaDefns aliceDefns bobDefns = do
  aliceSynhashes <- synhashDefns aliceDefns
  bobSynhashes <- synhashDefns bobDefns
  case maybeLcaDefns of
    Nothing -> pure (twoWayDiff aliceSynhashes bobSynhashes)
    Just lcaDefns -> do
      lcaSynhashes <- synhashDefns lcaDefns
      pure (threeWayDiff lcaSynhashes aliceSynhashes bobSynhashes)
  where
    synhashDefns ::
      NamespaceDefns BiMultimap TypeReference Name Referent Name ->
      m (NamespaceDefns BiMultimap Hash Name Hash Name)
    synhashDefns =
      synhashDefnsWith
        loadDecl
        loadTerm
        -- The order isn't important here for syntactic hashing
        (deepNamespaceDefinitionsToPpe aliceDefns `Ppe.addFallback` deepNamespaceDefinitionsToPpe bobDefns)

twoWayDiff ::
  NamespaceDefns BiMultimap hash1 name1 hash1 name1 ->
  NamespaceDefns BiMultimap hash2 name2 hash2 name2 ->
  ( NamespaceDefns Map name1 (DiffOp hash1) name1 (DiffOp hash1),
    NamespaceDefns Map name2 (DiffOp hash2) name2 (DiffOp hash2)
  )
twoWayDiff aliceSynhashes bobSynhashes =
  (synhashesToAdds aliceSynhashes, synhashesToAdds bobSynhashes)
  where
    synhashesToAdds ::
      NamespaceDefns BiMultimap hash name hash name ->
      NamespaceDefns Map name (DiffOp hash) name (DiffOp hash)
    synhashesToAdds NamespaceDefns {decls, terms} =
      NamespaceDefns
        { decls = Map.map Added (BiMultimap.range decls),
          terms = Map.map Added (BiMultimap.range terms)
        }

threeWayDiff ::
  (Ord name, Eq hash) =>
  NamespaceDefns BiMultimap hash name hash name ->
  NamespaceDefns BiMultimap hash name hash name ->
  NamespaceDefns BiMultimap hash name hash name ->
  ( NamespaceDefns Map name (DiffOp hash) name (DiffOp hash),
    NamespaceDefns Map name (DiffOp hash) name (DiffOp hash)
  )
threeWayDiff lcaSynhashes aliceSynhashes bobSynhashes =
  (diffNamespaceDefns lcaSynhashes aliceSynhashes, diffNamespaceDefns lcaSynhashes bobSynhashes)

diffNamespaceDefns ::
  forall hash name.
  (Eq hash, Ord name) =>
  NamespaceDefns BiMultimap hash name hash name ->
  NamespaceDefns BiMultimap hash name hash name ->
  NamespaceDefns Map name (DiffOp hash) name (DiffOp hash)
diffNamespaceDefns oldDefns newDefns =
  NamespaceDefns
    { decls = go (oldDefns ^. #decls) (newDefns ^. #decls),
      terms = go (oldDefns ^. #terms) (newDefns ^. #terms)
    }
  where
    go old new =
      Map.mapMaybe id (alignWith f (BiMultimap.range old) (BiMultimap.range new))
      where
        f :: These hash hash -> Maybe (DiffOp hash)
        f = \case
          This x -> Just (Deleted x)
          That y -> Just (Added y)
          These x y
            | x == y -> Nothing
            | otherwise -> Just (Updated x y)

------------------------------------------------------------------------------------------------------------------------
-- Pretty-print env helpers

deepNamespaceDefinitionsToPpe :: NamespaceDefns BiMultimap TypeReference Name Referent Name -> PrettyPrintEnv
deepNamespaceDefinitionsToPpe NamespaceDefns {decls, terms} =
  PrettyPrintEnv
    (\ref -> arbitraryName (referent1to2 ref) terms)
    (\ref -> arbitraryName ref decls)
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
  NamespaceDefns BiMultimap TypeReference Name Referent Name ->
  m (NamespaceDefns BiMultimap Hash Name Hash Name)
synhashDefnsWith loadDecl loadTerm ppe defns = do
  decls <- BiMultimap.unsafeTraverseDom (Synhash.hashDecl loadDecl ppe) (defns ^. #decls)
  terms <- BiMultimap.unsafeTraverseDom (synhashReferent loadTerm ppe) (defns ^. #terms)
  pure NamespaceDefns {decls, terms}

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
