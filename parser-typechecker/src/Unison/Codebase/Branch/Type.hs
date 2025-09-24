module Unison.Codebase.Branch.Type
  ( NamespaceHash,
    head,
    headHash,
    namespaceHash,
    Branch (..),
    Branch0 (asUnconflicted),
    branch0,
    terms_,
    types_,
    children_,
    nonEmptyChildren,
    history_,
    edits_,
    isEmpty0,
    deepDefns,
    deepTerms,
    deepTypes,
    deepPaths,
    deleteLibdeps,
    Star,
    UnwrappedBranch,
  )
where

import Control.Lens hiding (children, cons, transform, uncons)
import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.Bitraversable (bitraverse)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as Set.NonEmpty
import U.Codebase.HashTags (BranchHash (..), CausalHash, PatchHash (..))
import Unison.Codebase.Causal.Type (Causal)
import Unison.Codebase.Causal.Type qualified as Causal
import Unison.Codebase.Metadata qualified as Metadata
import Unison.Codebase.Patch (Patch)
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Hash (HashFor (..))
import Unison.Hash qualified as Hash
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.Reference (TypeReference, TermReference)
import Unison.Referent (Referent)
import Unison.UnconflictedLocalDefnsView (UnconflictedLocalDefnsView)
import Unison.UnconflictedLocalDefnsView qualified as UnconflictedLocalDefnsView
import Unison.Util.Conflicted (Conflicted (..))
import Unison.Util.Defn (Defn (..), DefnF)
import Unison.Util.Defns (Defns (..), DefnsF)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as R
import Unison.Util.Relation qualified as Relation
import Unison.Util.Star2 qualified as Star2
import Prelude hiding (head, read, subtract)

-- | A node in the Unison namespace hierarchy
-- along with its history.
newtype Branch m = Branch {_history :: UnwrappedBranch m}
  deriving (Eq, Ord)

type UnwrappedBranch m = Causal m (Branch0 m)

-- | A Hash for a namespace itself, it doesn't incorporate any history.
type NamespaceHash m = Hash.HashFor (Branch0 m)

type Star r n = Metadata.Star r n

head :: Branch m -> Branch0 m
head (Branch c) = Causal.head c

headHash :: Branch m -> CausalHash
headHash (Branch c) = Causal.currentHash c

namespaceHash :: forall m. Branch m -> BranchHash
namespaceHash (Branch c) = coerce @(HashFor (Branch0 m)) @BranchHash (Causal.valueHash c)

-- | A node in the Unison namespace hierarchy.
--
-- '_terms' and '_types' are the declarations at this level.
-- '_children' are the nodes one level below us.
-- '_edits' are the 'Patch's stored at this node in the code.
--
-- The remaining fields are derived from the four above.
-- None of the record fields are exported to avoid accidental tweaking without updating the
-- associated derived fields.
--
-- Use either the lensy accessors or the field getters.
data Branch0 m = Branch0
  { _terms :: Star Referent NameSegment,
    _types :: Star TypeReference NameSegment,
    -- | Note the 'Branch' here, not 'Branch0'.
    -- Every level in the tree has a history.
    _children :: Map NameSegment (Branch m),
    _edits :: Map NameSegment (PatchHash, m Patch),
    -- | True if a branch and its children have no definitions or edits in them.
    -- (Computed recursively, and small enough to justify storing here to avoid computing more than once.)
    _isEmpty0 :: Bool,
    -- names for this branch and its children
    _deepTerms :: Relation Referent Name,
    _deepTypes :: Relation TypeReference Name,
    _deepPaths :: Set Path,
    asUnconflicted ::
      Either
        ( Defn
            (Conflicted Name Referent)
            (Conflicted Name TypeReference)
        )
        UnconflictedLocalDefnsView
  }

instance Eq (Branch0 m) where
  a == b =
    _terms a == _terms b
      && _types a == _types b
      && _children a == _children b
      && (fmap fst . _edits) a == (fmap fst . _edits) b

history_ :: Iso' (Branch m) (UnwrappedBranch m)
history_ = iso _history Branch

edits_ :: Lens' (Branch0 m) (Map NameSegment (PatchHash, m Patch))
edits_ =
  lens
    _edits
    ( \b0 e ->
        b0 {_edits = e}
          & deriveIsEmpty
    )

terms_ :: Lens' (Branch0 m) (Star Referent NameSegment)
terms_ =
  lens
    _terms
    \branch terms ->
      branch {_terms = terms}
        & deriveDeepTerms
        & deriveAsUnconflicted
        & deriveIsEmpty

types_ :: Lens' (Branch0 m) (Star TypeReference NameSegment)
types_ =
  lens
    _types
    \branch types ->
      branch {_types = types}
        & deriveDeepTypes
        & deriveAsUnconflicted
        & deriveIsEmpty

isEmpty0 :: Branch0 m -> Bool
isEmpty0 = _isEmpty0

deepDefns :: Branch0 m -> Defns (Relation Referent Name) (Relation TypeReference Name)
deepDefns branch =
  Defns { terms = _deepTerms branch, types = _deepTypes branch }

deepTerms :: Branch0 m -> Relation Referent Name
deepTerms = _deepTerms

deepTypes :: Branch0 m -> Relation TypeReference Name
deepTypes = _deepTypes

deepPaths :: Branch0 m -> Set Path
deepPaths = _deepPaths

children_ :: Lens' (Branch0 m) (Map NameSegment (Branch m))
children_ = lens _children (\Branch0 {_terms, _types, _edits} x -> branch0 _terms _types x _edits)

nonEmptyChildren :: Branch0 m -> Map NameSegment (Branch m)
nonEmptyChildren b =
  b
    & _children
    & Map.filter (not . isEmpty0 . head)

-- creates a Branch0 from the primary fields and derives the others.
branch0 ::
  forall m.
  Metadata.Star Referent NameSegment ->
  Metadata.Star TypeReference NameSegment ->
  Map NameSegment (Branch m) ->
  Map NameSegment (PatchHash, m Patch) ->
  Branch0 m
branch0 terms types children edits =
  Branch0
    { _terms = terms,
      _types = types,
      _children = children,
      _edits = edits,
      _isEmpty0 = False,
      -- These are all overwritten immediately
      _deepTerms = R.empty,
      _deepTypes = R.empty,
      _deepPaths = Set.empty,
      asUnconflicted = undefined
    }
    & deriveDeepTerms
    & deriveDeepTypes
    & deriveAsUnconflicted
    & deriveDeepPaths
    & deriveIsEmpty

deriveIsEmpty :: Branch0 m -> Branch0 m
deriveIsEmpty b0 =
  let isEmpty' =
        R.null (Star2.d1 $ _terms b0)
          && R.null (Star2.d1 $ _types b0)
          && Map.null (_edits b0)
          && all (isEmpty0 . head) (_children b0)
   in b0 {_isEmpty0 = isEmpty'}

-- | Derive the 'deepTerms' field of a branch.
deriveDeepTerms :: Branch0 m -> Branch0 m
deriveDeepTerms branch =
  branch {_deepTerms = R.fromList (makeDeepTerms branch)}
  where
    makeDeepTerms :: Branch0 m -> [(Referent, Name)]
    makeDeepTerms branch = State.evalState (go (Seq.singleton ([], 0, branch)) mempty) Set.empty
      where
        -- `reversePrefix` might be ["Nat", "base", "lib"], and `b0` the `Nat` sub-namespace.
        -- Then `R.toList` might produce the NameSegment "+", and we put the two together to
        -- construct the name `Name Relative ("+" :| ["Nat","base","lib"])`.
        go ::
          forall m.
          Seq (DeepChildAcc m) ->
          [(Referent, Name)] ->
          DeepState [(Referent, Name)]
        go Seq.Empty acc = pure acc
        go (e@(reversePrefix, _, b0) Seq.:<| work) acc = do
          let terms :: [(Referent, Name)]
              terms =
                map
                  (second (Name.fromReverseSegments . (NonEmpty.:| reversePrefix)))
                  (R.toList (Star2.d1 (_terms b0)))
          children <- deepChildrenHelper e
          go (work <> children) (terms <> acc)

-- | Derive the 'deepTypes' field of a branch.
deriveDeepTypes :: forall m. Branch0 m -> Branch0 m
deriveDeepTypes branch =
  branch {_deepTypes = R.fromList (makeDeepTypes branch)}
  where
    makeDeepTypes :: Branch0 m -> [(TypeReference, Name)]
    makeDeepTypes branch = State.evalState (go (Seq.singleton ([], 0, branch)) mempty) Set.empty
      where
        go ::
          Seq (DeepChildAcc m) ->
          [(TypeReference, Name)] ->
          DeepState [(TypeReference, Name)]
        go Seq.Empty acc = pure acc
        go (e@(reversePrefix, _, b0) Seq.:<| work) acc = do
          let types :: [(TypeReference, Name)]
              types = map (second (Name.fromReverseSegments . (NonEmpty.:| reversePrefix))) (R.toList (Star2.d1 (_types b0)))
          children <- deepChildrenHelper e
          go (work <> children) (types <> acc)

-- | Derive the 'asUnconflicted' field of a branch.
deriveAsUnconflicted :: Branch0 m -> Branch0 m
deriveAsUnconflicted branch =
  branch {asUnconflicted = UnconflictedLocalDefnsView.fromDefns <$> narrowDefns defns}
  where
    branchWithoutLibdeps = deleteLibdeps branch
    defns =
      Defns
        { terms = Relation.swap (deepTerms branchWithoutLibdeps),
          types = Relation.swap (deepTypes branchWithoutLibdeps)
        }

-- | "Narrow" a namespace that may contain conflicted names, resulting in either a failure (if we find a conflicted
-- name), or the narrowed nametree without conflicted names.
narrowDefns ::
  forall term typ.
  (Ord term, Ord typ) =>
  DefnsF (Relation Name) term typ ->
  Either
    (DefnF (Conflicted Name) term typ)
    (DefnsF (Map Name) term typ)
narrowDefns =
  bitraverse
    (go (\name -> TermDefn . Conflicted name))
    (go (\name -> TypeDefn . Conflicted name))
  where
    go :: forall ref x. (Ord ref) => (Name -> NESet ref -> x) -> Relation Name ref -> Either x (Map Name ref)
    go conflicted =
      Map.traverseWithKey unconflicted . Relation.domain
      where
        unconflicted :: Name -> Set ref -> Either x ref
        unconflicted name refs0
          | Set.NonEmpty.size refs == 1 = Right (Set.NonEmpty.findMin refs)
          | otherwise = Left (conflicted name refs)
          where
            refs = Set.NonEmpty.unsafeFromSet refs0

-- | Derive the 'deepPaths' field of a branch.
deriveDeepPaths :: forall m. Branch0 m -> Branch0 m
deriveDeepPaths branch =
  branch {_deepPaths = makeDeepPaths branch}
  where
    makeDeepPaths :: Branch0 m -> Set Path
    makeDeepPaths branch = State.evalState (go (Seq.singleton ([], 0, branch)) mempty) Set.empty
      where
        go :: Seq (DeepChildAcc m) -> Set Path -> DeepState (Set Path)
        go Seq.Empty acc = pure acc
        go (e@(reversePrefix, _, b0) Seq.:<| work) acc = do
          let paths :: Set Path
              paths =
                if isEmpty0 b0
                  then Set.empty
                  else (Set.singleton . Path.fromList . reverse) reversePrefix
          children <- deepChildrenHelper e
          go (work <> children) (paths <> acc)

-- | State used by deepChildrenHelper to determine whether to descend into a child branch.
-- Contains the set of visited namespace hashes.
type DeepState = State (Set BranchHash)

-- | Represents a unit of remaining work in traversing children for computing `deep*`.
-- (reverse prefix to a branch, the number of `lib` segments in the reverse prefix, and the branch itself)
type DeepChildAcc m = ([NameSegment], Int, Branch0 m)

-- | Helper for knowing whether to descend into a child branch or not.
-- Accepts child namespaces with previously unseen hashes, and any nested under 1 or fewer `lib` segments.
deepChildrenHelper :: forall m. DeepChildAcc m -> DeepState (Seq (DeepChildAcc m))
deepChildrenHelper (reversePrefix, libDepth, b0) = do
  let go :: (NameSegment, Branch m) -> DeepState (Seq (DeepChildAcc m))
      go (ns, b) = do
        let h = namespaceHash b
        result <- do
          let isShallowDependency = libDepth <= 1
          isUnseenNamespace <- State.gets (Set.notMember h)
          pure
            if isShallowDependency || isUnseenNamespace
              then
                let libDepth' = if ns == NameSegment.libSegment then libDepth + 1 else libDepth
                 in Seq.singleton (ns : reversePrefix, libDepth', head b)
              else Seq.empty
        State.modify' (Set.insert h)
        pure result
  Monoid.foldMapM go (Map.toList (nonEmptyChildren b0))

-- | @deleteLibdeps branch@ deletes all libdeps from @branch@.
deleteLibdeps :: Branch0 m -> Branch0 m
deleteLibdeps =
  over children_ (Map.delete NameSegment.libSegment)
