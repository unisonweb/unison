module U.Codebase.Branch.Diff
  ( TreeDiff (..),
    NameChanges (..),
    DefinitionDiffs (..),
    Diff (..),
    NameBasedDiff (..),
    diffBranches,
    nameChanges,
    nameBasedDiff,
  )
where

import Control.Comonad.Cofree
import Control.Lens (ifoldMap)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Semialign as Align
import qualified Data.Set as Set
import Data.These
import U.Codebase.Branch
import qualified U.Codebase.Branch.Type as Branch
import qualified U.Codebase.Causal as Causal
import U.Codebase.Reference (Reference)
import U.Codebase.Referent (Referent)
import qualified U.Codebase.Referent as Referent
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment)
import Unison.Prelude
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Relation as Relation

data Diff a = Diff
  { adds :: Set a,
    removals :: Set a
  }
  deriving (Show, Eq, Ord)

-- | Represents the changes to definitions at a given path, not including child paths.
--
-- Note: doesn't yet include any info on metadata or patch diffs. Feel free to add it.
data DefinitionDiffs = DefinitionDiffs
  { termDiffs :: Map NameSegment (Diff Referent),
    typeDiffs :: Map NameSegment (Diff Reference)
    -- termMetadataDiffs :: Map (NameSegment, Referent) (Diff Reference),
    -- typeMetadataDiffs :: Map (NameSegment, Reference) (Diff Reference)
    -- patchDiffs :: Map NameSegment (Diff ())
  }
  deriving stock (Show, Eq, Ord)

instance Semigroup DefinitionDiffs where
  a <> b =
    DefinitionDiffs
      { termDiffs = termDiffs a <> termDiffs b,
        typeDiffs = typeDiffs a <> typeDiffs b
      }

instance Monoid DefinitionDiffs where
  mempty = DefinitionDiffs mempty mempty

-- | A tree of local diffs. Each node of the tree contains the definition diffs at that path.
newtype TreeDiff = TreeDiff
  { unTreeDiff :: Cofree (Map NameSegment) DefinitionDiffs
  }
  deriving stock (Show, Eq, Ord)

instance Semigroup TreeDiff where
  TreeDiff (a :< as) <> TreeDiff (b :< bs) =
    TreeDiff $ (a <> b) :< (Map.unionWith mergeCofrees as bs)
    where
      mergeCofrees x y = unTreeDiff (TreeDiff x <> TreeDiff y)

instance Monoid TreeDiff where
  mempty = TreeDiff (mempty :< mempty)

instance Lens.AsEmpty TreeDiff where
  _Empty = Lens.only mempty

-- | A summary of a 'TreeDiff', containing all names added and removed.
-- Note that there isn't a clear notion of a name "changing" since conflicts might muddy the notion
-- by having multiple copies of both the from and to names, so we just talk about adds and
-- removals instead.
data NameChanges = NameChanges
  { termNameAdds :: [(Name, Referent)],
    termNameRemovals :: [(Name, Referent)],
    typeNameAdds :: [(Name, Reference)],
    typeNameRemovals :: [(Name, Reference)]
  }

instance Semigroup NameChanges where
  NameChanges a b c d <> NameChanges a2 b2 c2 d2 =
    NameChanges (a <> a2) (b <> b2) (c <> c2) (d <> d2)

instance Monoid NameChanges where
  mempty = NameChanges mempty mempty mempty mempty

-- | A name-based diff for namespaces `N1` and `N2` is (for both terms and types) a relation between references, where
-- `a R b` if:
--
--   1. `a` has name `n` in `N1` and `b` has the same name `n` in `N2`
--   2. `a` != `b`
data NameBasedDiff = NameBasedDiff
  { terms :: Relation Reference Reference,
    types :: Relation Reference Reference
  }
  deriving stock (Generic, Show)

instance Monoid NameBasedDiff where
  mempty = NameBasedDiff mempty mempty
  mappend = (<>)

instance Semigroup NameBasedDiff where
  NameBasedDiff terms0 types0 <> NameBasedDiff terms1 types1 =
    NameBasedDiff (terms0 <> terms1) (types0 <> types1)

-- | Diff two Branches, returning a tree containing all of the changes
diffBranches :: forall m. Monad m => Branch m -> Branch m -> m TreeDiff
diffBranches from to = do
  let termDiffs = diffMap (Branch.terms from) (Branch.terms to)
  let typeDiffs = diffMap (Branch.types from) (Branch.types to)
  let defDiff = DefinitionDiffs {termDiffs, typeDiffs}
  childDiff <- do
    Align.align (children from) (children to)
      & wither \case
        This ca -> do
          -- TODO: For the names index we really don't need to know which exact
          -- names were removed, we just need to delete from the index using a
          -- prefix query, this would be faster than crawling to get all the deletes.
          removedChildBranch <- Causal.value ca
          Just . unTreeDiff <$> diffBranches removedChildBranch Branch.empty
        That ca -> do
          newChildBranch <- Causal.value ca
          Just . unTreeDiff <$> diffBranches Branch.empty newChildBranch
        These fromC toC
          | Causal.valueHash fromC == Causal.valueHash toC -> do
              -- This child didn't change.
              pure Nothing
          | otherwise -> do
              fromChildBranch <- Causal.value fromC
              toChildBranch <- Causal.value toC
              diffBranches fromChildBranch toChildBranch >>= \case
                Lens.Empty -> pure Nothing
                TreeDiff cfr -> pure . Just $ cfr
  pure $ TreeDiff (defDiff :< childDiff)
  where
    diffMap :: forall ref. Ord ref => Map NameSegment (Map ref (m MdValues)) -> Map NameSegment (Map ref (m MdValues)) -> Map NameSegment (Diff ref)
    diffMap l r =
      Align.align l r
        & fmap \case
          This refs -> Diff {removals = Map.keysSet refs, adds = mempty}
          That refs -> Diff {removals = mempty, adds = Map.keysSet refs}
          These l' r' ->
            let lRefs = Map.keysSet l'
                rRefs = Map.keysSet r'
             in Diff {removals = lRefs `Set.difference` rRefs, adds = rRefs `Set.difference` lRefs}

-- | Get a summary of all of the name adds and removals from a tree diff.
--
-- The provided name will be prepended to all names in the output diff, and can be useful if diffing branches at a
-- specific sub-tree, but you can pass 'Nothing' if you're diffing from the root.
nameChanges ::
  Maybe Name ->
  TreeDiff ->
  NameChanges
nameChanges namePrefix (TreeDiff (DefinitionDiffs {termDiffs, typeDiffs} :< children)) =
  let (termNameAdds, termNameRemovals) =
        termDiffs
          & ifoldMap \ns diff ->
            let name = appendName ns
             in (listifyNames name $ adds diff, listifyNames name $ removals diff)
      (typeNameAdds, typeNameRemovals) =
        typeDiffs
          & ifoldMap \ns diff ->
            let name = appendName ns
             in (listifyNames name $ adds diff, listifyNames name $ removals diff)
      childNameChanges =
        children
          & ifoldMap \ns childTree ->
            nameChanges (Just $ appendName ns) (TreeDiff childTree)
   in NameChanges {termNameAdds, termNameRemovals, typeNameAdds, typeNameRemovals} <> childNameChanges
  where
    appendName :: NameSegment -> Name
    appendName =
      case namePrefix of
        Nothing -> Name.fromSegment
        Just prefix -> (prefix Lens.|>)
    listifyNames :: (Name -> Set ref -> [(Name, ref)])
    listifyNames name xs =
      xs
        & Set.toList
        & fmap (name,)

-- | Get a 'NameBasedDiff' from a 'TreeDiff'.
nameBasedDiff :: TreeDiff -> NameBasedDiff
nameBasedDiff (TreeDiff (DefinitionDiffs {termDiffs, typeDiffs} :< children)) =
  let NameBasedDiff childrenTerms childrenTypes =
        foldMap (nameBasedDiff . TreeDiff) children
   in NameBasedDiff
        { terms = foldMap nameBasedTermDiff termDiffs <> childrenTerms,
          types = foldMap nameBasedTypeDiff typeDiffs <> childrenTypes
        }
  where
    nameBasedTermDiff :: Diff Referent -> Relation Reference Reference
    nameBasedTermDiff Diff {adds, removals} =
      let termAdds = mapMaybe Referent.toTermReference (Set.toList removals)
          termRemovals = mapMaybe Referent.toTermReference (Set.toList adds)
       in ((,) <$> termRemovals <*> termAdds)
            & filter (\(r0, r1) -> r0 /= r1)
            & Relation.fromList

    nameBasedTypeDiff :: Diff Reference -> Relation Reference Reference
    nameBasedTypeDiff Diff {adds, removals} =
      ((,) <$> Set.toList removals <*> Set.toList adds)
        & filter (\(r0, r1) -> r0 /= r1)
        & Relation.fromList
