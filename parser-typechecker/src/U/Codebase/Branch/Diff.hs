module U.Codebase.Branch.Diff where

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
import Unison.Name (Name)
import qualified Unison.Name as Name
import qualified Unison.NameSegment as NameSegment
import Unison.Prelude

data GDiff a = GDiff
  { adds :: Set a,
    removals :: Set a
  }
  deriving (Show, Eq, Ord)

-- | TODO: includ info on patch and metadata diffs, I just didn't need it yet.
data DefinitionDiffs = DefinitionDiffs
  { termDiffs :: Map NameSegment (GDiff Referent),
    typeDiffs :: Map NameSegment (GDiff Reference)
    -- termMetadataDiffs :: Map (NameSegment, Referent) (GDiff Reference),
    -- typeMetadataDiffs :: Map (NameSegment, Reference) (GDiff Reference)
    -- patchDiffs :: Map NameSegment (GDiff ())
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

-- | Note: currently doesn't detect causal changes.
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

diffBranches :: forall m. Monad m => Branch m -> Branch m -> m TreeDiff
diffBranches from to = do
  let termDiffs = diffMap (terms from) (terms to)
  let typeDiffs = diffMap (types from) (types to)
  let defDiff = DefinitionDiffs {termDiffs, typeDiffs}
  rec <- do
    Align.align (children from) (children to)
      & wither
        ( \case
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
        )
  pure $ TreeDiff (defDiff :< rec)
  where
    diffMap :: forall ref. Ord ref => Map NameSegment (Map ref (m MdValues)) -> Map NameSegment (Map ref (m MdValues)) -> Map NameSegment (GDiff ref)
    diffMap l r =
      Align.align l r
        & fmap
          ( \case
              (This refs) -> (GDiff {removals = Map.keysSet refs, adds = mempty})
              (That refs) -> (GDiff {removals = mempty, adds = Map.keysSet refs})
              (These l' r') ->
                let lRefs = Map.keysSet l'
                    rRefs = Map.keysSet r'
                 in (GDiff {removals = lRefs `Set.difference` rRefs, adds = rRefs `Set.difference` lRefs})
          )

collateChanges :: Maybe Name -> TreeDiff -> ([(Name, Either Reference Referent)], [(Name, Either Reference Referent)])
collateChanges namePrefix (TreeDiff (DefinitionDiffs {termDiffs, typeDiffs} :< children)) =
  ( termDiffs
      & ifoldMap \ns diff ->
        let name = appendName ns
         in (go name Right $ adds diff, go name Right $ removals diff)
  )
    <> ( typeDiffs
           & ifoldMap \ns diff ->
             let name = appendName ns
              in (go name Left $ adds diff, go name Left $ removals diff)
       )
    <> ( children
           & ifoldMap \ns childTree ->
             collateChanges (Just $ appendName ns) (TreeDiff childTree)
       )
  where
    appendName ns = maybe (Name.fromSegment . NameSegment.NameSegment . coerce $ ns) (`Lens.snoc` NameSegment.NameSegment (coerce ns)) namePrefix
    go name inj xs =
      xs
        & Set.toList
        & fmap ((name,) . inj)
