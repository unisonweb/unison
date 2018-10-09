{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Codebase.Branch where

-- import Unison.Codebase.NameEdit (NameEdit)

import           Control.Monad              (foldM)
import           Data.Foldable
import           Data.Functor.Identity      (runIdentity)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Relation              (Relation)
import qualified Data.Relation              as R
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
--import Control.Monad (join)
import           Unison.Codebase.Causal     (Causal)
import qualified Unison.Codebase.Causal     as Causal
import           Unison.Codebase.Name       (Name)
import           Unison.Codebase.TermEdit   (TermEdit, Typing)
import qualified Unison.Codebase.TermEdit   as TermEdit
import           Unison.Codebase.TypeEdit   (TypeEdit)
import qualified Unison.Codebase.TypeEdit   as TypeEdit
import           Unison.Hashable            (Hashable)
import qualified Unison.Hashable            as H
import           Unison.Reference           (Reference)
--import Data.Semigroup (sconcat)
--import Data.List.NonEmpty (nonEmpty)

-- todo:
-- probably should refactor Reference to include info about whether it
-- is a term reference, a type decl reference, or an effect decl reference
-- (maybe combine last two)
--
-- While we're at it, should add a `Cycle Int [Reference]` for referring to
-- an element of a cycle of references.
--
-- If we do that, can implement various operations safely since we'll know
-- if we are referring to a term or a type (and can prevent adding a type
-- reference to the term namespace, say)

-- A `Branch`, `b` should likely maintain that:
--
--  * If `r : Reference` is in `codebase b` or one of its
--    transitive dependencies then `b` should have a `Name` for `r`.
--
-- This implies that if you depend on some code, you pick names for that
-- code. The editing tool will likely pick names based on some convention.
-- (like if you import and use `Runar.foo` in a function you write, it will
--  republished under `dependencies.Runar`. Could also potentially put
--  deps alongside the namespace...)
--
-- Thought was that basically don't need `Release`, it's just that
-- some branches are unconflicted and we might indicate that in some way
-- in the UI.
--
-- To "delete" a definition, just remove it from the map.
--
-- Operations around making transitive updates, resolving conflicts...
-- determining remaining work before one branch "covers" another...
newtype Branch = Branch { unbranch :: Causal Branch0 }

data Branch0 =
  Branch0 { termNamespace :: Relation Name Reference
          , typeNamespace :: Relation Name Reference
          , editedTerms   :: Relation Reference TermEdit
          , editedTypes   :: Relation Reference TypeEdit
          -- doesn't need to be serialized:
          , dependents :: Map Reference (Set Reference)
          }

-- When adding a Reference `r` to a namespace as `n`:
--   * add names for all of its transitive dependencies to `backupNames`.
--   * cache its transitive dependencies in `transitiveDependencies`
--   * (q1) do we add r,n to backupNames? no
-- When removing a Reference `r` from a namespace:
--   * get its transitive dependencies `ds`
--   * remove `r` from dom(transitiveDependencies)
--   * for each `d <- ds`, if `d` isn't in ran(transitiveDependencies),
--                         then delete `d` from backupNames
--   * (q2) When renaming, do we need to update `backupNames`? no

instance Semigroup Branch0 where
  Branch0 n1 nt1 e1 et1 d1 <> Branch0 n2 nt2 e2 et2 d2 = Branch0
    (R.union n1 n2)
    (R.union nt1 nt2)
    (R.union e1 e2)
    (R.union et1 et2)
    (Map.union d1 d2)

instance Monoid Branch0 where
  mempty = Branch0 R.empty R.empty R.empty R.empty Map.empty
  mappend = (<>)

-- Use e.g. by `conflicts termNamespace branch`
conflicts :: Ord a => (Branch0 -> Relation a b) -> Branch -> Map a (Set b)
conflicts f = conflicts' . f . Causal.head . unbranch where
  conflicts' :: Ord a => Relation a b -> Map a (Set b)
  conflicts' r =
    -- iterate over the domain, looking for ranges with size > 1
    -- build a map of those sets
    foldl' go Map.empty (R.dom r) where
      go m a =
        let bs = lookupDom a r
        in if Set.size bs > 1 then Map.insert a bs m else m

-- Use as `resolved editedTerms branch`
resolved :: Ord a => (Branch0 -> Relation a b) -> Branch -> Map a b
resolved f = resolved' . f . Causal.head . unbranch where
  resolved' :: Ord a => Relation a b -> Map a b
  resolved' r = foldl' go Map.empty (R.dom r) where
    go m a =
      let bs = lookupDom a r
      in if Set.size bs == 1 then Map.insert a (Set.findMin bs) m else m


-- count of remaining work, including:
-- * conflicted thingies
-- * among unconflicted thingies:
--    * definitions depending on definitions that have been updated
--       * terms and types depending on updated types
--       * terms depending on updated terms
data RemainingWork
  = TermNameConflict Name (Set Reference)
  | TypeNameConflict Name (Set Reference)
  | TermEditConflict Reference (Set TermEdit)
  | TypeEditConflict Reference (Set TypeEdit)
  -- ObsoleteTerm r [(old,new)]: r depended on old, which has been updated to new
  | ObsoleteTerm Reference (Set (Reference, Either TermEdit TypeEdit))
  | ObsoleteType Reference (Set (Reference, TypeEdit))
  deriving (Eq, Ord, Show)
remaining :: forall m. Monad m => ReferenceOps m -> Branch -> m (Set RemainingWork)
remaining ops b@(Branch (Causal.head -> b0)) = do
-- If any of r's dependencies have been updated, r should be updated.
-- Alternatively: If `a` has been edited, then all of a's dependents
-- should be edited. (Maybe a warning if they are updated to something
-- that still uses `a`.)
  -- map from updated term to dependent + termedit
  (obsoleteTerms, obsoleteTypes) <- wrangleUpdatedTypes ops wrangleUpdatedTerms
  pure . Set.fromList $
    (uncurry TermNameConflict <$> Map.toList (conflicts termNamespace b)) ++
    (uncurry TypeNameConflict <$> Map.toList (conflicts typeNamespace b)) ++
    (uncurry TermEditConflict <$> Map.toList (conflicts editedTerms b)) ++
    (uncurry TypeEditConflict <$> Map.toList (conflicts editedTypes b)) ++
    (uncurry ObsoleteTerm <$> Map.toList obsoleteTerms) ++
    (uncurry ObsoleteType <$> Map.toList obsoleteTypes)
  where                    -- referent -> (oldreference, edit)
    wrangleUpdatedTerms ::  Map Reference (Set (Reference, Either TermEdit TypeEdit))
    wrangleUpdatedTerms =
      -- 1. filter the edits to find the ones that are resolved (not conflicted)
      -- 2. for each resolved (oldref,edit) pair,
      -- 2b.  look up the referents of that oldref.
      -- 2c.  if the referent is unedited, add it to the work:
      -- 2c(i).  add it to the term work list if it's a term ref,
      -- 2c(ii). only terms can depend on terms, so it's a term ref.
      let termEdits :: Map Reference TermEdit -- oldreference, edit
          termEdits = resolved editedTerms b
      in Map.unionsWith (<>)
        [ Map.singleton referent (Set.singleton (oldRef, Left edit))
             | (oldRef, edit) <- Map.toList termEdits
             , referent <- toList $ transitiveClosure1' (dependentsOrDie b0) oldRef
             , not $ R.memberDom referent (editedTerms b0)
             ]
    dependentsOrDie :: Branch0 -> Reference -> Set Reference
    dependentsOrDie b0 ref = case Map.lookup ref (dependents b0) of
      Just s -> s
      Nothing -> error $
        "wrangleUpdatedTerms tried to look up dependents of a ref " ++
        "before ever populating them."

    wrangleUpdatedTypes ::
      Monad m => ReferenceOps m
              -> Map Reference (Set (Reference, Either TermEdit TypeEdit))
              -> m (Map Reference (Set (Reference, Either TermEdit TypeEdit))
                   ,Map Reference (Set (Reference, TypeEdit)))
    wrangleUpdatedTypes ops initialTermEdits =
      -- 1. filter the edits to find the ones that are resolved (not conflicted)
      -- 2. for each resolved (oldref,edit) pair,
      -- 2b.  look up the referents of that oldref.
      -- 2c.  if the referent is unedited, add it to the work:
      -- 2c(i).  add it to the term work list if it's a term ref,
      -- 2c(ii). add it to the type work list if it's a type ref
      foldM go (initialTermEdits, Map.empty) (Map.toList typeEdits)
      where
        typeEdits :: Map Reference TypeEdit -- oldreference, edit
        typeEdits = resolved editedTypes b
        go :: Monad m
           => (Map Reference (Set (Reference, Either TermEdit TypeEdit))
                ,Map Reference (Set (Reference, TypeEdit)))
           -> (Reference, TypeEdit)
           -> m (Map Reference (Set (Reference, Either TermEdit TypeEdit))
                ,Map Reference (Set (Reference, TypeEdit)))
        go (termWork, typeWork) (oldRef, edit) =
          foldM go2 (termWork, typeWork)
                    (transitiveClosure1' (dependentsOrDie b0) oldRef) where
            single referent oldRef edit =
              Map.singleton referent (Set.singleton (oldRef, edit))
            singleRight referent oldRef edit =
              Map.singleton referent (Set.singleton (oldRef, Right edit))
            go2 (termWork, typeWork) referent =
              termOrTypeOp ops referent
                (pure $ if not $ R.memberDom referent (editedTerms b0)
                        then (termWork <> singleRight referent oldRef edit, typeWork)
                        else (termWork, typeWork))
                (pure $ if not $ R.memberDom referent (editedTypes b0)
                        then (termWork, typeWork <> single referent oldRef edit)
                        else (termWork, typeWork))

empty :: Branch
empty = Branch (Causal.one mempty)

merge :: Branch -> Branch -> Branch
merge (Branch b) (Branch b2) = Branch (Causal.merge b b2)

data ReferenceOps m = ReferenceOps
  { name         :: Reference -> m (Set Name)
  , isTerm       :: Reference -> m Bool
  , isType       :: Reference -> m Bool
  , dependencies :: Reference -> m (Set Reference)
  }

-- 0. bar depends on foo
-- 1. replace foo with foo'
-- 2. replace bar with bar' which depends on foo'
-- 3. replace foo' with foo''
-- "foo" points to foo''
-- "bar" points to bar'
--
-- foo -> Replace foo'
-- foo' -> Replace foo''
-- bar -> Replace bar'
--
-- foo -> Replace foo''
-- foo' -> Replace foo''
-- bar -> Replace bar'
--
-- foo -> Replace foo''
-- bar -> Replace bar''
-- foo' -> Replace foo'' *optional
-- bar' -> Replace bar'' *optional

replaceType
  :: Monad m => ReferenceOps m -> Reference -> Reference -> Branch -> m Branch
replaceType = undefined

add :: Monad m => ReferenceOps m -> Name -> Reference -> Branch -> m Branch
add ops n r (Branch b) = Branch <$> Causal.stepM go b where
  go b = do
    -- add dependencies to `backupNames` and `dependents`
    deps <- transitiveClosure1 (dependencies ops) r
    let dependents' = addDependenciesAsDependents r deps $ dependents b
    -- add to appropriate namespace
    b <- termOrTypeOp ops r
      (pure b { termNamespace = R.insert n r $ termNamespace b })
      (pure b { typeNamespace = R.insert n r $ typeNamespace b })
    pure b { dependents = dependents' }

addDependenciesAsDependents :: Ord a => a -> Set a -> Map a (Set a) -> Map a (Set a)
addDependenciesAsDependents dependent dependencies m =
  foldl' (\m dep -> Map.insertWith (<>) dep (Set.singleton dependent) m) m dependencies

insertNames :: Monad m
            => ReferenceOps m
            -> Relation Reference Name
            -> Reference -> m (Relation Reference Name)
insertNames ops m r = foldl' (flip $ R.insert r) m <$> name ops r

insertManyRan :: (Foldable f, Ord a, Ord b)
              => a -> f b -> Relation a b -> Relation a b
insertManyRan a bs r = foldl' (flip $ R.insert a) r bs

insertManyDom :: (Foldable f, Ord a, Ord b)
              => f a -> b -> Relation a b -> Relation a b
insertManyDom as b r = foldl' (flip $ flip R.insert b) r as

lookupRan :: Ord b => b -> Relation a b -> Set a
lookupRan b r = fromMaybe Set.empty $ R.lookupRan b r

lookupDom :: Ord a => a -> Relation a b -> Set b
lookupDom a r = fromMaybe Set.empty $ R.lookupDom a r

replaceDom :: (Ord a, Ord b) => a -> a -> Relation a b -> Relation a b
replaceDom a a' r =
  foldl' (\r b -> R.insert a' b $ R.delete a b r) r (lookupDom a r)

-- Todo: fork the relation library
replaceRan :: (Ord a, Ord b) => b -> b -> Relation a b -> Relation a b
replaceRan b b' r =
  foldl' (\r a -> R.insert a b' $ R.delete a b r) r (lookupRan b r)

deleteRan :: (Ord a, Ord b) => b -> Relation a b -> Relation a b
deleteRan b r = foldl' (\r a -> R.delete a b r) r $ lookupRan b r

deleteDom :: (Ord a, Ord b) => a -> Relation a b -> Relation a b
deleteDom a r = foldl' (\r b -> R.delete a b r) r $ lookupDom a r

replaceTerm :: Monad m
            => ReferenceOps m
            -> Reference -> Reference -> Typing
            -> Branch -> m Branch
replaceTerm ops old new typ (Branch b) = Branch <$> Causal.stepM go b where
  edit = TermEdit.Replace new typ
  go b = do
    -- add names for transitive dependencies of `new`
    newDeps <- transitiveClosure1 (dependencies ops) new
    -- stop tracking dependencies of `old` in `transitiveDependencies`
    -- and remove orphaned dependencies of `old` from `backupNames`
    let dependents' = addDependenciesAsDependents new newDeps (dependents b)
    pure b { editedTerms = R.insert old edit (editedTerms b)
           , termNamespace = replaceRan old new $ termNamespace b
           , dependents = dependents'
           }

-- If any `as` aren't in `b`, then delete them from `c` as well.  Kind of sad.
deleteOrphans :: (Ord a, Ord c) => Set a -> Relation a b -> Relation a c -> Relation a c
deleteOrphans as b c = foldl' (\c a -> if R.memberDom a b then c else deleteDom a c) c as

codebase :: Monad m => ReferenceOps m -> Branch -> m (Set Reference)
codebase ops (Branch (Causal.head -> Branch0 {..})) =
  let initial = Set.fromList $
        (snd <$> R.toList termNamespace) ++
        (snd <$> R.toList typeNamespace) ++
        (map snd (R.toList editedTerms) >>= TermEdit.references) ++
        (map snd (R.toList editedTypes) >>= TypeEdit.references)
  in transitiveClosure (dependencies ops) initial

transitiveClosure :: forall m a. (Monad m, Ord a)
                  => (a -> m (Set a))
                  -> Set a
                  -> m (Set a)
transitiveClosure getDependencies open =
  let go :: Set a -> [a] -> m (Set a)
      go closed [] = pure closed
      go closed (h:t) =
        if Set.member h closed
          then go closed t
        else do
          deps <- getDependencies h
          go (Set.insert h closed) (toList deps ++ t)
  in go Set.empty (toList open)

transitiveClosure1 :: forall m a. (Monad m, Ord a)
                   => (a -> m (Set a)) -> a -> m (Set a)
transitiveClosure1 f a = transitiveClosure f (Set.singleton a)

transitiveClosure1' :: Ord a => (a -> Set a) -> a -> Set a
transitiveClosure1' f a = runIdentity $ transitiveClosure1 (pure.f) a

deprecateTerm :: Reference -> Branch -> Branch
deprecateTerm old (Branch b) = Branch $ Causal.step go b where
  go b = b { editedTerms = R.insert old TermEdit.Deprecate (editedTerms b)
           , termNamespace = deleteRan old (termNamespace b)
           }


deprecateType :: Reference -> Branch -> Branch
deprecateType old (Branch b) = Branch $ Causal.step go b where
  go b = b { editedTypes = R.insert old TypeEdit.Deprecate (editedTypes b)
           , typeNamespace = deleteRan old (typeNamespace b)
           }

instance (Hashable a, Hashable b) => Hashable (Relation a b) where
  tokens r = H.tokens (R.toList r)

instance Hashable Branch0 where
  tokens (Branch0 {..}) =
    H.tokens termNamespace ++ H.tokens typeNamespace ++
    H.tokens editedTerms ++ H.tokens editedTypes

resolveTerm :: Name -> Branch -> Set Reference
resolveTerm n (Branch (Causal.head -> b)) = lookupDom n (termNamespace b)

resolveTermUniquely :: Name -> Branch -> Maybe Reference
resolveTermUniquely n b =
  case resolveTerm n b of
    s | Set.size s == 1 -> Set.lookupMin s
    _ -> Nothing

addTermName :: Reference -> Name -> Branch -> Branch
addTermName r new (Branch b) = Branch $ Causal.step go b where
  go b = b { termNamespace = R.insert new r (termNamespace b) }

addTypeName :: Reference -> Name -> Branch -> Branch
addTypeName r new (Branch b) = Branch $ Causal.step go b where
  go b = b { typeNamespace = R.insert new r (typeNamespace b) }

addName :: Monad m => ReferenceOps m -> Reference -> Name -> Branch -> m Branch
addName ops r new b =
  termOrTypeOp ops r (pure $ addTermName r new b) (pure $ addTypeName r new b)

termOrTypeOp :: Monad m => ReferenceOps m -> Reference
             -> m b -> m b -> m b
termOrTypeOp ops r ifTerm ifType = do
  isTerm <- isTerm ops r
  isType <- isType ops r
  if isTerm then ifTerm
  else if isType then ifType
  else fail $ "malformed reference: " ++ show r

renameType :: Name -> Name -> Branch -> Branch
renameType old new (Branch b) =
  Branch $ Causal.stepIf (R.memberDom old . typeNamespace) go b where
    go b = b { typeNamespace = replaceDom old new (typeNamespace b)}

renameTerm :: Name -> Name -> Branch -> Branch
renameTerm old new (Branch b) =
  Branch $ Causal.stepIf (R.memberDom old . termNamespace) go b where
    go b = b { termNamespace = replaceDom old new (termNamespace b)}
