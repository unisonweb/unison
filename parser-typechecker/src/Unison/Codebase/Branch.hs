{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Codebase.Branch where

-- import Unison.Codebase.NameEdit (NameEdit)

import           Control.Monad              (foldM)
import           Data.Foldable
import           Data.Maybe                 (fromMaybe, isJust)
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
newtype Branch = Branch (Causal Branch0)

data Branch0 =
  Branch0 { termNamespace :: Relation Name Reference
          , typeNamespace :: Relation Name Reference
          , editedTerms   :: Relation Reference TermEdit
          , editedTypes   :: Relation Reference TypeEdit
          , backupNames   :: Relation Reference Name
          -- doesn't need to be serialized:
          , transitiveDependencies :: Relation Reference Reference -- dependent, dependency
          }

-- When adding a Reference `r` to a namespace as `n`:
--   * add names for all of its transitive dependencies to `backupNames`.
--   * cache its transitive dependencies in `transitiveDependencies`
--   * (q1) do we add r,n to backupNames? (relates to q3)
-- When removing a Reference `r` from a namespace:
--   * get its transitive dependencies `ds`
--   * remove `r` from dom(transitiveDependencies)
--   * for each `d <- ds`, if `d` isn't in ran(transitiveDependencies),
--                         then delete `d` from backupNames
-- (q3) When renaming, do we need to update `backupNames`?
-- (a1, a3) don't care; no, no.

instance Semigroup Branch0 where
  Branch0 n1 nt1 t1 d1 bn1 dp1 <> Branch0 n2 nt2 t2 d2 bn2 dp2 = Branch0
    (R.union n1 n2)
    (R.union nt1 nt2)
    (R.union t1 t2)
    (R.union d1 d2)
    (R.union bn1 bn2)
    (R.union dp1 dp2)

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
    -- add dependencies to `backupNames` and `transitiveDependencies`
    deps <- transitiveClosure1 (dependencies ops) r
    backupNames' <- addBackupNames ops deps b
    let transitiveDependencies' = insertManyRan r deps $ transitiveDependencies b
    -- add to appropriate namespace
    b <- termOrTypeOp ops r
      (pure b { termNamespace = R.insert n r $ termNamespace b })
      (pure b { typeNamespace = R.insert n r $ typeNamespace b })
    pure b { backupNames = backupNames'
           , transitiveDependencies = transitiveDependencies'
           }


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

addBackupNames :: Monad m
               => ReferenceOps m
               -> Set Reference
               -> Branch0
               -> m (Relation Reference Name)
addBackupNames ops needNames b =
  foldM (insertNames ops) (backupNames b) needNames

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
    backupNames <- addBackupNames ops newDeps b
    -- stop tracking dependencies of `old` in `transitiveDependencies`
    -- and remove orphaned dependencies of `old` from `backupNames`
    let oldDeps = lookupDom old (transitiveDependencies b)
        transitiveDependencies' = deleteDom old (transitiveDependencies b)
        backupNames' = deleteOrphans oldDeps transitiveDependencies' backupNames
    pure b { editedTerms = R.insert old edit (editedTerms b)
           , termNamespace = replaceRan old new $ termNamespace b
           , backupNames = backupNames'
           , transitiveDependencies = transitiveDependencies'
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

deprecateTerm :: Reference -> Branch -> Branch
deprecateTerm old (Branch b) = Branch $ Causal.step go b where
  go b =
    let oldDeps :: Set Reference
        oldDeps = lookupDom old (transitiveDependencies b)
        allDeps :: Relation Reference Reference
        allDeps = deleteDom old (transitiveDependencies b)
        backupNames' = deleteOrphans oldDeps allDeps (backupNames b)
    in b { editedTerms = R.insert old TermEdit.Deprecate (editedTerms b)
         , termNamespace = deleteRan old (termNamespace b)
         , backupNames = backupNames'
         , transitiveDependencies = allDeps
         }


deprecateType :: Reference -> Branch -> Branch
deprecateType old (Branch b) = Branch $ Causal.step go b where
  go b =
    let oldDeps :: Set Reference
        oldDeps = lookupDom old (transitiveDependencies b)
        allDeps :: Relation Reference Reference
        allDeps = deleteDom old (transitiveDependencies b)
        backupNames' = deleteOrphans oldDeps allDeps (backupNames b)
    in b { editedTypes = R.insert old TypeEdit.Deprecate (editedTypes b)
         , typeNamespace = deleteRan old (typeNamespace b)
         , backupNames = backupNames'
         , transitiveDependencies = allDeps
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
  Branch $ Causal.stepIf (isJust . R.lookupDom old . typeNamespace) go b where
    go b = b { typeNamespace = replaceDom old new (typeNamespace b)}

renameTerm :: Name -> Name -> Branch -> Branch
renameTerm old new (Branch b) =
  Branch $ Causal.stepIf (isJust . R.lookupDom old . termNamespace) go b where
    go b = b { termNamespace = replaceDom old new (termNamespace b)}
