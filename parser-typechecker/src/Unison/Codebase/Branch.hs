{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Codebase.Branch where

-- import Unison.Codebase.NameEdit (NameEdit)

import           Control.Monad              (foldM)
import           Data.Foldable
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
newtype Branch = Branch (Causal Branch0)

data Branch0 =
  Branch0 { termNamespace :: Relation Name Reference
          , typeNamespace :: Relation Name Reference
          , editedTerms   :: Relation Reference TermEdit
          , editedTypes   :: Relation Reference TypeEdit
          , backupNames   :: Relation Reference Name
          -- , codebase       :: Set Reference
          }

instance Semigroup Branch0 where
  Branch0 n1 nt1 t1 d1 dp1 <> Branch0 n2 nt2 t2 d2 dp2 = Branch0
    (R.union n1 n2)
    (R.union nt1 nt2)
    (R.union t1 t2)
    (R.union d1 d2)
    (R.union dp1 dp2)

merge :: Branch -> Branch -> Branch
merge (Branch b) (Branch b2) = Branch (Causal.merge b b2)

data ReferenceOps m = ReferenceOps
  { name         :: Reference -> m (Set Name)
  , isTerm       :: Reference -> m Bool
  , isType       :: Reference -> m Bool
  , dependencies :: Reference -> m (Set Reference)
  -- , dependencies ::
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
    -- add dependencies to `backupNames`
    backupNames' <- updateBackupNames1 ops r b
    -- add (n,r) to backupNames
    let backupNames'' = R.insert r n backupNames'
    -- add to appropriate namespace
    termOrTypeOp ops r
      (pure b { termNamespace = R.insert n r $ termNamespace b
              , backupNames = backupNames''
              })
      (pure b { typeNamespace = R.insert n r $ typeNamespace b
              , backupNames = backupNames''
              })

updateBackupNames :: Monad m
                  => ReferenceOps m
                  -> Set Reference
                  -> Branch0
                  -> m (Relation Reference Name)
updateBackupNames ops refs b = do
  transitiveClosure <- transitiveClosure (dependencies ops) refs
  foldM insertNames (backupNames b) transitiveClosure
  where
    insertNames m r = foldl' (flip $ R.insert r) m <$> name ops r

updateBackupNames1 :: Monad m
                   => ReferenceOps m
                   -> Reference
                   -> Branch0
                   -> m (Relation Reference Name)
updateBackupNames1 ops r b = updateBackupNames ops (Set.singleton r) b

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
    backupNames <- updateBackupNames1 ops new b
    pure b { editedTerms = R.insert old edit (editedTerms b)
    -- todo: can we use backupNames to find the keys to update, instead of
    -- fmap
           , termNamespace = replaceRan old new $ termNamespace b
           , backupNames = backupNames
           }

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
  let
    bh = Causal.head b
    m0 = typeNamespace bh
  in Branch $ case R.lookupDom old m0 of
    Nothing -> b
    Just _ ->
      let m1 = replaceDom old new m0
      in Causal.cons (bh { typeNamespace = m1 }) b

renameTerm :: Name -> Name -> Branch -> Branch
renameTerm old new (Branch b) =
  let
    bh = Causal.head b
    m0 = termNamespace bh
  in Branch $ case R.lookupDom old m0 of
    Nothing -> b
    Just _ ->
      let m1 = replaceDom old new m0
      in Causal.cons (bh { termNamespace = m1 }) b

