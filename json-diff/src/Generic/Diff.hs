{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module Generic.Diff where
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Extra as Aeson
import Data.Map (Map)
import Control.Monad.Free (Free (Free, Pure))
import qualified Data.Functor.Foldable as FF
import Data.Functor.Foldable (Base)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Data.Zip as Zip
import Data.These
import Control.Applicative
import Control.Monad.Reader
import qualified Control.Monad.Free as Free
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Comonad.Cofree (Cofree)
import qualified Control.Comonad.Cofree as Cofree
import qualified Data.Foldable as Foldable
import Control.Monad.Trans.Writer.CPS (Writer)
import Data.Functor.Compose
import Data.Distributive
import Control.Arrow ((&&&))
import Generic.Data
import Data.Functor.Classes (Show1)
import Control.Comonad (extract)


hashValue :: forall f metavar. (FF.Base f metavar -> metavar) -> f -> Cofree (FF.Base f) metavar
hashValue hash v = FF.cata hashUpwards v
  where
    hashUpwards :: FF.Base f (Cofree (FF.Base f) metavar) -> Cofree (FF.Base f) metavar
    hashUpwards v = (hash (fmap extract v)) Cofree.:< v

-- | A Patch is a spine with Holes, where the holes are Changes.
type Patch f metavar = Free f (Change f metavar)
data Change f metavar = Change {deletions :: Free f metavar, insertions :: Free f metavar}
  deriving Show

data Pair a = Pair {one :: a, two :: a}
  deriving (Functor)

instance Distributive Pair where
  distribute f = Pair (fmap one f) (fmap two f)

toPair :: Change f metavar -> Pair (Free f metavar)
toPair (Change l r) = Pair l r

fromPair ::  Pair (Free f metavar) -> Change f metavar
fromPair (Pair l r) = Change l r

-- a.k.a. applyTree23
applyChange :: Ord metavar => Change (Base v) metavar -> v -> Maybe v
applyChange (Change d i) v = del d v >>= ins i

-- a.k.a. gcp - greatest common path
-- Finds the common prefix path between all insertions and deletions to minimize the patch
-- sites.
findCommonSpine :: Change (Base v) mv -> Free (Base v) (Change (Base v) mv)
findCommonSpine Change{deletions, insertions} = go deletions insertions
  where
    alignSpines :: These (Free (Base v) metavar) (Free (Base v) metavar) -> Free (Base v) (Change (Base v) metavar)
    alignSpines (These l r) = go l r
    alignSpines (This l) = error "TODO: Handle mismatched keys"
    alignSpines (That r) = error "TODO: Handle mismatched keys"
    go :: (Free (Base v) metavar -> Free (Base v) metavar -> Free (Base v) (Change (Base v) metavar))
    go insertions deletions = case (insertions, deletions) of
      (Free (ObjectF o), Free (ObjectF o')) -> Free (ObjectF $ Zip.alignWith alignSpines o o')
      (Free (ArrayF a), Free (ArrayF a')) -> Free (ArrayF $ Zip.alignWith alignSpines a a')
      (Free (StringF s), Free (StringF s')) | s == s' -> Free (StringF s)
      (Free (NumberF n), Free (NumberF n')) | n == n' -> Free (NumberF n)
      (Free (BoolF b), Free (BoolF b')) | b == b' -> Free (BoolF b)
      (Free NullF, Free NullF) -> Free NullF
      _ -> Pure (Change {insertions, deletions})

reclose :: Free (Base v) (Change (Base v) metavar) -> Change (Base v) metavar
reclose = fromPair . fmap join . collect toPair

type Closure mv = (Set mv, Set mv)
-- Fix metavariable containment breaks on a common spine prefix.
-- We can improve performance here
-- Can maybe use distributive with separate from Compactable?
closure :: Free (Base v) (Change (Base v) metavar) -> Patch (Base v) metavar
closure v = case Free.iter go' . fmap (closureSets &&& Pure) $ v of
  (clos, x)
    | isClosed clos -> x
    | otherwise -> error "Unable to compute closure over entire diff, this shouldn't happen."
  where
    go' :: (Base v) (Closure mv, Free (Base v) (Change (Base v) metavar)) -> (Closure mv, Free (Base v) (Change (Base v) metavar))
    go' f =
        let (totalClosure, rest) = sequenceA f
        in if any (not . isClosed . fst) f
              then (totalClosure, Pure . reclose . Free $ rest)
              else (totalClosure, Free $ rest)
    closureSets :: Change (Base v) metavar -> (Set metavar, Set metavar)
    closureSets (Change {deletions, insertions}) = (metavars deletions, metavars insertions)
    isClosed :: (Set metavar, Set metavar) -> Bool
    isClosed = uncurry (==)

metavars :: Ord a => Foldable f => f a -> Set a
metavars = foldMap Set.singleton

-- a.k.a. diffTree23
diffJSON :: v -> v -> Patch (Base v) metavar
diffJSON src dest = closure . findCommonSpine $ computeDiff src dest

applyPatch :: Patch (FF.Base f) metavar -> f -> Maybe f
applyPatch = _

-- a.k.a. changeTree23
-- Computes a diff FROM src to dest; diffs are directional!
computeDiff :: v -> v -> Change (Base v) metavar
computeDiff src dest = Change {deletions=extractSubtree oracle hashedSrc, insertions=extractSubtree oracle hashedDest}
  where
    hashedSrc, hashedDest :: Cofree (Base v) metavar
    (hashedSrc, hashedDest) = (hashValue src, hashValue dest)
    commonHashes :: Set metavar
    commonHashes = Set.intersection (foldMap Set.singleton hashedSrc) (foldMap Set.singleton hashedDest)
    oracle :: Cofree (Base v) metavar -> Maybe metavar
    oracle = selectSharedHash commonHashes

-- Given an oracle which determines whether a given hash exists in both diffed subtrees,
-- Replace subtrees of the given tree with the hash representing them.
extractSubtree :: (Cofree f metavar -> Maybe metavar) -> Cofree f metavar -> Free (Base v) metavar
extractSubtree oracle v = Free.unfold expand v
  where
    expand :: Cofree f metavar -> Either metavar ((Base v) (Cofree f metavar))
    expand v =
      case oracle v of
        Nothing -> Right (Cofree.unwrap v)
        Just h -> Left h

-- Expand a value into a Free structure, with no Hashes inside.
makeHoly :: v -> Free (Base v) metavar
makeHoly = Free.unfold (Right . FF.project)

-- postProcess will clean up situations where a subtree exists in both trees, but where it
-- may be embedded in a LARGER subtree in the other. By checking which metavars exist in both
-- inserts and deletes, we can ensure that we don't have any that are mismatched. Mismatched
-- metavars are simply restored to their original values.
postProcess :: Map metavar v -> Change (Base v) metavar -> Maybe (Change (Base v) metavar)
postProcess metaMap Change{deletions, insertions} = Change <$> cleanVars deletions <*> cleanVars insertions
  where
    allMetaVars :: Free (Base v) metavar -> Set metavar
    allMetaVars = foldMap Set.singleton
    matchedMetavars = allMetaVars deletions `Set.intersection` allMetaVars insertions
    cleanVars :: Free (Base v) metavar -> Maybe (Free (Base v) metavar)
    cleanVars f = sequenceA . runMaybeT $ lift f >>= \case
      h | Set.member h matchedMetavars -> pure h
        | otherwise -> case Map.lookup h metaMap of
            Just v -> lift $ makeHoly v
            -- We were missing a mapping for this metavar, which shouldn't ever happen.
            Nothing -> empty


-- tests
correctnessProperty :: v -> v -> Bool
correctnessProperty x y = applyChange (computeDiff x y) x == Just y

precisionProperty :: v -> v -> Bool
precisionProperty x y = applyChange (computeDiff x x) y == Just y

-- a = Node2 (Node2 t k) u
-- b = Node2 (Node2 t k) t
-- extractSubtree (wcs a b) a = Node2C (Hole 0) u
-- extractSubtree (wcs a b) b = Node2C (Hole 0) (Hole 1)

-- a.k.a. wcs Given two trees, find the third subtree if it exists in both other trees and
-- provide its metavar.
selectSharedHash :: Set metavar -> Cofree f metavar -> Maybe metavar
selectSharedHash s (h Cofree.:< _)
  | h `Set.member` s = Just h
  | otherwise = Nothing

-- short for “which common subtree”. The call wcs s d x returns Nothinд when x is not a subtree of s and d; if x is a subtree of both s and d, it returns Just i, for some metavariable i. The only condition we impose is injectivity of wcs s d: that is, if wcs s d x ≡ wcs s d y ≡ Just j, then x ≡ y. In other words, equal metavariables correspond to equal subtrees.

-- Given a set of delete locations embeded in a holy tree;
-- Assign the subtrees from the value we're altering to their paired metavars
del :: forall metavar v.  Ord metavar =>  Free (Base v) metavar -> v -> Maybe (Map metavar v)
del ctx tree = flip execStateT mempty $ go ctx tree
  where
    alignValues :: These (Free (Base v) metavar) v -> StateT (Map metavar v) Maybe ()
    alignValues (These a b) = go a b
    alignValues _ = empty
    -- Crawl the structures together and determine whether they match
    go :: Free (Base v) metavar -> v -> StateT (Map metavar v) Maybe ()
    go x y = case (x, y) of
      (Free (ObjectF o), Object o') -> sequence_ $ Zip.alignWith alignValues o o'
      (Free (ArrayF a), Array a') -> sequence_ $ Zip.zipWith go a a'
      (Free (StringF s), String s') | s == s' -> pure ()
      (Free (NumberF n), Number n') | n == n' -> pure ()
      (Free (BoolF b), Bool b') | b == b' -> pure ()
      (Free NullF, Null) -> pure ()
      -- The context has a hole, match it to the value.
      (Pure mv, v) -> gets (Map.lookup mv) >>= \case
        -- If we don't have this metavar match yet, add it to the map
        Nothing -> modify (Map.insert mv v)
        -- If the metavar exists, but is paired to a different value, we fail, this is
        -- invalid.
        Just va -> guard (v == va)
      -- Everything represents a mismatch and should be treated as a failure.
      _ -> empty

-- Given the metavar mapping from the deletion step, insert all the matching metavars into the
-- patch.
ins :: forall metavar v. Ord metavar =>  Free (Base v) metavar -> Map metavar v -> Maybe v
ins tree metaMapping = flip runReaderT metaMapping $ go tree
  where
    -- Crawl the structures together and determine whether they match
    go :: Free (Base v) metavar -> ReaderT (Map metavar v) Maybe v
    go = \case
      (Free (ObjectF o)) -> Object <$> traverse go o
      (Free (ArrayF a)) -> Array <$> traverse go a
      (Free (StringF s)) -> pure $ String s
      (Free (NumberF n)) -> pure $ Number n
      (Free (BoolF b)) -> pure $ Bool b
      (Free NullF) -> pure Null
      -- Look up the metavar, fail if its missing
      (Pure mv) -> asks (Map.lookup mv) >>= lift

checks :: v -> v -> Bool
checks x y = correctnessProperty x y && precisionProperty x y
