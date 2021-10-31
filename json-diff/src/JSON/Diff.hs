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
module JSON.Diff where
import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), KeyValue ((.=)))
import Data.Aeson.Extra (ValueF(..))
import Data.Map (Map)
import Control.Monad.Free (Free (Free, Pure))
import qualified Data.Functor.Foldable as FF
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
import Data.Hashable (hash)
import Data.Distributive
import Control.Arrow ((&&&))
import Generic.Data
import Data.Functor.Classes (Show1)
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as Vector
import qualified Data.Foldable as Foldable

type Hashed f = Cofree f Hash

hashValue :: Value -> Hashed ValueF
hashValue = Cofree.unfold expand
  where
    expand v = (Hash $ hash v, FF.project v)

-- Note: It's usually inefficient to use this, should probably get your bindings another
-- way.
allBindings :: Value -> Map Hash Value
allBindings v = Map.fromList . Foldable.toList $ Cofree.unfold expand v
  where
    expand v = ((Hash $ hash v, v), FF.project v)


deriving instance Generic1 ValueF
deriving via (Generically1 ValueF) instance Show1 ValueF

-- | A Patch is a spine with Holes, where the holes are Changes.
type Patch f metavar = Holy f (Change f metavar)
data Change f metavar = Change {deletions :: Holy f metavar, insertions :: Holy f metavar}
  deriving Show

data Pair a = Pair {one :: a, two :: a}
  deriving (Functor)

instance Distributive Pair where
  distribute f = Pair (fmap one f) (fmap two f)

toPair :: Change f metavar -> Pair (Holy f metavar)
toPair (Change l r) = Pair l r

fromPair ::  Pair (Holy f metavar) -> Change f metavar
fromPair (Pair l r) = Change l r

data Hash = Hash Int
  deriving (Eq, Show, Ord)

-- data Holy metavar f = Hole metavar | Solid (f (Holy metavar f))
-- 'Free' represents a recursive structure where any given subtree can be replaced with a
-- metavar as a Hole.
type Holy f mv = Free f mv

-- a.k.a. applyTree23
applyChange :: (Ord metavar, Show metavar) => Change ValueF metavar -> Value -> Maybe Value
applyChange (Change d i) v = del d v >>= ins i

-- a.k.a. gcp - greatest common path
-- Finds the common prefix path between all insertions and deletions to minimize the patch
-- sites.
findCommonSpine :: forall mv. Show mv =>  Change ValueF mv -> Holy ValueF (Change ValueF mv)
findCommonSpine Change{deletions, insertions} = go deletions insertions
  where
    alignSpines :: These (Free ValueF mv) (Free ValueF mv) -> Free ValueF (Change ValueF mv)
    alignSpines (These l r) = go l r
    alignSpines (This l) = error ("TODO: Handle mismatched keysL\n" <> show l)
    alignSpines (That r) = error ("TODO: Handle mismatched keysR\n" <> show r)
    go :: (Holy ValueF mv -> Holy ValueF mv -> Free ValueF (Change ValueF mv))
    go deletions insertions = case (deletions, insertions) of
      (Free (ObjectF o), Free (ObjectF o')) | HM.keys o == HM.keys o' -> Free (ObjectF $ Zip.alignWith alignSpines o o')
      (Free (ArrayF a), Free (ArrayF a'))   | Vector.length a == Vector.length a' -> Free (ArrayF $ Zip.alignWith alignSpines a a')
      (Free (StringF s), Free (StringF s')) | s == s' -> Free (StringF s)
      (Free (NumberF n), Free (NumberF n')) | n == n' -> Free (NumberF n)
      (Free (BoolF b), Free (BoolF b')) | b == b' -> Free (BoolF b)
      (Free NullF, Free NullF) -> Free NullF
      _ -> Pure (Change {deletions, insertions})

reclose :: Holy ValueF (Change ValueF Hash) -> Change ValueF Hash
reclose = fromPair . fmap join . collect toPair

type Closure = (Set Hash, Set Hash)
-- Fix metavariable containment breaks on a common spine prefix.
-- We can improve performance here
-- Can maybe use distributive with separate from Compactable?
closure :: Holy ValueF (Change ValueF Hash) -> Patch ValueF Hash
closure v = case Free.iter go' . fmap (closureSets &&& Pure) $ v of
  (clos, x)
    | isClosed clos -> x
    | otherwise -> error "Unable to compute closure over entire diff, this shouldn't happen."
  where
    go' :: ValueF (Closure, Holy ValueF (Change ValueF Hash)) -> (Closure, Holy ValueF (Change ValueF Hash))
    go' f =
        let (totalClosure, rest) = sequenceA f
        in if any (not . isClosed . fst) f
              then (totalClosure, Pure . reclose . Free $ rest)
              else (totalClosure, Free $ rest)
    closureSets :: Change ValueF Hash -> (Set Hash, Set Hash)
    closureSets (Change {deletions, insertions}) = (metavars deletions, metavars insertions)
    isClosed :: (Set Hash, Set Hash) -> Bool
    isClosed = uncurry (==)

metavars :: Ord a => Foldable f => f a -> Set a
metavars = foldMap Set.singleton

-- a.k.a. diffTree23
computePatch :: Value -> Value -> Patch ValueF Hash
computePatch src dest = closure . findCommonSpine $ computeDiff src dest

applyPatch :: Patch ValueF Hash -> Value -> Maybe Value
applyPatch = undefined

-- a.k.a. changeTree23
-- Computes a diff FROM src to dest; diffs are directional!
computeDiff :: Value -> Value -> Change ValueF Hash
computeDiff src dest = Change {deletions=extract oracle hashedSrc, insertions=extract oracle hashedDest}
  where
    hashedSrc, hashedDest :: Hashed ValueF
    (hashedSrc, hashedDest) = (hashValue src, hashValue dest)
    commonHashes :: Set Hash
    commonHashes = Set.intersection (foldMap Set.singleton hashedSrc) (foldMap Set.singleton hashedDest)
    oracle :: Hashed ValueF -> Maybe Hash
    oracle = selectSharedHash commonHashes

-- Given an oracle which determines whether a given hash exists in both diffed subtrees,
-- Replace subtrees of teh given tree with the hash representing them.
extract :: (Hashed ValueF -> Maybe Hash) -> Hashed ValueF -> Holy ValueF Hash
extract oracle v = Free.unfold expand v
  where
    expand :: Hashed ValueF -> Either Hash (ValueF (Hashed ValueF))
    expand v =
      case oracle v of
        Nothing -> Right (Cofree.unwrap v)
        Just h -> Left h

-- Expand a value into a Holy structure, with no Hashes inside.
makeHoly :: Value -> Holy ValueF Hash
makeHoly = Free.unfold (Right . FF.project)

-- postProcess will clean up situations where a subtree exists in both trees, but where it
-- may be embedded in a LARGER subtree in the other. By checking which metavars exist in both
-- inserts and deletes, we can ensure that we don't have any that are mismatched. Mismatched
-- metavars are simply restored to their original values.
postProcess :: Map Hash Value -> Change ValueF Hash -> Maybe (Change ValueF Hash)
postProcess metaMap Change{deletions, insertions} = Change <$> cleanVars deletions <*> cleanVars insertions
  where
    allMetaVars :: Holy ValueF Hash -> Set Hash
    allMetaVars = foldMap Set.singleton
    matchedMetavars = allMetaVars deletions `Set.intersection` allMetaVars insertions
    cleanVars :: Holy ValueF Hash -> Maybe (Free ValueF Hash)
    cleanVars f = sequenceA . runMaybeT $ lift f >>= \case
      h | Set.member h matchedMetavars -> pure h
        | otherwise -> case Map.lookup h metaMap of
            Just v -> lift $ makeHoly v
            -- We were missing a mapping for this metavar, which shouldn't ever happen.
            Nothing -> empty


-- tests
correctnessProperty :: Value -> Value -> Bool
correctnessProperty x y = applyChange (computeDiff x y) x == Just y

precisionProperty :: Value -> Value -> Bool
precisionProperty x y = applyChange (computeDiff x x) y == Just y

-- a = Node2 (Node2 t k) u
-- b = Node2 (Node2 t k) t
-- extract (wcs a b) a = Node2C (Hole 0) u
-- extract (wcs a b) b = Node2C (Hole 0) (Hole 1)

-- a.k.a. wcs Given two trees, find the third subtree if it exists in both other trees and
-- provide its metavar.
selectSharedHash :: Set Hash -> Hashed ValueF -> Maybe Hash
selectSharedHash s (h Cofree.:< _)
  | h `Set.member` s = Just h
  | otherwise = Nothing

-- short for “which common subtree”. The call wcs s d x returns Nothinд when x is not a subtree of s and d; if x is a subtree of both s and d, it returns Just i, for some metavariable i. The only condition we impose is injectivity of wcs s d: that is, if wcs s d x ≡ wcs s d y ≡ Just j, then x ≡ y. In other words, equal metavariables correspond to equal subtrees.

-- Given a set of delete locations embeded in a holy tree;
-- Assign the subtrees from the value we're altering to their paired metavars
del :: forall metavar.  (Ord metavar, Show metavar) =>  Holy ValueF metavar -> Value -> Maybe (Map metavar Value)
del ctx tree = flip execStateT mempty $ go ctx tree
  where
    alignValues :: These (Holy ValueF metavar) Value -> StateT (Map metavar Value) Maybe ()
    alignValues (These a b) = go a b
    -- TODO: is this right??
    alignValues _ = empty
    -- Crawl the structures together and determine whether they match
    go :: Holy ValueF metavar -> Value -> StateT (Map metavar Value) Maybe ()
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
ins :: forall metavar. Ord metavar =>  Holy ValueF metavar -> Map metavar Value -> Maybe Value
ins tree metaMapping = flip runReaderT metaMapping $ go tree
  where
    -- Crawl the structures together and determine whether they match
    go :: Holy ValueF metavar -> ReaderT (Map metavar Value) Maybe Value
    go = \case
      (Free (ObjectF o)) -> Object <$> traverse go o
      (Free (ArrayF a)) -> Array <$> traverse go a
      (Free (StringF s)) -> pure $ String s
      (Free (NumberF n)) -> pure $ Number n
      (Free (BoolF b)) -> pure $ Bool b
      (Free NullF) -> pure Null
      -- Look up the metavar, fail if its missing
      (Pure mv) -> asks (Map.lookup mv) >>= lift

checks :: Value -> Value -> Bool
checks x y = correctnessProperty x y && precisionProperty x y

simple1, simple2 :: Aeson.Value
simple1 = Aeson.Array ["one", "two", "three"]
simple2 = Object ["key" .= Aeson.Array ["two", "one"]]


-- alignSpines :: These (Free ValueF metavar) (Free ValueF metavar) -> Free ValueF (Change ValueF metavar)
-- ValueF metavar -> ValueF Value -> StateT (Map metavar Value) Maybe ()

-- partialZip :: Alternative m => f a -> f b -> (These a b -> m r) -> m r
-- partialZip :: f a -> f b -> Maybe (f (These a b))
-- partialZip (ObjectF xs) (ObjectF ys) = Just . ObjectF $ align xs ys
-- partialZip (ArrayF xs) (ArrayF ys) = Just . ArrayF $ align xs ys
-- partialZip (StringF xs) (StringF ys) = Nothing
-- partialZip _ _ = _

