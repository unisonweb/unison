{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Unison.Codebase.Causal3
  (Causal(), Load, Save, head,
  hashCausal, saveCausal)
where

import           Prelude                 hiding ( head
                                                , sequence
                                                )
import           Control.Applicative            ( liftA2 )
import           Control.Lens                   ( (<&>) )
import           Control.Monad.Extra            ( ifM, foldM )
import           Control.Monad.Loops            ( anyM )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Control.Monad.Reader           ( ReaderT(..) )
import           Data.Functor                   (($>))
import           Data.List                      ( foldl1' )
import           Unison.Hash                    ( Hash )
import qualified Unison.Hashable               as Hashable
import           Unison.Hashable                ( Hashable )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import Data.Set (Set)

{-
`Causal a` has 5 operations, specified algebraically here:

* `before : Causal m a -> Causal m a -> m Bool` defines a partial order on
            `Causal`.
* `head : Causal m a -> a`, which represents the "latest" `a` value in a causal
          chain.
* `one : a -> Causal m a`, satisfying `head (one hd) == hd`
* `cons : a -> Causal a -> Causal a`, satisfying `head (cons hd tl) == hd` and
          also `before tl (cons hd tl)`.
* `merge : CommutativeSemigroup a => Causal a -> Causal a -> Causal a`, which is
           associative and commutative and satisfies:
  * `before c1 (merge c1 c2)`
  * `before c2 (merge c1 c2)`
* `sequence : Causal a -> Causal a -> Causal a`, which is defined as
              `sequence c1 c2 = cons (head c2) (merge c1 c2)`.
  * `before c1 (sequence c1 c2)`
  * `head (sequence c1 c2) == head c2`
-}

-- We want a pure structure for the Causal, separate from the effect
-- of loading it. We will almost always want to read and write only
-- parts of it.
--
-- Specifically: We don't want to have to load a tail (from disk?)
-- in order to know whether we need to write it to disk.

data Causal e
  = One { head :: e }
  | Cons { head :: e, tail :: Hash }
  | Merge { head :: e, tails' :: Set Hash }

type Load' m e = Hash -> m (Maybe (Causal e))
type Load m e = ReaderT Hash (MaybeT m) (Causal e)
type Save m e = Causal e -> Hash -> m ()

hash :: Hashable e => e -> Hash
hash = Hashable.accumulate'

hashCausal :: Hashable e => Causal e -> Hash
hashCausal = \case
  One e -> hashOne e
  Cons e t -> hashCons e t
  Merge e ts -> hashMerge e ts
  where
  hashOne :: Hashable e => e -> Hash
  hashOne = hash

  hashCons :: Hashable e => e -> Hash -> Hash
  hashCons e tail = hash [hash e, tail]

  -- If the 'e' is a deterministic function of the tails,
  -- it may not be needed here as an argument, or as part of the
  -- resulting hash.
  hashMerge :: Hashable e => e -> Set Hash -> Hash
  hashMerge e tails = hash [hash e, hash tails]

instance Hashable a => Hashable (Causal a) where
  tokens = Hashable.tokens . hashCausal

saveCausal :: (Monad m, Hashable e) => Save m e -> Causal e -> m Hash
saveCausal save = \case
  One e -> saveOne save e
  Cons e t -> saveCons save e t
  Merge e ts -> saveMerge save e ts
  where
  -- save `e` and return its hash
  saveOne :: (Functor m, Hashable e) => Save m e -> e -> m Hash
  saveOne save e = let h = hash e in save (One e) h $> h

  saveCons :: (Functor m, Hashable e) => Save m e -> e -> Hash -> m Hash
  saveCons save e tl = let h = hash [hash e, tl] in save (Cons e tl) h $> h

  saveConsN :: (Monad m, Hashable e) => Save m e -> [e] -> Hash -> m Hash
  saveConsN save conss tail =
    foldM (\t e -> saveCons save e t) tail (reverse conss)
  saveMerge = error "todo"


-- pattern ConsN m <- (uncons -> Just m)

-- this will need a `Load m e`
uncons' :: Monad m => Load' m e -> Causal e -> Maybe (m ([(Hash, e)], Causal e))
uncons' load = \case
  One{} -> Nothing
  Merge{} -> Nothing
  x@Cons{} -> Just $ go [] x
  where
  go acc (Cons e t) = load t >>= \case
    Nothing ->
      -- how to raise load failure?  MonadError?
      error "todo"
    Just c ->
      -- come back to this after verifying the purpose of this function
      error "todo"
-- uncons One{}         = Nothing
-- uncons Merge{}       = Nothing
-- uncons x             = Just $ go [] x
--  where
--   go acc (Cons h e tail) = tail >>= go ((h, e) : acc)
--   go acc x               = pure (reverse acc, x)

instance Hashable a => Eq (Causal a) where
  a == b = hash a == hash b

instance Hashable a => Ord (Causal a) where
  a <= b = hash a <= hash b

-- merge :: Monad m
--   => Load m e -> ([e] -> m e) -> Causal e -> Causal e -> m (Causal e)
-- merge load f a b =
--   ifM (before load a b) (pure b) .
--   ifM (before load b a) (pure a) $
--   case (a, b) of
--     (Merge _ _ tls, Merge _ _ tls2) -> merge0 $ Map.union tls tls2
--     (Merge _ _ tls, b) -> merge0 $ Map.insert (currentHash b) (pure b) tls
--     (b, Merge _ _ tls) -> merge0 $ Map.insert (currentHash b) (pure b) tls
--     (a, b) ->
--       merge0 $ Map.fromList [(currentHash a, pure a), (currentHash b, pure b)]

-- -- Does `h2` incorporate all of `h1`?
-- before :: Monad m => Load m e -> Causal e -> Causal e -> m Bool
-- before load h1 h2 = go h1 h2
--  where
--   -- stopping condition if both are equal
--   go h1 h2 | h1 == h2 = pure True
--   -- otherwise look through tails if they exist
--   go _  (One _ _    ) = pure False
--   go h1 (Cons _ _ tl) = load tl >>= go h1
--   -- `m1` is a submap of `m2`
--   go (Merge _ _ m1) (Merge _ _ m2) | all (`Map.member` m2) (Map.keys m1) =
--     pure True
--   -- if not, see if `h1` is a subgraph of one of the tails
--   go h1 (Merge _ _ tls) =
--     (||) <$> pure (Map.member (currentHash h1) tls) <*> anyM (>>= go h1)
--                                                              (Map.elems tls)
--   -- Exponential algorithm of checking that all paths are present
--   -- in `h2` isn't necessary because of how merges are flattened
--   --go (Merge _ _ m1) h2@(Merge _ _ _)
--   --  all (\h1 -> go h1 h2) (Map.elems m1)

-- -- implementation detail, load and merge a set of tails `Merge`
-- merge0 :: Load m e -> ([e] -> m e) -> Set Hash -> m e
-- merge0 load f tails =
--   let e = if Map.null tails
--         then error "Causal.merge0 empty set"
--         -- (<>) isn't ideal, because it can't merge children, which would
--         -- involve more loading and saving.
--         -- If we do something like `foldl (<>)` and it creates and saves
--         -- a bunch of temporary nodes, we should have some way to garbage-
--         -- collect them.
--         else f =<< fmap load (Set.toList tails)
--       h = hash (Map.keys m) -- sorted order
--   in  e <&> \e -> Merge h e m

-- Whose responsibility is it to save?
-- We want to save on every Causal construction to make sure they exist somewhere
-- We don't want to save on every Causal construction though, if we are
-- creating causals incrementally (e.g. using `foldl1'` to merge two at a
-- time instead of all at once).  Alternatives include: not creating causals
-- incrementally (use a more complex algorithm to create merge many at once),
-- or don't worry about saving lots of temporary Causals (use some GC after
-- the fact instead).
-- Strategy: Start with "save all Causals on every creation".
-- Can later write GC or an algorithm that creates less garbage.

-- What about
-- step :: Hashable e => Save m e -> Load' m e -> (e -> e) -> Hash -> m Hash
-- step save load f h = load h >>= error "todo"
-- -- step :: Hashable e => Save m e -> (e -> e) -> Causal e -> Causal e
--
-- -- step f c = cons (f . head $ c) =<< saveCausal c
--
-- stepIf
--   :: (Applicative m, Hashable e)
--   => (e -> Bool)
--   -> (e -> e)
--   -> Causal m e
--   -> Causal m e
-- stepIf cond f c = if (cond $ head c) then step f c else c
--
-- stepM
--   :: (Applicative m, Hashable e) => (e -> m e) -> Causal m e -> m (Causal m e)
-- stepM f c = (`cons` c) <$> f (head c)
