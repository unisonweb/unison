{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Unison.Codebase.Causal2 where

import           Prelude                 hiding ( head
                                                , sequence
                                                )
import           Control.Applicative            ( liftA2 )
import           Control.Lens                   ( (<&>) )
import           Control.Monad.Extra            ( ifM )
import           Control.Monad.Loops            ( anyM )
import           Data.List                      ( foldl1' )
import           Unison.Hash                    ( Hash )
import qualified Unison.Hashable               as Hashable
import           Unison.Hashable                ( Hashable )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

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

data Causal m e
  = One { currentHash :: Hash, head :: e }
  | Cons { currentHash :: Hash, head :: e, tail :: m (Causal m e) }
  -- The merge operation `<>` flattens and normalizes for order
  | Merge { currentHash :: Hash, head :: e, tails :: Map Hash (m (Causal m e)) }

consN :: Applicative m => [(Hash, e)] -> Causal m e -> Causal m e
consN conss tail = foldr (\(h,e) t -> Cons h e (pure t)) tail conss

pattern ConsN m <- (uncons -> Just m)

uncons :: Monad m => Causal m e -> Maybe (m ([(Hash, e)], Causal m e))
uncons (One _ _    ) = Nothing
uncons (Merge _ _ _) = Nothing
uncons x             = Just $ go [] x
 where
  go acc (Cons h e tail) = tail >>= go ((h, e) : acc)
  go acc x               = pure (reverse acc, x)

instance Eq (Causal m a) where
  a == b = currentHash a == currentHash b

instance Ord (Causal m a) where
  a <= b = currentHash a <= currentHash b

merge :: (Monad m, Semigroup e) => Causal m e -> Causal m e -> m (Causal m e)
a `merge` b =
  ifM (before a b) (pure b) . ifM (before b a) (pure a) $ case (a, b) of
    (Merge _ _ tls, Merge _ _ tls2) -> merge0 $ Map.union tls tls2
    (Merge _ _ tls, b) -> merge0 $ Map.insert (currentHash b) (pure b) tls
    (b, Merge _ _ tls) -> merge0 $ Map.insert (currentHash b) (pure b) tls
    (a, b) ->
      merge0 $ Map.fromList [(currentHash a, pure a), (currentHash b, pure b)]

-- Does `h2` incorporate all of `h1`?
before :: Monad m => Causal m e -> Causal m e -> m Bool
before h1 h2 = go h1 h2
 where
  -- stopping condition if both are equal
  go h1 h2 | h1 == h2 = pure True
  -- otherwise look through tails if they exist
  go _  (One _ _    ) = pure False
  go h1 (Cons _ _ tl) = tl >>= go h1
  -- `m1` is a submap of `m2`
  go (Merge _ _ m1) (Merge _ _ m2) | all (`Map.member` m2) (Map.keys m1) =
    pure True
  -- if not, see if `h1` is a subgraph of one of the tails
  go h1 (Merge _ _ tls) =
    (||) <$> pure (Map.member (currentHash h1) tls) <*> anyM (>>= go h1)
                                                             (Map.elems tls)
  -- Exponential algorithm of checking that all paths are present
  -- in `h2` isn't necessary because of how merges are flattened
  --go (Merge _ _ m1) h2@(Merge _ _ _)
  --  all (\h1 -> go h1 h2) (Map.elems m1)

instance (Monad m, Semigroup e) => Semigroup (m (Causal m e)) where
  a <> b = do
    x <- a
    y <- b
    merge x y

-- implementation detail, form a `Merge`
merge0
  :: (Applicative m, Semigroup e) => Map Hash (m (Causal m e)) -> m (Causal m e)
merge0 m =
  let e = if Map.null m
        then error "Causal.merge0 empty map"
        else foldl1' (liftA2 (<>)) (fmap head <$> Map.elems m)
      h = hash (Map.keys m) -- sorted order
  in  e <&> \e -> Merge h e m

hash :: Hashable e => e -> Hash
hash = Hashable.accumulate'

step :: (Applicative m, Hashable e) => (e -> e) -> Causal m e -> Causal m e
step f c = f (head c) `cons` c

stepIf
  :: (Applicative m, Hashable e)
  => (e -> Bool)
  -> (e -> e)
  -> Causal m e
  -> Causal m e
stepIf cond f c = if (cond $ head c) then step f c else c

stepM
  :: (Applicative m, Hashable e) => (e -> m e) -> Causal m e -> m (Causal m e)
stepM f c = (`cons` c) <$> f (head c)

one :: Hashable e => e -> Causal m e
one e = One (hash e) e

cons :: (Applicative m, Hashable e) => e -> Causal m e -> Causal m e
cons e tl = Cons (hash [hash e, currentHash tl]) e (pure tl)

