{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Unison.Codebase.Causal40
where

import           Prelude                 hiding ( head
                                                , sequence
                                                )
import           Control.Applicative            ( liftA2 )
import           Control.Lens                   ( (<&>) )
import           Control.Monad                  ( (>=>) )
import           Control.Monad.Except           ( MonadError )
import           Control.Monad.Extra            ( ifM )
import           Control.Monad.Loops            ( anyM )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Control.Monad.Reader           ( ReaderT(..) )
import           Data.List                      ( foldl1' )
import           Unison.Hash                    ( Hash )
import qualified Unison.Hashable               as Hashable
import           Unison.Hashable                ( Hashable, tokens )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Unison.Codebase.Serialization  (Get, Put)
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

data LoadError = LoadFailure Hash
data TypedHash a = TypedHash Hash
class Loadable b where
  load :: forall m v a.
    ReadOnlyCodebase LoadError m v a ->
    TypedHash b ->
    m (Either LoadError b)

class HashLoad m where
  -- load :: Loadable a => TypedHash a -> m a
  load_ :: Loadable a => TypedHash a -> m (Either LoadError a)

class HashStore m where
  save_ :: Saveable a => a -> m (TypedHash a)

load :: (MonadError LoadError m, Loadable a) => TypedHash a -> m a
load h = load_ h >>= \case
  Left e -> throwError e
  Right a -> pure a

-- save :: (HashStore m, Saveable a) =>

class (HashLoad a m, Hashable a) => HashStore a m where
  save :: a -> m Hash

type LoadCausal e m = HashLoad (Causal e) m
type SaveCausal e m = HashStore (Causal e) m
loadCausal :: LoadCausal e m => Hash -> m (Causal e)
loadCausal = load

data Causal e
  = One { head :: e }
  | Cons { head :: e, tail :: Hash}
  -- a Merge node's head is a function of its tails,
  -- and is here only for convenience
  | Merge { head :: e, tails :: Set Hash }

step :: SaveCausal e m => (e -> e) -> Causal e -> m (Causal e)
step f c = Cons (f $ head c) <$> save c

stepM :: SaveCausal e m => (e -> m e) -> Causal e -> m (Causal e)
stepM f c = Cons <$> f (head c) <*> save c

stepIf ::
  SaveCausal e m => (e -> Bool) -> (e -> e) -> Causal e -> m (Causal e)
stepIf cond f c = if (cond $ head c) then step f c else pure c

step' :: SaveCausal e m => (e -> e) -> Hash -> m Hash
step' f = load >=> step f >=> save

stepIf' :: SaveCausal e m => (e -> Bool) -> (e -> e) -> Hash -> m Hash
stepIf' cond f = load >=> stepIf cond f >=> save

stepM' :: SaveCausal e m => (e -> m e) -> Hash -> m Hash
stepM' f = load >=> stepM f >=> save


hash :: forall e. Hashable e => e -> Hash
hash = Hashable.accumulate'

pattern ConsN m <- (uncons -> Just m)

uncons :: LoadCausal e m => Causal e -> Maybe (m ([e], Causal e))
uncons = \case
  One{} -> Nothing
  Merge{} -> Nothing
  x@Cons{} -> Just $ go [] x
  where
  go acc (Cons e t) = go (e : acc) =<< load t
  go acc x          = pure (reverse acc, x)

merge
  :: forall e m . SaveCausal e m => ([e] -> m e) -> Causal e -> Causal e -> m (Causal e)
merge f a b = do
  ha <- save a
  hb <- save b
  ifM (before' @e @m ha hb) (pure b) .
    ifM (before' @e @m hb ha) (pure a) $
    case (a, b) of
      (Merge _ tls, Merge _ tls2) -> merge0 f (Set.union tls tls2)
      (Merge _ tls, b) -> merge0 f (Set.insert hb tls)
      (b, Merge _ tls) -> merge0 f (Set.insert hb tls)
      (a, b) -> merge0 f (Set.fromList [ha, hb])

-- merge :: SaveCausal e m => ([e] -> m e) -> Hash -> Set Hash -> m (Causal e)

-- Does `h2` incorporate all of `h1`?
before' :: forall e m. LoadCausal e m => Hash -> Hash -> m Bool
before' h1 h2 =
  -- stopping condition if both are equal
  if h1 == h2 then pure True
  -- otherwise see if tails of h2 incorporate h1
  else loadCausal @e h2 >>= \case
  -- h2 doesn't have a tail
  One _       -> pure False
  Cons _ tl   -> before' @e @m h1 tl
  -- h2 has many tails
  Merge _ tls -> loadCausal @e h1 >>= \case
    -- tails h1 is a subset of tails h2, so h2 incorporates all of h1
    Merge _ s1 | all (`Set.member` tls) s1 -> pure True
    -- h1 is one of the tails, or h1 is incorporated in one of the tails
    c1 -> (Set.member h1 tls ||) <$> anyM (before' @e @m h1) (Set.toList tls)

-- Q: Why can't `Hashable e` be inferred from `LoadCausal e m` here?
before :: forall e m. (Hashable e, LoadCausal e m) => Causal e -> Causal e -> m Bool
-- ^Does `c2` incorporate all of `c1`?
before c1 c2 = before' @e @m (hash c1) (hash c2)

-- implementation detail, load and merge a set of tails
-- Note that `f` will likely be doing writing, and may create temporary
-- garbage.  At some point we will want to do some garbage collection,
-- either by using an in-memory `m` for the temporary operations, and then
-- flushing only the final values to disk, or as a manual cleanup CLI
-- command.
merge0 :: SaveCausal e m => ([e] -> m e) -> Set Hash -> m (Causal e)
merge0 f tails =
  if null tails then error "Causal.merge0 empty set"
  else do
    es <- traverse (fmap head . loadCausal) (Set.toList tails)
    e <- f es
    pure (Merge e tails)

instance Hashable e => Hashable (Causal e) where
  tokens = tokens . \case
    One e -> [hash e]
    Cons e t -> [hash e, t]
    Merge e ts -> [hash ts]
instance Hashable a => Eq (Causal a) where
  a == b = hash a == hash b
instance Hashable a => Ord (Causal a) where
  a <= b = hash a <= hash b
