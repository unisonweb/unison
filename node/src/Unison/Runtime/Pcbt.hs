{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveFoldable #-}
{-# Language ExistentialQuantification #-}
{-# Language GADTs #-}
{-# Language Rank2Types #-}

module Unison.Runtime.Pcbt where

import Control.Monad
import qualified Unison.Runtime.Vector as V

data Labels p a = Labels { path :: p, maxPath :: p, hit :: Maybe a }
  deriving (Functor, Foldable, Traversable)

type IsBin = Bool

-- | A binary tree along with a set of labels attached to each node in the tree
data Pcbt m p a = Pcbt
  { structure :: [Bool] -> m IsBin
  , labels :: [Bool] -> m (Labels p a) }

type Traversal m p a b r = Free (Instruction m p a b) r

data Instruction m p a b r where
  Effect :: m x -> Instruction m p a b x
  IsLeaf :: Instruction m p a b Bool
  Ask :: Instruction m p a b (Labels p a)
  Skip :: Instruction m p a b ()
  Continue :: Instruction m p a b ()
  Emit :: b -> Instruction m p a b ()

run :: Monad m
    => Traversal m p a b r
    -> V.Vector Bool
    -> V.Vector b
    -> Pcbt m p a
    -> m (V.Vector b)
run f cursor acc t = case f of
  Pure _ -> pure acc
  Bind req k -> case req of
    Effect m -> m >>= (\x -> run (k x) cursor acc t)
    Ask -> labels t (V.toList cursor) >>= \ls -> run (k ls) cursor acc t
    IsLeaf -> structure t (V.toList cursor) >>= \isLeaf -> run (k isLeaf) cursor acc t
    Emit b -> run (k ()) cursor (acc `V.snoc` b) t
    Continue -> do
      cursor <- advance cursor
      maybe (pure acc) (\cursor -> run (k ()) cursor acc t) cursor
      where
      advance i = structure t (V.toList i) >>= \isLeaf -> case isLeaf of
        False -> pure (Just (i `V.snoc` False))
        True -> pure (incr i)
    Skip -> maybe (pure acc) (\cursor -> run (k ()) cursor acc t) (incr cursor)

incr :: V.Vector Bool -> Maybe (V.Vector Bool)
incr i = case V.dropRightWhile id i of
  i | V.isEmpty i -> Nothing
    | otherwise   -> Just (V.init i `V.snoc` True)

data Free f a
  = Pure a
  | forall x . Bind (f x) (x -> Free f a)

instance Functor (Free f) where
  fmap = liftM

instance Applicative (Free f) where
  pure = return
  (<*>) = ap

instance Monad (Free f) where
  return = Pure
  Bind x f >>= g = Bind x ((g =<<) . f)
  Pure x >>= f = f x

eval :: f a -> Free f a
eval a = Bind a pure

translate :: (forall a . f a -> g a) -> Free f a -> Free g a
translate _ (Pure a) = Pure a
translate u (Bind x f) = Bind (u x) (translate u . f)

interpret :: Monad f => Free f a -> f a
interpret (Bind x f) = x >>= (interpret . f)
interpret (Pure a) = return a
