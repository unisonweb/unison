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

data Source m b r where
  Effect' :: m x -> Source m b x
  Output :: b -> Source m b ()

type Stream f o = Free (Source f o) ()

evals :: f a -> Free (Source f o) a
evals a = eval (Effect' a)

output :: o -> Stream f o
output o = eval (Output o)

run :: Traversal m p a b r -> V.Vector Bool -> Pcbt m p a -> Stream m b
run f cursor t = case f of
  Pure _ -> pure ()
  Bind req k -> case req of
    Effect m -> evals m >>= (\x -> run (k x) cursor t)
    Ask -> evals (labels t (V.toList cursor)) >>= \ls -> run (k ls) cursor t
    IsLeaf -> evals (structure t (V.toList cursor)) >>= \isLeaf -> run (k isLeaf) cursor t
    Emit b -> output b >> run (k ()) cursor t
    Continue -> do
      cursor <- advance cursor
      maybe (pure ()) (\cursor -> run (k ()) cursor t) cursor
      where
      advance i = evals (structure t (V.toList i)) >>= \isLeaf -> case isLeaf of
        False -> pure (Just (i `V.snoc` False))
        True -> pure (incr i)
    Skip -> maybe (pure ()) (\cursor -> run (k ()) cursor t) (incr cursor)

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
