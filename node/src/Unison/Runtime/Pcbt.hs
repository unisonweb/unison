{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveFoldable #-}
{-# Language ExistentialQuantification #-}
{-# Language GADTs #-}
{-# Language Rank2Types #-}

module Unison.Runtime.Pcbt where

import Control.Monad
import qualified Data.Map as Map
import qualified Unison.Runtime.Vector as V

type Bitpath = [Bool]
type IsLeaf = Bool
type Choices p = V.Vector (p, Bool)

data Labels p a = Labels { path :: p, maxPath :: p, hit :: Maybe a }
  deriving (Functor, Foldable, Traversable)

-- | A binary tree along with a set of labels attached to each node in the tree
data Pcbt m p a = Pcbt
  { structure :: [Bool] -> m IsLeaf
  , labels :: [Bool] -> m (Maybe (Labels p a)) }

data View m p a
  = View { stream :: Stream m (Choices p, a)
         , at     :: Choices p -> Free m (Maybe a) }

view :: Ord p => Pcbt m p a -> View m p a
view t = View (stream V.empty) at where
  stream cursor = do
    let bp = bitpath cursor
    isLeaf <- evals (structure t bp)
    l <- evals (labels t bp)
    case l of
      Nothing -> next cursor
      Just (Labels p _ hit) -> case hit of
        Nothing -> tryDescend p isLeaf cursor
        Just a -> emit (cursor, a) >> tryDescend p isLeaf cursor
  -- Move up until at a 0-branch, then flip to a 1, e.g. [0,0,0,1,1] -> [0,0,1]
  next cursor = case V.dropRightWhile snd cursor of
    c | V.isEmpty c -> done
      | otherwise   -> stream (V.modifyLast (\(p,_) -> (p,True)) c)
  -- Move down the 0-branch
  descend p cursor = stream (cursor `V.snoc` (p,False)) -- go down 0 branch
  tryDescend p isLeaf c = if isLeaf then next c else descend p c
  done = pure ()
  at cursor = at' (Map.fromList (V.toList cursor)) V.empty
  at' query cursor = do
    l <- eval (labels t (V.toList cursor))
    case l of
      Nothing -> pure Nothing
      Just (Labels path maxPath hit)
        | Map.null query -> pure hit
        | maxPath < fst (Map.findMin query) -> pure Nothing
        | otherwise -> case Map.lookup path query of
          -- query picks a definite branch
          Just b -> at' (Map.delete path query) (cursor `V.snoc` b)
          -- query doesn't say, check both 0-branch and 1-branch
          Nothing -> at' query (cursor `V.snoc` False) >>= \r -> case r of
            Just a -> pure (Just a) -- we've got a hit from 0-branch, stop
            Nothing -> at' query (cursor `V.snoc` True)

bitpath :: Choices p -> Bitpath
bitpath c = map snd (V.toList c)

data StreamF m b r where
  Effect :: m x -> StreamF m b x
  Emit :: b -> StreamF m b ()

type Stream m o = Free (StreamF m o) ()

data Bit = Zero | One | Both

bitMatches :: Bit -> Bool -> Bool
bitMatches Both _ = True
bitMatches Zero False = True
bitMatches One True = True
bitMatches _ _ = False

evals :: m a -> Free (StreamF m o) a
evals a = eval (Effect a)

emit :: o -> Stream f o
emit o = eval (Emit o)

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
