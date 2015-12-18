{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveFoldable #-}

module Unison.Runtime.Pcbt where

import Data.Maybe
import Unison.Runtime.Free (Free)
import Unison.Runtime.Stream (Stream)
import qualified Data.Map as Map
import qualified Unison.Runtime.Vector as V
import qualified Unison.Runtime.Stream as Stream
import qualified Unison.Runtime.Free as Free

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
  = View { stream :: Stream m (Choices p, a) ()
         , at     :: V.Vector (p,Bit) -> Free m (Pcbt' p a) }

data Pcbt' p a
  = Bin' (Maybe a) p (Pcbt' p a) (Pcbt' p a)
  | Tip' (Maybe a)

bin' :: Maybe a -> p -> Pcbt' p a -> Pcbt' p a -> Pcbt' p a
bin' hit _ (Tip' Nothing) (Tip' Nothing) = Tip' hit
bin' hit p l r = Bin' hit p l r

view :: Ord p => Pcbt m p a -> View m p a
view t = View (stream V.empty) at where
  stream cursor = do
    let bp = bitpath cursor
    isLeaf <- Stream.eval (structure t bp)
    l <- Stream.eval (labels t bp)
    case l of
      Nothing -> next cursor
      Just (Labels p _ hit) -> case hit of
        Nothing -> tryDescend p isLeaf cursor
        Just a -> Stream.emit (cursor, a) `Stream.append` tryDescend p isLeaf cursor
  -- Move up until at a 0-branch, then flip to a 1, e.g. [0,0,0,1,1] -> [0,0,1]
  next cursor = case V.dropRightWhile snd cursor of
    c | V.isEmpty c -> pure ()
      | otherwise   -> stream (V.modifyLast (\(p,_) -> (p,True)) c)
  -- Move down the 0-branch
  descend p cursor = stream (cursor `V.snoc` (p,False)) -- go down 0 branch
  tryDescend p isLeaf c = if isLeaf then next c else descend p c
  at cursor = at' (Map.fromList (V.toList cursor)) V.empty
  at' query cursor = do
    l <- Free.eval (labels t (V.toList cursor))
    case l of
      Nothing -> pure (Tip' Nothing)
      Just (Labels path maxPath hit)
        | Map.null query -> pure (Tip' hit)
        | maxPath < fst (Map.findMin query) -> pure (Tip' hit)
        | otherwise -> case fromMaybe Both $ Map.lookup path query of
          -- query picks a definite branch
          Zero ->
            bin' hit path <$> at' (Map.delete path query) (cursor `V.snoc` False)
                          <*> pure (Tip' Nothing)
          One ->
            bin' hit path (Tip' Nothing) <$> at' (Map.delete path query) (cursor `V.snoc` True)
          -- query doesn't say, check both 0-branch and 1-branch
          Both -> bin' hit path <$> at' query (cursor `V.snoc` False)
                                   <*> at' query (cursor `V.snoc` True)

bitpath :: Choices p -> Bitpath
bitpath c = map snd (V.toList c)

data Bit = Zero | One | Both

bitMatches :: Bit -> Bool -> Bool
bitMatches Both _ = True
bitMatches Zero False = True
bitMatches One True = True
bitMatches _ _ = False
