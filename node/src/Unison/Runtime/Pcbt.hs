{-# Language BangPatterns #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveFoldable #-}

module Unison.Runtime.Pcbt where

import Data.List
import Unison.Runtime.Free (Free)
import Unison.Runtime.Stream (Stream)
import Unison.Path (Path)
import qualified Data.Set as Set
import qualified Unison.Path as Path
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
         , at     :: (Choices p -> p -> p -> Maybe Bit) -> Free m (Pcbt' p a) }

data Pcbt' p a
  = Bin' (Maybe a) p (Pcbt' p a) (Pcbt' p a)
  | Tip' (Maybe a)
  | More' (Maybe a)

bin' :: Maybe a -> p -> Pcbt' p a -> Pcbt' p a -> Pcbt' p a
bin' hit _ (Tip' Nothing) (Tip' Nothing) = Tip' hit
bin' hit p l r = Bin' hit p l r

view :: Ord p => Pcbt m p a -> View m p a
view t = View (stream V.empty) (flip at V.empty) where
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
  at query cursor = do
    l <- Free.eval (labels t (bitpath cursor))
    case l of
      Nothing -> pure (Tip' Nothing)
      Just (Labels path maxPath hit) -> case query cursor path maxPath of
        Nothing -> pure (More' hit)
        Just Neither -> pure (Tip' Nothing)
        Just Zero -> -- query picks a definite branch
          bin' hit path <$> at query (cursor `V.snoc` (path,False)) <*> pure (Tip' Nothing)
        Just One ->
          bin' hit path (Tip' Nothing) <$> at query (cursor `V.snoc` (path,True))
        Just Both -> -- query doesn't say, check both 0-branch and 1-branch
          bin' hit path <$> at query (cursor `V.snoc` (path,False))
                        <*> at query (cursor `V.snoc` (path,True))
        Just PreferOne ->
          at query (cursor `V.snoc` (path,True)) >>= \one -> case one of
            Tip' Nothing -> bin' hit path <$> at query (cursor `V.snoc` (path,False))
                                          <*> pure one
            _ -> pure (bin' hit path (Tip' Nothing) one)
        Just PreferZero ->
          at query (cursor `V.snoc` (path,False)) >>= \zero -> case zero of
            Tip' Nothing -> bin' hit path <$> pure zero
                                          <*> at query (cursor `V.snoc` (path,False))
            _ -> pure (bin' hit path zero (Tip' Nothing))

type Sort p = Choices p

limit :: (Ord p, Path p) => Sort p -> Int -> View m p a -> Free m [a]
limit ord n t = go Set.empty n ord []
  where
  combine ord ordTl = foldl' V.snoc ord ordTl
  go seen n ord ordTl = at t (toQuery seen (combine ord ordTl)) >>= \t' -> case hits t' of
    hs | V.length hs < n -> case V.unsnoc ord of
      Nothing -> pure (map snd (V.toList hs))
      -- if just tried preferring 0-branch for path and got all hits we could,
      -- don't bother searching that branch again; likewise for 1-branch
      Just (ord, (p,b)) ->
        let seen' = Set.union seen (Set.fromList (map fst $ V.toList hs))
        in (map snd (V.toList hs) ++) <$> go seen' (n - V.length hs) ord ((p, not b) : ordTl)
    hs -> pure (take n . map snd $ V.toList hs)
  toQuery seen ord =
    let
      search choices _ _ | Set.member choices seen = Just Neither
      search _ p _ = case find (\(po,_) -> Path.isSubpath po p) (V.toList ord) of
        Nothing -> Just Both
        Just (_,b) -> if not b then Just PreferZero else Just PreferOne
    in search

hits :: Pcbt' p a -> V.Vector (Choices p, a)
hits t = go V.empty V.empty t where
  go !acc !cursor (Bin' h p l r) =
    let acc' = maybe acc (\h -> V.snoc acc (cursor,h)) h
        cursorl = V.snoc cursor (p, False)
        cursorr = V.snoc cursor (p, True)
    in go (go acc' cursorl l) cursorr r
  go !acc !cursor (Tip' h) = maybe acc (\h -> V.snoc acc (cursor,h)) h
  go !acc !cursor (More' h) = maybe acc (\h -> V.snoc acc (cursor,h)) h

bitpath :: Choices p -> Bitpath
bitpath c = map snd (V.toList c)

data Bit = Zero | One | PreferZero | PreferOne | Both | Neither

bitMatches :: Bit -> Bool -> Bool
bitMatches Both _ = True
bitMatches Zero False = True
bitMatches One True = True
bitMatches _ _ = False
