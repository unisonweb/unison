{-# Language BangPatterns #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveFoldable #-}
{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}

module Unison.Runtime.Pcbt where

import Control.Applicative
import Data.Maybe
import Data.Tuple (swap)
import Unison.Runtime.Stream (Stream)
import qualified Data.Map as Map
import qualified Unison.Runtime.Vector as V
import qualified Unison.Runtime.Stream as Stream

type Bitpath = [Bool]
type IsLeaf = Bool
type Choices p = V.Vector (p, Bool)

data Labels p a = Labels { path :: p, hit :: Maybe a }
  deriving (Functor, Foldable, Traversable)

-- | A binary tree along with a set of labels attached to each node in the tree
data Pcbt m p a = Pcbt
  { structure :: [Bool] -> m IsLeaf
  , labels :: [Bool] -> m (Maybe (Labels p a)) }

data View m p a = View (m (PcbtF' m p a)) deriving Functor

data PcbtF' m p a
  = Bin' (Maybe a) p (View m p a) (View m p a)
  | Tip' (Maybe a) deriving Functor

viewAt :: Applicative m => V.Vector Bool -> Pcbt m p a -> View m p a
viewAt cursor t = View $ liftA2 f (structure t (V.toList cursor)) (labels t (V.toList cursor))
  where
  f isLeaf l = case l of
    Nothing -> Tip' Nothing
    Just l ->
      if isLeaf then Tip' (hit l)
      else Bin' (hit l) (path l)
             (viewAt (cursor `V.snoc` False) t)
             (viewAt (cursor `V.snoc` True) t)

view :: Applicative m => Pcbt m p a -> View m p a
view = viewAt V.empty

stream' :: Choices p -> View m p a -> Stream m (Choices p, a) ()
stream' c (View mt) = Stream.eval mt >>= \t -> case t of
  Tip' h -> emits h
  Bin' h p l r -> emits h >> stream' (c `V.snoc` (p,False)) l
                            >> stream' (c `V.snoc` (p,True) ) r
  where emits h = maybe (pure ()) (\h -> Stream.emit (c,h)) h

stream :: View m p a -> Stream m a ()
stream v = Stream.mapEmits snd (stream' V.empty v)

-- | Composite paths. `Base p` points to a bitvector, and `offset`
-- picks out bits of this common bitvector.
class Composite p where
  type Base p :: *
  base :: p -> Base p
  offset :: p -> Int
  composite :: Base p -> Int -> p

class Pathed a where
  type Path a :: *
  bitAt :: Path a -> a -> Maybe Bit
  paths :: a -> [Path a]

data Search
  = Branch Search Search
  | Closed
  | Open

branch :: Search -> Search -> Search
branch Closed Closed = Closed
branch a b = Branch a b

instance Monoid Search where
  mempty = Open
  mappend s1 s2 = case (s1,s2) of
    (Closed,_) -> Closed
    (_,Closed) -> Closed
    (Open,s2)  -> s2
    (s1,Open)  -> s1
    (Branch l1 r1, Branch l2 r2) -> Branch (l1 `mappend` l2) (r1 `mappend` r2)

close :: Bitpath -> Search -> Search
close [] _ = Closed
close (False : bp) s = case s of
  Open -> branch (close bp Open) Open
  Branch l r -> branch (close bp l) r
  Closed -> Closed
close (True : bp) s = case s of
  Open -> branch Open (close bp Open)
  Branch l r -> branch l (close bp r)
  Closed -> Closed

isClosed :: Bitpath -> Search -> Bool
isClosed _ Closed = True
isClosed _ Open = False
isClosed [] _ = False
isClosed (False : bp) (Branch l _) = isClosed bp l
isClosed (True : bp) (Branch _ r) = isClosed bp r

-- | For each bitrange, a choice of ascending (False) or descending (True)
type Order p = Choices (Base p, Int, Maybe Int)

union0 :: (Eq p, Applicative m)
       => (View m p a -> View m p a -> View m p a)
       -> View m p a -> View m p a -> View m p a
union0 u (View v1) (View v2) = View $ liftA2 f v1 v2 where
  f t1 t2 = case (t1, t2) of
    (Tip' h1, Tip' h2) -> Tip' (h1 <|> h2)
    (Bin' h1 p l r, Tip' h2) -> Bin' (h1 <|> h2) p l r
    (Tip' h1, Bin' h2 p l r) -> Bin' (h1 <|> h2) p l r
    (Bin' h1 p1 l1 r1, b@(Bin' h2 p2 l2 r2))
      | p1 == p2  -> Bin' (h1 <|> h2) p1 (u l1 l2) (u r1 r2)
      | otherwise -> Bin' h1 p1 (u l1 (View (pure b))) (u r1 (View (pure b)))

union :: (Eq p, Applicative m) => View m p a -> View m p a -> View m p a
union v1 v2 = union0 union v1 v2

unionz :: (Eq p, Applicative m) => View m p a -> View m p a -> View m p a
unionz v1 v2 = union0 (flip unionz) v1 v2

intersection0 :: (Eq p, Applicative m)
              => (View m p a -> View m p a -> View m p a)
              -> View m p a -> View m p a -> View m p a
intersection0 u (View v1) (View v2) = View $ liftA2 f v1 v2 where
  f t1 t2 = case (t1, t2) of
    (Tip' h1, Tip' h2) -> Tip' (h1 <|> h2)
    (Bin' h1 _ _ _, Tip' h2) -> Tip' (h1 <|> h2)
    (Tip' h1, Bin' h2 _ _ _) -> Tip' (h1 <|> h2)
    (Bin' h1 p1 l1 r1, b@(Bin' h2 p2 l2 r2))
      | p1 == p2  -> Bin' (h1 <|> h2) p1 (u l1 l2) (u r1 r2)
      | otherwise -> Bin' h1 p1 (u l1 (View (pure b))) (u r1 (View (pure b)))

intersection :: (Eq p, Applicative m) => View m p a -> View m p a -> View m p a
intersection v1 v2 = intersection0 intersection v1 v2

intersectionz :: (Eq p, Applicative m) => View m p a -> View m p a -> View m p a
intersectionz v1 v2 = intersection0 (flip intersectionz) v1 v2

joinOn0 :: (Eq p, Applicative m)
        => ((p -> Maybe p) -> View m p a -> View m p b -> View m p (a,b))
        -> (p -> Maybe p) -> View m p a -> View m p b -> View m p (a,b)
joinOn0 u col (View v1) (View v2) = View $ liftA2 f v1 v2 where
  f t1 t2 = case (t1, t2) of
    (Tip' Nothing, _) -> Tip' Nothing
    (_, Tip' Nothing) -> Tip' Nothing
    (Tip' (Just a), b) -> (,) a <$> b
    (a, Tip' (Just b)) -> flip (,) b <$> a
    (Bin' h1 p1 l1 r1, b@(Bin' h2 p2 l2 r2))
      | col p1 == Just p2 -> Bin' (liftA2 (,) h1 h2) p1 (u col l1 l2) (u col r1 r2)
      | otherwise -> Bin' Nothing p1 (u col l1 (View (pure b))) (u col r1 (View (pure b)))

joinOn :: (Eq p, Applicative m)
       => (p -> Maybe p) -> View m p a -> View m p b -> View m p (a,b)
joinOn col v1 v2 = joinOn0 joinOn col v1 v2

joinOnz :: (Eq p, Applicative m)
        => (p -> Maybe p) -> View m p a -> View m p b -> View m p (a,b)
joinOnz col v1 v2 = joinOn0 (\col v1 v2 -> swap <$> joinOnz col v2 v1) col v1 v2

sort :: (Applicative m, Composite p, Ord (Base p))
     => Order p -> View m p a -> Stream m (Choices p, a) ()
sort ord v = () <$ go ord Open where
  go _ Closed = pure Closed
  go ord seen = sort0 seen ord v >>= go (V.init ord)

sort0 :: (Applicative m, Composite p, Ord (Base p))
    => Search -> Order p -> View m p a -> Stream m (Choices p, a) Search
sort0 seen ord (View v) = go V.empty seen v where
  queryMap = Map.fromList [ (base, (i,j,b)) | ((base,i,j), b) <- V.toList ord ]
  query p = case Map.lookup (base p) queryMap of
    Just (i,j,b) | offset p >= i && offset p < (fromMaybe (offset p + 1) j) ->
      if b then One else Zero
    _ -> Both
  go c seen v = do
    t <- Stream.eval v
    let bp = bitpath c
    case t of
      _ | isClosed bp seen -> pure seen
      Tip' h -> emits h >> pure (close bp seen)
      Bin' h p (View l) (View r) -> emits h >> case query p of
        Zero -> Stream.uncons' (go cl seen l) >>= \e -> case e of
          Left seen -> close bpr <$> go cr (close bpl seen) r
          Right (hd, tl) -> Stream.emit hd >> (close bpl <$> tl)
        One -> Stream.uncons' (go cr seen r) >>= \e -> case e of
          Left seen -> close bpl <$> go cl (close bpr seen) l
          Right (hd, tl) -> Stream.emit hd >> (close bpr <$> tl)
        Both | V.isEmpty ord -> (close bpl <$> go cl seen l) >>= \seen ->
                                (close bpr <$> go cr seen r)
        Both -> close bpr . close bpl <$> merge (go cl seen l) (go cr seen r)
        where
        (cl,cr) = (c `V.snoc` (p,False), c `V.snoc` (p,True))
        (bpl,bpr) = (bitpath cl, bitpath cr)
    where
    emits h = maybe (pure ()) (\h -> Stream.emit (c,h)) h
    hitpos pb i j b c = case Map.lookup pb c of
      Just (offset, b')
        | b == b' && offset >= i && offset < fromMaybe (offset+1) j -> Just offset
      _ -> Nothing
    prefer i _ _ | i >= V.length ord = True
    prefer i c1 c2 = case V.unsafeIndex ord i of
      ((pb,i,j), b) -> case (hitpos pb i j b c1, hitpos pb i j b c2) of
        (Nothing,Nothing) -> prefer (i+1) c1 c2
        (Nothing,Just _) -> False
        (Just _, Nothing) -> True
        (Just pos1, Just pos2) | pos1 < pos2 -> True
                               | pos2 > pos1 -> False
                               | otherwise   -> prefer (i+1) c1 c2
    merge s1 s2 = do
      e1 <- Stream.uncons' s1
      e2 <- Stream.uncons' s2
      let push hd tl = Stream.emit hd >> tl
      let process c = Map.fromList [ (base p, (offset p, b)) | (p,b) <- V.toList c ]
      case (e1, e2) of
        (Left seen1, Left seen2) -> pure (seen1 `mappend` seen2)
        (Left seen1, Right (hd,tl)) -> Stream.emit hd >> ((`mappend` seen1) <$> tl)
        (Right (hd,tl), Left seen2) -> Stream.emit hd >> ((`mappend` seen2) <$> tl)
        (Right (h1,t1), Right (h2,t2)) ->
          if prefer 0 (process $ fst h1) (process $ fst h2)
          then Stream.emit h1 >> merge t1 (push h2 t2)
          else Stream.emit h2 >> merge (push h1 t1) t2

-- | Trim a `View` to only contain branches which may contain results
-- consistent with the query.
trim :: Applicative m => (p -> Bit) -> View m p a -> View m p a
trim query (View mt) = View $ flip fmap mt $ \t -> case t of
  Tip' h -> Tip' h
  Bin' h p l r -> case query p of
    Zero -> Bin' h p (trim query l) (View (pure (Tip' Nothing)))
    One -> Bin' h p (View (pure (Tip' Nothing))) (trim query r)
    Both -> Bin' h p (trim query l) (trim query r)

bitpath :: Choices p -> Bitpath
bitpath c = map snd (V.toList c)

data Bit = Zero | One | Both deriving (Eq,Ord,Show)
