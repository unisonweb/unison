{-# Language BangPatterns #-}
{-# Language ConstrainedClassMethods #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveFoldable #-}
{-# Language FlexibleContexts #-}
{-# Language TypeFamilies #-}

module Unison.Runtime.Pcbt where

import Control.Applicative
import Data.Maybe
import Data.Tuple (swap)
import Unison.Runtime.Bits (Bits, Bit(..))
import Unison.Runtime.Stream (Stream)
import qualified Data.Map as Map
import qualified Unison.Runtime.Bits as Bits
import qualified Unison.Runtime.Stream as Stream
import qualified Unison.Runtime.Vector as V

type Bitpath = [Bool]
type IsLeaf = Bool
type Choices p = V.Vector (p, Bool)

data Pcbt m p a = Pcbt (m (PcbtF' m p a)) deriving Functor

hit :: PcbtF' m p a -> Maybe a
hit (Bin' h _ _ _) = h
hit (Tip' h) = h

data PcbtF' m p a
  = Bin' (Maybe a) p (Pcbt m p a) (Pcbt m p a)
  | Tip' (Maybe a) deriving Functor

stream' :: Choices p -> Pcbt m p a -> Stream m (Choices p, a) ()
stream' c (Pcbt mt) = Stream.eval mt >>= \t -> case t of
  Tip' h -> emits h
  Bin' h p l r -> emits h >> stream' (c `V.snoc` (p,False)) l
                          >> stream' (c `V.snoc` (p,True) ) r
  where emits h = maybe (pure ()) (\h -> Stream.emit (c,h)) h

stream :: Pcbt m p a -> Stream m a ()
stream v = Stream.mapEmits snd (stream' V.empty v)

-- | Composite paths. `Base p` points to a bitvector, and `offset`
-- picks out bits of this common bitvector. Should satisfy:
-- `base (composite b i) == b` and `offset (composite b i) == i`.
class Composite p where
  type Base p :: *
  base :: p -> Base p
  offset :: p -> Int
  composite :: Base p -> Int -> p

class Pathed a where
  type Path a :: *
  bitAt :: Path a -> a -> Maybe Bit
  -- | Given `a1`, `a2`, `differingPath a1 a2` finds a `p`
  -- such that `bitAt p a1 /= bitAt p a2`, or returns `Nothing`
  -- if no such path exists (which implies `a1` equals `a2`).
  differingPath :: a -> a -> Maybe (Path a)
  bitpaths :: Composite (Path a) => a -> Trie (Base (Path a)) Bits

mergeTrie :: (Ord p, Monoid v) => Trie p v -> Trie p v -> Trie p v
mergeTrie (Trie p n v children) (Trie p2 m v2 children2)
  | p /= p2 = error "Trie paths must align"
  | otherwise = Trie p (n+m) (mappend v v2) (merge children children2) where
      merge [] c = c
      merge c [] = c
      merge (h1@(Trie p _ _ _):t1) (h2@(Trie p2 _ _ _):t2)
        | p == p2 = mergeTrie h1 h2 : merge t1 t2
        | p < p2  = h1 : merge t1 (h2 : t2)
        | p > p2  = h2 : merge (h1 : t1) t2
        | otherwise = error "impossible"

data Trie p v = Trie p !Int v [Trie p v] deriving Functor

fromList :: (Pathed a, Composite (Path a), Ord (Base (Path a)), Applicative f)
         => [a] -> Pcbt f (Path a) a
fromList as = go (as `zip` map trie as) where
  trie h = pure <$> bitpaths h
  miss = Pcbt (pure (Tip' Nothing))
  value best = maybe (-1) (\(score,_,_) -> score) best
  finish (_, base, offset) = composite base offset
  find best [] = finish <$> best
  find best (Trie _ n _ _ : tl) | value best >= fromIntegral n / 2 = find best tl
  find best (Trie p _ msb cs : tl) = case msb of
    Just (i,s) | value best < s -> find (Just (s, p, i)) (cs ++ tl)
    _ -> find best (cs ++ tl)
  go [] = miss
  go [(x,_)] = Pcbt (pure (Tip' (Just x)))
  go ((_,t0):xs) =
    let t = Bits.mostSignificantBit <$> foldr mergeTrie t0 (map snd xs)
    in case find Nothing [t] of
    Nothing -> miss
    Just p -> Pcbt . pure $ Bin' Nothing p (go zeros) (go ones)
      where
      zeros = filter (\(x,_) -> maybe False (`Bits.matches` False) (bitAt p x)) xs
      ones = filter (\(x,_) -> maybe False (`Bits.matches` True) (bitAt p x)) xs

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

alignLevel :: (Eq p, Applicative m, Pathed a, Path a ~ p)
           => PcbtF' m p a -> PcbtF' m p a -> (PcbtF' m p a, PcbtF' m p a)
alignLevel l r = case (hit l, hit r) of
  (_, Nothing) -> (l,r)
  (Nothing, _) -> (l,r)
  (Just h1, Just h2) -> case differingPath h1 h2 of
    Nothing -> (l,r)
    Just p -> (tweak p l h1, tweak p r h2) where

tweak :: (Applicative m, Pathed a) => Path a -> PcbtF' m (Path a) a1 -> a -> PcbtF' m (Path a) a1
tweak p t h  = case bitAt p h of
  Just Zero -> Bin' Nothing p (Pcbt (pure t)) (Pcbt $ pure (Tip' Nothing))
  Just One -> Bin' Nothing p (Pcbt $ pure (Tip' Nothing)) (Pcbt (pure t))
  Just Both -> Bin' Nothing p (Pcbt (pure t)) (Pcbt (pure t))
  Nothing -> error "alignLevel differing path invalid"

union0 :: (Eq p, Applicative m, Pathed a, Path a ~ p)
       => (Pcbt m p a -> Pcbt m p a -> Pcbt m p a)
       -> Pcbt m p a -> Pcbt m p a -> Pcbt m p a
union0 u (Pcbt v1) (Pcbt v2) = Pcbt $ liftA2 f v1 v2 where
  f t1 t2 = case alignLevel t1 t2 of
    (Tip' h1, Tip' h2) -> Tip' (h1 <|> h2)
    (Bin' h1 p l r, Tip' h2) -> Bin' (h1 <|> h2) p l r
    (Tip' h1, Bin' h2 p l r) -> Bin' (h1 <|> h2) p l r
    (Bin' h1 p1 l1 r1, b@(Bin' h2 p2 l2 r2))
      | p1 == p2  -> Bin' (h1 <|> h2) p1 (u l1 l2) (u r1 r2)
      | otherwise -> Bin' h1 p1 (u l1 (Pcbt (pure b))) (u r1 (Pcbt (pure b)))

union :: (Eq p, Applicative m, Pathed a, Path a ~ p) => Pcbt m p a -> Pcbt m p a -> Pcbt m p a
union v1 v2 = union0 union v1 v2

unionz :: (Eq p, Applicative m, Pathed a, Path a ~ p) => Pcbt m p a -> Pcbt m p a -> Pcbt m p a
unionz v1 v2 = union0 (flip unionz) v1 v2

intersection0 :: (Eq p, Applicative m, Pathed a, Path a ~ p)
              => (Pcbt m p a -> Pcbt m p a -> Pcbt m p a)
              -> Pcbt m p a -> Pcbt m p a -> Pcbt m p a
intersection0 u (Pcbt v1) (Pcbt v2) = Pcbt $ liftA2 f v1 v2 where
  f t1 t2 = case alignLevel t1 t2 of
    (Tip' h1, Tip' h2) -> Tip' (h1 <|> h2)
    (Bin' h1 _ _ _, Tip' h2) -> Tip' (h1 <|> h2)
    (Tip' h1, Bin' h2 _ _ _) -> Tip' (h1 <|> h2)
    (Bin' h1 p1 l1 r1, b@(Bin' h2 p2 l2 r2))
      | p1 == p2  -> Bin' (h1 <|> h2) p1 (u l1 l2) (u r1 r2)
      | otherwise -> Bin' h1 p1 (u l1 (Pcbt (pure b))) (u r1 (Pcbt (pure b)))

intersection :: (Eq p, Applicative m, Pathed a, Path a ~ p)
             => Pcbt m p a -> Pcbt m p a -> Pcbt m p a
intersection v1 v2 = intersection0 intersection v1 v2

intersectionz :: (Eq p, Applicative m, Pathed a, Path a ~ p)
              => Pcbt m p a -> Pcbt m p a -> Pcbt m p a
intersectionz v1 v2 = intersection0 (flip intersectionz) v1 v2

joinOn0 :: (Eq p, Applicative m, Pathed a, Pathed b, Path a ~ p, Path b ~ p)
        => ((p -> Maybe p) -> Pcbt m p a -> Pcbt m p b -> Pcbt m p (a,b))
        -> (p -> Maybe p) -> Pcbt m p a -> Pcbt m p b -> Pcbt m p (a,b)
joinOn0 u col (Pcbt v1) (Pcbt v2) = Pcbt $ liftA2 f v1 v2 where
  f t1 t2 = case (t1, t2) of
    (Tip' Nothing, _) -> Tip' Nothing
    (_, Tip' Nothing) -> Tip' Nothing
    (Tip' (Just a), b) -> (,) a <$> b
    (a, Tip' (Just b)) -> flip (,) b <$> a
    (a@(Bin' h1 p1 l1 r1), b@(Bin' h2 p2 l2 r2))
      | col p1 == Just p2 -> Bin' (liftA2 (,) h1 h2) p1 (u col l1 l2) (u col r1 r2)
      | otherwise -> case h2 >>= (\h -> const h <$> bitAt p2 h) of
        Nothing -> Bin' Nothing p1 (u col l1 (Pcbt (pure b))) (u col r1 (Pcbt (pure b)))
        -- if h2 is defined, and `p1` is a valid path into it, we push `b` down
        Just h2 -> f a (tweak p1 b h2)

joinOn :: (Eq p, Applicative m, Pathed a, Pathed b, Path a ~ p, Path b ~ p)
       => (p -> Maybe p) -> Pcbt m p a -> Pcbt m p b -> Pcbt m p (a,b)
joinOn col v1 v2 = joinOn0 joinOn col v1 v2

joinOnz :: (Eq p, Applicative m, Pathed a, Pathed b, Path a ~ p, Path b ~ p)
        => (p -> Maybe p) -> Pcbt m p a -> Pcbt m p b -> Pcbt m p (a,b)
joinOnz col v1 v2 = joinOn0 (\col v1 v2 -> swap <$> joinOnz col v2 v1) col v1 v2

sort :: (Applicative m, Composite p, Ord (Base p))
     => Order p -> Pcbt m p a -> Stream m (Choices p, a) ()
sort ord v = () <$ go ord Open where
  go _ Closed = pure Closed
  go ord seen = sort0 seen ord v >>= go (V.init ord)

sort0 :: (Applicative m, Composite p, Ord (Base p))
    => Search -> Order p -> Pcbt m p a -> Stream m (Choices p, a) Search
sort0 seen ord (Pcbt v) = go V.empty seen v where
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
      Bin' h p (Pcbt l) (Pcbt r) -> emits h >> case query p of
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

-- | Trim a `Pcbt` to only contain branches which may contain results
-- consistent with the query.
trim :: Applicative m => (p -> Bit) -> Pcbt m p a -> Pcbt m p a
trim query (Pcbt mt) = Pcbt $ flip fmap mt $ \t -> case t of
  Tip' h -> Tip' h
  Bin' h p l r -> case query p of
    Zero -> Bin' h p (trim query l) (Pcbt (pure (Tip' Nothing)))
    One -> Bin' h p (Pcbt (pure (Tip' Nothing))) (trim query r)
    Both -> Bin' h p (trim query l) (trim query r)

bitpath :: Choices p -> Bitpath
bitpath c = map snd (V.toList c)
