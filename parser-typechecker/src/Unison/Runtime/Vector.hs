{-# Language GADTs #-}
{-# Language TypeOperators #-}
{-# Language BangPatterns #-}

module Unison.Runtime.Vector where

import Prelude as P hiding (length)
import qualified Data.Massiv.Array as A

import Unison.Prelude
-- import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as UV
import qualified Data.Bit as B
-- import qualified Data.Vector.Unboxed.Mutable as MV

data Vec ix a where
  U :: A.Unbox a => Type a -> A.Array A.U ix a -> Vec ix a
  Zip :: Vec ix a -> Vec ix b -> Vec ix (a,b)
  -- to get an index, lookup in first array, if False, do lookup in left, else right
  Eithers :: A.Array A.U ix (ix, Bool) -> Vec ix a -> Vec ix b -> Vec ix (Either a b)
  Maybes  :: A.Array A.U ix (ix, Bool) -> Vec ix a -> Vec ix (Maybe a)

data Type a where
  Int :: Type Int
  Word :: Type Word
  Byte :: Type Word8
  Bit :: Type B.Bit
  Double :: Type Double

at1 :: Int -> Vec A.Ix1 a -> a
at1 !i v = case v of
  U t v -> case t of
    Int -> A.index' v i
    Double -> A.index' v i
    Word -> A.index' v i
    Byte -> A.index' v i
    Bit -> A.index' v i
  Zip v1 v2 -> (at1 i v1, at1 i v2)
  Maybes inds elems ->
    let (!i', !b) = A.index' inds i
    in if b then Nothing else Just $ at1 i' elems
  Eithers inds lefts rights ->
    let (!i',!b) = A.index' inds i
    in if b then Right $ at1 i' rights
       else      Left  $ at1 i' lefts

at :: (A.Unbox ix, A.Index ix) => ix -> Vec ix a -> a
at !i v = case v of
  U t v -> case t of
    Int -> A.index' v i
    Double -> A.index' v i
    Word -> A.index' v i
    Byte -> A.index' v i
    Bit -> A.index' v i
  Zip v1 v2 -> (at i v1, at i v2)
  Maybes inds elems ->
    let (!i', !b) = A.index' inds i
    in if b then Nothing else Just $ at i' elems
  Eithers inds lefts rights ->
    let (!i',!b) = A.index' inds i
    in if b then Right $ at i' rights
       else      Left  $ at i' lefts

size :: (A.Unbox ix, A.Index ix) => Vec ix a -> A.Sz ix
size v = case v of
  U _ v -> A.size v
  Zip v1 v2 -> size v1 `min` size v2
  Eithers v _ _ -> A.size v
  Maybes v _ -> A.size v

size1 :: Vec A.Ix1 a -> Int
size1 v = case size v of A.Sz i -> i

zip :: Vec ix a -> Vec ix b -> Vec ix (a,b)
zip = Zip

toIndexedList :: (A.Unbox ix, A.Index ix) => Vec ix a -> [(ix,a)]
toIndexedList v = case v of
  U _ v -> A.toList $ A.imap (\i e -> (i,e)) v
  Zip v v2 -> [ (ix,(a,b)) | ((ix,a),(_,b)) <- toIndexedList v `P.zip` toIndexedList v2 ]
  Maybes inds elems ->
    [ (ind, if b then Just (at i elems) else Nothing)
    | (ind, (i,b)) <- A.toList $ A.imap (\i e -> (i,e)) inds ]
  Eithers inds lefts rights ->
    [ (ind, if b then Right (at i rights) else Left (at i lefts))
    | (ind, (i,b)) <- A.toList $ A.imap (\i e -> (i,e)) inds ]

indices :: (A.Unbox ix, A.Index ix) => Vec ix a -> [ix]
indices = map fst . toIndexedList

toList :: (A.Unbox ix, A.Index ix) => Vec ix a -> [a]
toList = map snd . toIndexedList

compareAt :: (A.Unbox ix, A.Index ix) => ix -> ix -> Vec ix a -> Vec ix a -> Ordering
compareAt i j v v2 = case (v,v2) of
  (U t v, U _ v2) -> case t of
    Int    -> A.index' v i `compare` A.index' v2 j
    Double -> A.index' v i `compare` A.index' v2 j
    Word   -> A.index' v i `compare` A.index' v2 j
    Byte   -> A.index' v i `compare` A.index' v2 j
    Bit    -> A.index' v i `compare` A.index' v2 j
  (Zip v1a v2a, Zip v1b v2b) -> case compareAt i j v1a v1b of
    EQ -> compareAt i j v2a v2b
    c -> c
  (Maybes indsa elemsa, Maybes indsb elemsb) ->
    let (!i', !b1) = A.index' indsa i
        (!j', !b2) = A.index' indsb i
    in if b1 && b2 then compareAt i' j' elemsa elemsb
       else if b1 then GT
       else if b2 then LT
       else EQ
  (Eithers indsa leftsa rightsa, Eithers indsb leftsb rightsb) ->
    let (!i', !b1) = A.index' indsa i
        (!j', !b2) = A.index' indsb i
    in if b1 && b2 then compareAt i' j' rightsa rightsb
       else if b1 == b2 then compareAt i' j' leftsa leftsb
       else if b1 then GT
       else LT
  _ -> error "impossible"

class Vectorizable a where
  fromList :: [a] -> Vec A.Ix1 a

pick :: (A.Unbox ix, A.Index ix) => Vec ix ix -> Vec ix a -> Vec ix a
pick inds v = case v of
  U t v -> U t $ case t of
    Double -> A.compute @A.U $ A.backpermute' (size inds) (`at` inds) v
    Word   -> A.compute @A.U $ A.backpermute' (size inds) (`at` inds) v
    Int    -> A.compute @A.U $ A.backpermute' (size inds) (`at` inds) v
    Byte   -> A.compute @A.U $ A.backpermute' (size inds) (`at` inds) v
    Bit    -> A.compute @A.U $ A.backpermute' (size inds) (`at` inds) v
  Zip v1 v2 -> Zip (pick inds v1) (pick inds v2)
  -- NB: this leaves the elements arrays alone and just permutes the index
  Maybes is elems ->
    let is' = A.compute @A.U $ A.backpermute' (size inds) (`at` inds) is
    in Maybes is' elems
  Eithers is lefts rights ->
    let is' = A.compute @A.U $ A.backpermute' (size inds) (`at` inds) is
    in Eithers is' lefts rights

_1 :: Vec ix (a,b) -> Vec ix a
_1 (Zip a _) = a
_1 _ = error "impossible"

_2 :: Vec ix (a,b) -> Vec ix b
_2 (Zip _ b) = b
_2 _ = error "impossible"

instance (A.Unbox ix, A.Index ix, Ord ix) => Eq (Vec ix a) where
  v1 == v2 = compare v1 v2 == EQ

instance (A.Unbox ix, A.Index ix, Ord ix) => Ord (Vec ix a) where
  v1 `compare` v2 =
    case dropWhile (== EQ) [ compareAt i j v1 v2 | (i,j) <- indices v1 `P.zip` indices v2 ] of
      [] -> size v1 `compare` size v2
      hd : _ -> hd

instance Vectorizable Int where fromList vs = U Int (A.fromList A.Seq vs)
instance Vectorizable Double where fromList vs = U Double (A.fromList A.Seq vs)
instance Vectorizable Word where fromList vs = U Word (A.fromList A.Seq vs)
instance Vectorizable Word8 where fromList vs = U Byte (A.fromList A.Seq vs)
instance Vectorizable B.Bit where fromList vs = U Bit (A.fromList A.Seq vs)
instance (Vectorizable a, Vectorizable b) => Vectorizable (a,b) where
  fromList tups = let (as, bs) = unzip tups in Zip (fromList as) (fromList bs)
instance (Vectorizable a) => Vectorizable (Maybe a) where
  fromList mays = Maybes ind elems where
    ind = A.fromList A.Seq (go 0 mays)
    elems = fromList [ a | Just a <- mays ]
    go !_ [] = []
    go !i (Nothing : tl) = (-1,False) : go i tl
    go !i (Just _  : tl)  = (i,True) : go (i+1) tl
instance (Vectorizable a, Vectorizable b) => Vectorizable (Either a b) where
  fromList eithers = Eithers ind lefts rights where
    ind = A.fromList A.Seq (go 0 0 eithers)
    lefts = fromList [ a | Left a <- eithers ]
    rights = fromList [ b | Right b <- eithers ]
    go !_ !_ [] = []
    go !i !j (Left _  : tl) = (i,False) : go (i+1) j tl
    go !i !j (Right _ : tl) = (j,True)  : go i (j+1) tl
