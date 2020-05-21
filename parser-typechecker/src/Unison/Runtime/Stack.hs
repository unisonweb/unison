{-# language GADTs #-}
{-# language DataKinds #-}
{-# language BangPatterns #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}
{-# language PatternGuards #-}
{-# language PatternSynonyms #-}

module Unison.Runtime.Stack where

import Prelude hiding (words)

import Control.Monad (when)
import Control.Monad.Primitive

import Data.Foldable (toList)
import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
import Data.Primitive.Array
import Data.Word

import Unison.Runtime.ANF (Mem(..), unpackTags)
import Unison.Runtime.Foreign
import Unison.Runtime.MCode

import Unison.Util.WordContainers as WC

-- Evaluation stack
data K
  = KE
  -- mark continuation with a prompt
  | Mark !WordSet
         !(WordMap Closure)
         !K
  -- save information about a frame for later resumption
  | Push !Int -- unboxed frame size
         !Int -- boxed frame size
         !Int -- pending unboxed args
         !Int -- pending boxed args
         !Section -- code
         !K
  deriving (Eq, Ord)

-- Comb with an identifier
data IComb
  = IC !Word64 !Comb
  deriving (Show)

instance Eq IComb where
  IC i _ == IC j _ = i == j

pattern Lam_ ua ba uf bf entry <- IC _ (Lam ua ba uf bf entry)

-- TODO: more reliable ordering for combinators
instance Ord IComb where
  compare (IC i _) (IC j _) = compare i j

data Closure
  = PAp {-# unpack #-} !IComb     -- code
        {-# unpack #-} !(Seg 'UN) -- unboxed args
        {-  unpack  -} !(Seg 'BX) -- boxed args
  | Enum !Word64
  | DataU1 !Word64 !Int
  | DataU2 !Word64 !Int !Int
  | DataB1 !Word64 !Closure
  | DataB2 !Word64 !Closure !Closure
  | DataUB !Word64 !Int !Closure
  | DataG !Word64 !(Seg 'UN) !(Seg 'BX)
  | Captured !K {-# unpack #-} !(Seg 'UN) !(Seg 'BX)
  | Foreign !Foreign
  | BlackHole
  deriving (Show, Eq, Ord)

splitData :: Closure -> Maybe (Word64, [Int], [Closure])
splitData (Enum t) = Just (t, [], [])
splitData (DataU1 t i) = Just (t, [i], [])
splitData (DataU2 t i j) = Just (t, [i,j], [])
splitData (DataB1 t x) = Just (t, [], [x])
splitData (DataB2 t x y) = Just (t, [], [x,y])
splitData (DataUB t i y) = Just (t, [i], [y])
splitData (DataG t us bs) = Just (t, ints us, toList bs)
splitData _ = Nothing

ints :: ByteArray -> [Int]
ints ba = fmap (indexByteArray ba) [0..n]
  where
  n = sizeofByteArray ba `div` 8

pattern DataC rt ct us bs <-
  (splitData -> Just (unpackTags -> (rt, ct), us, bs))

pattern PApV ic us bs <- PAp ic (ints -> us) (toList -> bs)

{-# complete DataC, PAp, Captured, Foreign, BlackHole #-}
{-# complete DataC, PApV, Captured, Foreign, BlackHole #-}

marshalToForeign :: Closure -> Foreign
marshalToForeign (Foreign x) = x
marshalToForeign _ = error "marshalToForeign: unhandled closure"

type Off = Int
type SZ = Int
type FP = Int

type UA = MutableByteArray (PrimState IO)
type BA = MutableArray (PrimState IO) Closure

words :: Int -> Int
words n = n `div` 8

bytes :: Int -> Int
bytes n = n * 8

uargOnto :: UA -> Off -> UA -> Off -> Args' -> IO Int
uargOnto stk sp cop cp0 (Arg1 i) = do
  (x :: Int) <- readByteArray stk (sp-i)
  writeByteArray cop cp x
  pure cp
 where cp = cp0+1
uargOnto stk sp cop cp0 (Arg2 i j) = do
  (x :: Int) <- readByteArray stk (sp-i)
  (y :: Int) <- readByteArray stk (sp-j)
  writeByteArray cop cp x
  writeByteArray cop (cp-1) y
  pure cp
 where cp = cp0+2
uargOnto stk sp cop cp0 (ArgN v) = do
  buf <- if overwrite
         then newByteArray $ bytes sz
         else pure cop
  let loop i
        | i < 0     = return ()
        | otherwise = do
            (x :: Int) <- readByteArray stk (sp-indexPrimArray v i)
            writeByteArray buf (sz-1-i) x
            loop $ i-1
  loop $ sz-1
  when overwrite $
    copyMutableByteArray cop (bytes $ cp+1) buf 0 (bytes sz)
  pure cp
 where
 cp = cp0+sz
 sz = sizeofPrimArray v
 overwrite = sameMutableByteArray stk cop
uargOnto stk sp cop cp0 (ArgR i l) = do
  moveByteArray cop cbp stk sbp (bytes l)
  pure $ cp0+l
 where
 cbp = bytes cp0
 sbp = bytes $ sp-i-l

bargOnto :: BA -> Off -> BA -> Off -> Args' -> IO Int
bargOnto stk sp cop cp0 (Arg1 i) = do
  x <- readArray stk (sp-i)
  writeArray cop cp x
  pure cp
 where cp = cp0+1
bargOnto stk sp cop cp0 (Arg2 i j) = do
  x <- readArray stk (sp-i)
  y <- readArray stk (sp-j)
  writeArray cop cp x
  writeArray cop (cp-1) y
  pure cp
 where cp = cp0+2
bargOnto stk sp cop cp0 (ArgN v) = do
  buf <- if overwrite
         then newArray sz undefined
         else pure cop
  let loop i
        | i < 0     = return ()
        | otherwise = do
            x <- readArray stk $ sp-indexPrimArray v i
            writeArray buf (sz-1-i) x
            loop $ i-1
  loop $ sz-1
  when overwrite $
    copyMutableArray cop (cp0+1) buf 0 sz
  pure cp
 where
 cp = cp0+sz
 sz = sizeofPrimArray v
 overwrite = stk == cop
bargOnto stk sp cop cp0 (ArgR i l) = do
  copyMutableArray cop cp0 stk (sp-i-l) l
  pure $ cp0+l

data Dump = A | F Int | S

dumpAP :: Int -> Int -> Int -> Dump -> Int
dumpAP _  fp sz d@(F _) = dumpFP fp sz d
dumpAP ap _  _  _     = ap

dumpFP :: Int -> Int -> Dump -> Int
dumpFP fp _  S = fp
dumpFP fp sz A = fp+sz
dumpFP fp sz (F n) = fp+sz-n

class MEM (b :: Mem) where
  data Stack b :: *
  type Elem b :: *
  type Seg b :: *
  alloc :: IO (Stack b)
  peek :: Stack b -> IO (Elem b)
  peekOff :: Stack b -> Off -> IO (Elem b)
  poke :: Stack b -> Elem b -> IO ()
  pokeOff :: Stack b -> Off -> Elem b -> IO ()
  grab :: Stack b -> SZ -> IO (Seg b, Stack b)
  ensure :: Stack b -> SZ -> IO (Stack b)
  bump :: Stack b -> IO (Stack b)
  bumpn :: Stack b -> SZ -> IO (Stack b)
  duplicate :: Stack b -> IO (Stack b)
  discardFrame :: Stack b -> IO (Stack b)
  saveFrame :: Stack b -> IO (Stack b, SZ, SZ)
  restoreFrame :: Stack b -> SZ -> SZ -> IO (Stack b)
  prepareArgs :: Stack b -> Args' -> IO (Stack b)
  acceptArgs :: Stack b -> Int -> IO (Stack b)
  frameArgs :: Stack b -> IO (Stack b)
  augSeg :: Stack b -> Seg b -> Args' -> IO (Seg b)
  dumpSeg :: Stack b -> Seg b -> Dump -> IO (Stack b)
  fsize :: Stack b -> SZ
  asize :: Stack b -> SZ

instance MEM 'UN where
  data Stack 'UN
    -- Note: uap <= ufp <= usp
    = US { uap  :: !Int -- arg pointer
         , ufp  :: !Int -- frame pointer
         , usp  :: !Int -- stack pointer
         , ustk :: {-# unpack #-} !(MutableByteArray (PrimState IO))
         }
  type Elem 'UN = Int
  type Seg 'UN = ByteArray
  alloc = US (-1) (-1) (-1) <$> newByteArray 4096
  {-# inline alloc #-}
  peek (US _ _ sp stk) = readByteArray stk sp
  {-# inline peek #-}
  peekOff (US _ _ sp stk) i = readByteArray stk (sp-i)
  {-# inline peekOff #-}
  poke (US _ _ sp stk) n = writeByteArray stk sp n
  {-# inline poke #-}
  pokeOff (US _ _ sp stk) i n = writeByteArray stk (sp-i) n
  {-# inline pokeOff #-}

  -- Eats up arguments
  grab (US _ fp sp stk) sze = do
    mut <- newByteArray sz
    copyMutableByteArray mut 0 stk (bfp-sz) sz
    seg <- unsafeFreezeByteArray mut
    moveByteArray stk (bfp-sz) stk bfp fsz
    pure (seg, US (fp-sze) (fp-sze) (sp-sze) stk)
   where
   sz = bytes sze
   bfp = bytes $ fp+1
   fsz = bytes $ sp-fp
  {-# inline grab #-}

  ensure stki@(US ap fp sp stk) sze
    | sze <= 0
    || bytes (sp+sze+1) < ssz = pure stki
    | otherwise = do
      stk' <- resizeMutableByteArray stk (ssz+10240)
      pure $ US ap fp sp stk'
   where
   ssz = sizeofMutableByteArray stk
  {-# inline ensure #-}

  bump (US ap fp sp stk) = pure $ US ap fp (sp+1) stk
  {-# inline bump #-}

  bumpn (US ap fp sp stk) n = pure $ US ap fp (sp+n) stk
  {-# inline bumpn #-}

  duplicate (US ap fp sp stk)
    = US ap fp sp <$> do
        b <- newByteArray sz
        copyMutableByteArray b 0 stk 0 sz
        pure b
    where
    sz = sizeofMutableByteArray stk
  {-# inline duplicate #-}

  discardFrame (US ap fp _ stk) = pure $ US ap fp fp stk
  {-# inline discardFrame #-}

  saveFrame (US ap fp sp stk) = pure (US sp sp sp stk, sp-fp, fp-ap)
  {-# inline saveFrame #-}

  restoreFrame (US _ fp0 sp stk) fsz asz = pure $ US ap fp sp stk
   where fp = fp0-fsz
         ap = fp-asz
  {-# inline restoreFrame #-}

  prepareArgs (US ap fp sp stk) (ArgR i l)
    | fp+l+i == sp = pure $ US ap (sp-i) (sp-i) stk
  prepareArgs (US ap fp sp stk) args = do
    sp <- uargOnto stk sp stk fp args
    pure $ US ap sp sp stk
  {-# inline prepareArgs #-}

  acceptArgs (US ap fp sp stk) n = pure $ US ap (fp-n) sp stk
  {-# inline acceptArgs #-}

  frameArgs (US ap _ sp stk) = pure $ US ap ap sp stk
  {-# inline frameArgs #-}

  augSeg (US _ _ sp stk) seg args = do
    cop <- newByteArray $ ssz+asz
    copyByteArray cop asz seg 0 ssz
    _ <- uargOnto stk sp cop (-1) args
    unsafeFreezeByteArray cop
   where
   ssz = sizeofByteArray seg
   asz = case args of
          Arg1 _   -> 8
          Arg2 _ _ -> 16
          ArgN v   -> bytes $ sizeofPrimArray v
          ArgR _ l -> bytes l
  {-# inline augSeg #-}

  dumpSeg (US ap fp sp stk) seg mode = do
    copyByteArray stk bsp seg 0 ssz
    pure $ US ap' fp' sp' stk
   where
   bsp = bytes $ sp+1
   ssz = sizeofByteArray seg
   sz = words ssz
   sp' = sp+sz
   fp' = dumpFP fp sz mode
   ap' = dumpAP ap fp sz mode
  {-# inline dumpSeg #-}

  fsize (US _ fp sp _) = sp-fp
  {-# inline fsize #-}

  asize (US ap fp _ _) = fp-ap
  {-# inline asize #-}

peekN :: Stack 'UN -> IO Word64
peekN (US _ _ sp stk) = readByteArray stk sp
{-# inline peekN #-}

peekOffN :: Stack 'UN -> Int -> IO Word64
peekOffN (US _ _ sp stk) i = readByteArray stk (sp-i)
{-# inline peekOffN #-}

pokeN :: Stack 'UN -> Word64 -> IO ()
pokeN (US _ _ sp stk) n = writeByteArray stk sp n
{-# inline pokeN #-}

pokeOffN :: Stack 'UN -> Int -> Word64 -> IO ()
pokeOffN (US _ _ sp stk) i n = writeByteArray stk (sp-i) n
{-# inline pokeOffN #-}

unull :: Seg 'UN
unull = byteArrayFromListN 0 ([] :: [Int])

bnull :: Seg 'BX
bnull = fromListN 0 []

sentinel :: a
sentinel = error "bad stack access"

instance Show (Stack 'BX) where
  show (BS ap fp sp _)
    = "BS " ++ show ap ++ " " ++ show fp ++ " " ++ show sp
instance Show (Stack 'UN) where
  show (US ap fp sp _)
    = "US " ++ show ap ++ " " ++ show fp ++ " " ++ show sp
instance Show K where
  show k = "[" ++ go "" k
    where
    go _ KE = "]"
    go com (Push uf bf ua ba _ k)
      = com ++ show (uf,bf,ua,ba) ++ go "," k
    go com (Mark ps _ k) = com ++ "M" ++ show ps ++ go "," k

instance MEM 'BX where
  data Stack 'BX
    = BS { bap :: !Int
         , bfp :: !Int
         , bsp :: !Int
         , bstk :: {-# unpack #-} !(MutableArray (PrimState IO) Closure)
         }
  type Elem 'BX = Closure
  type Seg 'BX = Array Closure

  alloc = BS (-1) (-1) (-1) <$> newArray 512 sentinel
  {-# inline alloc #-}

  peek (BS _ _ sp stk) = readArray stk sp
  {-# inline peek #-}

  peekOff (BS _ _ sp stk) i = readArray stk (sp-i)
  {-# inline peekOff #-}

  poke (BS _ _ sp stk) x = writeArray stk sp x
  {-# inline poke #-}

  pokeOff (BS _ _ sp stk) i x = writeArray stk (sp-i) x
  {-# inline pokeOff #-}

  grab (BS _ fp sp stk) sz = do
    seg <- unsafeFreezeArray =<< cloneMutableArray stk (fp+1-sz) sz
    copyMutableArray stk (fp+1-sz) stk (fp+1) fsz
    pure (seg, BS (fp-sz) (fp-sz) (sp-sz) stk)
   where fsz = sp-fp
  {-# inline grab #-}

  ensure stki@(BS ap fp sp stk) sz
    | sz <= 0 = pure stki
    | sp+sz+1 < ssz = pure stki
    | otherwise = do
      stk' <- newArray (ssz+1280) sentinel
      copyMutableArray stk' 0 stk 0 (sp+1)
      pure $ BS ap fp sp stk'
    where ssz = sizeofMutableArray stk
  {-# inline ensure #-}

  bump (BS ap fp sp stk) = pure $ BS ap fp (sp+1) stk
  {-# inline bump #-}

  bumpn (BS ap fp sp stk) n = pure $ BS ap fp (sp+n) stk
  {-# inline bumpn #-}

  duplicate (BS ap fp sp stk)
    = BS ap fp sp <$> cloneMutableArray stk 0 (sizeofMutableArray stk)
  {-# inline duplicate #-}

  discardFrame (BS ap fp _ stk) = pure $ BS ap fp fp stk
  {-# inline discardFrame #-}

  saveFrame (BS ap fp sp stk) = pure (BS sp sp sp stk, sp-fp, fp-ap)
  {-# inline saveFrame #-}

  restoreFrame (BS _ fp0 sp stk) fsz asz = pure $ BS ap fp sp stk
   where
   fp = fp0-fsz
   ap = fp-asz
  {-# inline restoreFrame #-}

  prepareArgs (BS ap fp sp stk) (ArgR i l)
    | fp+l+i == sp = pure $ BS ap sp sp stk
  prepareArgs (BS ap fp sp stk) args = do
    sp <- bargOnto stk sp stk fp args
    pure $ BS ap sp sp stk
  {-# inline prepareArgs #-}

  acceptArgs (BS ap fp sp stk) n = pure $ BS ap (fp-n) sp stk
  {-# inline acceptArgs #-}

  frameArgs (BS ap _ sp stk) = pure $ BS ap ap sp stk
  {-# inline frameArgs #-}

  augSeg (BS _ _ sp stk) seg args = do
    cop <- newArray (ssz+asz) sentinel
    copyArray cop asz seg 0 ssz
    _ <- bargOnto stk sp cop (-1) args
    unsafeFreezeArray cop
   where
   ssz = sizeofArray seg
   asz = case args of
          Arg1 _   -> 1
          Arg2 _ _ -> 2
          ArgN v   -> sizeofPrimArray v
          ArgR _ l -> l
  {-# inline augSeg #-}

  dumpSeg (BS ap fp sp stk) seg mode = do
    copyArray stk (sp+1) seg 0 sz
    pure $ BS ap' fp' sp' stk
   where
   sz = sizeofArray seg
   sp' = sp+sz
   fp' = dumpFP fp sz mode
   ap' = dumpAP ap fp sz mode
  {-# inline dumpSeg #-}

  fsize (BS _ fp sp _) = sp-fp
  {-# inline fsize #-}

  asize (BS ap fp _ _) = fp-ap

uscount :: Seg 'UN -> Int
uscount seg = words $ sizeofByteArray seg

bscount :: Seg 'BX -> Int
bscount seg = sizeofArray seg

