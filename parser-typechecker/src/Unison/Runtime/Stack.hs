{-# language TypeFamilies #-}
{-# language DataKinds #-}

module Unison.Runtime.Stack where

import Prelude hiding (words)

import Control.Monad.Primitive
import Data.Primitive.ByteArray
import Data.Primitive.PrimArray
import Data.Primitive.Array

import Unison.Runtime.IR2

data Mem = UN | BX

-- Evaluation stack
data K
  = KE
  | Mark !Int !K -- mark continuation with a prompt
  | Push !Int -- frame size
         !IR  -- code
         !K

data Closure
  = PAp                !IR   -- code
        {-# unpack #-} !(Seg 'UN) -- unboxed args
  | Bx !Int
  | Enum !Int
  | DataU1 !Int !Int
  | DataU2 !Int !Int !Int
  | DataB1 !Int !Closure
  | DataB2 !Int !Closure !Closure
  | DataUB !Int !Int !Closure
  | DataG !Int !(Seg 'UN) !(Seg 'BX)
  | Captured !K {-# unpack #-} !(Seg 'UN) !(Seg 'BX)

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
  let loop i
        | i < 0     = return ()
        | otherwise = do
            (x :: Int) <- readByteArray stk (sp-indexPrimArray v i)
            writeByteArray cop (cp-i) x
            loop $ i-1
  loop $ sz-1
  pure cp
 where
 cp = cp0+sz
 sz = sizeofPrimArray v
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
  let loop i
        | i < 0     = return ()
        | otherwise = do
            x <- readArray stk (sp-indexPrimArray v i)
            writeArray cop (cp-i) x
            loop $ i-1
  loop $ sz-1
  pure cp
 where
 cp = cp0+sz
 sz = sizeofPrimArray v
bargOnto stk sp cop cp0 (ArgR i l) = do
  copyMutableArray cop cp0 stk (sp-i-l) l
  pure $ cp0+l

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
  free :: Stack b -> SZ -> IO (Stack b)
  frame :: Stack b -> IO (Stack b, SZ)
  restore :: Stack b -> SZ -> IO (Stack b)
  margs :: Stack b -> Int -> Args' -> IO (Stack b)
  augSeg :: Stack b -> Seg b -> Args' -> IO (Seg b)
  dumpSeg :: Stack b -> Seg b -> IO (Stack b)
  fsize :: Stack b -> SZ

instance MEM 'UN where
  data Stack 'UN
    = US { ufp  :: !Int -- frame pointer
         , usp  :: !Int -- stack pointer
         , ustk :: {-# unpack #-} !(MutableByteArray (PrimState IO))
         }
  type Elem 'UN = Int
  type Seg 'UN = ByteArray
  alloc = US (-1) (-1) <$> newByteArray 4096
  {-# inline alloc #-}
  peek (US _ sp stk) = readByteArray stk sp
  {-# inline peek #-}
  peekOff (US _ sp stk) i = readByteArray stk (sp-i)
  {-# inline peekOff #-}
  poke (US _ sp stk) n = writeByteArray stk sp n
  {-# inline poke #-}
  pokeOff (US _ sp stk) i n = writeByteArray stk (sp-i) n
  {-# inline pokeOff #-}

  grab (US fp sp stk) sze = do
    mut <- newByteArray sz
    copyMutableByteArray mut 0 stk (bp-sz) sz
    seg <- unsafeFreezeByteArray mut
    pure (seg, US fp (sp-sz) stk)
   where
   sz = bytes sze
   bp = bytes sp
  {-# inline grab #-}

  ensure stki@(US fp sp stk) sze
    | sze <= 0
    || bytes (sp+sze+1) < ssz = pure stki
    | otherwise = do
      stk' <- resizeMutableByteArray stk (ssz+10240)
      pure $ US fp sp stk'
   where
   ssz = sizeofMutableByteArray stk
  {-# inline ensure #-}

  bump (US fp sp stk) = pure $ US fp (sp+1) stk
  {-# inline bump #-}

  bumpn (US fp sp stk) n = pure $ US fp (sp+n) stk
  {-# inline bumpn #-}

  free (US fp sp stk) sz = pure $ US fp (sp-sz) stk
  {-# inline free #-}

-- ureturn :: UStk -> SZ -> Args -> IO UStk
-- ureturn (US fp _  stk) sz Arg0 = pure $ US (fp-sz) fp stk
-- ureturn (US fp sp stk) sz (Arg1 i) = do
--   (x :: Int) <- readByteArray stk (sp-i)
--   let sp' = fp+1
--   writeByteArray stk sp' x
--   pure $ US (fp-sz) sp' stk
-- ureturn (US fp sp stk) sz (Arg2 i j) = do
--   (x :: Int) <- readByteArray stk (sp-i)
--   (y :: Int) <- readByteArray stk (sp-j)
--   let sp' = fp+2
--   writeByteArray stk sp' x
--   writeByteArray stk (sp'-1) y
--   pure $ US (fp-sz) sp' stk
-- ureturn (US fp _  stk) sz (ArgN v) = do
--   let sp' = fp + sizeofPrimArray v
--   pure $ US (fp-sz) sp' stk
-- {-# inline ureturn #-}

  frame (US fp sp stk) =
    pure (US sp sp stk, sp-fp)
  {-# inline frame #-}

  restore (US fp sp stk) sz =
    pure $ US (fp-sz) sp stk
  {-# inline restore #-}

  margs istk pop (ArgR i l)
    | i == 0
   && pop == l = pure istk
  margs (US fp sp stk) pop args = do
    sp <- uargOnto stk sp stk (sp-pop) args
    pure $ US fp sp stk
  {-# inline margs #-}

  augSeg (US _ sp stk) seg args = do
    cop <- newByteArray $ ssz+asz
    copyByteArray cop 0 seg 0 ssz
    _ <- uargOnto stk sp cop (sz-1) args
    unsafeFreezeByteArray cop
   where
   ssz = sizeofByteArray seg
   sz = words ssz
   asz = case args of
          Arg1 _   -> 8
          Arg2 _ _ -> 16
          ArgN v   -> bytes $ sizeofPrimArray v
          ArgR _ l -> bytes l
  {-# inline augSeg #-}

  dumpSeg (US fp sp stk) seg = do
    copyByteArray stk bsp seg 0 ssz
    pure $ US fp (sp+sz) stk
   where
   bsp = bytes $ sp+1
   ssz = sizeofByteArray seg
   sz = words ssz
  {-# inline dumpSeg #-}

  fsize (US fp sp _) = sp-fp
  {-# inline fsize #-}

unull :: Seg 'UN
unull = byteArrayFromListN 0 ([] :: [Int])

bnull :: Seg 'BX
bnull = fromListN 0 []

sentinel :: a
sentinel = error "bad stack access"

instance MEM 'BX where
  data Stack 'BX
    = BS { bfp :: !Int
         , bsp :: !Int
         , bstk :: {-# unpack #-} !(MutableArray (PrimState IO) Closure)
         }
  type Elem 'BX = Closure
  type Seg 'BX = Array Closure

  alloc = BS (-1) (-1) <$> newArray 512 sentinel
  {-# inline alloc #-}

  peek (BS _ sp stk) = readArray stk sp
  {-# inline peek #-}

  peekOff (BS _ sp stk) i = readArray stk (sp-i)
  {-# inline peekOff #-}

  poke (BS _ sp stk) x = writeArray stk sp x
  {-# inline poke #-}

  pokeOff (BS _ sp stk) i x = writeArray stk (sp-i) x
  {-# inline pokeOff #-}

  grab (BS fp sp stk) sz = do
    seg <- unsafeFreezeArray =<< cloneMutableArray stk (sp-sz) sz
    pure (seg, BS fp (sp-sz) stk)
  {-# inline grab #-}

  ensure stki@(BS fp sp stk) sz
    | sz <= 0 = pure stki
    | sp+sz+1 < ssz = pure stki
    | otherwise = do
      stk' <- newArray (ssz+1280) sentinel
      copyMutableArray stk' 0 stk 0 (sp+1)
      pure $ BS fp sp stk'
    where ssz = sizeofMutableArray stk
  {-# inline ensure #-}

  bump (BS fp sp stk) = pure $ BS fp (sp+1) stk
  {-# inline bump #-}

  bumpn (BS fp sp stk) n = pure $ BS fp (sp+n) stk
  {-# inline bumpn #-}

  free (BS fp sp stk) sz = pure $ BS fp (sp-sz) stk
  {-# inline free #-}

-- breturn :: BStk -> SZ -> Args -> IO BStk
-- breturn (BS fp _  stk) sz Arg0 = pure $ BS (fp-sz) fp stk
-- breturn (BS fp sp stk) sz (Arg1 i) = do
--   x <- readArray stk (sp-i)
--   let sp' = fp+1
--   writeArray stk sp' x
--   pure $ BS (fp-sz) sp' stk
-- breturn (BS fp sp stk) sz (Arg2 i j) = do
--   x <- readArray stk (sp-i)
--   y <- readArray stk (sp-j)
--   let sp' = fp+2
--   writeArray stk sp' x
--   writeArray stk (sp'-1) y
--   pure $ BS (fp-sz) sp' stk
-- breturn (BS fp _  stk) sz (ArgN v) = do
--   let sp' = fp + sizeofPrimArray v
--   pure $ BS (fp-sz) sp' stk
-- {-# inline breturn #-}

  frame (BS fp sp stk) =
    pure (BS sp sp stk, sp-fp)
  {-# inline frame #-}

  restore (BS fp sp stk) sz =
    pure $ BS (fp-sz) sp stk
  {-# inline restore #-}

  margs istk pop (ArgR i l)
    | i == 0
   && pop == l = pure istk
  margs (BS fp sp stk) pop args = do
    sp <- bargOnto stk sp stk (sp-pop) args
    pure $ BS fp sp stk
  {-# inline margs #-}

  augSeg (BS _ sp stk) seg args = do
    cop <- newArray (ssz+asz) sentinel
    copyArray cop 0 seg 0 ssz
    _ <- bargOnto stk sp cop (ssz-1) args
    unsafeFreezeArray cop
   where
   ssz = sizeofArray seg
   asz = case args of
          Arg1 _   -> 1
          Arg2 _ _ -> 2
          ArgN v   -> sizeofPrimArray v
          ArgR _ l -> l
  {-# inline augSeg #-}

  dumpSeg (BS fp sp stk) seg = do
    copyArray stk (sp+1) seg 0 sz
    pure $ BS fp (sp+sz) stk
   where
   sz = sizeofArray seg
  {-# inline dumpSeg #-}

  fsize (BS fp sp _) = sp-fp
  {-# inline fsize #-}
