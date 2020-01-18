{-# language TypeFamilies #-}
{-# language DataKinds #-}

module Unison.Runtime.Stack where

import Control.Monad.Primitive
import Data.Primitive.ByteArray
-- import Data.Primitive.PrimArray
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
  | Boxed !Int !(Seg 'UN) !(Seg 'BX)
  | Captured !K {-# unpack #-} !(Seg 'UN) !(Seg 'BX)

type Off = Int
type SZ = Int
type FP = Int

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
  margs :: Stack b -> Args' -> IO (Stack b)
  avail :: Stack b -> SZ

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
   sz = sze*8
   bp = sp*8
  {-# inline grab #-}

  ensure stki@(US fp sp stk) sze
    | sze <= 0 = pure stki
    | (sp+sze+1)*8 < ssz = pure stki
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

  margs (US fp sp stk) (Arg1 i) = do
    (x :: Int) <- readByteArray stk (sp-i)
    let sp' = fp+1
    writeByteArray stk sp' x
    pure $ US fp sp' stk
  margs (US fp sp stk) (Arg2 i j) = do
    (x :: Int) <- readByteArray stk (sp-i)
    (y :: Int) <- readByteArray stk (sp-j)
    let sp' = fp+2
    writeByteArray stk sp' x
    writeByteArray stk (sp'-1) y
    pure $ US fp sp' stk
  margs _ (ArgN _) = error "uargs N"
  margs istk@(US fp sp _  ) (ArgR i l)
    | i == 0 && l == sz = pure istk
    | otherwise = error "uargs range"
   where
   sz = sp-fp
  {-# inline margs #-}

  avail (US fp sp _) = sp-fp
  {-# inline avail #-}

sentinel :: a
sentinel = error "bad stack access"

instance MEM 'BX where
  data Stack 'BX
    = BS { bsp :: !Int
         , bfp :: !Int
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
    | sp+sz+1 < sz = pure stki
    | otherwise = do
      stk' <- newArray (ssz+1280) sentinel
      copyMutableArray stk' 0 stk 0 sp
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

  margs _ _ = error "bargs"
  {-# inline margs #-}

  avail (BS fp sp _) = sp-fp
  {-# inline avail #-}
