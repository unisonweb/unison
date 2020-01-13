
module Unison.Runtime.Stack where

import Control.Monad.Primitive
import Data.Primitive.ByteArray
-- import Data.Primitive.PrimArray
import Data.Primitive.Array

import Unison.Runtime.IR2

-- Evaluation stack
data K
  = KE
  | Mark !Int !K -- mark continuation with a prompt
  | Push !Int -- frame size
         !IR  -- code
         !K

data Closure
  = PAp                !IR   -- code
        {-# unpack #-} !USeg -- unboxed args
  | Bx !Int
  | Boxed !Int !USeg !BSeg
  | Captured !K {-# unpack #-} !USeg {-# unpack #-} !BSeg

type Off = Int
type SZ = Int
type FP = Int

data UStk
  = US { ufp  :: !Int -- frame pointer
       , usp  :: !Int -- stack pointer
       , ustk :: {-# unpack #-} !(MutableByteArray (PrimState IO))
       }
type USeg = ByteArray

ualloc :: IO UStk
ualloc = US (-1) (-1) <$> newByteArray 4096
{-# inline ualloc #-}

upeek :: UStk -> IO Int
upeek (US _ sp stk) = readByteArray stk sp
{-# inline upeek #-}

upeekOff :: UStk -> Off -> IO Int
upeekOff (US _ sp stk) i = readByteArray stk (sp-i)
{-# inline upeekOff #-}

upoke :: UStk -> Int -> IO ()
upoke (US _ sp stk) n = writeByteArray stk sp n
{-# inline upoke #-}

upokeOff :: UStk -> Off -> Int -> IO ()
upokeOff (US _ sp stk) i n = writeByteArray stk (sp-i) n
{-# inline upokeOff #-}

ugrab :: UStk -> SZ -> IO (USeg, UStk)
ugrab (US fp sp stk) sze = do
  mut <- newByteArray sz
  copyMutableByteArray mut 0 stk (bp-sz) sz
  seg <- unsafeFreezeByteArray mut
  pure (seg, US fp (sp-sz) stk)
 where
 sz = sze*8
 bp = sp*8
{-# inline ugrab #-}

uensure :: UStk -> SZ -> IO UStk
uensure stki@(US fp sp stk) sze
  | sze <= 0 = pure stki
  | (sp+sze+1)*8 < ssz = pure stki
  | otherwise = do
    stk' <- resizeMutableByteArray stk (ssz+10240)
    pure $ US fp sp stk'
 where
 ssz = sizeofMutableByteArray stk
{-# inline uensure #-}

ubump :: UStk -> IO UStk
ubump (US fp sp stk) = pure $ US fp (sp+1) stk
{-# inline ubump #-}

ubumpn :: UStk -> SZ -> IO UStk
ubumpn (US fp sp stk) n = pure $ US fp (sp+n) stk
{-# inline ubumpn #-}

ufree :: UStk -> SZ -> IO UStk
ufree (US fp sp stk) sz = pure $ US fp (sp-sz) stk
{-# inline ufree #-}

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

uframe :: UStk -> IO (UStk, SZ)
uframe (US fp sp stk) =
  pure (US sp sp stk, sp-fp)
{-# inline uframe #-}

urestore :: UStk -> SZ -> IO UStk
urestore (US fp sp stk) sz =
  pure $ US (fp-sz) sp stk
{-# inline urestore #-}

uargs :: UStk -> Args' -> IO UStk
uargs (US fp sp stk) (Arg1 i) = do
  (x :: Int) <- readByteArray stk (sp-i)
  let sp' = fp+1
  writeByteArray stk sp' x
  pure $ US fp sp' stk
uargs (US fp sp stk) (Arg2 i j) = do
  (x :: Int) <- readByteArray stk (sp-i)
  (y :: Int) <- readByteArray stk (sp-j)
  let sp' = fp+2
  writeByteArray stk sp' x
  writeByteArray stk (sp'-1) y
  pure $ US fp sp' stk
uargs _ (ArgN _) = error "uargs N"
uargs istk@(US fp sp _  ) (ArgR i l)
  | i == 0 && l == sz = pure istk
  | otherwise = error "uargs range"
 where
 sz = sp-fp
{-# inline uargs #-}

data BStk
  = BS { bsp :: !Int
       , bfp :: !Int
       , bstk :: {-# unpack #-} !(MutableArray (PrimState IO) Closure)
       }
type BSeg = Array Closure

sentinel :: a
sentinel = error "bad stack access"

balloc :: IO BStk
balloc = BS (-1) (-1) <$> newArray 512 sentinel
{-# inline balloc #-}

bpeek :: BStk -> IO Closure
bpeek (BS _ sp stk) = readArray stk sp
{-# inline bpeek #-}

bpeekOff :: BStk -> SZ -> IO Closure
bpeekOff (BS _ sp stk) i = readArray stk (sp-i)
{-# inline bpeekOff #-}

bpoke :: BStk -> Closure -> IO ()
bpoke (BS _ sp stk) x = writeArray stk sp x
{-# inline bpoke #-}

bpokeOff :: BStk -> Int -> Closure -> IO ()
bpokeOff (BS _ sp stk) i x = writeArray stk (sp-i) x
{-# inline bpokeOff #-}

bgrab :: BStk -> SZ -> IO (BSeg, BStk)
bgrab (BS fp sp stk) sz = do
  seg <- unsafeFreezeArray =<< cloneMutableArray stk (sp-sz) sz
  pure (seg, BS fp (sp-sz) stk)
{-# inline bgrab #-}

bensure :: BStk -> SZ -> IO BStk
bensure stki@(BS fp sp stk) sz
  | sz <= 0 = pure stki
  | sp+sz+1 < sz = pure stki
  | otherwise = do
    stk' <- newArray (ssz+1280) sentinel
    copyMutableArray stk' 0 stk 0 sp
    pure $ BS fp sp stk'
  where ssz = sizeofMutableArray stk
{-# inline bensure #-}

bbump :: BStk -> IO BStk
bbump (BS fp sp stk) = pure $ BS fp (sp+1) stk
{-# inline bbump #-}

bbumpn :: BStk -> SZ -> IO BStk
bbumpn (BS fp sp stk) n = pure $ BS fp (sp+n) stk
{-# inline bbumpn #-}

bfree :: BStk -> SZ -> IO BStk
bfree (BS fp sp stk) sz = pure $ BS fp (sp-sz) stk
{-# inline bfree #-}

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

bframe :: BStk -> IO (BStk, SZ)
bframe (BS fp sp stk) =
  pure (BS sp sp stk, sp-fp)
{-# inline bframe #-}

bargs :: BStk -> Args' -> IO BStk
bargs _ _ = error "bargs"
{-# inline bargs #-}
