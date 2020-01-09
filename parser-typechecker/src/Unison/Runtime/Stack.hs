
module Unison.Runtime.Stack where

import Control.Monad.Primitive
import Data.Primitive.ByteArray

type Off = Int
type SZ = Int
data UStk = US !Int {-# unpack #-} !(MutableByteArray (PrimState IO))
type USeg = ByteArray

ualloc :: IO UStk
ualloc = US (-1) <$> newByteArray 4096
{-# inline ualloc #-}

upeek :: UStk -> IO Int
upeek (US sp stk) = readByteArray stk sp
{-# inline upeek #-}

upeekOff :: UStk -> Off -> IO Int
upeekOff (US sp stk) i = readByteArray stk (sp-i)
{-# inline upeekOff #-}

upoke :: UStk -> Int -> IO ()
upoke (US sp stk) n = writeByteArray stk sp n
{-# inline upoke #-}

upokeOff :: UStk -> Off -> Int -> IO ()
upokeOff (US sp stk) i n = writeByteArray stk (sp-i) n
{-# inline upokeOff #-}

ugrab :: UStk -> SZ -> IO (USeg, UStk)
ugrab (US sp stk) sze = do
  mut <- newByteArray sz
  copyMutableByteArray mut 0 stk (bp-sz) sz
  seg <- unsafeFreezeByteArray mut
  pure (seg, US (sp-sz) stk)
 where
 sz = sze*8
 bp = sp*8
{-# inline ugrab #-}

uensure :: UStk -> SZ -> IO UStk
uensure stki@(US sp stk) sze
  | (sp+sze+1)*8 < ssz = pure stki
  | otherwise = do
    stk' <- resizeMutableByteArray stk (ssz+10240)
    pure $ US sp stk'
 where
 ssz = sizeofMutableByteArray stk
{-# inline uensure #-}

ubump :: UStk -> IO UStk
ubump (US sp stk) = pure $ US (sp+1) stk
{-# inline ubump #-}

ubumpn :: UStk -> SZ -> IO UStk
ubumpn (US sp stk) n = pure $ US (sp+n) stk
{-# inline ubumpn #-}

ufree :: UStk -> SZ -> IO UStk
ufree (US sp stk) sz = pure $ US (sp-sz) stk
{-# inline ufree #-}
