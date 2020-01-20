{-# language DataKinds #-}
{-# language BangPatterns #-}

module Unison.Runtime.Rt2 where

import Unison.Runtime.Stack

import Unison.Runtime.IR2

type Tag = Int
type Env = Int -> Comb

eval0 :: Env -> IR -> IO ()
eval0 !env !co = do
  ustk <- alloc
  bstk <- alloc
  eval env ustk bstk KE co

eval :: Env -> Stack 'UN -> Stack 'BX -> K -> IR -> IO ()
eval !env !ustk !bstk !k (App up bp r args) =
  resolve env ustk bstk r >>= apply env ustk bstk k up bp args
eval !env !ustk !bstk !k (Reset p nx) =
  eval env ustk bstk (Mark p k) nx
eval !env !ustk !bstk !k (Capture p nx) = do
  (_ , ustk, bstk, _  , k') <- splitCont ustk bstk k p
  -- TODO: boxed stuff; right now this just discards continuations
  eval env ustk bstk k' nx
eval !env !ustk !bstk !k (Let e nx) = do
  (ustk, usz) <- frame ustk
  eval env ustk bstk (Push usz nx k) e
eval !env !ustk !bstk !k (Prim1 op i nx) =
  prim1 env ustk bstk k op i nx
eval !env !ustk !bstk !k (Prim2 op i j nx) =
  prim2 env ustk bstk k op i j nx
eval !env !ustk !bstk !k (Pack t args nx) = do
  (useg, bseg) <- closeArgs ustk bstk unull bnull args
  bstk <- bump bstk
  poke bstk $ Boxed t useg bseg
  eval env ustk bstk k nx
eval !env !ustk !bstk !k (Unpack i nx) = do
  Boxed t useg bseg <- peekOff bstk i
  ustk <- dumpSeg ustk useg
  bstk <- dumpSeg bstk bseg
  ustk <- bump ustk
  poke ustk t
  eval env ustk bstk k nx
eval !env !ustk !bstk !k (Match i br) = do
  t <- peekOff ustk i
  eval env ustk bstk k $ selectBranch t br
eval !env !ustk !bstk !k (Print i nx) = do
  m <- peekOff ustk i
  print m
  eval env ustk bstk k nx
eval !env !ustk !bstk !k (Lit n nx) = do
  ustk <- bump ustk
  poke ustk n
  eval env ustk bstk k nx
eval !env !ustk !bstk !k (Yield u b args) = do
  (ustk, bstk) <- moveArgs ustk bstk u b args
  yield env ustk bstk k

apply
  :: Env -> Stack 'UN -> Stack 'BX -> K
  -> Int -> Int -> Args -> Comb -> IO ()
apply !env !ustk !bstk !k !up !bp !args (Lam ua ba uf bf fun)
  | ua <= uac && ba <= bac = do
    ustk <- ensure ustk uf
    bstk <- ensure bstk bf
    (ustk, bstk) <- moveArgs ustk bstk up bp args
    eval env ustk bstk k fun
  | otherwise = do
    (useg, bseg) <- closeArgs ustk bstk unull bnull args
    useg `seq` bseg `seq` error "TODO"
 where
 uac = fsize ustk + ucount args - up
 bac = fsize bstk + bcount args - bp
{-# inline apply #-}

moveArgs
  :: Stack 'UN -> Stack 'BX
  -> Int -> Int
  -> Args -> IO (Stack 'UN, Stack 'BX)
moveArgs !ustk !bstk !up !bp ZArgs = do
  ustk <- free ustk up
  bstk <- free bstk bp
  pure (ustk, bstk)
moveArgs !ustk !bstk !up !bp (UArg1 i) = do
  ustk <- margs ustk up (Arg1 i)
  bstk <- free bstk bp
  pure (ustk, bstk)
moveArgs !ustk !bstk !up !bp (UArg2 i j) = do
  ustk <- margs ustk up (Arg2 i j)
  bstk <- free bstk bp
  pure (ustk, bstk)
moveArgs !ustk !bstk !up !bp (UArgR i l) = do
  ustk <- margs ustk up (ArgR i l)
  bstk <- free bstk bp
  pure (ustk, bstk)
moveArgs !ustk !bstk !up !bp (BArg1 i) = do
  ustk <- free ustk up
  bstk <- margs bstk bp (Arg1 i)
  pure (ustk, bstk)
moveArgs !ustk !bstk !up !bp (BArg2 i j) = do
  ustk <- free ustk up
  bstk <- margs bstk bp (Arg2 i j)
  pure (ustk, bstk)
moveArgs !ustk !bstk !up !bp (BArgR i l) = do
  ustk <- free ustk up
  bstk <- margs bstk bp (ArgR i l)
  pure (ustk, bstk)
moveArgs !ustk !bstk !up !bp (DArg2 i j) = do
  ustk <- margs ustk up (Arg1 i)
  bstk <- margs bstk bp (Arg1 j)
  pure (ustk, bstk)
moveArgs !ustk !bstk !up !bp (DArgR ui ul bi bl) = do
  ustk <- margs ustk up (ArgR ui ul)
  bstk <- margs bstk bp (ArgR bi bl)
  pure (ustk, bstk)
{-# inline moveArgs #-}

-- Note: although the representation allows it, it is impossible
-- to under-apply one sort of argument while over-applying the
-- other. Thus, it is unnecessary to worry about doing tricks to
-- only grab a certain number of arguments.
closeArgs
  :: Stack 'UN -> Stack 'BX
  -> Seg 'UN -> Seg 'BX
  -> Args -> IO (Seg 'UN, Seg 'BX)
closeArgs !_    !_    !useg !bseg ZArgs = pure (useg, bseg)
closeArgs !ustk !_    !useg !bseg (UArg1 i) = do
  useg <- augSeg ustk useg (Arg1 i)
  pure (useg, bseg)
closeArgs !ustk !_    !useg !bseg (UArg2 i j) = do
  useg <- augSeg ustk useg (Arg2 i j)
  pure (useg, bseg)
closeArgs !ustk !_    !useg !bseg (UArgR i l) = do
  useg <- augSeg ustk useg (ArgR i l)
  pure (useg, bseg)
closeArgs !_    !bstk !useg !bseg (BArg1 i) = do
  bseg <- augSeg bstk bseg (Arg1 i)
  pure (useg, bseg)
closeArgs !_    !bstk !useg !bseg (BArg2 i j) = do
  bseg <- augSeg bstk bseg (Arg2 i j)
  pure (useg, bseg)
closeArgs !_    !bstk !useg !bseg (BArgR i l) = do
  bseg <- augSeg bstk bseg (ArgR i l)
  pure (useg, bseg)
closeArgs !ustk !bstk !useg !bseg (DArg2 i j) = do
  useg <- augSeg ustk useg (Arg1 i)
  bseg <- augSeg bstk bseg (Arg1 j)
  pure (useg, bseg)
closeArgs !ustk !bstk !useg !bseg (DArgR ui ul bi bl) = do
  useg <- augSeg ustk useg (ArgR ui ul)
  bseg <- augSeg bstk bseg (ArgR bi bl)
  pure (useg, bseg)

prim1 :: Env -> Stack 'UN -> Stack 'BX -> K -> Prim1 -> Int -> IR -> IO ()
prim1 !env !ustk !bstk !k Dec !i !nx = do
  m <- peekOff ustk i
  ustk <- bump ustk
  poke ustk (m-1)
  eval env ustk bstk k nx
prim1 !env !ustk !bstk !k Inc !i !nx = do
  m <- peekOff ustk i
  ustk <- bump ustk
  poke ustk (m+1)
  eval env ustk bstk k nx
{-# inline prim1 #-}

prim2 :: Env -> Stack 'UN -> Stack 'BX -> K -> Prim2 -> Int -> Int -> IR -> IO ()
prim2 !env !ustk !bstk !k Add !i !j !nx = do
  m <- peekOff ustk i
  n <- peekOff ustk j
  ustk <- bump ustk
  poke ustk (m+n)
  eval env ustk bstk k nx
prim2 !env !ustk !bstk !k Sub !i !j !nx = do
  m <- peekOff ustk i
  n <- peekOff ustk j
  ustk <- bump ustk
  poke ustk (m-n)
  eval env ustk bstk k nx
{-# inline prim2 #-}

yield :: Env -> Stack 'UN -> Stack 'BX -> K -> IO ()
yield !env !ustk !bstk !k = leap k
 where
 leap (Mark _ k) = leap k
 leap (Push sz nx k) = do
   ustk <- restore ustk sz
   eval env ustk bstk k nx
 leap KE = pure ()
{-# inline yield #-}

selectBranch :: Tag -> Branch -> IR
selectBranch _ (Prod nx) = nx
selectBranch t (Test1 u y n)
  | t == u    = y
  | otherwise = n
selectBranch t (Test2 u cu v cv e)
  | t == u    = cu
  | t == v    = cv
  | otherwise = e
{-# inline selectBranch #-}

splitCont :: Stack 'UN -> Stack 'BX -> K -> Int -> IO (K, Stack 'UN, Stack 'BX, Seg 'UN, K)
splitCont !ustk !bstk !k !p = walk 0 KE k
-- splitCont !ustk !k !p = walk 0 KE k
 where
 walk :: Int -> K -> K -> IO (K, Stack 'UN, Stack 'BX, Seg 'UN, K)
 walk !sz !ck KE = finish sz ck KE
 walk !sz !ck (Mark q k)
   | p == q    = finish sz ck k
   | otherwise = walk sz (Mark q ck) k
 walk !sz !ck (Push n br k) = walk (sz+n) (Push n br ck) k

 finish :: Int -> K -> K -> IO (K, Stack 'UN, Stack 'BX, Seg 'UN, K)
 finish !sz !ck !k = do
   (seg, ustk) <- grab ustk sz
   return (ck, ustk, bstk, seg, k)
{-# inline splitCont #-}

resolve :: Env -> Stack 'UN -> Stack 'BX -> Ref -> IO Comb
resolve env _ _ (Env i) = return (env i)
resolve _ _ bstk (Stk i) = do
  _ <- peekOff bstk i
  error "huh"
-- resolve _   _ _ _ = error "TODO: resolve"
