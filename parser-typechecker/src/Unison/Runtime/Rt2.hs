{-# language DataKinds #-}
{-# language BangPatterns #-}

module Unison.Runtime.Rt2 where

import Unison.Runtime.Stack

import Unison.Runtime.IR2

type Tag = Int
type Env = Int -> Comb

info :: Show a => String -> a -> IO ()
info ctx x = infos ctx (show x)
infos :: String -> String -> IO ()
infos ctx s = putStrLn $ ctx ++ ": " ++ s

eval0 :: Env -> IR -> IO ()
eval0 !env !co = do
  ustk <- alloc
  bstk <- alloc
  eval env ustk bstk KE co

eval :: Env -> Stack 'UN -> Stack 'BX -> K -> IR -> IO ()
eval !env !ustk !bstk !k (Info tx nx) = do
  info tx ustk
  info tx bstk
  info tx k
  eval env ustk bstk k nx
eval !env !ustk !bstk !k (App r args) =
  resolve env ustk bstk r >>= apply env ustk bstk k args
eval !env !ustk !bstk !k (Jump i args) =
  peekOff bstk i >>= jump env ustk bstk k args
eval !env !ustk !bstk !k (Reset p nx) =
  eval env ustk bstk (Mark p k) nx
eval !env !ustk !bstk !k (Capture p nx) = do
  (sk , ustk, bstk, useg, bseg, k') <- splitCont ustk bstk k p
  bstk <- bump bstk
  poke bstk $ Captured sk useg bseg
  eval env ustk bstk k' nx
eval !env !ustk !bstk !k (Let e nx) = do
  (ustk, ufsz, uasz) <- saveFrame ustk
  (bstk, bfsz, basz) <- saveFrame bstk
  eval env ustk bstk (Push ufsz bfsz uasz basz nx k) e
eval !env !ustk !bstk !k (Prim1 op i nx) =
  prim1 env ustk bstk k op i nx
eval !env !ustk !bstk !k (Prim2 op i j nx) =
  prim2 env ustk bstk k op i j nx
eval !env !ustk !bstk !k (Pack t args nx) = do
  clo <- buildData ustk bstk t args
  bstk <- bump bstk
  poke bstk clo
  eval env ustk bstk k nx
eval !env !ustk !bstk !k (Unpack i nx) = do
  (ustk, bstk) <- dumpData ustk bstk =<< peekOff bstk i
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
eval !env !ustk !bstk !k (Yield args) = do
  (ustk, bstk) <- moveArgs ustk bstk args
  ustk <- frameArgs ustk
  bstk <- frameArgs bstk
  yield env ustk bstk k

apply
  :: Env -> Stack 'UN -> Stack 'BX -> K
  -> Args -> Closure -> IO ()
apply !env !ustk !bstk !k !args
      (PAp comb@(Lam ua ba uf bf fun) useg bseg)
  | ua <= uac && ba <= bac = do
    ustk <- ensure ustk uf
    bstk <- ensure bstk bf
    (ustk, bstk) <- moveArgs ustk bstk args
    ustk <- dumpSeg ustk useg A
    bstk <- dumpSeg bstk bseg A
    ustk <- acceptArgs ustk ua
    bstk <- acceptArgs bstk ba
    eval env ustk bstk k fun
  | otherwise = do
    (useg, bseg) <- closeArgs ustk bstk useg bseg args
    ustk <- discardFrame ustk
    bstk <- discardFrame bstk
    bstk <- bump bstk
    poke bstk $ PAp comb useg bseg
    yield env ustk bstk k
 where
 uac = asize ustk + ucount args + uscount useg
 bac = asize bstk + bcount args + bscount bseg
apply _ _ _ _ _ _ = error "applying non-function"
{-# inline apply #-}

jump
  :: Env -> Stack 'UN -> Stack 'BX -> K
  -> Args -> Closure -> IO ()
jump !env !ustk !bstk !k !args (Captured sk useg bseg) = do
  (useg, bseg) <- closeArgs ustk bstk useg bseg args
  ustk <- discardFrame ustk
  bstk <- discardFrame bstk
  ustk <- dumpSeg ustk useg . F $ ucount args
  bstk <- dumpSeg bstk bseg . F $ bcount args
  yield env ustk bstk $ repush sk k
 where
 repush KE k = k
 repush (Mark p sk) k = repush sk $ Mark p k
 repush (Push un bn ua ba nx sk) k = repush sk $ Push un bn ua ba nx k
jump !_ !_ !_ !_ !_ !_ = error "jump: non-cont"
{-# inline jump #-}

moveArgs
  :: Stack 'UN -> Stack 'BX
  -> Args -> IO (Stack 'UN, Stack 'BX)
moveArgs !ustk !bstk ZArgs = do
  ustk <- discardFrame ustk
  bstk <- discardFrame bstk
  pure (ustk, bstk)
moveArgs !ustk !bstk (UArg1 i) = do
  ustk <- prepareArgs ustk (Arg1 i)
  bstk <- discardFrame bstk
  pure (ustk, bstk)
moveArgs !ustk !bstk (UArg2 i j) = do
  ustk <- prepareArgs ustk (Arg2 i j)
  bstk <- discardFrame bstk
  pure (ustk, bstk)
moveArgs !ustk !bstk (UArgR i l) = do
  ustk <- prepareArgs ustk (ArgR i l)
  bstk <- discardFrame bstk
  pure (ustk, bstk)
moveArgs !ustk !bstk (BArg1 i) = do
  ustk <- discardFrame ustk
  bstk <- prepareArgs bstk (Arg1 i)
  pure (ustk, bstk)
moveArgs !ustk !bstk (BArg2 i j) = do
  ustk <- discardFrame ustk
  bstk <- prepareArgs bstk (Arg2 i j)
  pure (ustk, bstk)
moveArgs !ustk !bstk (BArgR i l) = do
  ustk <- discardFrame ustk
  bstk <- prepareArgs bstk (ArgR i l)
  pure (ustk, bstk)
moveArgs !ustk !bstk (DArg2 i j) = do
  ustk <- prepareArgs ustk (Arg1 i)
  bstk <- prepareArgs bstk (Arg1 j)
  pure (ustk, bstk)
moveArgs !ustk !bstk (DArgR ui ul bi bl) = do
  ustk <- prepareArgs ustk (ArgR ui ul)
  bstk <- prepareArgs bstk (ArgR bi bl)
  pure (ustk, bstk)
{-# inline moveArgs #-}

buildData
  :: Stack 'UN -> Stack 'BX -> Tag -> Args -> IO Closure
buildData !_    !_    !t ZArgs = pure $ Enum t
buildData !ustk !_    !t (UArg1 i) = do
  x <- peekOff ustk i
  pure $ DataU1 t x
buildData !ustk !_    !t (UArg2 i j) = do
  x <- peekOff ustk i
  y <- peekOff ustk j
  pure $ DataU2 t x y
buildData !_    !bstk !t (BArg1 i) = do
  x <- peekOff bstk i
  pure $ DataB1 t x
buildData !_    !bstk !t (BArg2 i j) = do
  x <- peekOff bstk i
  y <- peekOff bstk j
  pure $ DataB2 t x y
buildData !ustk !bstk !t (DArg2 i j) = do
  x <- peekOff ustk i
  y <- peekOff bstk j
  pure $ DataUB t x y
buildData !ustk !_    !t (UArgR i l) = do
  useg <- augSeg ustk unull (ArgR i l)
  pure $ DataG t useg bnull
buildData !_    !bstk !t (BArgR i l) = do
  bseg <- augSeg bstk bnull (ArgR i l)
  pure $ DataG t unull bseg
buildData !ustk !bstk !t (DArgR ui ul bi bl) = do
  useg <- augSeg ustk unull (ArgR ui ul)
  bseg <- augSeg bstk bnull (ArgR bi bl)
  pure $ DataG t useg bseg
{-# inline buildData #-}

dumpData
  :: Stack 'UN -> Stack 'BX -> Closure -> IO (Stack 'UN, Stack 'BX)
dumpData !ustk !bstk (Enum t) = do
  ustk <- bump ustk
  poke ustk t
  pure (ustk, bstk)
dumpData !ustk !bstk (DataU1 t x) = do
  ustk <- bumpn ustk 2
  pokeOff ustk 1 x
  poke ustk t
  pure (ustk, bstk)
dumpData !ustk !bstk (DataU2 t x y) = do
  ustk <- bumpn ustk 3
  pokeOff ustk 2 y
  pokeOff ustk 1 x
  poke ustk t
  pure (ustk, bstk)
dumpData !ustk !bstk (DataB1 t x) = do
  ustk <- bump ustk
  bstk <- bump bstk
  poke bstk x
  poke ustk t
  pure (ustk, bstk)
dumpData !ustk !bstk (DataB2 t x y) = do
  ustk <- bump ustk
  bstk <- bumpn bstk 2
  pokeOff bstk 1 y
  poke bstk x
  poke ustk t
  pure (ustk, bstk)
dumpData !ustk !bstk (DataUB t x y) = do
  ustk <- bumpn ustk 2
  bstk <- bump bstk
  pokeOff ustk 1 x
  poke bstk y
  poke ustk t
  pure (ustk, bstk)
dumpData !ustk !bstk (DataG t us bs) = do
  ustk <- dumpSeg ustk us S
  bstk <- dumpSeg bstk bs S
  ustk <- bump ustk
  poke ustk t
  pure (ustk, bstk)
dumpData !_    !_  _ = error "dumpData: bad closure"
{-# inline dumpData #-}

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
 leap (Push ufsz bfsz uasz basz nx k) = do
   ustk <- restoreFrame ustk ufsz uasz
   bstk <- restoreFrame bstk bfsz basz
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

splitCont
  :: Stack 'UN -> Stack 'BX
  -> K -> Int
  -> IO (K, Stack 'UN, Stack 'BX, Seg 'UN, Seg 'BX, K)
splitCont !ustk !bstk !k !p = walk (asize ustk) (asize bstk) KE k
 where
 walk !usz !bsz !ck KE = error "fell off stack" >> finish usz bsz ck KE
 walk !usz !bsz !ck (Mark q k)
   | p == q    = finish usz bsz ck k
   | otherwise = walk usz bsz (Mark q ck) k
 walk !usz !bsz !ck (Push un bn ua ba br k)
   = walk (usz+un+ua) (bsz+bn+ba) (Push un bn ua ba br ck) k

 finish !usz !bsz !ck !k = do
   (useg, ustk) <- grab ustk usz
   (bseg, bstk) <- grab bstk bsz
   return (ck, ustk, bstk, useg, bseg, k)
{-# inline splitCont #-}

resolve :: Env -> Stack 'UN -> Stack 'BX -> Ref -> IO Closure
resolve env _ _ (Env i) = return $ PAp (env i) unull bnull
resolve _ _ bstk (Stk i) = peekOff bstk i
-- resolve _   _ _ _ = error "TODO: resolve"
