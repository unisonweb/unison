{-# language DataKinds #-}
{-# language BangPatterns #-}

module Unison.Runtime.Rt2 where

import Data.Maybe (fromMaybe)

import qualified Data.IntMap.Strict as M

import Unison.Runtime.Stack

import Unison.Runtime.IR2

type Tag = Int
type Env = Int -> Comb
type DEnv = M.IntMap Closure

info :: Show a => String -> a -> IO ()
info ctx x = infos ctx (show x)
infos :: String -> String -> IO ()
infos ctx s = putStrLn $ ctx ++ ": " ++ s

eval0 :: Env -> Section -> IO ()
eval0 !env !co = do
  ustk <- alloc
  bstk <- alloc
  eval env M.empty ustk bstk KE co

lookupDenv :: Int -> DEnv -> Closure
lookupDenv p denv = fromMaybe BlackHole $ M.lookup p denv

exec
  :: Env -> DEnv
  -> Stack 'UN -> Stack 'BX -> K
  -> Instr
  -> IO (DEnv, Stack 'UN, Stack 'BX, K)
exec !_   !denv !ustk !bstk !k (Info tx) = do
  info tx ustk
  info tx bstk
  info tx k
  pure (denv, ustk, bstk, k)
exec !env !denv !ustk !bstk !k (Name n args) = do
  bstk <- name ustk bstk args (env n)
  pure (denv, ustk, bstk, k)
exec !_   !denv !ustk !bstk !k (SetDyn p i) = do
  clo <- peekOff bstk i
  pure (M.insert p clo denv, ustk, bstk, k)
exec !_   !denv !ustk !bstk !k (Capture p) = do
  (sk,denv,ustk,bstk,useg,bseg,k) <- splitCont denv ustk bstk k p
  bstk <- bump bstk
  poke bstk $ Captured sk useg bseg
  pure (denv, ustk, bstk, k)
exec !_   !denv !ustk !bstk !k (Prim1 op i) = do
  ustk <- prim1 ustk op i
  pure (denv, ustk, bstk, k)
exec !_   !denv !ustk !bstk !k (Prim2 op i j) = do
  ustk <- prim2 ustk op i j
  pure (denv, ustk, bstk, k)
exec !_   !denv !ustk !bstk !k (Pack t args) = do
  clo <- buildData ustk bstk t args
  bstk <- bump bstk
  poke bstk clo
  pure (denv, ustk, bstk, k)
exec !_   !denv !ustk !bstk !k (Unpack i) = do
  (ustk, bstk) <- dumpData ustk bstk =<< peekOff bstk i
  pure (denv, ustk, bstk, k)
exec !_   !denv !ustk !bstk !k (Print i) = do
  m <- peekOff ustk i
  print m
  pure (denv, ustk, bstk, k)
exec !_   !denv !ustk !bstk !k (Lit n) = do
  ustk <- bump ustk
  poke ustk n
  pure (denv, ustk, bstk, k)
exec !_   !denv !ustk !bstk !k (Reset p) = do
  pure (denv, ustk, bstk, Mark p clo k)
 where clo = lookupDenv p denv
{-# inline exec #-}

eval :: Env -> DEnv -> Stack 'UN -> Stack 'BX -> K -> Section -> IO ()
eval !env !denv !ustk !bstk !k (Match i br) = do
  t <- peekOff ustk i
  eval env denv ustk bstk k $ selectBranch t br
eval !env !denv !ustk !bstk !k (Yield args) = do
  (ustk, bstk) <- moveArgs ustk bstk args
  ustk <- frameArgs ustk
  bstk <- frameArgs bstk
  yield env denv ustk bstk k
eval !env !denv !ustk !bstk !k (App ck r args) =
  resolve env denv ustk bstk r >>= apply env denv ustk bstk k ck args
eval !env !denv !ustk !bstk !k (Call ck n args) =
  enter env denv ustk bstk k ck args $ env n
eval !env !denv !ustk !bstk !k (Jump i args) =
  peekOff bstk i >>= jump env denv ustk bstk k args
eval !env !denv !ustk !bstk !k (Let nw nx) = do
  (ustk, ufsz, uasz) <- saveFrame ustk
  (bstk, bfsz, basz) <- saveFrame bstk
  eval env denv ustk bstk (Push ufsz bfsz uasz basz nx k) nw
eval !env !denv !ustk !bstk !k (Ins i nx) = do
  (denv, ustk, bstk, k) <- exec env denv ustk bstk k i
  eval env denv ustk bstk k nx
{-# noinline eval #-}

-- fast path application
enter
  :: Env -> DEnv -> Stack 'UN -> Stack 'BX -> K
  -> Bool -> Args -> Comb -> IO ()
enter !env !denv !ustk !bstk !k !ck !args (Lam ua ba uf bf fun) = do
  ustk <- if ck then ensure ustk uf else pure ustk
  bstk <- if ck then ensure bstk bf else pure bstk
  (ustk, bstk) <- moveArgs ustk bstk args
  ustk <- acceptArgs ustk ua
  bstk <- acceptArgs bstk ba
  eval env denv ustk bstk k fun
{-# inline enter #-}

-- fast path by-name delaying
name :: Stack 'UN -> Stack 'BX -> Args -> Comb -> IO (Stack 'BX)
name !ustk !bstk !args !comb = do
  (useg, bseg) <- closeArgs ustk bstk unull bnull args
  bstk <- bump bstk
  poke bstk $ PAp comb useg bseg
  pure bstk
{-# inline name #-}

-- slow path application
apply
  :: Env -> DEnv -> Stack 'UN -> Stack 'BX -> K
  -> Bool -> Args -> Closure -> IO ()
apply !env !denv !ustk !bstk !k !ck !args clo = case clo of
  PAp comb@(Lam ua ba uf bf fun) useg bseg
    | ck || ua <= uac && ba <= bac -> do
      ustk <- ensure ustk uf
      bstk <- ensure bstk bf
      (ustk, bstk) <- moveArgs ustk bstk args
      ustk <- dumpSeg ustk useg A
      bstk <- dumpSeg bstk bseg A
      ustk <- acceptArgs ustk ua
      bstk <- acceptArgs bstk ba
      eval env denv ustk bstk k fun
    | otherwise -> do
      (useg, bseg) <- closeArgs ustk bstk useg bseg args
      ustk <- discardFrame ustk
      bstk <- discardFrame bstk
      bstk <- bump bstk
      poke bstk $ PAp comb useg bseg
      yield env denv ustk bstk k
   where
   uac = asize ustk + ucount args + uscount useg
   bac = asize bstk + bcount args + bscount bseg
  _ -> error "applying non-function"
{-# inline apply #-}

jump
  :: Env -> DEnv -> Stack 'UN -> Stack 'BX -> K
  -> Args -> Closure -> IO ()
jump !env !denv !ustk !bstk !k !args clo = case clo of
  Captured sk useg bseg -> do
    (useg, bseg) <- closeArgs ustk bstk useg bseg args
    ustk <- discardFrame ustk
    bstk <- discardFrame bstk
    ustk <- dumpSeg ustk useg . F $ ucount args
    bstk <- dumpSeg bstk bseg . F $ bcount args
    repush env ustk bstk denv sk k
  _ ->  error "jump: non-cont"
{-# inline jump #-}

repush :: Env -> Stack 'UN -> Stack 'BX -> DEnv -> K -> K -> IO ()
repush !env !ustk !bstk = go
 where
 go !denv KE !k = yield env denv ustk bstk k
 go !denv (Mark p c sk) !k = go denv' sk $ Mark p c' k
  where
  denv' = M.insert p c denv
  c' = lookupDenv p denv
 go !denv (Push un bn ua ba nx sk) !k
   = go denv sk $ Push un bn ua ba nx k
{-# inline repush #-}

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

prim1 :: Stack 'UN -> Prim1 -> Int -> IO (Stack 'UN)
prim1 !ustk Dec !i = do
  m <- peekOff ustk i
  ustk <- bump ustk
  poke ustk (m-1)
  pure ustk
prim1 !ustk Inc !i = do
  m <- peekOff ustk i
  ustk <- bump ustk
  poke ustk (m+1)
  pure ustk
{-# inline prim1 #-}

prim2 :: Stack 'UN -> Prim2 -> Int -> Int -> IO (Stack 'UN)
prim2 !ustk Add !i !j = do
  m <- peekOff ustk i
  n <- peekOff ustk j
  ustk <- bump ustk
  poke ustk (m+n)
  pure ustk
prim2 !ustk Sub !i !j = do
  m <- peekOff ustk i
  n <- peekOff ustk j
  ustk <- bump ustk
  poke ustk (m-n)
  pure ustk
{-# inline prim2 #-}

yield :: Env -> DEnv -> Stack 'UN -> Stack 'BX -> K -> IO ()
yield !env !denv !ustk !bstk !k = leap denv k
 where
 leap !denv (Mark q c k) = leap (M.insert q c denv) k
 leap !denv (Push ufsz bfsz uasz basz nx k) = do
   ustk <- restoreFrame ustk ufsz uasz
   bstk <- restoreFrame bstk bfsz basz
   eval env denv ustk bstk k nx
 leap _ KE = pure ()
{-# inline yield #-}

selectBranch :: Tag -> Branch -> Section
selectBranch t (Test1 u y n)
  | t == u    = y
  | otherwise = n
selectBranch t (Test2 u cu v cv e)
  | t == u    = cu
  | t == v    = cv
  | otherwise = e
{-# inline selectBranch #-}

splitCont
  :: DEnv -> Stack 'UN -> Stack 'BX -> K
  -> Int -> IO (K, DEnv, Stack 'UN, Stack 'BX, Seg 'UN, Seg 'BX, K)
splitCont !denv !ustk !bstk !k !p
  = walk denv (asize ustk) (asize bstk) KE k
 where
 walk !denv !usz !bsz !ck KE
   = error "fell off stack" >> finish denv usz bsz ck KE
 walk !denv !usz !bsz !ck (Mark q c k)
   | p == q    = finish denv' usz bsz ck k
   | otherwise = walk denv' usz bsz (Mark q c' ck) k
  where
  denv' = M.insert q c denv
  c' = lookupDenv q denv
 walk !denv !usz !bsz !ck (Push un bn ua ba br k)
   = walk denv (usz+un+ua) (bsz+bn+ba) (Push un bn ua ba br ck) k

 finish !denv !usz !bsz !ck !k = do
   (useg, ustk) <- grab ustk usz
   (bseg, bstk) <- grab bstk bsz
   return (ck, denv, ustk, bstk, useg, bseg, k)
{-# inline splitCont #-}

resolve :: Env -> DEnv -> Stack 'UN -> Stack 'BX -> Ref -> IO Closure
resolve env _ _ _ (Env i) = return $ PAp (env i) unull bnull
resolve _ _ _ bstk (Stk i) = peekOff bstk i
resolve _ denv _ _ (Dyn i) = case M.lookup i denv of
  Just clo -> pure clo
  _ -> error $ "resolve: looked up bad dynamic: " ++ show i
