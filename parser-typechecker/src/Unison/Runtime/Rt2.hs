{-# language BangPatterns #-}

module Unison.Runtime.Rt2 where

import Unison.Runtime.Stack

import Unison.Runtime.IR2

type Tag = Int
type Env = Int -> Comb

eval0 :: Env -> IR -> IO ()
eval0 !env !co = do
  ustk <- ualloc
  bstk <- balloc
  eval env ustk bstk KE co

eval :: Env -> UStk -> BStk -> K -> IR -> IO ()
eval !env !ustk !bstk !k (App r args) = do
  Lam _  _  uf _ fun <- resolve env ustk bstk r
  ustk <- uensure ustk uf
  (ustk, bstk) <- moveArgs ustk bstk args
  eval env ustk bstk k fun
eval !env !ustk !bstk !k (Reset p nx) =
  eval env ustk bstk (Mark p k) nx
eval !env !ustk !bstk !k (Capture p nx) = do
  (_ , ustk, bstk, _  , k') <- splitCont ustk bstk k p
  -- TODO: boxed stuff; right now this just discards continuations
  eval env ustk bstk k' nx
eval !env !ustk !bstk !k (Let e nx) = do
  (ustk, usz) <- uframe ustk
  eval env ustk bstk (Push usz nx k) e
eval !env !ustk !bstk !k (Prim1 op i nx) =
  prim1 env ustk bstk k op i nx
eval !env !ustk !bstk !k (Prim2 op i j nx) =
  prim2 env ustk bstk k op i j nx
eval !env !ustk !bstk !k (Pack _ _    nx) = do
  _ <- error "eval Pack"
  eval env ustk bstk k nx
eval !env !ustk !bstk !k (Unpack _ nx) = do
  _ <- error "eval Unpack"
  eval env ustk bstk k nx
eval !env !ustk !bstk !k (Match i br) = do
  t <- upeekOff ustk i
  eval env ustk bstk k $ selectBranch t br
eval !env !ustk !bstk !k (Print i nx) = do
  m <- upeekOff ustk i
  print m
  eval env ustk bstk k nx
eval !env !ustk !bstk !k (Lit n nx) = do
  ustk <- ubump ustk
  upoke ustk n
  eval env ustk bstk k nx
eval !env !ustk !bstk !k (Yield args) = do
  (ustk, bstk) <- moveArgs ustk bstk args
  yield env ustk bstk k

moveArgs :: UStk -> BStk -> Args -> IO (UStk, BStk)
moveArgs !ustk !bstk ZArgs = pure (ustk, bstk)
moveArgs !ustk !bstk (UArg1 i) = do
  ustk <- uargs ustk (Arg1 i)
  pure (ustk, bstk)
moveArgs !ustk !bstk (UArg2 i j) = do
  ustk <- uargs ustk (Arg2 i j)
  pure (ustk, bstk)
moveArgs !ustk !bstk (UArgR i l) = do
  ustk <- uargs ustk (ArgR i l)
  pure (ustk, bstk)
moveArgs !ustk !bstk (BArg1 i) = do
  bstk <- bargs bstk (Arg1 i)
  pure (ustk, bstk)
moveArgs !ustk !bstk (BArg2 i j) = do
  bstk <- bargs bstk (Arg2 i j)
  pure (ustk, bstk)
moveArgs !ustk !bstk (BArgR i l) = do
  bstk <- bargs bstk (ArgR i l)
  pure (ustk, bstk)
moveArgs !ustk !bstk (DArg2 i j) = do
  ustk <- uargs ustk (Arg1 i)
  bstk <- bargs bstk (Arg1 j)
  pure (ustk, bstk)
moveArgs !ustk !bstk (DArgR ui ul bi bl) = do
  ustk <- uargs ustk (ArgR ui ul)
  bstk <- bargs bstk (ArgR bi bl)
  pure (ustk, bstk)
{-# inline moveArgs #-}

prim1 :: Env -> UStk -> BStk -> K -> Prim1 -> Int -> IR -> IO ()
prim1 !env !ustk !bstk !k Dec !i !nx = do
  m <- upeekOff ustk i
  ustk <- ubump ustk
  upoke ustk (m-1)
  eval env ustk bstk k nx
prim1 !env !ustk !bstk !k Inc !i !nx = do
  m <- upeekOff ustk i
  ustk <- ubump ustk
  upoke ustk (m+1)
  eval env ustk bstk k nx
{-# inline prim1 #-}

prim2 :: Env -> UStk -> BStk -> K -> Prim2 -> Int -> Int -> IR -> IO ()
prim2 !env !ustk !bstk !k Add !i !j !nx = do
  m <- upeekOff ustk i
  n <- upeekOff ustk j
  ustk <- ubump ustk
  upoke ustk (m+n)
  eval env ustk bstk k nx
prim2 !env !ustk !bstk !k Sub !i !j !nx = do
  m <- upeekOff ustk i
  n <- upeekOff ustk j
  ustk <- ubump ustk
  upoke ustk (m-n)
  eval env ustk bstk k nx
{-# inline prim2 #-}

yield :: Env -> UStk -> BStk -> K -> IO ()
yield !env !ustk !bstk !k = leap k
 where
 leap (Mark _ k) = leap k
 leap (Push sz nx k) = do
   ustk <- urestore ustk sz
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

splitCont :: UStk -> BStk -> K -> Int -> IO (K, UStk, BStk, USeg, K)
splitCont !ustk !bstk !k !p = walk 0 KE k
-- splitCont !ustk !k !p = walk 0 KE k
 where
 walk :: Int -> K -> K -> IO (K, UStk, BStk, USeg, K)
 walk !sz !ck KE = finish sz ck KE
 walk !sz !ck (Mark q k)
   | p == q    = finish sz ck k
   | otherwise = walk sz (Mark q ck) k
 walk !sz !ck (Push n br k) = walk (sz+n) (Push n br ck) k

 finish :: Int -> K -> K -> IO (K, UStk, BStk, USeg, K)
 finish !sz !ck !k = do
   (seg, ustk) <- ugrab ustk sz
   return (ck, ustk, bstk, seg, k)
{-# inline splitCont #-}

resolve :: Env -> UStk -> BStk -> Ref -> IO Comb
resolve env _ _ (Env i) = return (env i)
resolve _ _ bstk (Stk i) = do
  _ <- bpeekOff bstk i
  error "huh"
-- resolve _   _ _ _ = error "TODO: resolve"
