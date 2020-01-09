{-# language BangPatterns #-}

module Unison.Runtime.Rt2 where

import Unison.Runtime.Stack

import Unison.Runtime.IR2

data Closure
  = PAp                !IR   -- code
        {-# unpack #-} !USeg -- unboxed args
  | Bx !Int


-- Evaluation stack
data K
  = KE
  | Mark !Int !K -- mark continuation with a prompt
  | Push !Int -- frame size
         !Branch  -- code
         !K
  | Store !IR !K

type Tag = Int
type Env = Int -> IR

eval0 :: Env -> IR -> IO ()
eval0 !env !co = do
  ustk <- ualloc
  eval env ustk KE co

eval :: Env -> UStk -> K -> IR -> IO ()
eval !env !ustk !k (App u _ r args) = do
  fun <- resolve env ustk r
  ustk <- moveArgs ustk u args
  eval env ustk k fun
eval !env !ustk !k (Reset p nx) =
  eval env ustk (Mark p k) nx
eval !env !ustk !k (Capture p nx) = do
  (_ , ustk, _  , k') <- splitCont ustk k p
  -- TODO: boxed stuff; right now this just discards continuations
  eval env ustk k' nx
eval !env !ustk !k (Let e nx) =
  eval env ustk (Store nx k) e
eval !env !ustk !k (Prim1 op i nx) =
  prim1 env ustk k op i nx
eval !env !ustk !k (Prim2 op i j nx) =
  prim2 env ustk k op i j nx
eval !env !ustk !k (Unpack _ nx) = do
  eval env ustk k nx
eval !env !ustk !k (Match i br) = do
  t <- upeekOff ustk i
  eval env ustk k $ selectBranch t br
eval !env !ustk !k (Con u _ t args) = do
  ustk <- moveArgs ustk u args
  yield env ustk t k
eval !env !ustk !k (Select u _ i) = do
  t <- upeekOff ustk i
  ustk <- ufree ustk u
  yield env ustk t k
eval !env !ustk !k (Print i nx) = do
  m <- upeekOff ustk i
  print m
  eval env ustk k nx
eval !env !ustk !k (Lit n nx) = do
  ustk <- ubump ustk
  upoke ustk n
  eval env ustk k nx

prim1 :: Env -> UStk -> K -> Prim1 -> Int -> IR -> IO ()
prim1 !env !ustk !k Dec !i !nx = do
  m <- upeekOff ustk i
  ustk <- ubump ustk
  upoke ustk (m-1)
  eval env ustk k nx
{-# inline prim1 #-}

prim2 :: Env -> UStk -> K -> Prim2 -> Int -> Int -> IR -> IO ()
prim2 !env !ustk !k Add !i !j !nx = do
  m <- upeekOff ustk i
  n <- upeekOff ustk j
  ustk <- ubump ustk
  upoke ustk (m+n)
  eval env ustk k nx
{-# inline prim2 #-}

yield :: Env -> UStk -> Tag -> K -> IO ()
yield !env !ustk !t !k = leap k
 where
 leap (Mark _ k) = leap k
 leap (Push _ br k) =
   eval env ustk k (selectBranch t br)
 leap (Store nx k) = do
   putStrLn "TODO: store"
   eval env ustk k nx
 leap KE = pure ()
{-# inline yield #-}

selectBranch :: Tag -> Branch -> IR
selectBranch _ (Prod nx) = nx
selectBranch t (TestEq u y n)
  | t == u    = y
  | otherwise = n
{-# inline selectBranch #-}

moveArgs :: UStk -> Int -> Args -> IO UStk
moveArgs !ustk !u Arg0 = ufree ustk u
moveArgs !ustk !u (Arg1 i) = do
  x <- upeekOff ustk i
  ustk <- ufree ustk (u-1)
  upoke ustk x
  return ustk
moveArgs !ustk !u (Arg2 i j) = do
  x <- upeekOff ustk i
  y <- upeekOff ustk j
  ustk <- ufree ustk (u-2)
  upoke ustk x
  upokeOff ustk 1 y
  return ustk
moveArgs !_    !_ (ArgN _) = error "TODO: moveArgs N"
{-# inline moveArgs #-}

splitCont :: UStk -> K -> Int -> IO (K, UStk, USeg, K)
splitCont !ustk !k !p = walk 0 KE k
-- splitCont !ustk !k !p = walk 0 KE k
 where
 walk :: Int -> K -> K -> IO (K, UStk, USeg, K)
 walk !sz !ck KE = finish sz ck KE
 walk !sz !ck (Mark q k)
   | p == q    = finish sz ck k
   | otherwise = walk sz (Mark q ck) k
 walk !sz !ck (Store nx k) = walk sz (Store nx ck) k
 walk !sz !ck (Push n br k) = walk (sz+n) (Push n br ck) k

 finish :: Int -> K -> K -> IO (K, UStk, USeg, K)
 finish !sz !ck !k = do
   (seg, ustk) <- ugrab ustk sz
   return (ck, ustk, seg, k)
{-# inline splitCont #-}

resolve :: Env -> UStk -> Ref -> IO IR
resolve env _ (Env i) = return (env i)
resolve _   _ _ = error "TODO: resolve"
