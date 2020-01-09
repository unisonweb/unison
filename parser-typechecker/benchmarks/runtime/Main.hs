
module Main(main) where

import Criterion.Main

import Unison.Runtime.IR2
import Unison.Runtime.Rt2

loop :: IR
loop = Match 0 $ TestEq 0 (Con 2 0 0 $ Arg1 1) rec
  where
  rec = Prim2 Add 0 1 $ Prim1 Dec 1 $ App 4 0 (Env 0) (Arg2 0 1)

-- Boxed version of loop to see how fast we are without
-- worker/wrapper.
sloop :: IR
sloop = Unpack 1 . Unpack 0 $ body
  where
  body = Match 1 $ TestEq 0 (Con 0 2 0 $ Arg1 0) rec
  rec  = Prim2 Add 0 1 $ Prim1 Dec 1 $ Let bld1 $ Let bld0 $ (App 2 2 (Env 1) (Arg2 0 1)) -- ?
  bld0 = Con 0 0 0 $ Arg1 0
  bld1 = Con 0 0 0 $ Arg1 1

testEnv :: Int -> IR
testEnv 0 = loop
testEnv 1 = sloop
testEnv _ = error "testEnv"

setup :: Int -> IR
setup n = Lit 0 $ Lit n $ App 0 2 (Env 0) (Arg2 0 1)

main = defaultMain
  [ bgroup "loop"
      [ bench "2500" . whnfIO . eval0 testEnv $ setup 2500
      , bench "5000" . whnfIO . eval0 testEnv $ setup 5000
      , bench "10000" . whnfIO . eval0 testEnv $ setup 10000
      , bench "100000" . whnfIO . eval0 testEnv $ setup 100000
      , bench "1000000" . whnfIO . eval0 testEnv $ setup 1000000
      ]
  ]
