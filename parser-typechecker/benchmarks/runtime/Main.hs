
module Main(main) where

import Criterion.Main

import Unison.Runtime.IR2
import Unison.Runtime.Rt2

loop :: IR
loop = Match 0 $ Test1 0 (Yield 2 0 $ UArg1 1) rec
  where
  rec = Prim2 Add 0 1
      $ Prim1 Dec 1
      $ App 4 0 (Env 0) (UArg2 0 1)

-- Boxed version of loop to see how fast we are without
-- worker/wrapper.
sloop :: IR
sloop = Unpack 1 . Unpack 0 $ body
  where
  body = Match 1 $ Test1
           0 (Pack 0 (UArg1 3) $ Yield 4 3 (BArg1 0))
           {-else-} rec
  rec  = Prim2 Add 1 3
       $ Prim1 Dec 2
       $ Pack 0 (UArg1 1)
       $ Pack 0 (UArg1 0)
       $ App 6 4 (Env 1) (BArg2 0 1)

fib :: IR
fib = Match 0 $ Test2
        0 (Lit 0 . Yield 2 0 $ UArg1 0)
        1 (Lit 1 . Yield 2 0 $ UArg1 0)
        {-else-} rec
  where
  rec = Prim1 Dec 0
      $ Prim1 Dec 0
      $ Let (App 0 0 (Env 2) (UArg1 1))
      $ Let (App 0 0 (Env 2) (UArg1 1))
      $ Prim2 Add 0 1 $ Yield 6 0 (UArg1 0)

stackEater :: IR
stackEater
  = Match 0 $ Test1
      0 (Yield 1 0 ZArgs)
    $ Prim1 Dec 0 $ Let (App 0 0 (Env 4) $ UArg1 0) (Yield 2 0 ZArgs)

testEnv :: Int -> Comb
testEnv 0 = Lam 2 0 4 0 loop
testEnv 1 = Lam 0 2 6 4 sloop
testEnv 2 = Lam 1 0 6 0 fib
testEnv 4 = Lam 1 0 1 0 stackEater
testEnv _ = error "testEnv"

setupu1 :: Int -> Int -> IR
setupu1 f n = Lit n $ App 1 0 (Env f) (UArg1 0)

setupu2 :: Int -> Int -> Int -> IR
setupu2 f m n = Lit m $ Lit n $ App 2 0 (Env f) (UArg2 0 1)

setupb2 :: Int -> Int -> Int -> IR
setupb2 f m n
  = Lit m $ Pack 0 (UArg1 0)
  $ Lit n $ Pack 0 (UArg1 0)
  $ App 2 2 (Env f) (BArgR 0 2)

benchEv :: String -> IR -> Benchmark
benchEv str code = bench str . whnfIO . eval0 testEnv $ code

main = defaultMain
  [ bgroup "sloop"
      [ benchEv "2500"    $ setupb2 1 0 2500
      , benchEv "5000"    $ setupb2 1 0 5000
      , benchEv "10000"   $ setupb2 1 0 10000
      , benchEv "100000"  $ setupb2 1 0 100000
      , benchEv "1000000" $ setupb2 1 0 1000000
      ]
  , bgroup "loop"
      [ benchEv "2500"    $ setupu2 0 0 2500
      , benchEv "5000"    $ setupu2 0 0 5000
      , benchEv "10000"   $ setupu2 0 0 10000
      , benchEv "100000"  $ setupu2 0 0 100000
      , benchEv "1000000" $ setupu2 0 0 1000000
      ]
  , bgroup "fib"
      [ benchEv "10" $ setupu1 2 10
      , benchEv "15" $ setupu1 2 15
      , benchEv "20" $ setupu1 2 20
      , benchEv "25" $ setupu1 2 25
      , benchEv "30" $ setupu1 2 30
      ]
  , bgroup "stackEater"
      [ benchEv "100"    $ setupu1 4 100
      , benchEv "1000"   $ setupu1 4 1000
      , benchEv "10000"  $ setupu1 4 10000
      , benchEv "100000" $ setupu1 4 100000
      ]
  ]
