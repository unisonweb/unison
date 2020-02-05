
module Main(main) where

import Criterion.Main

import Unison.Runtime.IR2
import Unison.Runtime.Rt2

loop :: IR
loop = Match 0 $ Test1 0 (Yield $ UArg1 1) rec
  where
  rec = Prim2 Add 0 1
      $ Prim1 Dec 1
      $ App (Env 0) (UArg2 0 1)

-- Boxed version of loop to see how fast we are without
-- worker/wrapper.
sloop :: IR
sloop = Unpack 1 . Unpack 0 $ body
  where
  body = Match 1 $ Test1
           0 (Pack 0 (UArg1 3) $ Yield (BArg1 0))
           {-else-} rec
  rec  = Prim2 Add 1 3
       $ Prim1 Dec 2
       $ Pack 0 (UArg1 1)
       $ Pack 0 (UArg1 0)
       $ App (Env 1) (BArg2 0 1)

konst :: IR
konst = Yield (BArg1 0)

add :: IR
add = Unpack 1
    $ Unpack 0
    $ Prim2 Add 1 3
    $ Pack 0 (UArg1 0)
    $ Yield (BArg1 0)

-- get = shift $ \k s -> k s s
-- put s = shift $ \k _ -> k () s
-- loop :: Int -> Int -> Int
-- loop n s0 = reset (body n) s0
--   where
--   body m | m == 0 = x = get ; f _ = x ; f
--          | otherwise = x = get ; put (x+m) ; body (m-1)

-- k s => (k s) s -- k continuation
diag :: IR
diag = Let (Reset 0 $ Jump 0 (BArg1 1))
     $ App (Stk 0) (BArg1 2)

-- => shift k. diag k
get :: IR
get = Capture 0
    $ App (Env 12) (BArg1 0)

-- k s _ => (k) s
kid :: IR
kid = Let (Reset 0 $ Jump 0 ZArgs)
    $ App (Stk 0) (BArg1 2)

-- s => shift k. kid k s
put :: IR
put = Capture 0
    $ App (Env 15) (BArg2 0 1)

-- m => ...
kloopb :: IR
kloopb
  = Match 0 $ Test1
      0 (Let (App (Env 13) ZArgs) $ App (Env 10) (BArg1 0))
      {-else-} rec
 where
 rec = Let (App (Env 13) ZArgs) -- get
     $ Pack 0 (UArg1 0)
     $ Let (App (Env 11) (BArg2 0 1)) -- add
     $ Let (App (Env 14) (BArg1 0)) -- put
     $ Prim1 Dec 0
     $ App (Env 5) (UArg1 0)

-- m a => f = reset (kloopb m) ; y = f (I# a) ; print y
kloop :: IR
kloop = Let (Reset 0 $ App (Env 5) (UArg1 0))
      $ Pack 0 (UArg1 1)
      $ App (Stk 1) (BArg1 0)

fib :: IR
fib = Match 0 $ Test2
        0 (Lit 0 . Yield $ UArg1 0)
        1 (Lit 1 . Yield $ UArg1 0)
        {-else-} rec
  where
  rec = Prim1 Dec 0
      $ Prim1 Dec 0
      $ Let (App (Env 2) (UArg1 1))
      $ Let (App (Env 2) (UArg1 1))
      $ Prim2 Add 0 1 $ Yield (UArg1 0)

stackEater :: IR
stackEater
  = Match 0 $ Test1
      0 (Yield ZArgs)
    $ Prim1 Dec 0 $ Let (App (Env 4) $ UArg1 0) (Yield ZArgs)

testEnv :: Int -> Comb
testEnv 0 = Lam 2 0 4 0 loop
testEnv 1 = Lam 0 2 6 4 sloop
testEnv 2 = Lam 1 0 6 0 fib
testEnv 4 = Lam 1 0 1 0 stackEater
testEnv 5 = Lam 1 0 2 3 kloopb
testEnv 6 = Lam 2 0 2 2 kloop
testEnv 10 = Lam 0 2 0 2 konst
testEnv 11 = Lam 0 2 5 3 add
testEnv 12 = Lam 0 2 0 2 diag
testEnv 13 = Lam 0 0 0 1 get
testEnv 14 = Lam 0 1 0 2 put
testEnv 15 = Lam 0 3 0 3 kid
testEnv _ = error "testEnv"

setupu1 :: Int -> Int -> IR
setupu1 f n = Lit n $ App (Env f) (UArg1 0)

setupu2 :: Int -> Int -> Int -> IR
setupu2 f m n = Lit m $ Lit n $ App (Env f) (UArg2 0 1)

setupb2 :: Int -> Int -> Int -> IR
setupb2 f m n
  = Lit m $ Pack 0 (UArg1 0)
  $ Lit n $ Pack 0 (UArg1 0)
  $ App (Env f) (BArgR 0 2)

benchEv :: String -> IR -> Benchmark
benchEv str code = bench str . whnfIO . eval0 testEnv $ code

main = defaultMain
  [ bgroup "loop"
      [ benchEv "2500"    $ setupu2 0 0 2500
      , benchEv "5000"    $ setupu2 0 0 5000
      , benchEv "10000"   $ setupu2 0 0 10000
      , benchEv "100000"  $ setupu2 0 0 100000
      , benchEv "1000000" $ setupu2 0 0 1000000
      ]
  , bgroup "sloop"
      [ benchEv "2500"    $ setupb2 1 0 2500
      , benchEv "5000"    $ setupb2 1 0 5000
      , benchEv "10000"   $ setupb2 1 0 10000
      , benchEv "100000"  $ setupb2 1 0 100000
      , benchEv "1000000" $ setupb2 1 0 1000000
      ]
  , bgroup "kloop"
      [ benchEv "2500"    $ setupu2 6 0 2500
      , benchEv "5000"    $ setupu2 6 0 5000
      , benchEv "10000"   $ setupu2 6 0 10000
      , benchEv "100000"  $ setupu2 6 0 100000
      , benchEv "1000000" $ setupu2 6 0 1000000
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
