{-# language PatternSynonyms #-}

module Main(main) where

import Criterion.Main

import Unison.Runtime.IR2
import Unison.Runtime.Rt2

pattern App b r a = Appl (-1) Nothing (Unknown b r a)
pattern LApp b r a ret = Appl (-1) (Just ret) (Unknown b r a)
pattern Call b i a = Appl (-1) Nothing (Known b i a)
pattern LCall b i a ret = Appl (-1) (Just ret) (Known b i a)

infixr 0 $<, $$, ><
($<) :: Instr -> Flow -> Section
i $< f = Section (i :< NL) f
($$) :: Instr -> Section -> Section
i $$ (Section is fl) = Section (i :< is) fl
esn :: Flow -> Section
esn = Section NL
(><) :: (Section -> a) -> Flow -> a
h >< fl = h $ esn fl

loop :: Section
loop = esn $ Match 0 $ Test1 0 (esn . Yield $ UArg1 1) rec
  where
  rec = Prim2 Add 0 1
     $$ Prim1 Dec 1
     $< App False (Env 0) (UArg2 0 1)

-- Boxed version of loop to see how fast we are without
-- worker/wrapper.
sloop :: Section
sloop = Unpack 1 $$ Unpack 0 $< body
  where
  body = Match 1 $ Test1
           0 (Pack 0 (UArg1 3) $< Yield (BArg1 0))
           {-else-} rec
  rec  = Prim2 Add 1 3
      $$ Prim1 Dec 2
      $$ Pack 0 (UArg1 1)
      $$ Pack 0 (UArg1 0)
      $< App False (Env 1) (BArg2 0 1)

-- loop with fast path optimization
oloop :: Section
oloop = esn $ Match 0 $ Test1 0 (esn . Yield $ UArg1 1) rec
  where
  rec = Prim2 Add 0 1
     $$ Prim1 Dec 1
     $< Call False 7 (UArg2 0 1)

-- sloop with fast path optimization
soloop :: Section
soloop = Unpack 1 $$ Unpack 0 $< body
  where
  body = Match 1 $ Test1
           0 (Pack 0 (UArg1 3) $< Yield (BArg1 0))
           {-else-} rec
  rec = Prim2 Add 1 3
     $$ Prim1 Dec 2
     $$ Pack 0 (UArg1 1)
     $$ Pack 0 (UArg1 0)
     $< Call False 8 (BArg2 0 1)

konst :: Section
konst = esn $ Yield (BArg1 0)

add :: Section
add = Unpack 1
   $$ Unpack 0
   $$ Prim2 Add 1 3
   $$ Pack 0 (UArg1 0)
   $< Yield (BArg1 0)

-- get = shift $ \k s -> k s s
-- put s = shift $ \k _ -> k () s
-- loop :: Int -> Int -> Int
-- loop n s0 = reset (body n) s0
--   where
--   body m | m == 0 = x = get ; f _ = x ; f
--          | otherwise = x = get ; put (x+m) ; body (m-1)

-- k s => (k s) s -- k continuation
diag :: Section
diag = esn $ Appl 0 resume (Jump 0 (BArg1 1))
 where resume = Just >< App False (Stk 0) (BArg1 2)

-- => shift k. diag k
get :: Section
get = Capture 0
   $< App False (Env 12) (BArg1 0)

-- k s _ => (k) s
kid :: Section
kid = esn $ Appl 0 resume (Jump 0 ZArgs)
 where resume = Just >< App False (Stk 0) (BArg1 2)

-- s => shift k. kid k s
put :: Section
put = Capture 0
   $< App False (Env 15) (BArg2 0 1)

-- m => ...
kloopb :: Section
kloopb = esn $
    Match 0 $ Test1
      0 (esn $ LApp False (Env 13) ZArgs >< App False (Env 10) (BArg1 0))
      {-else-} $ esn rec
 where
 rec = LApp False (Env 13) ZArgs -- get
     $ Pack 0 (UArg1 0)
    $< LApp False (Env 11) (BArg2 0 1) -- add
    >< LApp False (Env 14) (BArg1 0) -- put
     $ Prim1 Dec 0
    $< App False (Env 5) (UArg1 0)

-- m a => f = reset (kloopb m) ; y = f (I# a) ; print y
kloop :: Section
kloop = esn $ Appl 0 resume (Unknown False (Env 5) (UArg1 0))
 where
 resume = Just $ Pack 0 (UArg1 1) $< App False (Stk 1) (BArg1 0)

-- s0 0 => s0
-- s0 1 s => tinst s setDyn 0 (teff s)
teff :: Section
teff
  = esn $ Match 0 $ Test1
      0 (esn . Yield $ BArg1 0)
   >< {-else-} Call True 21 ZArgs

-- s => setDyn 0 (teff s)
tinst :: Section
tinst
  = Name 20 (BArg1 0)
 $$ SetDyn 0 0
 $< Yield ZArgs

-- m => ...
tloopb :: Section
tloopb = esn $
    Match 0 $ Test1
      0 (Lit 0 $< App True (Dyn 0) (UArg1 0)) -- get
      {-else-} rec
  where
  -- rec = Let (Lit 0 $ App False (Dyn 0) (UArg1 0)) -- get
  --    $$ Pack 0 (UArg1 0) -- I# m
  --    $$ Let (App False (Env 11) (BArg2 0 1)) -- add
  --    $$ Let (Lit 1 $ App False (Dyn 0) (UArg1 0)) -- put
  --    $$ Prim1 Dec 0
  --    $< Call False 25 (UArg1 0)
  rec = Lit 0 $< LApp False (Dyn 0) (UArg1 0) -- get
      $ Pack 0 (UArg1 1) -- I# m
     $< LApp False (Env 11) (BArg2 0 1) -- add
      $ Lit 1 $< LApp False (Dyn 0) (UArg1 0) -- put
      $ Prim1 Dec 2
     $< Call False 25 (UArg1 0)

-- m s => reset (tinst (I# s) ; tloopb m)
tloop :: Section
-- tloop = Reset 0
--   $$ Pack 0 (UArg1 1)
--   $$ Let (Call True 21 $ BArg1 0)
--   $< Call True 25 $ UArg1 0
tloop = Pack 0 (UArg1 1)
     $< Appl 0 ret (Known True 21 $ BArg1 0)
 where ret = Just >< Call True 25 $ UArg1 0

fib :: Section
fib = esn $ Match 0 $ Test2
        0 (Lit 0 $< Yield $ UArg1 0)
        1 (Lit 1 $< Yield $ UArg1 0)
        {-else-} rec
  where
  rec = Prim1 Dec 0
     $$ Prim1 Dec 0
     $< LApp False (Env 2) (UArg1 1)
     >< LApp False (Env 2) (UArg1 1)
      $ Prim2 Add 0 1 $< Yield (UArg1 0)

ofib :: Section
ofib = esn $ Match 0 $ Test2
         0 (Lit 0 $< Yield $ UArg1 0)
         1 (Lit 1 $< Yield $ UArg1 0)
         {-else-} rec
  where
  rec = Prim1 Dec 0
     $$ Prim1 Dec 0
     $< LCall True 9 (UArg1 1)
     >< LCall True 9 (UArg1 1)
      $ Prim2 Add 0 1 $< Yield (UArg1 0)

stackEater :: Section
stackEater
  = esn $ Match 0 $ Test1
      0 (esn $ Yield ZArgs)
    $ Prim1 Dec 0
   $< LApp False (Env 4) (UArg1 0)
   >< (Yield ZArgs)

testEnv :: Int -> Comb
testEnv 0 = Lam 2 0 4 0 loop
testEnv 1 = Lam 0 2 6 4 sloop
testEnv 2 = Lam 1 0 6 0 fib
testEnv 4 = Lam 1 0 1 0 stackEater
testEnv 5 = Lam 1 0 2 3 kloopb
testEnv 6 = Lam 2 0 2 2 kloop
testEnv 7 = Lam 2 0 4 0 oloop
testEnv 8 = Lam 0 2 6 4 soloop
testEnv 9 = Lam 1 0 6 0 ofib
testEnv 10 = Lam 0 2 0 2 konst
testEnv 11 = Lam 0 2 5 3 add
testEnv 12 = Lam 0 2 0 2 diag
testEnv 13 = Lam 0 0 0 1 get
testEnv 14 = Lam 0 1 0 2 put
testEnv 15 = Lam 0 3 0 3 kid
testEnv 20 = Lam 1 1 1 2 teff
testEnv 21 = Lam 0 1 0 2 tinst
testEnv 25 = Lam 1 0 4 3 tloopb
testEnv 26 = Lam 1 0 4 3 tloop
testEnv _ = error "testEnv"

setupu1 :: Int -> Int -> Section
setupu1 f n = Lit n $< App False (Env f) (UArg1 0)

setupu2 :: Int -> Int -> Int -> Section
setupu2 f m n = Lit m $$ Lit n $< App False (Env f) (UArg2 0 1)

setupb2 :: Int -> Int -> Int -> Section
setupb2 f m n
  = Lit m $$ Pack 0 (UArg1 0)
 $$ Lit n $$ Pack 0 (UArg1 0)
 $< App False (Env f) (BArgR 0 2)

benchEv :: String -> Section -> Benchmark
benchEv str code = bench str . whnfIO . eval0 testEnv $ code

main = defaultMain
  [ bgroup "loop"
      [ benchEv "2500"    $ setupu2 0 0 2500
      , benchEv "5000"    $ setupu2 0 0 5000
      , benchEv "10000"   $ setupu2 0 0 10000
      , benchEv "100000"  $ setupu2 0 0 100000
      , benchEv "1000000" $ setupu2 0 0 1000000
      ]
  , bgroup "oloop"
      [ benchEv "2500"    $ setupu2 7 0 2500
      , benchEv "5000"    $ setupu2 7 0 5000
      , benchEv "10000"   $ setupu2 7 0 10000
      , benchEv "100000"  $ setupu2 7 0 100000
      , benchEv "1000000" $ setupu2 7 0 1000000
      ]
  , bgroup "sloop"
      [ benchEv "2500"    $ setupb2 1 0 2500
      , benchEv "5000"    $ setupb2 1 0 5000
      , benchEv "10000"   $ setupb2 1 0 10000
      , benchEv "100000"  $ setupb2 1 0 100000
      , benchEv "1000000" $ setupb2 1 0 1000000
      ]
  , bgroup "soloop"
      [ benchEv "2500"    $ setupb2 8 0 2500
      , benchEv "5000"    $ setupb2 8 0 5000
      , benchEv "10000"   $ setupb2 8 0 10000
      , benchEv "100000"  $ setupb2 8 0 100000
      , benchEv "1000000" $ setupb2 8 0 1000000
      ]
  , bgroup "kloop"
      [ benchEv "2500"    $ setupu2 6 0 2500
      , benchEv "5000"    $ setupu2 6 0 5000
      , benchEv "10000"   $ setupu2 6 0 10000
      , benchEv "100000"  $ setupu2 6 0 100000
      , benchEv "1000000" $ setupu2 6 0 1000000
      ]
  , bgroup "tloop"
      [ benchEv "2500"    $ setupu2 26 0 2500
      , benchEv "5000"    $ setupu2 26 0 5000
      , benchEv "10000"   $ setupu2 26 0 10000
      , benchEv "100000"  $ setupu2 26 0 100000
      , benchEv "1000000" $ setupu2 26 0 1000000
      ]
  , bgroup "fib"
      [ benchEv "10" $ setupu1 2 10
      , benchEv "15" $ setupu1 2 15
      , benchEv "20" $ setupu1 2 20
      , benchEv "25" $ setupu1 2 25
      , benchEv "30" $ setupu1 2 30
      ]
  , bgroup "ofib"
      [ benchEv "10" $ setupu1 9 10
      , benchEv "15" $ setupu1 9 15
      , benchEv "20" $ setupu1 9 20
      , benchEv "25" $ setupu1 9 25
      , benchEv "30" $ setupu1 9 30
      ]
  , bgroup "stackEater"
      [ benchEv "100"    $ setupu1 4 100
      , benchEv "1000"   $ setupu1 4 1000
      , benchEv "10000"  $ setupu1 4 10000
      , benchEv "100000" $ setupu1 4 100000
      ]
  ]
