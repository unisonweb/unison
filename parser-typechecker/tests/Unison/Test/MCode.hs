{-# language PatternGuards #-}
{-# language TypeApplications #-}
{-# language OverloadedStrings #-}

module Unison.Test.MCode where

import EasyTest

import Control.Monad.State (evalState)
import Control.Monad.Reader (runReaderT)

import qualified Data.Set as Set
import qualified Data.IntMap as IMap
import qualified Data.Map.Strict as Map

import Unison.Var (Var)
import Unison.Symbol (Symbol)
import Unison.Reference (Reference(Builtin))
import Unison.Runtime.Pattern (splitPatterns)
import Unison.Runtime.ANF
  ( ANFM
  , superNormalize
  , lamLift
  )
import Unison.Runtime.MCode
  ( Section(..)
  , Instr(..)
  , Prim1(..)
  , Prim2(..)
  , Args(..)
  , Comb(..)
  , Branch(..)
  , emitComb
  , emitCombs
  )
import Unison.Runtime.Builtin
import Unison.Runtime.Rt2
  ( eval0 )

import Unison.Test.Common (tm)

runANF :: Var v => (Reference -> Int) -> ANFM v a -> a
runANF rslv m = evalState (runReaderT m (Set.empty, rslv)) (Set.empty, [])

testEval0 :: (Int -> Comb) -> Section -> Test ()
testEval0 env sect = do
  io $ eval0 env sect
  ok

builtins :: Reference -> Int
builtins r
  | Builtin "todo" <- r = - (Map.size builtinNumbering+1)
  | Just i <- Map.lookup r builtinNumbering = - i
  | otherwise = error "builtins"

cenv :: Map.Map Int Comb
cenv = fmap (emitComb mempty) $ numberedLookup @Symbol

benv :: Int -> Comb
benv i
  | i == Map.size builtinNumbering + 1 = Lam 0 1 2 1 asrt
  | Just c <- Map.lookup i cenv = c
  | otherwise = error "benv"

env :: IMap.IntMap Comb -> Int -> Comb
env m = \n -> if n < 0 then benv $ -n else m IMap.! n

i2b :: Section
i2b = Match 0
    $ Test1 0 (Ins (Pack 0 ZArgs) . Yield $ BArg1 0)
              (Ins (Pack 1 ZArgs) . Yield $ BArg1 0)

eqn :: Section
eqn = Ins (Unpack 1)
    $ Ins (Unpack 0)
    $ Ins (Prim2 EQLI 1 3)
    $ i2b

asrt :: Section
asrt = Ins (Unpack 0)
     $ Match 0
     $ Test1 1 (Yield ZArgs)
               (Die "assertion failed")

bi1w :: Prim1 -> Section
bi1w op = Ins (Unpack 0)
        $ Ins (Prim1 op 1)
        $ Ins (Pack 0 $ UArg1 0)
        $ Yield $ BArg1 0
{-# inline bi1w #-}

bi2w :: Prim2 -> Section
bi2w op = Ins (Unpack 1)
        $ Ins (Unpack 0)
        $ Ins (Prim2 op 1 3)
        $ Ins (Pack 0 $ UArg1 0)
        $ Yield $ BArg1 0
{-# inline bi2w #-}

deci :: Section
deci = bi1w DECI

inci :: Section
inci = bi1w INCI

addi :: Section
addi = bi2w ADDI

subi :: Section
subi = bi2w SUBI

drpn :: Section
drpn = Ins (Unpack 1)
     $ Ins (Unpack 0)
     $ Ins (Prim2 LESI 3 1)
     $ Match 0
     $ Test1 1 (Ins (Lit 0) pk)
     $ Ins (Prim2 SUBI 4 2)
     $ pk
 where pk = Ins (Pack 0 $ UArg1 0) . Yield $ BArg1 0

multRec :: String
multRec
  = "let\n\
    \  n = 5\n\
    \  f acc i = match i with\n\
    \    0 -> acc\n\
    \    _ -> f (##Nat.+ acc n) (##Nat.sub i 1)\n\
    \  ##todo (##Nat.== (f 0 1000) 5000)"

testEval :: String -> Test ()
testEval s = testEval0 (env aux) main
  where
  (Lam 0 0 _ _ main, aux, _)
    = emitCombs 0
    . superNormalize builtins
    . lamLift
    . splitPatterns
    $ tm s

nested :: String
nested
  = "let\n\
    \  x = match 2 with\n\
    \        0 -> ##Nat.+ 0 1\n\
    \        m@n -> n\n\
    \  ##todo (##Nat.== x 2)"

test :: Test ()
test = scope "mcode" . tests $
  [ scope "2=2" $ testEval "##todo (##Nat.== 2 2)"
  , scope "2=1+1" $ testEval "##todo (##Nat.== 2 (##Nat.+ 1 1))"
  , scope "2=3-1" $ testEval "##todo (##Nat.== 2 (##Nat.sub 3 1))"
  , scope "5*5=25"
  $ testEval "##todo (##Nat.== (##Nat.* 5 5) 25)"
  , scope "5*1000=5000"
  $ testEval "##todo (##Nat.== (##Nat.* 5 1000) 5000)"
  , scope "5*1000=5000 rec" $ testEval multRec
  , scope "nested"
  $ testEval nested
  ]
