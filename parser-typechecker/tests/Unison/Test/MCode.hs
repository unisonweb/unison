{-# language PatternGuards #-}
{-# language OverloadedStrings #-}

module Unison.Test.MCode where

import EasyTest

import Control.Monad.State (evalState)
import Control.Monad.Reader (runReaderT)

import qualified Data.Set as Set
import qualified Data.Map as Map

import Unison.Var (Var)
import Unison.Reference (Reference(Builtin))
import Unison.Runtime.ANF
  ( ANFM
  , anfTerm
  , superNormalize
  )
import Unison.Runtime.MCode
  ( Section(..)
  , Instr(..)
  , Prim1(..)
  , Prim2(..)
  , Args(..)
  , Comb(..)
  , Branch(..)
  , emitSection
  , emitComb
  )
import Unison.Runtime.Rt2
  ( eval0 )

import Unison.Test.Common (tm)

runANF :: Var v => (Reference -> Int) -> ANFM v a -> a
runANF rslv m = evalState (runReaderT m rslv) (Set.empty, 0, [])

testEval0 :: (Int -> Comb) -> Section -> Test ()
testEval0 env sect = do
  io $ eval0 env sect
  ok

builtins :: Reference -> Int
builtins (Builtin t)
  | t == "Nat.==" = -1
  | t == "todo" = -2
  | t == "Nat.+" = -3
  | t == "Nat.sub" = -4
  | t == "Nat.dec" = -5
  | t == "Nat.increment" = -6
  | t == "Nat.drop" = -7
  | t == "Nat.*" = 20
builtins _ = error "builtins"

benv :: Int -> Comb
benv 1 = Lam 0 2 5 3 eqn
benv 2 = Lam 0 1 1 1 asrt
benv 3 = Lam 0 2 5 3 addi
benv 4 = Lam 0 2 5 3 subi
benv 5 = Lam 0 1 3 2 deci
benv 6 = Lam 0 1 3 2 inci
benv 7 = Lam 0 2 6 3 drpn
benv _ = error "benv"

env :: [(Int, Comb)] -> Int -> Comb
env cs = \n -> if n < 0 then benv $ -n else m Map.! n
  where m = Map.fromList cs

i2b :: Section
i2b = Match 0
    $ Test1 0 (Ins (Pack 0 ZArgs) . Yield $ BArg1 0)
              (Ins (Pack 1 ZArgs) . Yield $ BArg1 0)

eqn :: Section
eqn = Ins (Unpack 1)
    $ Ins (Unpack 0)
    $ Ins (Prim2 Eqn 1 3)
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
deci = bi1w Dec

inci :: Section
inci = bi1w Inc

addi :: Section
addi = bi2w Add

subi :: Section
subi = bi2w Sub

drpn :: Section
drpn = Ins (Unpack 1)
     $ Ins (Unpack 0)
     $ Ins (Prim2 Gtn 1 3)
     $ Match 0
     $ Test1 1 (Ins (Lit 0) pk)
     $ Ins (Prim2 Sub 4 2)
     $ pk
 where pk = Ins (Pack 0 $ UArg1 0) . Yield $ BArg1 0

mkComb :: String -> Comb
mkComb txt = case superNormalize builtins $ tm txt of
  [(_, sn)] -> emitComb sn
  _ -> error "unexpected multi-cobminator"

multc :: Comb
multc
  = mkComb "m n -> match m with\n\
           \  0 -> 0\n\
           \  _ -> ##Nat.+ n (##Nat.* (##Nat.sub m 1) n)"

multAccc :: Comb
multAccc
  = mkComb "m n a -> match n with\n\
           \  0 -> a\n\
           \  _ -> ##Nat.* m (##Nat.sub n 1) (##Nat.+ a m)"

testEval :: [(Int, Comb)] -> String -> Test ()
testEval cs s = testEval0 (env cs) mc
  where
  t = tm s
  a = runANF builtins $ anfTerm t
  mc = emitSection [] a

test :: Test ()
test = scope "mcode" . tests $
  [ scope "2=2" $ testEval [] "##todo (##Nat.== 2 2)"
  , scope "2=1+1" $ testEval [] "##todo (##Nat.== 2 (##Nat.+ 1 1))"
  , scope "2=3-1" $ testEval [] "##todo (##Nat.== 2 (##Nat.sub 3 1))"
  , scope "5*5=25"
  $ testEval [(20,multc)] "##todo (##Nat.== (##Nat.* 5 5) 25)"
  , scope "5*1000=5000 acc"
  $ testEval [(20,multAccc)] "##todo (##Nat.== (##Nat.* 5 1000 0) 5000)"
  ]
