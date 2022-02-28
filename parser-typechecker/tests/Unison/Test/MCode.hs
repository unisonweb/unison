{-# language PatternGuards #-}
{-# language TypeApplications #-}
{-# language OverloadedStrings #-}

module Unison.Test.MCode where

import EasyTest

import Control.Concurrent.STM

import qualified Data.Map.Strict as Map

import Data.Bits (bit)
import Data.Word (Word64)

import Unison.Util.EnumContainers as EC

import Unison.Term (unannotate)
import Unison.Reference (Reference(Builtin))
import Unison.Runtime.Pattern
import Unison.Runtime.ANF
  ( superNormalize
  , lamLift
  )
import Unison.Runtime.MCode
  ( Section(..)
  , Instr(..)
  , Args(..)
  , Comb(..)
  , Combs
  , Branch(..)
  , RefNums(..)
  , emitComb
  , emitCombs
  )
import Unison.Runtime.Builtin
import Unison.Runtime.Machine
  ( CCache(..), eval0, baseCCache )

import Unison.Test.Common (tm)

dummyRef :: Reference
dummyRef = Builtin "dummy"

modifyTVarTest :: TVar a -> (a -> a) -> Test ()
modifyTVarTest v f = io . atomically $ modifyTVar v f

testEval0 :: EnumMap Word64 Combs -> Section -> Test ()
testEval0 env sect = do
  cc <- io baseCCache
  modifyTVarTest (combs cc) (env <>)
  modifyTVarTest (combRefs cc) ((dummyRef <$ env) <>)
  io $ eval0 cc Nothing sect
  ok

builtins :: Reference -> Word64
builtins r
  | Builtin "todo" <- r = bit 64
  | Just i <- Map.lookup r builtinTermNumbering = i
  | otherwise = error $ "builtins: " ++ show r

cenv :: EnumMap Word64 Combs
cenv = fmap (emitComb numbering 0 mempty . (0,))
       numberedTermLookup

env :: Combs -> EnumMap Word64 Combs
env m = mapInsert (bit 24) m
      . mapInsert (bit 64) (mapSingleton 0 $ Lam 0 1 2 1 asrt)
      $ cenv

asrt :: Section
asrt = Ins (Unpack Nothing 0)
     $ Match 0
     $ Test1 1 (Yield (BArg1 0))
               (Die "assertion failed")

multRec :: String
multRec
  = "let\n\
    \  n = 5\n\
    \  f acc i = match i with\n\
    \    0 -> acc\n\
    \    _ -> f (##Nat.+ acc n) (##Nat.sub i 1)\n\
    \  ##todo (##Nat.== (f 0 1000) 5000)"

numbering :: RefNums
numbering = RN (builtinTypeNumbering Map.!) builtins

testEval :: String -> Test ()
testEval s = testEval0 (env aux) main
  where
  Lam 0 0 _ _ main = aux ! 0
  aux
    = emitCombs numbering (bit 24)
    . superNormalize
    . fst
    . lamLift
    . splitPatterns builtinDataSpec
    . unannotate
    $ tm s

nested :: String
nested
  = "let\n\
    \  x = match 2 with\n\
    \        0 -> ##Nat.+ 0 1\n\
    \        m@n -> n\n\
    \  ##todo (##Nat.== x 2)"

matching'arguments :: String
matching'arguments
  = "let\n\
    \  f x y z = y\n\
    \  g x = f x\n\
    \  blorf = let\n\
    \    a = 0\n\
    \    b = 1\n\
    \    d = 2\n\
    \    h = g a b\n\
    \    c = 2\n\
    \    h c\n\
    \  ##todo (##Nat.== blorf 1)"

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
  , scope "matching arguments"
  $ testEval matching'arguments
  ]
