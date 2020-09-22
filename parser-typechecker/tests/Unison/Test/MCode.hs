{-# language PatternGuards #-}
{-# language TypeApplications #-}
{-# language OverloadedStrings #-}

module Unison.Test.MCode where

import EasyTest

import qualified Data.Map.Strict as Map

import Data.Bits (bit)
import Data.Word (Word64)

import Unison.Util.EnumContainers as EC

import Unison.Term (unannotate)
import Unison.Symbol (Symbol)
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
  , Branch(..)
  , emitComb
  , emitCombs
  )
import Unison.Runtime.Builtin
import Unison.Runtime.Machine
  ( SEnv(..), eval0 )

import Unison.Test.Common (tm)

testEval0 :: EnumMap Word64 Comb -> Section -> Test ()
testEval0 env sect = do
  io $ eval0 (SEnv env builtinForeigns mempty mempty) sect
  ok

builtins :: Reference -> Word64
builtins r
  | Builtin "todo" <- r = bit 64
  | Just i <- Map.lookup r builtinTermNumbering = i
  | otherwise = error $ "builtins: " ++ show r

cenv :: EnumMap Word64 Comb
cenv = fmap (emitComb mempty) $ numberedTermLookup @Symbol

env :: EnumMap Word64 Comb -> EnumMap Word64 Comb
env m = m <> mapInsert (bit 64) (Lam 0 1 2 1 asrt) cenv

asrt :: Section
asrt = Ins (Unpack 0)
     $ Match 0
     $ Test1 1 (Yield ZArgs)
               (Die "assertion failed")

multRec :: String
multRec
  = "let\n\
    \  n = 5\n\
    \  f acc i = match i with\n\
    \    0 -> acc\n\
    \    _ -> f (##Nat.+ acc n) (##Nat.sub i 1)\n\
    \  ##todo (##Nat.== (f 0 1000) 5000)"

dataSpec :: DataSpec
dataSpec = mempty

testEval :: String -> Test ()
testEval s = testEval0 (env aux) main
  where
  (Lam 0 0 _ _ main, aux, _)
    = emitCombs (bit 24)
    . superNormalize builtins (builtinTypeNumbering Map.!)
    . lamLift
    . splitPatterns dataSpec
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
