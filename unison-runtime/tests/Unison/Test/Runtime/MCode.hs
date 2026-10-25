{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Unison.Test.Runtime.MCode where

import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import EasyTest
import Unison.ABT.Normalized (pattern TAbs, pattern TAbss)
import Unison.Reference (Reference, Reference' (Builtin))
import Unison.Runtime.ANF
  ( Cacheability (..),
    Code (..),
    Direction (..),
    Func (..),
    Lit (..),
    Mem (..),
    SuperGroup (..),
    SuperNormal (..),
    lamLift,
    superNormalize,
    pattern TApp,
    pattern TLetD,
    pattern TLets,
    pattern TLit,
    pattern TVar,
  )
import Unison.Runtime.ANF qualified as ANF
import Unison.Runtime.Exception (RuntimeExn (..))
import Unison.Runtime.JIT qualified as JIT
import Unison.Runtime.Machine
  ( CCache (..),
    apply0,
    baseCCache,
    cacheAdd,
    reifyValue,
  )
import Unison.Runtime.Pattern
import Unison.Runtime.Referenced (Referenced (..))
import Unison.Symbol (Symbol)
import Unison.Term (unannotate)
import Unison.Test.Common (tm)
import Unison.Util.EnumContainers (mapFromList)
import Unison.Util.Pretty qualified as P
import Unison.Var qualified as Var

dummyRef :: Reference
dummyRef = Builtin "dummy"

mainRef :: Reference
mainRef = Builtin "main"

modifyTVarTest :: TVar a -> (a -> a) -> Test ()
modifyTVarTest v f = io . atomically $ modifyTVar v f

testEval0 ::
  [(Reference, SuperGroup Reference Symbol)] ->
  SuperGroup Reference Symbol ->
  Test ()
testEval0 env main =
  ok << io do
    jit <- JIT.newJIT =<< JIT.readConfig
    cc <- baseCCache jit False
    _ <- cacheAdd ((fmap . fmap) uncacheable $ (mainRef, main) : env) cc
    rtm <- readTVarIO (refTm cc)
    apply0 Nothing cc Nothing (rtm Map.! mainRef)
  where
    (<<) = flip (>>)
    uncacheable sg = CodeRep sg Uncacheable

multRec :: String
multRec =
  "let\n\
  \  n = 5\n\
  \  f acc i = match i with\n\
  \    0 -> acc\n\
  \    _ -> f (##Nat.+ acc n) (##Nat.sub i 1)\n\
  \  if (##Nat.== (f 0 1000) 5000) then () else ##bug ()"

testEval :: String -> Test ()
testEval s = testEval0 (fmap superNormalize <$> ctx) (superNormalize ll)
  where
    (ll, _, _, ctx, _) =
      lamLift mempty
        . splitPatterns builtinDataSpec
        . unannotate
        $ tm s

nested :: String
nested =
  "let\n\
  \  x = match 2 with\n\
  \        0 -> ##Nat.+ 0 1\n\
  \        m@n -> n\n\
  \  if (##Nat.== x 2) then () else ##bug ()"

matching'arguments :: String
matching'arguments =
  "let\n\
  \  f x y z = y\n\
  \  g x = f x\n\
  \  blorf = let\n\
  \    a = 0\n\
  \    b = 1\n\
  \    d = 2\n\
  \    h = g a b\n\
  \    c = 2\n\
  \    h c\n\
  \  if (##Nat.== blorf 1) then () else ##bug ()"

test :: Test ()
test =
  scope "mcode" . tests $
    [ scope "2=2" $ testEval "if (##Nat.== 2 2) then () else ##bug ()",
      scope "2=1+1" $ testEval "if (##Nat.== 2 (##Nat.+ 1 1)) then () else ##bug ()",
      scope "2=3-1" $ testEval "if (##Nat.== 2 (##Nat.sub 3 1)) then () else ##bug ()",
      scope "5*5=25" $
        testEval "if (##Nat.== (##Nat.* 5 5) 25) then () else ##bug ()",
      scope "5*1000=5000" $
        testEval "if (##Nat.== (##Nat.* 5 1000) 5000) then () else ##bug ()",
      scope "5*1000=5000 rec" $ testEval multRec,
      scope "nested" $
        testEval nested,
      scope "matching arguments" $
        testEval matching'arguments,
      malformed
    ]

-- | Loads a group as the main definition of a fresh cache.
loaded :: SuperGroup Reference Symbol -> IO (CCache ())
loaded main = do
  jit <- JIT.newJIT =<< JIT.readConfig
  cc <- baseCCache jit False
  _ <- cacheAdd [(mainRef, CodeRep main Uncacheable)] cc
  pure cc

-- | Loading the group fails with the emitter's error.
expectEmitError :: String -> SuperGroup Reference Symbol -> Test ()
expectEmitError msg main =
  io (try (loaded main)) >>= \case
    Left (e :: SomeException)
      | msg `isInfixOf` show e -> ok
      | otherwise -> crash ("refused for another reason: " ++ show e)
    Right _ -> crash "loaded; expected a refusal"

-- | Decoding the value in the cache fails with the decoder's error.
expectValueError :: String -> CCache () -> ANF.Value Reference -> Test ()
expectValueError msg cc v =
  io (try (reifyValue cc (Plain v))) >>= \case
    Left (PE _ _ doc)
      | msg `isInfixOf` Text.unpack (P.toPlain 0 doc) -> ok
      | otherwise -> crash ("refused for another reason: " ++ Text.unpack (P.toPlain 0 doc))
    Left e -> crash ("refused for another reason: " ++ show e)
    Right _ -> crash "decoded; expected a refusal"

-- | Code and values that a typechecked program never produces, refused
-- before any machine code or closure is built from them: a record literal
-- of the wrong arity, a let whose binding yields fewer values than it
-- names, an unboxed sum match away from its primitive, a decoded record
-- with too few values and a decoded partial application with too many
-- arguments. Code and values like these come from bytes a program decodes
-- (Code.deserialize, Value.deserialize).
malformed :: Test ()
malformed =
  scope "malformed" . tests $
    [ scope "a record literal of the wrong arity is refused" $
        expectEmitError "a record literal of 1 values for a shape of 2 fields" $
          Rec [] (Lambda [] (TLetD x UN (TLit (N 1)) (TApp (FRec schema) [x]))),
      scope "a let naming more values than its binding yields is refused" $
        expectEmitError "a let of 2 names whose binding yields 1 values" $
          Rec [] (Lambda [] (TLetD x UN (TLit (N 1)) (TLets (Indirect 1) [a, b] [UN, UN] (TVar x) (TVar b)))),
      scope "a function body yielding two values is refused" $
        expectEmitError "a function body that yields 2 values" $
          Rec [] (Lambda [BX] (TAbs x (ANF.TPrm ANF.LOAD [x]))),
      scope "an unboxed sum match away from its primitive is refused" $
        expectEmitError "an unboxed sum match" $
          Rec [] (Lambda [] (TLetD x UN (TLit (N 1)) (ANF.TMatch x (ANF.MatchSum (mapFromList [(0, ([BX, BX], TAbss [a, b] (TVar b)))]))))),
      scope "a decoded record with too few values is refused" do
        cc <- io (loaded (Rec [] (Lambda [] (TLetD x UN (TLit (N 1)) (TApp (FRec schema) [x, x])))))
        expectValueError "a record of 1 values for a schema of 2 fields" cc (ANF.Record schema [ANF.BLit (ANF.Text "x")]),
      scope "a decoded partial application of too many arguments is refused" do
        cc <- io (loaded (Rec [] (Lambda [] (TLit (N 1)))))
        -- main's combinator 0 takes no arguments
        expectValueError "a partial application of 1 arguments to a function of 0" cc (ANF.Partial (ANF.GR mainRef 0) [ANF.BLit (ANF.Text "x")])
    ]
  where
    x, a, b :: Symbol
    x = Var.named "x"
    a = Var.named "a"
    b = Var.named "b"
    schema = ANF.RecordSchema (Set.fromList ["f1", "f2"])
