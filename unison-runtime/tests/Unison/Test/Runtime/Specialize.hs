{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The specializer's pieces that run inside the runtime
-- (unison-runtime/src/Unison/Runtime/ANF/Specialize/design.md).
module Unison.Test.Runtime.Specialize where

import Control.Monad (forM_)
import EasyTest
import Unison.Reference (Reference, Reference' (Builtin))
import Unison.Runtime.ANF (Cacheability (..), Code (..))
import Unison.Runtime.ANF qualified as ANF
import Unison.Runtime.ANF.Convert (lamLift, superNormalize)
import Unison.Runtime.JIT qualified as JIT
import Unison.Runtime.Machine
  ( CCache,
    KnownLimits (..),
    baseCCache,
    cacheAdd,
    defaultKnownLimits,
    reifyValue,
    runKnown,
  )
import Unison.Runtime.Pattern (builtinDataSpec, splitPatterns)
import Unison.Runtime.Referenced (Referenced (..))
import Unison.Runtime.Stack (Val, pattern NatVal)
import Unison.Term (unannotate)
import Unison.Test.Common (tm)

-- | A fresh cache with each term loaded under its reference (and the
-- lambdas lifted out of it under references of their own).
load :: [(Reference, String)] -> IO (CCache ())
load defs = do
  jit <- JIT.newJIT =<< JIT.readConfig
  cc <- baseCCache jit False
  forM_ defs \(r, s) -> do
    let (ll, _, _, ctx, _) =
          lamLift mempty . splitPatterns builtinDataSpec . unannotate $ tm s
        groups = (r, superNormalize ll) : (fmap superNormalize <$> ctx)
    _ <- cacheAdd [(r', CodeRep g Uncacheable) | (r', g) <- groups] cc
    pure ()
  pure cc

-- | The function a reference names, as a closure of no arguments.
fun :: CCache () -> Reference -> IO Val
fun cc r =
  reifyValue cc (Plain (ANF.Partial (ANF.GR r 0) [])) >>= \case
    Right v -> pure v
    Left missing -> fail ("not loaded: " ++ show missing)

expectNat :: Maybe Val -> Word -> Test ()
expectNat (Just (NatVal n)) m | fromIntegral n == m = ok
expectNat r m = crash ("expected " ++ show m ++ ", got " ++ show r)

expectCant :: Maybe Val -> Test ()
expectCant Nothing = ok
expectCant (Just v) = crash ("ran, giving " ++ show v ++ "; expected it not to run")

sumTo, traced, readsRef, loops, fails, add2 :: Reference
sumTo = Builtin "test.sumTo"
traced = Builtin "test.traced"
readsRef = Builtin "test.readsRef"
loops = Builtin "test.loops"
fails = Builtin "test.fails"
add2 = Builtin "test.add2"

defs :: [(Reference, String)]
defs =
  [ ( sumTo,
      "n -> let\n\
      \  go acc i = match i with\n\
      \    0 -> acc\n\
      \    _ -> go (##Nat.+ acc i) (##Nat.sub i 1)\n\
      \  go 0 n"
    ),
    (traced, "n -> let\n  _ = ##Debug.trace \"n\" n\n  n"),
    -- the read is on a branch never taken, which still counts
    (readsRef, "n -> match n with\n  0 -> 0\n  _ -> ##Ref.read n"),
    (loops, "n -> let\n  go i = go (##Nat.+ i 1)\n  go n"),
    (fails, "n -> ##bug n"),
    (add2, "a b -> ##Nat.+ a b")
  ]

test :: Test ()
test =
  scope "specialize" . scope "runKnown" $ do
    cc <- io (load defs)
    let run r args = io do
          f <- fun cc r
          runKnown cc defaultKnownLimits f (NatVal <$> args)
    tests
      [ scope "a pure loop runs" $ run sumTo [1000] >>= (`expectNat` 500500),
        scope "a tracked builtin doesn't" $ run traced [1] >>= expectCant,
        scope "reading mutable state doesn't" $ run readsRef [0] >>= expectCant,
        scope "a call that fails doesn't" $ run fails [1] >>= expectCant,
        scope "a loop stops when the fuel runs out" do
          r <- io do
            f <- fun cc loops
            runKnown cc defaultKnownLimits {knownFuel = 10000} f [NatVal 0]
          expectCant r,
        scope "fewer arguments give a closure" do
          r <- io do
            f <- fun cc add2
            runKnown cc defaultKnownLimits f [NatVal 1] >>= \case
              Just g -> runKnown cc defaultKnownLimits g [NatVal 2]
              Nothing -> pure Nothing
          expectNat r 3,
        scope "all the arguments give the result" $ run add2 [1, 2] >>= (`expectNat` 3)
      ]
