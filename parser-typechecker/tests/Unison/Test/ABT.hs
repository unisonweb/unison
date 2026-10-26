{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.ABT where

import Control.Monad (replicateM_)
import Data.Set as Set
import EasyTest
import Unison.ABT as ABT
import Unison.Symbol (Symbol (..))
import Unison.Var as Var

test :: Test ()
test =
  scope "abt" $
    tests
      [ scope "freshInBoth" $
          let t1 = var 1 "a"
              t2 = var 0 "a"
              fresh = ABT.freshInBoth t1 t2 $ symbol 0 "a"
           in tests
                [ scope "first" $ expect (not $ Set.member fresh (ABT.freeVars t1)),
                  scope "second" $ expect (not $ Set.member fresh (ABT.freeVars t2))
                ],
        scope "rename" $ do
          -- rename x to a in \a  -> [a, x] should yield
          --                  \a1 -> [a1, a]
          let t1 = ABT.abs (symbol 0 "a") (ABT.tm [var 0 "a", var 0 "x"])
              t2 = ABT.rename (symbol 0 "x") (symbol 0 "a") t1
              fvs = toList . ABT.freeVars $ t2
          -- make sure the variable wasn't captured
          expectEqual fvs [symbol 0 "a"]
          -- make sure the resulting term is alpha equiv to \a1 -> [a1, a]
          expectEqual t2 (ABT.abs (symbol 0 "b") (ABT.tm [var 0 "b", var 0 "a"])),
        scope "substsInheritAnnotation" substsTest
      ]
  where
    symbol i n = Symbol i (Var.User n)
    var i n = ABT.var $ symbol i n

-- `substsInheritAnnotation` substitutes in one pass what substituting its
-- replacements one at a time, in order, would: the same term, up to the
-- names of binders renamed to avoid capture.
substsTest :: Test ()
substsTest =
  tests
    [ scope "later replacements apply inside earlier ones" $
        -- [x := [y], y := [z]] on [x, y] is [[[z]], [z]]: the y free in x's
        -- replacement is replaced too
        expectSame [(x, tm [vy]), (y, tm [vz])] (tm [vx, vy]),
      scope "earlier replacements apply to the term" $
        -- [y := [z], x := [y]] on [x, y] is [[y], [z]]: x's replacement
        -- arrives after y's substitution is done
        expectSame [(y, tm [vz]), (x, tm [vy])] (tm [vx, vy]),
      scope "the first of two replacements for a variable takes the term" $
        -- [x := [x, y], x := [z]] on [x] is [[[z], y]]
        expectSame [(x, tm [vx, vy]), (x, tm [vz])] (tm [vx]),
      scope "a binder of a replaced variable shadows it" $
        expectSame [(x, tm [vy])] (abs x (tm [vx])),
      scope "a binder is renamed rather than capturing" $ do
        -- [x := [a]] on \a -> [x, a] is \a1 -> [[a], a1]
        let t = abs a (tm [vx, va])
            t' = substsInheritAnnotation [(x, tm [va])] t
        expectEqual (ABT.freeVars t') (Set.singleton a)
        expectEqual t' (abs b (tm [tm [va], vb]))
        expectSame [(x, tm [va])] t,
      scope "a binder is renamed only for a replacement that reaches its body" $ do
        -- [x := [a], y := [b]] on [\a -> [x, a], \b -> [b, z], y]: the
        -- binder a is renamed for x's replacement, and the binder b, which
        -- no replacement reaches, is not
        let t = tm [abs a (tm [vx, va]), abs b (tm [vb, vz]), vy]
            t' = substsInheritAnnotation [(x, tm [va]), (y, tm [vb])] t
        expectEqual (ABT.freeVars t') (Set.fromList [a, b, z])
        expectSame [(x, tm [va]), (y, tm [vb])] t,
      scope "a wide term" $ do
        -- a left-nested chain of a thousand distinct variables, each replaced
        -- (the shape that is quadratic one replacement at a time)
        let vs = [symbol i "v" | i <- [1 .. 1000]]
            chain = Prelude.foldl (\acc v -> tm [acc, var v]) vx vs
            rs = [(v, tm []) | v <- vs]
            t' = substsInheritAnnotation rs chain
        expectEqual (ABT.freeVars t') (Set.singleton x)
        expectSame rs chain,
      scope "random" . replicateM_ 500 $ do
        t <- randomTerm 4
        n <- int' 0 4
        rs <- listOf n (pair randomVar (randomTerm 2))
        expectSame rs t
    ]
  where
    -- the one-at-a-time substitution the single pass must agree with
    sequential :: [(Symbol, Term [] Symbol ())] -> Term [] Symbol () -> Term [] Symbol ()
    sequential rs t = Prelude.foldr (uncurry substInheritAnnotation) t (reverse rs)
    expectSame :: [(Symbol, Term [] Symbol ())] -> Term [] Symbol () -> Test ()
    expectSame rs t = do
      let fast = substsInheritAnnotation rs t
          slow = sequential rs t
      -- `==` is alpha-equivalence
      expectEqual fast slow
      expectEqual (ABT.freeVars fast) (ABT.freeVars slow)
    symbol i n = Symbol i (Var.User n)
    var :: Symbol -> Term [] Symbol ()
    var = ABT.var
    tm :: [Term [] Symbol ()] -> Term [] Symbol ()
    tm = ABT.tm
    abs :: Symbol -> Term [] Symbol () -> Term [] Symbol ()
    abs = ABT.abs
    x = symbol 0 "x"
    y = symbol 0 "y"
    z = symbol 0 "z"
    a = symbol 0 "a"
    b = symbol 0 "b"
    (vx, vy, vz, va, vb) = (var x, var y, var z, var a, var b)
    randomVar = pick [x, y, z, a, b]
    randomTerm :: Int -> Test (Term [] Symbol ())
    randomTerm 0 = var <$> randomVar
    randomTerm depth = do
      k <- int' 0 3
      case k of
        0 -> var <$> randomVar
        1 -> abs <$> randomVar <*> randomTerm (depth - 1)
        _ -> do
          n <- int' 0 3
          tm <$> listOf n (randomTerm (depth - 1))
