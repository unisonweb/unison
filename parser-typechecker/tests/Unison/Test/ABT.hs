{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.ABT where

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
        scope "substsInheritAnnotation" $
          tests
            [ scope "later substitutions also reach earlier replacements" $
                expectEqual (var 0 "z") $
                  ABT.substsInheritAnnotation
                    [(symbol 0 "x", var 0 "y"), (symbol 0 "y", var 0 "z")]
                    (var 0 "x"),
              scope "replacement order remains observable" $
                expectEqual (var 0 "y") $
                  ABT.substsInheritAnnotation
                    [(symbol 0 "y", var 0 "z"), (symbol 0 "x", var 0 "y")]
                    (var 0 "x"),
              scope "duplicate keys retain sequential semantics" $
                expectEqual (var 0 "y") $
                  ABT.substsInheritAnnotation
                    [(symbol 0 "x", var 0 "y"), (symbol 0 "x", var 0 "z")]
                    (var 0 "x"),
              scope "independent substitutions avoid capture" $
                let body = ABT.abs (symbol 0 "y") (ABT.tm [var 0 "x", var 0 "y"])
                    expected = ABT.abs (symbol 0 "fresh") (ABT.tm [var 0 "y", var 0 "fresh"])
                 in expectEqual expected $ ABT.substsInheritAnnotation [(symbol 0 "x", var 0 "y")] body,
              scope "dependent substitutions avoid capture with sequential result" $
                let body = ABT.abs (symbol 0 "x") (var 0 "y")
                    expected = ABT.abs (symbol 0 "fresh") (var 0 "z")
                 in expectEqual expected $
                      ABT.substsInheritAnnotation
                        [(symbol 0 "y", var 0 "x"), (symbol 0 "x", var 0 "z")]
                        body
            ]
      ]
  where
    symbol i n = Symbol i (Var.User n)
    var i n = ABT.var $ symbol i n
