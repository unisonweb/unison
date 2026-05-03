{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Type where

import EasyTest
import Unison.Hashing.V2.Convert qualified as HashingConvert
import Unison.Symbol (Symbol)
import Unison.Type
import Unison.Typechecker qualified as Typechecker
import Unison.Var qualified as Var

infixr 1 -->

(-->) :: (Ord v) => Type v () -> Type v () -> Type v ()
(-->) a b = arrow () a b

test :: Test ()
test =
  scope "type" $
    tests
      [ scope "unArrows" $
          let x = arrow () (builtin () "a") (builtin () "b") :: Type Symbol ()
           in case x of
                Arrows' [i, o] ->
                  expect (i == builtin () "a" && o == builtin () "b")
                _ -> crash "unArrows (a -> b) did not return a spine of [a,b]",
        scope "subtype" $ do
          let v = Var.named "a"
              v2 = Var.named "b"
              vt = var () v
              vt2 = var () v2
              x = forAll () v (nat () --> effect () [vt, builtin () "eff"] (nat ())) :: Type Symbol ()
              y = forAll () v2 (nat () --> effect () [vt2] (nat ())) :: Type Symbol ()
          expect . not $ Typechecker.isSubtype x y,
        -- ADR-019 / chunk C1.1: 'ImplicitArrow' is a distinct AST node
        -- from 'Arrow' and must hash distinctly (per ADR-014). The
        -- v2-hashing entry point used here is the same one the codebase
        -- uses to compute type hashes for storage.
        scope "ImplicitArrow hashes distinctly from Arrow" $ do
          let v = Var.named "x" :: Symbol
              vt = var () v :: Type Symbol ()
              b = builtin () "b"
              tArrow = forAll () v (arrow () vt b)
              tImplicit = forAll () v (implicitArrow () vt b)
              refA = HashingConvert.typeToReference tArrow
              refI = HashingConvert.typeToReference tImplicit
          expect (refA /= refI),
        -- ADR-019: a type containing 'ImplicitArrow' produces a stable
        -- hash, demonstrating the v2-hashing tokenizer handles the new
        -- constructor. Two structurally identical types must hash equal.
        scope "ImplicitArrow hashing is stable" $ do
          let v = Var.named "x" :: Symbol
              vt = var () v :: Type Symbol ()
              b = builtin () "b"
              t1 = forAll () v (implicitArrow () vt (arrow () vt b))
              t2 = forAll () v (implicitArrow () vt (arrow () vt b))
          expect (HashingConvert.typeToReference t1 == HashingConvert.typeToReference t2)
      ]
