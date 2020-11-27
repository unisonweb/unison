{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Test.Codebase where

import Data.Functor.Identity
import Data.Map ((!))
import qualified Data.Map as Map
import EasyTest
import qualified Unison.Codebase as Codebase
import Unison.Codebase.CodeLookup (CodeLookup (..))
import qualified Unison.Hash as Hash
import qualified Unison.Reference as R
import Unison.Symbol (Symbol)
import qualified Unison.Term as Term
import qualified Unison.UnisonFile as UF
import qualified Unison.Var as Var

test :: Test ()
test =
  scope "codebase" $
    tests
      [ scope "makeSelfContained" $
          let h = Hash.unsafeFromBase32Hex "abcd"
              ref = R.Derived h 0 1
              v1 = Var.refNamed @Symbol ref
              foo = Var.named "foo"
              -- original binding: `foo = \v1 -> ref`
              binding = (foo, Term.lam () v1 (Term.ref () ref))
              uf = UF.UnisonFileId mempty mempty [binding] mempty
              code :: CodeLookup Symbol Identity ()
              code =
                CodeLookup
                  { getTerm = \rid ->
                      pure $
                        if R.DerivedId rid == ref
                          then Just (Term.int () 42)
                          else Nothing,
                    getTypeDeclaration = \_ -> pure Nothing
                  }
              -- expected binding after makeSelfContained: `foo = \v1 -> v2`, where `v2 /= v1`
              UF.UnisonFile _ _ (Map.fromList -> bindings) _ = runIdentity $ Codebase.makeSelfContained' code uf
              Term.LamNamed' _ (Term.Var' v2) = bindings ! foo
           in expect $ v2 /= v1
      ]
