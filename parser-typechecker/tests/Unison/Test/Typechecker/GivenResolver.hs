module Unison.Test.Typechecker.GivenResolver (test) where

import Data.List (permutations)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import EasyTest
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.Symbol (Symbol)
import Unison.Type qualified as Type
import Unison.Typechecker.GivenResolver
import Unison.Var qualified as Var

test :: Test ()
test =
  scope "given-selection" . tests $
    [ scope "no-candidate" $
        expectMissing (resolve [] nat),
      scope "exact-type" do
        expectChoice "nat" (resolve [ambient "text" text, ambient "nat" nat] nat)
        expectMissing (resolve [ambient "text" text] nat),
      scope "alias-deduplication" $
        expectChoice "nat" (resolve [ambient "nat" nat, ambient "nat" nat] nat),
      scope "innermost-binding" do
        let outer = Given (Local (Var.named "outer")) nat (Lexical 0)
            inner = Given (Local (Var.named "inner")) nat (Lexical 1)
        for_ (permutations [ambient "nat" nat, outer, inner]) \pool ->
          case resolve pool nat of
            Right name -> expectEqual (givenName inner) name
            Left _ -> crash "expected the innermost matching binding",
      scope "nonmatching-inner-binding" $
        expectChoice "nat" (resolve [ambient "nat" nat, Given (Local (Var.named "inner")) text (Lexical 2)] nat),
      scope "binding-identity" do
        let name = Var.named "same"
            freshName = Var.freshIn (Set.singleton name) name
            outer = Given (Local name) nat (Lexical 0)
            inner = Given (Local freshName) nat (Lexical 1)
        expectEqual (Var.reset name) (Var.reset freshName)
        case resolve [outer, inner] nat of
          Right chosen -> expectEqual (Local freshName) chosen
          Left _ -> crash "distinct bindings must not be collapsed by printed name",
      scope "stable-ambiguity" do
        for_ (permutations [ambient "a" nat, ambient "b" nat, ambient "a" nat]) \pool ->
          case resolve pool nat of
            Left (Ambiguous _ names) -> expectEqual [Global (Reference.Builtin "a"), Global (Reference.Builtin "b")] (NonEmpty.toList names)
            _ -> crash "expected ambiguity independent of candidate order",
      scope "rigid-variables" do
        let a = Type.var () (Var.named "a")
            b = Type.var () (Var.named "b")
        expectMissing (resolve [ambient "nat" nat] a)
        expectMissing (resolve [ambient "a" a] b)
        expectChoice "a" (resolve [ambient "a" a] a)
    ]
  where
    nat, text :: Type.Type Symbol ()
    nat = Type.builtin () "Nat"
    text = Type.builtin () "Text"
    ambient :: Text -> Type.Type Symbol () -> Given Symbol ()
    ambient name ty = Given (Global (Reference.Builtin name)) ty Ambient
    expectChoice name = \case
      Right chosen -> expectEqual (Global (Reference.Builtin name)) chosen
      Left _ -> crash "expected a unique dictionary"
    expectMissing = \case
      Left (NoGiven _) -> ok
      _ -> crash "expected no matching dictionary"
