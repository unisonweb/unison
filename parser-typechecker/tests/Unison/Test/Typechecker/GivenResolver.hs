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
    [ scope "candidate-type-decomposition" do
        let a = Var.named "a"
            va = Type.var () a
            candidate =
              givenFromType
                (Global (Reference.Builtin "generic"))
                Ambient
                (Type.forAll () a (Type.implicitArrow () (app "C" va) (app "R" va)))
        expectChoice "generic" (resolve [candidate, ambient "cNat" (app "C" nat)] (app "R" nat))
        expectMissing (resolve [candidate, ambient "cText" (app "C" text)] (app "R" nat)),
      scope "shadowed-quantifier-is-not-the-same-variable" do
        let a = Var.named "a"
            va = Type.var () a
            candidate =
              givenFromType
                (Global (Reference.Builtin "generic"))
                Ambient
                (Type.forAll () a (Type.implicitArrow () (app "C" va) (Type.forAll () a (app "R" va))))
        expectEqual 2 (Set.size (Set.fromList (givenTyVars candidate)))
        expectMissing (resolve [candidate, ambient "cNat" (app "C" nat)] (app "R" nat)),
      scope "quantifiers-between-premises" do
        let a = Var.named "a"
            b = Var.named "b"
            va = Type.var () a
            vb = Type.var () b
            pair x y = Type.app () (app "Pair" x) y
            candidate =
              givenFromType
                (Global (Reference.Builtin "generic"))
                Ambient
                ( Type.forAll
                    ()
                    a
                    ( Type.implicitArrow
                        ()
                        (app "C" va)
                        (Type.forAll () b (Type.implicitArrow () (app "D" vb) (pair va vb)))
                    )
                )
        expectChoice "generic" (resolve [candidate, ambient "cNat" (app "C" nat), ambient "dText" (app "D" text)] (pair nat text)),
      scope "no-candidate" $
        expectMissing (resolve [] nat),
      scope "exact-type" do
        expectChoice "nat" (resolve [ambient "text" text, ambient "nat" nat] nat)
        expectMissing (resolve [ambient "text" text] nat),
      scope "alias-deduplication" $
        expectChoice "nat" (resolve [ambient "nat" nat, ambient "nat" nat] nat),
      scope "innermost-binding" do
        let outer = Given (Local (Var.named "outer")) [] [] nat (Lexical 0)
            inner = Given (Local (Var.named "inner")) [] [] nat (Lexical 1)
        for_ (permutations [ambient "nat" nat, outer, inner]) \pool ->
          case resolve pool nat of
            Right name -> expectEqual (givenName inner) (resolvedGiven name)
            Left _ -> crash "expected the innermost matching binding",
      scope "implicit-parameter-between-source-scopes" do
        let outer = Given (Local (Var.named "outer")) [] [] nat (Lexical 0)
            parameter = Given (Local (Var.named "parameter")) [] [] nat (Parameter 0 1)
            later = Given (Local (Var.named "later")) [] [] nat (Parameter 0 2)
            inner = Given (Local (Var.named "inner")) [] [] nat (Lexical 1)
            picks expected pool = case resolve pool nat of
              Right chosen -> expectEqual (givenName expected) (resolvedGiven chosen)
              Left _ -> crash "expected lexical precedence"
        for_ (permutations [outer, parameter]) (picks parameter)
        for_ (permutations [outer, parameter, later]) (picks later)
        for_ (permutations [outer, parameter, later, inner]) (picks inner),
      scope "nonmatching-inner-binding" $
        expectChoice "nat" (resolve [ambient "nat" nat, Given (Local (Var.named "inner")) [] [] text (Lexical 2)] nat),
      scope "binding-identity" do
        let name = Var.named "same"
            freshName = Var.freshIn (Set.singleton name) name
            outer = Given (Local name) [] [] nat (Lexical 0)
            inner = Given (Local freshName) [] [] nat (Lexical 1)
        expectEqual (Var.reset name) (Var.reset freshName)
        case resolve [outer, inner] nat of
          Right chosen -> expectEqual (Local freshName) (resolvedGiven chosen)
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
        expectChoice "a" (resolve [ambient "a" a] a),
      scope "polymorphic-candidates" do
        let a = Var.named "a"
            b = Var.named "b"
            va = Type.var () a
            vb = Type.var () b
            pair x y = Type.app () (Type.app () (Type.builtin () "Pair") x) y
            generic = Given (Global (Reference.Builtin "generic")) [a] [] (pair va va) Ambient
        expectChoice "generic" (resolve [generic] (pair nat nat))
        expectMissing (resolve [generic] (pair nat text))
        expectChoice "generic" (resolve [generic] (pair vb vb))
        expectChoice "generic" (resolve [generic] (pair va va))
        expectMissing (resolve [generic] (pair va vb)),
      scope "higher-kinded-pattern" do
        let f = Var.named "f"
            a = Var.named "a"
            patternType = Type.app () (Type.var () f) (Type.var () a)
            generic = Given (Global (Reference.Builtin "generic")) [f, a] [] patternType Ambient
        expectChoice "generic" (resolve [generic] (Type.app () (Type.builtin () "List") nat)),
      scope "alpha-equivalent-bound-variables" do
        let a = Var.named "a"
            b = Var.named "b"
            identity v = Type.forAll () v (Type.arrow () (Type.var () v) (Type.var () v))
        expectChoice "identity" (resolve [ambient "identity" (identity a)] (identity b)),
      scope "bound-variables-cannot-escape" do
        let a = Var.named "a"
            b = Var.named "b"
            va = Type.var () a
            vb = Type.var () b
            quantified x y = Type.forAll () b (Type.arrow () x y)
            generic = Given (Global (Reference.Builtin "generic")) [a] [] (quantified va vb) Ambient
        expectMissing (resolve [generic] (quantified vb vb))
        expectChoice "generic" (resolve [generic] (quantified nat vb)),
      scope "premises-share-determined-types" do
        let a = Var.named "a"
            va = Type.var () a
            root = Given (Global (Reference.Builtin "root")) [a] [app "C" va, app "D" va] (app "R" va) Ambient
            c = ambient "cNat" (app "C" nat)
            d = ambient "dText" (app "D" text)
        expectMissing (resolve [root, c, d] (app "R" va))
        expectMissing (resolve [root, c, d] (app "R" nat))
        case resolve [root, c, ambient "dNat" (app "D" nat)] (app "R" nat) of
          Right tree -> expectEqual [Global (Reference.Builtin "cNat"), Global (Reference.Builtin "dNat")] (map resolvedGiven (resolvedPremises tree))
          Left _ -> crash "expected the two consistent premises to resolve",
      scope "rigid-goal-is-not-a-cycle" do
        let a = Var.named "a"
            va = Type.var () a
            poly = Given (Global (Reference.Builtin "poly")) [a] [app "C" nat] (app "C" va) Ambient
        expectChoice "poly" (resolve [poly, ambient "base" (app "C" nat)] (app "C" va)),
      scope "cycles-do-not-hide-ambiguity" do
        let ty = Type.builtin ()
            edge name premise conclusion = Given (Global (Reference.Builtin name)) [] [ty premise] (ty conclusion) Ambient
            pool = [ambient "B_base" (ty "B"), ambient "C_base" (ty "C"), edge "A_from_B" "B" "A", edge "A_from_C" "C" "A", edge "B_from_C" "C" "B", edge "C_from_B" "B" "C"]
        for_ (permutations pool) \ordered -> expectMissing (resolve ordered (ty "A")),
      scope "cycle-pruned-per-branch" do
        let self = Given (Global (Reference.Builtin "self")) [] [nat] nat Ambient
        expectChoice "base" (resolve [self, ambient "base" nat] nat),
      scope "undetermined-premise-variable" do
        let a = Var.named "a"
            generic = Given (Global (Reference.Builtin "generic")) [a] [Type.var () a] text Ambient
        expectMissing (resolve [generic, ambient "nat" nat] text),
      scope "depth-limit-is-inconclusive" do
        let a = Var.named "a"
            va = Type.var () a
            grow = Given (Global (Reference.Builtin "grow")) [a] [app "C" (app "List" va)] (app "C" va) Ambient
        case resolveWith (Limits 5 1000) [grow, ambient "base" (app "C" nat)] (app "C" nat) of
          Left (SearchLimit _) -> ok
          _ -> crash "a search limit cannot establish that base is the unique winner",
      scope "work-limit-and-aliases" do
        let aliases = replicate 20 (ambient "base" nat)
        expectChoice "base" (resolveWith (Limits 5 1) aliases nat)
        case resolveWith (Limits 5 0) aliases nat of
          Left (SearchLimit _) -> ok
          _ -> crash "expected the work limit",
      scope "shadowed-search-is-irrelevant" do
        let a = Var.named "a"
            va = Type.var () a
            grow = Given (Global (Reference.Builtin "grow")) [a] [app "C" (app "List" va)] (app "C" va) Ambient
            local = Given (Local a) [] [] (app "C" nat) (Lexical 0)
        case resolveWith (Limits 5 1) [grow, local] (app "C" nat) of
          Right tree -> expectEqual (Local a) (resolvedGiven tree)
          Left _ -> crash "a matching local dictionary must shadow the unbounded ambient search",
      scope "more-specific-declared-conclusion" do
        let a = Var.named "a"
            va = Type.var () a
            generic = Given (Global (Reference.Builtin "generic")) [a] [] (app "C" va) Ambient
            concrete = ambient "concrete" (app "C" nat)
        for_ (permutations [generic, concrete]) \pool ->
          expectChoice "concrete" (resolve pool (app "C" nat)),
      scope "equivalent-patterns-stay-ambiguous" do
        let a = Var.named "a"
            b = Var.named "b"
            generic name v = Given (Global (Reference.Builtin name)) [v] [] (app "C" (Type.var () v)) Ambient
        case resolve [generic "first" a, generic "second" b] (app "C" nat) of
          Left (Ambiguous _ names) -> expectEqual 2 (length names)
          _ -> crash "alpha-equivalent patterns must tie",
      scope "incomparable-patterns-stay-ambiguous" do
        let a = Var.named "a"
            va = Type.var () a
            pair x y = Type.app () (app "Pair" x) y
            repeated = Given (Global (Reference.Builtin "repeated")) [a] [] (pair va va) Ambient
            fixed = Given (Global (Reference.Builtin "fixed")) [a] [] (pair va nat) Ambient
        case resolve [repeated, fixed] (pair nat nat) of
          Left (Ambiguous _ _) -> ok
          _ -> crash "overlapping patterns need not be ordered by specificity",
      scope "lexical-scope-precedes-specificity" do
        let a = Var.named "a"
            generic = Given (Local a) [a] [] (app "C" (Type.var () a)) (Lexical 0)
        case resolve [generic, ambient "concrete" (app "C" nat)] (app "C" nat) of
          Right tree -> expectEqual (Local a) (resolvedGiven tree)
          Left _ -> crash "the local generic dictionary must shadow the ambient concrete one",
      scope "exponentially-growing-goals" do
        let a = Var.named "a"
            va = Type.var () a
            pair = Type.app () (app "Pair" va) va
            grow = Given (Global (Reference.Builtin "grow")) [a] [app "C" pair] (app "C" va) Ambient
        case resolve [grow] (app "C" nat) of
          Left (SearchLimit chain) -> expect (length chain < 15)
          _ -> crash "goal size must be bounded independently of recursion depth"
    ]
  where
    nat, text :: Type.Type Symbol ()
    nat = Type.builtin () "Nat"
    text = Type.builtin () "Text"
    app name = Type.app () (Type.builtin () name)
    ambient :: Text -> Type.Type Symbol () -> Given Symbol ()
    ambient name ty = Given (Global (Reference.Builtin name)) [] [] ty Ambient
    expectChoice name = \case
      Right chosen -> expectEqual (Global (Reference.Builtin name)) (resolvedGiven chosen)
      Left _ -> crash "expected a unique dictionary"
    expectMissing = \case
      Left (NoGiven _) -> ok
      _ -> crash "expected no matching dictionary"
