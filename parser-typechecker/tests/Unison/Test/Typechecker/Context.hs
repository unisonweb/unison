{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Typechecker.Context (test) where

import Data.Foldable (for_, toList)
import Data.Map qualified as Map
import Data.Set qualified as Set
import EasyTest
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference qualified as Reference
import Unison.Symbol (Symbol)
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Typechecker.TypeVar qualified as TypeVar
import Unison.Typechecker.Variance
import Unison.Var qualified as Var

test :: Test ()
test =
  scope "context" $
    tests
      [ scope "verifyClosedTerm" verifyClosedTermTest,
        scope "lexicalGivensThreading" lexicalGivensThreadingTest,
        scope "constraintGoalEmission" constraintGoalEmissionTest
      ]

type TV = Context.TypeVar Symbol ()

verifyClosedTermTest :: Test ()
verifyClosedTermTest =
  tests
    [ scope "report-all-free-vars" $
        let a = Var.named @Symbol "a"
            b = Var.named @Symbol "b"
            a' = Var.named @TV "a'"
            b' = Var.named @TV "b'"
            -- (a : a')(b : b')
            t =
              Term.app
                ()
                (Term.ann () (Term.var () a) (Type.var () a'))
                (Term.ann () (Term.var () b) (Type.var () b'))
            res = Context.synthesizeClosed PPE.empty Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Enabled defaultVariances [] Map.empty Set.empty mempty t
            errors = Context.typeErrors res
            expectUnknownSymbol (Context.ErrorNote cause _) = case cause of
              Context.UnknownSymbol _ _ -> ok
              e -> crash $ "Unexpected type error " <> show e
         in do
              expectEqual 4 (length errors) -- there are 4 unknown symbols: a, a', b, b'
              for_ errors expectUnknownSymbol
    ]

-- | Confirms the lexical given environment is threaded through
-- nested binding forms ('Lam', 'Let1') without disturbing the
-- typechecker's result. Two expectations:
--
--   1. Typechecking a nested term (a let-binding whose RHS is a
--      lambda, then applied to a literal) succeeds — exercises the
--      'withLexicalGivens' wrappers in the 'Term.Let1Top'' and
--      'Term.Lam'' arms of 'synthesizeWanted' without semantic
--      effect.
--
--   2. Passing a non-empty initial givens map produces the same
--      outcome as passing 'Map.empty'.
--
-- The match-arm and letrec wrappers are exercised indirectly by the
-- existing typechecker test suite — every transcript with a 'match'
-- or mutual recursion crosses 'withLexicalGivens'.
lexicalGivensThreadingTest :: Test ()
lexicalGivensThreadingTest =
  tests
    [ scope "typechecks-with-empty-givens" $
        case run Map.empty of
          Right _ -> ok
          Left err -> crash err,
      scope "typechecks-with-nonempty-givens" $
        -- A non-empty givens map should be accepted. We verify that
        -- 'synthesizeClosed' with a populated initial env produces
        -- the same outcome as with an empty one.
        let nonEmpty =
              Map.singleton
                -- Any builtin reference works as a placeholder key;
                -- we only care that the field is observed and saved
                -- across binders.
                Type.intRef
                (Type.int ())
         in case run nonEmpty of
              Right _ -> ok
              Left err -> crash err
    ]
  where
    -- Term: @let f = (n -> n) in f 42@.
    --
    -- The let-binding body forces 'Term.Let1Top'' synthesis (which
    -- now wraps its body in 'withLexicalGivens'); the lambda forces
    -- the 'Term.Lam'' arm (also wrapped).
    --
    -- We deliberately use a 'Nat' literal rather than a richer
    -- construction so the test does not depend on builtin
    -- lookups — 'mempty' below is a 'TypeLookup' with no decls. The
    -- kind-inference pass requires data decls, so we disable it via
    -- the 'Disabled' switch.
    run givens =
      let n = Var.named @Symbol "n"
          f = Var.named @Symbol "f"
          fortyTwo = Term.nat () 42
          inner = Term.lamWithoutBindingAnns () [n] (Term.var () n)
          term =
            Term.let1'
              False
              [(f, inner)]
              (Term.app () (Term.var () f) fortyTwo)
          res =
            Context.synthesizeClosed
              PPE.empty
              Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled
              defaultVariances
              []
              givens
              Set.empty
              mempty
              term
       in case Context.typeErrors res of
            errs
              | null errs -> Right ()
              | otherwise ->
                  Left ("unexpected type errors: " <> show (length errs))

-- | Confirms inference emits a 'ConstraintGoal' info note for each
-- 'ImplicitArrow' parameter at an apply-site, with the right type
-- and the lexical given environment captured into the goal's scope
-- snapshot.
--
-- Strategy: register a builtin term reference @f@ in the 'TypeLookup'
-- whose declared type is @C => Nat -> Nat@ (a single implicit
-- parameter, then a regular arrow). Synthesize the closed term @f 42@.
-- Inspect the emitted 'InfoNote's:
--
--   1. exactly one 'ConstraintGoal' note appears (one implicit slot);
--   2. its 'goalType' equals the constraint type @C@;
--   3. with a non-empty initial @lexicalGivens@ map, the goal's
--      'goalScope' captures it faithfully.
--
-- Multi-constraint signatures @(C1, C2) =>@ desugar to nested
-- 'ImplicitArrow'; the same emission code recurses, so a
-- two-implicit case is exercised by the existing fixed-point on the
-- conclusion. We test the single-implicit case here for clarity.
constraintGoalEmissionTest :: Test ()
constraintGoalEmissionTest =
  tests
    [ scope "single-implicit-emits-one-goal" $
        let notes = runWith Map.empty
            goals = constraintGoals notes
         in do
              expectEqual 1 (length goals)
              for_ goals $ \(_, ty, _) ->
                -- Lower the captured 'Context.Type' back to a surface
                -- 'Type.Type Symbol ()' so we can compare with the
                -- declared constraint type.
                expectEqual constraintTy (TypeVar.lowerType ty),
      scope "captures-lexical-givens-snapshot" $
        -- Seed givens at the surface 'Type.Type' level for clarity,
        -- then lift to 'Context.Type' for 'runWith' (the typechecker
        -- internally uses 'TypeVar' annotations).
        let surfaceGivens =
              Map.singleton (Reference.Builtin "Snapshot.K") snapshotTy
            givens = TypeVar.liftType <$> surfaceGivens
            notes = runWith givens
            goals = constraintGoals notes
         in do
              expectEqual 1 (length goals)
              for_ goals $ \(_, _, scopeSnap) ->
                -- Lower each captured type to surface form for the
                -- structural comparison; the keys are unaffected.
                expectEqual surfaceGivens (TypeVar.lowerType <$> scopeSnap),
      scope "no-implicit-no-goals" $
        -- Sanity check: a function with a plain 'Arrow' type produces
        -- zero 'ConstraintGoal' notes.
        let notes = runPlain
            goals = constraintGoals notes
         in expectEqual 0 (length goals)
    ]
  where
    -- Builtin "C" stands in for a class type at the test level; we
    -- never need a real declaration since the typechecker only
    -- reasons about the type structurally for goal emission.
    constraintRef = Reference.Builtin "TestC"
    constraintTy = Type.ref () constraintRef
    snapshotTy = Type.int ()
    -- f : C => Nat -> Nat
    fRef = Reference.Builtin "TestImplicitFn"
    fType =
      Type.implicitArrow
        ()
        constraintTy
        (Type.arrow () (Type.nat ()) (Type.nat ()))
    -- g : Nat -> Nat (no implicit)
    gRef = Reference.Builtin "TestPlainFn"
    gType = Type.arrow () (Type.nat ()) (Type.nat ())

    typeLookup :: TL.TypeLookup Symbol ()
    typeLookup =
      mempty
        { TL.typeOfTerms =
            Map.fromList
              [ (fRef, fType),
                (gRef, gType)
              ]
        }

    -- 'synthesizeClosed' takes the givens map at the typechecker's
    -- internal 'Context.Type' shape (which is
    -- @Type.Type (TypeVar v loc) loc@). Surface-form types must be
    -- lifted via 'TypeVar.liftType' before passing. We let inference
    -- pick the var when 'givens' is 'Map.empty'.
    runWith ::
      Map.Map Reference.TermReference (Context.Type Symbol ()) ->
      [Context.InfoNote Symbol ()]
    runWith givens =
      let term = Term.app () (Term.ref () fRef) (Term.nat () 42)
          res =
            Context.synthesizeClosed
              PPE.empty
              Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled
              defaultVariances
              []
              givens
              Set.empty
              typeLookup
              term
       in toList (Context.infoNotes res)

    runPlain :: [Context.InfoNote Symbol ()]
    runPlain =
      let term = Term.app () (Term.ref () gRef) (Term.nat () 42)
          res =
            Context.synthesizeClosed
              PPE.empty
              Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled
              defaultVariances
              []
              (Map.empty :: Map.Map Reference.TermReference (Context.Type Symbol ()))
              Set.empty
              typeLookup
              term
       in toList (Context.infoNotes res)

    -- Project the ConstraintGoal notes into a tuple form for
    -- inspection. Other note variants are dropped.
    --
    -- Note: 'ConstraintGoal' carries 'Context.Type' (which is
    -- 'Type.Type (TypeVar v loc) loc') rather than the surface
    -- 'Type.Type v loc'. We keep the internal form here since the
    -- typechecker has not yet generalized.
    constraintGoals ::
      [Context.InfoNote Symbol ()] ->
      [((), Context.Type Symbol (), Map.Map Reference.TermReference (Context.Type Symbol ()))]
    constraintGoals = mapMaybe' $ \case
      Context.ConstraintGoal l ty sc -> Just (l, ty, sc)
      _ -> Nothing
      where
        mapMaybe' f = foldr (\x acc -> maybe acc (: acc) (f x)) []
