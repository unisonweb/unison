{-# LANGUAGE OverloadedStrings #-}

-- | ADR-019 / chunk C2.3: tests for @given@-declaration validation.
--
-- A top-level binding like
--
-- > given Show.list : forall a. Show a => Show (List a) = body
--
-- typechecks under the C2.2 'ImplicitArrow' machinery: the body is
-- checked against the conclusion @Show (List a)@ after the implicit
-- @Show a@ parameter is stripped via 'checkWanted''s 'ImplicitArrow'
-- clause. C2.3 sits on top of that: when validation succeeds, a
-- 'Context.GivenDecl' info note is emitted carrying the binding's
-- variable, declared type, and stripped conclusion. 'synthesizeFile'
-- (and the UCM command surface in B2) consume these notes to mark
-- namespace metadata via 'Unison.Codebase.Givens.markGivenAt'.
--
-- Cases covered:
--
--   1. monomorphic given (no @forall@, no constraints):
--      @given f : Nat = 0@ — should /not/ emit a 'GivenDecl' note
--      (no implicit arrows in the signature).
--   2. constraint-bearing given (single @=>@):
--      @given f : C => Nat = 42@ — emits exactly one 'GivenDecl'
--      note with conclusion @Nat@.
--   3. polymorphic constraint-bearing given (the spec's running
--      example shape): @given f : forall a. C a => Nat = 42@ —
--      emits a 'GivenDecl' note whose conclusion has the @forall a.@
--      stripped (since 'Type.unImplicitArrows' looks through outer
--      'Type.Forall').
--   4. body whose type does not match the conclusion: produces a
--      type error and no 'GivenDecl' note.
module Unison.Test.Typechecker.GivenDecl (test) where

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
import Unison.Typechecker.TypeVar qualified as TypeVar
import Unison.Typechecker.Variance (defaultVariances)
import Unison.Var qualified as Var

test :: Test ()
test =
  scope "givenDecl" $
    tests
      [ scope "monomorphic-no-implicit-no-note" monomorphicNoNoteTest,
        scope "single-constraint-emits-note" singleConstraintEmitsNoteTest,
        scope "polymorphic-with-constraint-emits-note" polymorphicEmitsNoteTest,
        scope "body-mismatch-conclusion-fails" bodyMismatchTest
      ]

-- | A plain @given f : Nat = 42@ binding has no implicit arrows in
-- its declared type. C2.3 should /not/ emit a 'GivenDecl' note for
-- it — the note is gated on the presence of at least one
-- 'ImplicitArrow' in the signature.
monomorphicNoNoteTest :: Test ()
monomorphicNoNoteTest =
  let f = Var.named @Symbol "f"
      -- We desugar a top-level binding into a singleton letrec so
      -- the typechecker hits 'annotateLetRecBindings'' (where the
      -- C2.3 emission lives).
      binding = Term.ann () (Term.nat () 42) (Type.nat ())
      term =
        Term.letRec'
          True
          [(f, (), binding)]
          (constUnit ())
      notes = runClosed term
   in expectEqual 0 (length (givenDecls notes))

-- | A constraint-bearing given like @given f : C => Nat = 42@ should
-- emit exactly one 'GivenDecl' note. The conclusion type is @Nat@.
singleConstraintEmitsNoteTest :: Test ()
singleConstraintEmitsNoteTest =
  let f = Var.named @Symbol "f"
      cRef = Reference.Builtin "TestC"
      cTy = Type.ref () cRef
      -- declared: C => Nat
      declared = Type.implicitArrow () cTy (Type.nat ())
      -- body: 42 — matches the conclusion 'Nat' directly. Per
      -- ADR-019 / C2.2, checking against an 'ImplicitArrow' recurses
      -- on the conclusion.
      body = Term.nat () 42
      binding = Term.ann () body declared
      term =
        Term.letRec'
          True
          [(f, (), binding)]
          (constUnit ())
      -- ADR-010 / chunk L2 fixup: the parser tags @given@-bound vars,
      -- so this programmatic test stands in for the parser by handing
      -- the typechecker the same set directly.
      notes = runClosedWithGivens (Set.singleton f) term
      decls = givenDecls notes
   in tests
        [ scope "exactly-one-note" $ expectEqual 1 (length decls),
          scope "note-records-the-variable" $
            for_ decls $
              \(v, _d, _c) -> expectEqual f (Var.reset v),
          scope "conclusion-is-nat" $
            for_ decls $ \(_v, _d, c) ->
              expectEqual (Type.nat ()) (TypeVar.lowerType c)
        ]

-- | A polymorphic given @given f : forall a. C a => Nat = 42@ emits
-- a note whose conclusion strips both the outer 'Forall' and the
-- leading 'ImplicitArrow'.
polymorphicEmitsNoteTest :: Test ()
polymorphicEmitsNoteTest =
  let f = Var.named @Symbol "f"
      a = Var.named @Symbol "a"
      cRef = Reference.Builtin "TestC1"
      -- C a (a is the bound forall var)
      ca = Type.app () (Type.ref () cRef) (Type.var () a)
      conclusionTy = Type.nat ()
      -- declared: forall a. C a => Nat
      declared =
        Type.forAll () a (Type.implicitArrow () ca conclusionTy)
      body = Term.nat () 42
      binding = Term.ann () body declared
      term =
        Term.letRec'
          True
          [(f, (), binding)]
          (constUnit ())
      notes = runClosedWithGivens (Set.singleton f) term
      decls = givenDecls notes
   in tests
        [ scope "exactly-one-note" $ expectEqual 1 (length decls),
          scope "conclusion-strips-forall-and-implicit-arrow" $
            for_ decls $ \(_v, _d, c) ->
              expectEqual conclusionTy (TypeVar.lowerType c)
        ]

-- | A given whose body's type does /not/ match the declared
-- conclusion produces a type error. The existing 'Mismatch'
-- machinery (driven by C2.2's @=>App@ recursion into the conclusion)
-- is the error path; C2.3 inherits it without modification.
bodyMismatchTest :: Test ()
bodyMismatchTest =
  let f = Var.named @Symbol "f"
      cRef = Reference.Builtin "TestC2"
      cTy = Type.ref () cRef
      -- declared: C => Nat
      declared = Type.implicitArrow () cTy (Type.nat ())
      -- body: a Text literal — doesn't match 'Nat' conclusion.
      body = Term.text () "not a nat"
      binding = Term.ann () body declared
      term =
        Term.letRec'
          True
          [(f, (), binding)]
          (constUnit ())
      res =
        Context.synthesizeClosed
          PPE.empty
          Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled
          defaultVariances
          []
          (Map.empty :: Map.Map Reference.TermReference (Context.Type Symbol ()))
          Set.empty
          mempty
          (TypeVar.liftTerm term)
      notes = toList (Context.infoNotes res)
      errs = Context.typeErrors res
   in tests
        [ scope "at-least-one-type-error" $
            expect (not (null errs)),
          scope "no-givendecl-note-on-failure" $
            expectEqual 0 (length (givenDecls notes))
        ]

-- Helpers ---------------------------------------------------------------

-- A unit-typed body to use as the 'in' of a top-level letrec. We
-- avoid pulling in 'Unison.Builtin.Decls' here so the test stays
-- self-contained; the typechecker accepts a literal 'Nat' as the
-- letrec body for our purposes.
constUnit :: a -> Term.Term Symbol a
constUnit a = Term.nat a 0

runClosed :: Term.Term Symbol () -> [Context.InfoNote Symbol ()]
runClosed = runClosedWithGivens Set.empty

-- | Like 'runClosed' but pre-populates the typechecker's
-- 'givenBindings' set with the supplied variable names — used by tests
-- that programmatically construct a binding the parser would have
-- tagged via the @given@ keyword. (After the L2 fixup, the predicate
-- is name-based; without this set the typechecker correctly refuses
-- to emit 'GivenDecl' / 'extendLexicalGivenFromBinding' for an
-- arbitrary type-annotated binding.)
runClosedWithGivens ::
  Set.Set Symbol ->
  Term.Term Symbol () ->
  [Context.InfoNote Symbol ()]
runClosedWithGivens gbs term =
  -- 'synthesizeClosed' wants a 'Context.Term' (TypeVar-tagged).
  -- Constructed terms here use the surface 'Term.Term' so we lift
  -- via 'TypeVar.liftTerm', the same step that 'Typechecker.synthesize'
  -- performs at the public API boundary.
  let res =
        Context.synthesizeClosed
          PPE.empty
          Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled
          defaultVariances
          []
          (Map.empty :: Map.Map Reference.TermReference (Context.Type Symbol ()))
          gbs
          mempty
          (TypeVar.liftTerm term)
   in toList (Context.infoNotes res)

-- Project 'GivenDecl' notes into (var, declared, conclusion) tuples.
-- Other note variants are dropped.
givenDecls ::
  [Context.InfoNote Symbol ()] ->
  [(Symbol, Context.Type Symbol (), Context.Type Symbol ())]
givenDecls = mapMaybe' $ \case
  Context.GivenDecl _ v d c -> Just (v, d, c)
  _ -> Nothing
  where
    mapMaybe' f = foldr (\x acc -> maybe acc (: acc) (f x)) []
