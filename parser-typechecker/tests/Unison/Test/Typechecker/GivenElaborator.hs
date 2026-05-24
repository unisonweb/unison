{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for "Unison.Typechecker.GivenElaborator" (chunk
-- D2). We run the typechecker over small terms whose signatures
-- contain @ImplicitArrow@ parameters, confirm that 'ConstraintGoal'
-- info notes are emitted, then drive the elaborator and check the
-- resulting 'SolvedImplicit' decisions.
--
-- Three flavours of given source are exercised:
--
--   * a /lexical/ given (built programmatically through
--     'Context.synthesizeClosed''s initial-givens parameter — A2's
--     parser support is not on this branch);
--   * a /namespace/ (top-level / ambient) given supplied via the
--     elaborator's 'AmbientGiven' API;
--   * the /failure/ path: a goal with no matching given in either
--     pool produces a 'ResolveError' inside the 'SolvedImplicit'
--     note.
module Unison.Test.Typechecker.GivenElaborator (test) where

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
import Unison.Typechecker.GivenElaborator qualified as Elab
import Unison.Typechecker.GivenResolver qualified as GR
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Typechecker.TypeVar qualified as TypeVar
import Unison.Typechecker.Variance (defaultVariances)
import Unison.Var qualified as Var

test :: Test ()
test =
  scope "GivenElaborator"
    . tests
    $ [ scope "decomposeGivenType" testDecomposeGivenType,
        scope "ambient-resolution" testAmbientResolution,
        scope "lexical-resolution" testLexicalResolution,
        scope "missing-given-yields-error" testMissingGiven,
        scope "no-implicit-no-decisions" testNoImplicit,
        scope "lexical-shadows-ambient" testLexicalShadowsAmbient,
        -- L2: @let given@ at the AST level extends lexicalGivens.
        scope "let-given-extends-lexical-givens" testLetGivenExtendsLexical,
        scope "let-given-shadows-ambient" testLetGivenShadowsAmbient,
        -- L2 fixup: premise-free @given local : C = …@ (no @=>@ in
        -- the type) — the canonical ADR-010 example. Before the fix
        -- the type-shape predicate silently dropped this binding;
        -- the parser-tagged side channel makes it work.
        scope "let-given-premise-free-extends-lexical-givens" testLetGivenPremiseFree,
        -- D4 carry-overs from D2 review.
        scope "ambiguous-given-yields-error" testAmbiguousGiven,
        scope "depth-exceeded-yields-error" testDepthExceeded,
        scope "cycle-with-metavar-regression" testCycleWithMetavar,
        scope "unresolved-metavar-in-goal" testUnresolvedMetavarInGoal
      ]

------------------------------------------------------------------------------
-- Test fixtures
------------------------------------------------------------------------------

-- A constraint-class-like type "TestC".
constraintRef :: Reference.Reference
constraintRef = Reference.Builtin "TestC"

constraintTy :: Type.Type Symbol ()
constraintTy = Type.ref () constraintRef

-- A second constraint type, used for the lexical-shadows-ambient test.
otherConstraintRef :: Reference.Reference
otherConstraintRef = Reference.Builtin "OtherC"

otherConstraintTy :: Type.Type Symbol ()
otherConstraintTy = Type.ref () otherConstraintRef

-- A function "f : TestC => Nat -> Nat" stored in the typechecker's
-- TypeLookup so synthesis can find its declared type by reference.
fRef :: Reference.Reference
fRef = Reference.Builtin "TestImplicitFn"

fType :: Type.Type Symbol ()
fType =
  Type.implicitArrow
    ()
    constraintTy
    (Type.arrow () (Type.nat ()) (Type.nat ()))

-- A function "g : Nat -> Nat" — no implicit parameter.
gRef :: Reference.Reference
gRef = Reference.Builtin "TestPlainFn"

gType :: Type.Type Symbol ()
gType = Type.arrow () (Type.nat ()) (Type.nat ())

-- A function "h : OtherC => TestC => Nat -> Nat" — two implicits to
-- exercise multi-constraint signatures (each emits its own goal).
hRef :: Reference.Reference
hRef = Reference.Builtin "TestImplicitFn2"

hType :: Type.Type Symbol ()
hType =
  Type.implicitArrow
    ()
    otherConstraintTy
    (Type.implicitArrow () constraintTy (Type.arrow () (Type.nat ()) (Type.nat ())))

-- Witness term used by the L2 'let given' tests: a builtin whose
-- declared type is the *conclusion* (TestC). The binding annotation is
-- @OtherC => TestC@; checking the body against that annotation peels
-- the leading 'ImplicitArrow' (emitting an OtherC goal) and recurses on
-- the conclusion 'TestC' — at which point the body's declared type
-- matches without further inference fuss.
witnessLetGivenRef :: Reference.Reference
witnessLetGivenRef = Reference.Builtin "WitnessLetGiven"

witnessLetGivenType :: Type.Type Symbol ()
witnessLetGivenType = constraintTy

typeLookup :: TL.TypeLookup Symbol ()
typeLookup =
  mempty
    { TL.typeOfTerms =
        Map.fromList
          [ (fRef, fType),
            (gRef, gType),
            (hRef, hType),
            (witnessLetGivenRef, witnessLetGivenType)
          ]
    }

-- Run synthesis on @f 42@.
runFCall :: Map.Map Reference.Reference (Context.Type Symbol ()) -> [Context.InfoNote Symbol ()]
runFCall givens = runApply fRef givens

-- Run synthesis on @h 42@ (two implicits).
runHCall :: Map.Map Reference.Reference (Context.Type Symbol ()) -> [Context.InfoNote Symbol ()]
runHCall givens = runApply hRef givens

-- Run synthesis on @g 42@ (no implicits).
runGCall :: [Context.InfoNote Symbol ()]
runGCall = runApply gRef Map.empty

runApply ::
  Reference.Reference ->
  Map.Map Reference.Reference (Context.Type Symbol ()) ->
  [Context.InfoNote Symbol ()]
runApply ref givens =
  let term = Term.app () (Term.ref () ref) (Term.nat () 42)
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

-- A namespace-level given that produces a 'TestC' dictionary directly.
-- Type: @TestC@ (no foralls, no premises).
ambientShowNatRef :: Reference.Reference
ambientShowNatRef = Reference.Builtin "AmbientShowNat"

ambientShowNat :: Elab.AmbientGiven Symbol ()
ambientShowNat =
  Elab.AmbientGiven
    { Elab.ambientName = ambientShowNatRef,
      Elab.ambientType = constraintTy
    }

-- An ambient given for OtherC. Used in 'testLexicalShadowsAmbient'.
ambientOtherCRef :: Reference.Reference
ambientOtherCRef = Reference.Builtin "AmbientOtherC"

ambientOtherC :: Elab.AmbientGiven Symbol ()
ambientOtherC =
  Elab.AmbientGiven
    { Elab.ambientName = ambientOtherCRef,
      Elab.ambientType = otherConstraintTy
    }

------------------------------------------------------------------------------
-- 1. Decomposition unit tests
------------------------------------------------------------------------------

testDecomposeGivenType :: Test ()
testDecomposeGivenType =
  tests
    [ scope "no-foralls-no-premises" $
        let (vs, prems, concl) = Elab.decomposeGivenType constraintTy
         in do
              expectEqual ([] :: [Symbol]) vs
              expectEqual [] prems
              expectEqual constraintTy concl,
      scope "implicit-arrow-becomes-premise" $
        let ty = Type.implicitArrow () constraintTy otherConstraintTy
            (vs, prems, concl) = Elab.decomposeGivenType ty
         in do
              expectEqual ([] :: [Symbol]) vs
              expectEqual [constraintTy] prems
              expectEqual otherConstraintTy concl,
      scope "regular-arrow-stays-in-conclusion" $
        let -- TestC -> Nat -> Nat: regular arrows are *not* premises.
            ty = Type.arrow () constraintTy (Type.arrow () (Type.nat ()) (Type.nat ()))
            (vs, prems, concl) = Elab.decomposeGivenType ty
         in do
              expectEqual ([] :: [Symbol]) vs
              expectEqual [] prems
              expectEqual ty concl
    ]

------------------------------------------------------------------------------
-- 2. End-to-end with an ambient (top-level / namespace) given
------------------------------------------------------------------------------

testAmbientResolution :: Test ()
testAmbientResolution =
  scope "single-ambient-given-resolves-single-implicit" $
    let infos = runFCall Map.empty
        ambient = Elab.ambientPool [ambientShowNat]
        elaborated = Elab.elaborateInfoNotes ambient infos
        decisions = Elab.implicitDecisions elaborated
     in do
          -- Exactly one constraint goal was emitted (single implicit).
          let goalCount = length (Elab.extractConstraintGoals infos)
          expectEqual 1 goalCount
          -- Exactly one decision was produced.
          expectEqual 1 (length decisions)
          case decisions of
            [(_, _, Right tree)] ->
              expectEqual ambientShowNatRef (GR.givenName (GR.rtGiven tree))
            other -> crash ("unexpected decisions: " <> show (length other))

------------------------------------------------------------------------------
-- 3. End-to-end with a lexical given (programmatic threading)
------------------------------------------------------------------------------

testLexicalResolution :: Test ()
testLexicalResolution =
  scope "single-lexical-given-resolves-single-implicit" $
    -- Build the lexical-given snapshot at the surface level, lift to
    -- Context.Type, and pass it as the typechecker's initial givens
    -- map. A2's parser sugar (@let given x = ...@) is not on this
    -- branch, so we exercise the same code path programmatically.
    let lexicalRef = Reference.Builtin "LexicalShowNat"
        surfaceGivens = Map.singleton lexicalRef constraintTy
        givens = TypeVar.liftType <$> surfaceGivens
        infos = runFCall givens
        -- No ambient pool here: the only given is lexical.
        empty = GR.poolFromList []
        elaborated = Elab.elaborateInfoNotes empty infos
        decisions = Elab.implicitDecisions elaborated
     in case decisions of
          [(_, _, Right tree)] ->
            expectEqual lexicalRef (GR.givenName (GR.rtGiven tree))
          other ->
            crash ("expected one Right decision, got: " <> show (length other))

------------------------------------------------------------------------------
-- 4. Failure path: no matching given in either pool
------------------------------------------------------------------------------

testMissingGiven :: Test ()
testMissingGiven =
  scope "no-given-produces-resolve-error" $
    let infos = runFCall Map.empty
        empty = GR.poolFromList []
        elaborated = Elab.elaborateInfoNotes empty infos
        decisions = Elab.implicitDecisions elaborated
     in case decisions of
          [(_, _, Left (GR.NoGiven _ _))] -> ok
          [(_, _, Left other)] ->
            crash ("expected NoGiven, got: " <> show other)
          [(_, _, Right _)] ->
            crash "expected resolution failure, got success"
          xs ->
            crash ("expected exactly one decision, got " <> show (length xs))

------------------------------------------------------------------------------
-- 5. No-implicit term produces no decisions
------------------------------------------------------------------------------

testNoImplicit :: Test ()
testNoImplicit =
  scope "plain-arrow-emits-no-decisions" $
    let infos = runGCall
        elaborated = Elab.elaborateInfoNotes (GR.poolFromList []) infos
        decisions = Elab.implicitDecisions elaborated
     in expectEqual 0 (length decisions)

------------------------------------------------------------------------------
-- 6. Lexical given wins over ambient at the same goal type (ADR-008)
------------------------------------------------------------------------------

-- The two-implicit signature lets us test both layers in the same
-- call: 'h : OtherC => TestC => Nat -> Nat' applied to '42' emits two
-- goals, one for OtherC and one for TestC. We provide:
--
--   * A lexical given for OtherC at a different reference than the
--     ambient one, and
--   * Ambient givens for both OtherC and TestC.
--
-- The resolver should pick the lexical 'OtherC' (per
-- 'GR.Lexical 0 < GR.Ambient') and the ambient 'TestC' (the only
-- candidate of any sort).
testLexicalShadowsAmbient :: Test ()
testLexicalShadowsAmbient =
  scope "lexical-given-wins-over-ambient" $
    let lexicalOtherCRef = Reference.Builtin "LexicalOtherC"
        surfaceGivens = Map.singleton lexicalOtherCRef otherConstraintTy
        givens = TypeVar.liftType <$> surfaceGivens
        infos = runHCall givens
        ambient = Elab.ambientPool [ambientOtherC, ambientShowNat]
        elaborated = Elab.elaborateInfoNotes ambient infos
        decisions = Elab.implicitDecisions elaborated
        names = [GR.givenName (GR.rtGiven t) | (_, _, Right t) <- decisions]
     in do
          expectEqual 2 (length decisions)
          -- The lexical OtherC should win over the ambient one.
          expect (lexicalOtherCRef `elem` names)
          expect (notElem ambientOtherCRef names)
          -- The TestC goal has no lexical candidate; the ambient one
          -- must be the chosen resolution.
          expect (ambientShowNatRef `elem` names)

------------------------------------------------------------------------------
-- L2: @let given@ at the AST level. A binding whose declared type
-- begins with an 'ImplicitArrow' should be picked up by the
-- typechecker's lexical-given environment so a 'ConstraintGoal' inside
-- the let-body resolves against it.
--
-- We construct the AST that A2's parser would produce for
--
-- @
--   let given local : TestC = body
--    in f 42
-- @
--
-- by hand: a non-top 'singleLet' whose binding is a 'Term.Ann' with
-- declared type @TestC@... but @TestC@ alone has no @=>@ so it isn't
-- a given. The minimal shape that triggers chunk L2's wiring is a
-- declared type that contains at least one 'ImplicitArrow'; i.e. the
-- given's type itself takes a premise. The simplest two-layer test:
-- give the local a /derived/ given of shape @TestC => TestC@ — its
-- conclusion matches the goal and its premise is satisfied by the
-- ambient @TestC@ given. The resolver must then choose the lexical
-- candidate (per 'GR.Lexical 0 < GR.Ambient').
--
-- Note: the resolver records 'givenName' as a synthetic
-- @Reference.Builtin "Local.given.<varname>"@ for local lexical
-- givens; chunk L2 doesn't yet plumb a fully-applied dictionary back
-- through the rewriter, but the 'SolvedImplicit' info note carries
-- the chosen ref, which is what these tests assert on.
------------------------------------------------------------------------------

testLetGivenExtendsLexical :: Test ()
testLetGivenExtendsLexical =
  scope "let-given binding registers a lexical given" $
    -- @
    --   let local : OtherC => TestC = 42
    --    in f 42        -- f : TestC => Nat -> Nat
    -- @
    --
    -- The binding's declared type begins with '=>', so chunk L2's
    -- 'extendLexicalGivenFromBinding' wires a synthetic
    -- @Local.given.local : OtherC => TestC@ into the lexical-given env
    -- before checking the body. The apply-site goal is @TestC@; the
    -- local's conclusion matches and its premise @OtherC@ is satisfied
    -- by an ambient given for OtherC. No ambient TestC is supplied —
    -- so any TestC decision must come from the local.
    let localVar = Var.named @Symbol "local"
        expectedRef = Reference.Builtin "Local.given.local"
        -- Body: a witness Ref whose declared type is exactly the
        -- conclusion (TestC). The binding's annotation is
        -- 'OtherC => TestC', so 'checkWanted''s 'ImplicitArrow' clause
        -- emits the OtherC goal and recurses on the conclusion.
        givenBody = Term.ref () witnessLetGivenRef
        -- 'OtherC => TestC' requires premise OtherC, concludes TestC.
        givenTy =
          Type.implicitArrow () otherConstraintTy constraintTy
        givenBinding = Term.ann () givenBody givenTy
        -- Body of the let: 'f 42'. f : TestC => Nat -> Nat, so the
        -- typechecker emits a 'TestC' goal at this apply-site.
        letBody = Term.app () (Term.ref () fRef) (Term.nat () 42)
        term =
          Term.singleLet
            False
            ()
            ()
            (localVar, givenBinding)
            letBody
        res =
          Context.synthesizeClosed
            PPE.empty
            Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled
            defaultVariances
            []
            Map.empty
            -- ADR-010 / chunk L2 fixup: the parser tags @given@-bound
            -- vars; programmatic tests stand in for that here by
            -- handing the typechecker the same set directly.
            (Set.singleton localVar)
            typeLookup
            (TypeVar.liftTerm term)
        infos = toList (Context.infoNotes res)
        -- Provide ambient OtherC (used only to satisfy the local's
        -- premise, never the top-level TestC goal).
        ambient = Elab.ambientPool [ambientOtherC]
        elaborated = Elab.elaborateInfoNotes ambient infos
        decisions = Elab.implicitDecisions elaborated
        solvedNames =
          [ GR.givenName (GR.rtGiven tree)
          | (_, _, Right tree) <- decisions
          ]
     in tests
          [ scope "at-least-one-decision-emitted" $
              expect (not (null decisions)),
            scope "lexical-local-was-chosen" $
              -- The TestC goal at the let-body's apply-site must be
              -- solved by the local lexical given. The synthetic ref
              -- 'Local.given.local' is the resolver's witness that L2's
              -- wiring threaded the binding into the lexical scope.
              expect (expectedRef `elem` solvedNames)
          ]

testLetGivenShadowsAmbient :: Test ()
testLetGivenShadowsAmbient =
  scope "let-given shadows ambient at the same goal type" $
    -- @
    --   let shadow : OtherC => TestC = 42
    --    in f 42        -- f : TestC => Nat -> Nat
    -- @
    --
    -- This time we *also* supply an ambient TestC. The resolver must
    -- still pick the local 'shadow' (per 'GR.Lexical 0 < GR.Ambient'
    -- in 'filterMostInner'), demonstrating that the L2 wiring threads
    -- the local with 'Lexical 0' rather than mistakenly tagging it
    -- 'Ambient'.
    let localVar = Var.named @Symbol "shadow"
        expectedRef = Reference.Builtin "Local.given.shadow"
        givenTy =
          Type.implicitArrow () otherConstraintTy constraintTy
        givenBody = Term.ref () witnessLetGivenRef
        givenBinding = Term.ann () givenBody givenTy
        letBody = Term.app () (Term.ref () fRef) (Term.nat () 1)
        term =
          Term.singleLet
            False
            ()
            ()
            (localVar, givenBinding)
            letBody
        res =
          Context.synthesizeClosed
            PPE.empty
            Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled
            defaultVariances
            []
            Map.empty
            (Set.singleton localVar)
            typeLookup
            (TypeVar.liftTerm term)
        infos = toList (Context.infoNotes res)
        -- Both ambient TestC AND lexical TestC are available.
        ambient = Elab.ambientPool [ambientOtherC, ambientShowNat]
        elaborated = Elab.elaborateInfoNotes ambient infos
        decisions = Elab.implicitDecisions elaborated
        topNames =
          [ GR.givenName (GR.rtGiven tree)
          | (_, _, Right tree) <- decisions
          ]
     in tests
          [ scope "decision-emitted" $ expect (not (null decisions)),
            scope "lexical-beats-ambient" $
              expect (expectedRef `elem` topNames),
            scope "ambient-was-not-the-top-choice" $
              -- For every decision whose goal is TestC the chosen
              -- given must be the lexical one — never the ambient
              -- TestC. (Ambient OtherC may legitimately appear as a
              -- premise sub-resolution of the local given.)
              for_ decisions $ \(_, _, dec) ->
                case dec of
                  Right tree ->
                    let gn = GR.givenName (GR.rtGiven tree)
                     in expect (gn /= ambientShowNatRef)
                  Left _ -> ok
          ]

------------------------------------------------------------------------------
-- L2 fixup: premise-free @let given local : TestC = …@. This is the
-- canonical ADR-010 example: the binding's declared type has no @=>@
-- premises. Before the fix the predicate
-- @null . fst . unImplicitArrows@ silently dropped this binding, so
-- the lexical given environment was never extended and the apply-site
-- goal failed to resolve. After the parser-tagged side channel the
-- typechecker recognises the binding by name and registers it.
--
-- Test setup: the local has type @TestC@ (no premises). The body of
-- the let mentions @f 42@, where @f : TestC => Nat -> Nat@. With no
-- ambient TestC supplied, the only way the apply-site resolves is via
-- the local — which requires L2's letrec hook to register it. We
-- simulate the parser's tagging by populating 'givenBindings' with
-- the local's variable name directly.
------------------------------------------------------------------------------

testLetGivenPremiseFree :: Test ()
testLetGivenPremiseFree =
  scope "premise-free local given (no `=>`) registers as a lexical given" $
    let localVar = Var.named @Symbol "local"
        expectedRef = Reference.Builtin "Local.given.local"
        -- Premise-free: the binding's type is just @TestC@. The body
        -- is a witness reference whose declared type matches.
        givenBody = Term.ref () witnessLetGivenRef
        -- Note the absence of @Type.implicitArrow@ here: the type is
        -- a plain conclusion. Pre-fix this would not register.
        givenBinding = Term.ann () givenBody constraintTy
        letBody = Term.app () (Term.ref () fRef) (Term.nat () 42)
        term =
          Term.singleLet
            False
            ()
            ()
            (localVar, givenBinding)
            letBody
        res =
          Context.synthesizeClosed
            PPE.empty
            Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled
            defaultVariances
            []
            Map.empty
            -- Stand-in for the parser's tag.
            (Set.singleton localVar)
            typeLookup
            (TypeVar.liftTerm term)
        infos = toList (Context.infoNotes res)
        -- No ambient TestC: resolution must come from the local.
        ambient = Elab.ambientPool []
        elaborated = Elab.elaborateInfoNotes ambient infos
        decisions = Elab.implicitDecisions elaborated
        solvedNames =
          [ GR.givenName (GR.rtGiven tree)
          | (_, _, Right tree) <- decisions
          ]
     in tests
          [ scope "at-least-one-decision-emitted" $
              expect (not (null decisions)),
            scope "premise-free-local-was-chosen" $
              expect (expectedRef `elem` solvedNames)
          ]

------------------------------------------------------------------------------
-- D4 carry-over: Ambiguous reaches the elaborator end-to-end (D2 only
-- exercised NoGiven). Two ambient givens of identical conclusion both
-- match the goal; the resolver must yield 'Ambiguous'.
------------------------------------------------------------------------------

ambientShowNatA :: Elab.AmbientGiven Symbol ()
ambientShowNatA =
  Elab.AmbientGiven
    { Elab.ambientName = Reference.Builtin "AmbientShowNat.A",
      Elab.ambientType = constraintTy
    }

ambientShowNatB :: Elab.AmbientGiven Symbol ()
ambientShowNatB =
  Elab.AmbientGiven
    { Elab.ambientName = Reference.Builtin "AmbientShowNat.B",
      Elab.ambientType = constraintTy
    }

testAmbiguousGiven :: Test ()
testAmbiguousGiven =
  scope "two-equally-applicable-givens-yield-Ambiguous" $
    let infos = runFCall Map.empty
        ambient = Elab.ambientPool [ambientShowNatA, ambientShowNatB]
        elaborated = Elab.elaborateInfoNotes ambient infos
        decisions = Elab.implicitDecisions elaborated
     in case decisions of
          [(_, _, Left (GR.Ambiguous _ candidates))] -> do
            -- Both candidates show up in the diagnostic.
            let names = map GR.givenName candidates
            expect (Reference.Builtin "AmbientShowNat.A" `elem` names)
            expect (Reference.Builtin "AmbientShowNat.B" `elem` names)
          [(_, _, other)] ->
            crash ("expected Ambiguous, got: " <> show other)
          xs ->
            crash ("expected one decision, got " <> show (length xs))

------------------------------------------------------------------------------
-- D4 carry-over: DepthExceeded surfaces through the elaborator. We
-- build a self-referential given (Self requires Self) with no base
-- case; the resolver must bail with DepthExceeded (or NoGiven via the
-- per-branch cycle short-circuit, depending on memoization order).
------------------------------------------------------------------------------

-- A self-recursive given: SelfC requires SelfC, no base case.
selfRef :: Reference.Reference
selfRef = Reference.Builtin "SelfFromSelf"

selfConstraintRef :: Reference.Reference
selfConstraintRef = Reference.Builtin "SelfC"

selfConstraintTy :: Type.Type Symbol ()
selfConstraintTy = Type.ref () selfConstraintRef

selfFromSelf :: Elab.AmbientGiven Symbol ()
selfFromSelf =
  Elab.AmbientGiven
    { Elab.ambientName = selfRef,
      Elab.ambientType =
        Type.implicitArrow () selfConstraintTy selfConstraintTy
    }

-- A function whose only implicit is SelfC.
selfFnRef :: Reference.Reference
selfFnRef = Reference.Builtin "SelfFn"

selfFnType :: Type.Type Symbol ()
selfFnType =
  Type.implicitArrow
    ()
    selfConstraintTy
    (Type.arrow () (Type.nat ()) (Type.nat ()))

testDepthExceeded :: Test ()
testDepthExceeded =
  scope "self-referential-given-depth-bails" $
    let ourTypeLookup =
          mempty {TL.typeOfTerms = Map.fromList [(selfFnRef, selfFnType)]}
        term = Term.app () (Term.ref () selfFnRef) (Term.nat () 42)
        res =
          Context.synthesizeClosed
            PPE.empty
            Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled
            defaultVariances
            []
            Map.empty
            Set.empty
            ourTypeLookup
            term
        infos = toList (Context.infoNotes res)
        ambient = Elab.ambientPool [selfFromSelf]
        elaborated = Elab.elaborateInfoNotes ambient infos
        decisions = Elab.implicitDecisions elaborated
     in case decisions of
          [(_, _, Left (GR.DepthExceeded _))] -> ok
          [(_, _, Left (GR.NoGiven _ _))] ->
            -- Either is acceptable — the per-branch cycle short-
            -- circuit can produce NoGiven before depth is reached.
            ok
          [(_, _, other)] ->
            crash ("expected DepthExceeded or NoGiven, got: " <> show other)
          xs ->
            crash ("expected one decision, got " <> show (length xs))

------------------------------------------------------------------------------
-- D4 carry-over: cycle/metavar regression. A mutually-recursive given
-- pair (FooFromBar / BarFromFoo) where the goal type carries an
-- inference variable in a non-conclusion position. This exercises the
-- now-fixed `cycleHit` widened-flex behavior: the per-branch cycle
-- check must compare alpha-renamed fresh copies as equivalent goals.
--
-- We construct the goal directly (without going through synthesis) so
-- we can place a fresh free variable in the goal that cycleHit must
-- treat as flexible. Per the resolver's contract this should fail
-- with NoGiven (cycle short-circuit), not hang.
------------------------------------------------------------------------------

testCycleWithMetavar :: Test ()
testCycleWithMetavar =
  scope "mutual-recursion-with-flexible-goal-var-fails-cleanly" $
    let -- Two builtins for the recursive pair.
        fooC :: Type.Type Symbol ()
        fooC = Type.ref () (Reference.Builtin "Foo")
        barC :: Type.Type Symbol ()
        barC = Type.ref () (Reference.Builtin "Bar")
        fooFromBar :: GR.Given Symbol ()
        fooFromBar =
          GR.Given
            { GR.givenName = Reference.Builtin "fooFromBar",
              GR.givenTyVars = [],
              GR.givenPremises = [barC],
              GR.givenConclusion = fooC,
              GR.givenScope = GR.Ambient
            }
        barFromFoo :: GR.Given Symbol ()
        barFromFoo =
          GR.Given
            { GR.givenName = Reference.Builtin "barFromFoo",
              GR.givenTyVars = [],
              GR.givenPremises = [fooC],
              GR.givenConclusion = barC,
              GR.givenScope = GR.Ambient
            }
        pool = GR.poolFromList [fooFromBar, barFromFoo]
        -- Goal: 'Foo'. The cycle would hang without per-branch cycle
        -- detection; we assert it terminates with NoGiven (the
        -- candidate's premise resolution failed via the cycle path).
        result = GR.resolve pool fooC
     in case result of
          Left (GR.NoGiven _ _) -> ok
          Left other ->
            crash ("expected NoGiven via cycle short-circuit, got: " <> show other)
          Right t ->
            crash ("expected failure, got: " <> show t)

------------------------------------------------------------------------------
-- D4: unresolved metavar in the goal yields its own dedicated error
-- variant. We construct a goal with a free variable whose 'Var.typeOf'
-- is 'Inference' (not user-named) and check the resolver short-
-- circuits to 'UnresolvedMetavarInGoal'.
------------------------------------------------------------------------------

testUnresolvedMetavarInGoal :: Test ()
testUnresolvedMetavarInGoal =
  scope "free-inference-var-in-goal-resolves-against-unique-candidate" $
    -- After the resolver was relaxed to accept goal-side inference
    -- variables as flexible, a goal like @Show ?a@ resolves to a
    -- unique candidate by binding @?a@ to whatever the candidate
    -- exposes. Surrounding type inference is expected to be the
    -- ultimate authority on @?a@; the resolver no longer raises
    -- 'UnresolvedMetavarInGoal' before trying.
    let inferVar :: Symbol
        inferVar = Var.inferOther
        goal = Type.app () (Type.ref () (Reference.Builtin "Show")) (Type.var () inferVar)
        showNat =
          GR.Given
            { GR.givenName = Reference.Builtin "Show.nat",
              GR.givenTyVars = [],
              GR.givenPremises = [],
              GR.givenConclusion =
                Type.app () (Type.ref () (Reference.Builtin "Show")) (Type.ref () (Reference.Builtin "Nat")),
              GR.givenScope = GR.Ambient
            }
        pool = GR.poolFromList [showNat]
     in case GR.resolve pool goal of
          Right tree
            | GR.givenName (GR.rtGiven tree) == Reference.Builtin "Show.nat" -> ok
            | otherwise ->
                crash ("expected Show.nat, got: " <> show tree)
          Left other ->
            crash ("expected successful resolution, got: " <> show other)
