{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "Unison.Typechecker.GivenApply": the post-typecheck
-- pass that walks a term and substitutes resolved implicit arguments
-- into 'App' nodes.
--
-- These tests build small terms programmatically and exercise the
-- elaborate-then-apply pipeline directly:
--
--  1. Build a term that mentions a function with an @ImplicitArrow@
--     in its type.
--  2. Run 'Context.synthesizeClosed' to emit 'ConstraintGoal' notes.
--  3. Run 'Elab.elaborateInfoNotes' with an ambient pool to convert
--     the goals into 'SolvedImplicit' decisions.
--  4. Run 'GivenApply.applyGivenDecisions' on the original term and
--     verify the resulting 'App' structure.
module Unison.Test.Typechecker.GivenApply (test) where

import Data.Foldable (for_, toList)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import EasyTest
import Unison.ABT qualified as ABT
import Unison.Lexer.Pos qualified as L
import Unison.Parser.Ann (Ann (..))
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference qualified as Reference
import Unison.Result qualified as Result
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Test.Common qualified as Common
import Unison.Type qualified as Type
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.GivenApply qualified as GA
import Unison.Typechecker.GivenElaborator qualified as Elab
import Unison.Typechecker.GivenResolver qualified as GR
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Typechecker.TypeVar qualified as TypeVar
import Unison.Typechecker.Variance (defaultVariances)
import Unison.Var qualified as Var

test :: Test ()
test =
  scope "GivenApply"
    . tests
    $ [ scope "buildDictionary" testBuildDictionary,
        scope "end-to-end-single-implicit" testEndToEndSingle,
        scope "end-to-end-chained-dictionaries" testEndToEndChained,
        scope "source-level-end-to-end" testSourceLevelEndToEnd,
        scope "source-level-let-given-shadows-ambient" testSourceLevelLetGivenShadowsAmbient,
        scope "source-level-file-internal-given" testSourceLevelFileInternalGiven,
        -- Premise-free givens at file scope and inside a @let@ block.
        scope "source-level-file-given-premise-free" testSourceLevelFileGivenPremiseFree,
        scope "source-level-let-given-premise-free" testSourceLevelLetGivenPremiseFree,
        scope "override-skips-decision" testOverrideSkipsDecision,
        scope "no-implicit-no-rewrite" testNoImplicitNoRewrite,
        scope "missing-decision-leaves-term-alone" testMissingDecision,
        scope "override-marker-preserved" testOverrideMarkerPreserved
      ]

------------------------------------------------------------------------------
-- 1. Pure unit tests for 'buildDictionary'
------------------------------------------------------------------------------

testBuildDictionary :: Test ()
testBuildDictionary =
  tests
    [ scope "leaf-given-becomes-ref" $
        let g = mkGiven (Reference.Builtin "Show.nat") []
            tree = GR.ResolutionTree g Map.empty []
            t = GA.buildDictionary External tree :: Term.Term Symbol Ann
         in case ABT.out t of
              ABT.Tm (Term.Ref r) ->
                expectEqual (Reference.Builtin "Show.nat") r
              other -> crash ("expected Ref, got: " <> show other),
      scope "nested-given-becomes-app" $
        let inner = GR.ResolutionTree (mkGiven (Reference.Builtin "Show.nat") []) Map.empty []
            outerG = mkGiven (Reference.Builtin "Show.list") []
            outer = GR.ResolutionTree outerG Map.empty [inner]
            t = GA.buildDictionary External outer :: Term.Term Symbol Ann
         in case ABT.out t of
              ABT.Tm (Term.App f a) -> do
                case ABT.out f of
                  ABT.Tm (Term.Ref r) -> expectEqual (Reference.Builtin "Show.list") r
                  o -> crash ("function head: " <> show o)
                case ABT.out a of
                  ABT.Tm (Term.Ref r) -> expectEqual (Reference.Builtin "Show.nat") r
                  o -> crash ("argument: " <> show o)
              other -> crash ("expected App, got: " <> show other)
    ]

------------------------------------------------------------------------------
-- 2. End-to-end single-implicit substitution
------------------------------------------------------------------------------

-- A constraint-class-like type "TestC".
constraintRef :: Reference.Reference
constraintRef = Reference.Builtin "TestC"

constraintTy :: Type.Type Symbol Ann
constraintTy = Type.ref External constraintRef

-- A function `fImplicit : TestC => Nat -> Nat`.
fImplicitRef :: Reference.Reference
fImplicitRef = Reference.Builtin "TestImplicitFn"

fImplicitType :: Type.Type Symbol Ann
fImplicitType =
  Type.implicitArrow
    External
    constraintTy
    (Type.arrow External (Type.nat External) (Type.nat External))

-- A plain function `fPlain : Nat -> Nat`.
fPlainRef :: Reference.Reference
fPlainRef = Reference.Builtin "TestPlainFn"

fPlainType :: Type.Type Symbol Ann
fPlainType = Type.arrow External (Type.nat External) (Type.nat External)

-- An ambient given that resolves to a `TestC` dictionary.
ambientGivenRef :: Reference.Reference
ambientGivenRef = Reference.Builtin "AmbientTestC"

ambientGiven :: Elab.AmbientGiven Symbol Ann
ambientGiven =
  Elab.AmbientGiven {Elab.ambientName = ambientGivenRef, Elab.ambientType = constraintTy}

typeLookup :: TL.TypeLookup Symbol Ann
typeLookup =
  mempty
    { TL.typeOfTerms =
        Map.fromList
          [ (fImplicitRef, fImplicitType),
            (fPlainRef, fPlainType)
          ]
    }

-- Run the end-to-end pipeline on a term and return the rewritten
-- term plus the info-notes (with implicit decisions added).
--
-- The term is built at the user-facing 'Term.Term Symbol Ann' shape;
-- we lift it to 'Context.Term' with 'TypeVar.liftTerm' before
-- synthesis (which lives in the 'TypeVar'-wrapped world), then run
-- the apply pass on the original surface term.
elaborateAndApply ::
  Term Symbol Ann ->
  GR.Pool Symbol Ann ->
  Term Symbol Ann
elaborateAndApply term ambient =
  let lifted = TypeVar.liftTerm term
      res =
        Context.synthesizeClosed
          PPE.empty
          Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled
          defaultVariances
          []
          Map.empty
          Set.empty
          typeLookup
          lifted
      infos = toList (Context.infoNotes res)
      enriched = Elab.elaborateInfoNotes ambient infos
   in fst (GA.applyGivenDecisions enriched typeLookup term)

testEndToEndSingle :: Test ()
testEndToEndSingle =
  scope "f 42 with ambient TestC ⇒ App (App f dict) 42" $
    let term = Term.app External (Term.ref External fImplicitRef) (Term.nat External 42)
        ambient = Elab.ambientPool [ambientGiven]
        rewritten = elaborateAndApply term ambient
     in -- Expected shape: App (App fImplicit ambientGiven) 42
        case ABT.out rewritten of
          ABT.Tm (Term.App outerF arg42) -> do
            -- outerF should be App fImplicitRef ambientGivenRef
            case ABT.out outerF of
              ABT.Tm (Term.App f dict) -> do
                case ABT.out f of
                  ABT.Tm (Term.Ref r) -> expectEqual fImplicitRef r
                  o -> crash ("expected fImplicitRef, got: " <> show o)
                case ABT.out dict of
                  ABT.Tm (Term.Ref r) -> expectEqual ambientGivenRef r
                  o -> crash ("expected ambientGivenRef, got: " <> show o)
              o -> crash ("expected App at outer-f, got: " <> show o)
            -- The user's argument should still be 42.
            case ABT.out arg42 of
              ABT.Tm (Term.Nat 42) -> ok
              o -> crash ("expected Nat 42, got: " <> show o)
          o -> crash ("expected App, got: " <> show o)

------------------------------------------------------------------------------
-- 4. Chained dictionaries: Show (List Nat) via Show.list (Show.nat)
------------------------------------------------------------------------------

-- Build a polymorphic given Show.list : forall a. Show a => Show
-- (List a), plus an ambient Show.nat : Show Nat. The constraint is
-- just `Reference.Builtin "Show"` applied to a type argument; we
-- model it directly in Type.Type.

showRef :: Reference.Reference
showRef = Reference.Builtin "Show"

listRef :: Reference.Reference
listRef = Reference.Builtin "List"

showOf :: Type.Type Symbol Ann -> Type.Type Symbol Ann
showOf t = Type.app External (Type.ref External showRef) t

-- A function `fShow : Show (List Nat) => Nat -> Nat`.
fShowRef :: Reference.Reference
fShowRef = Reference.Builtin "TestShowFn"

fShowType :: Type.Type Symbol Ann
fShowType =
  Type.implicitArrow
    External
    (showOf (Type.app External (Type.ref External listRef) (Type.nat External)))
    (Type.arrow External (Type.nat External) (Type.nat External))

showNatRef :: Reference.Reference
showNatRef = Reference.Builtin "Show.nat"

showListRef :: Reference.Reference
showListRef = Reference.Builtin "Show.list"

showNatGiven :: Elab.AmbientGiven Symbol Ann
showNatGiven =
  Elab.AmbientGiven
    { Elab.ambientName = showNatRef,
      Elab.ambientType = showOf (Type.nat External)
    }

-- forall a. Show a => Show (List a)
showListGiven :: Elab.AmbientGiven Symbol Ann
showListGiven =
  let a = Type.var External (Var.named "a" :: Symbol)
      sig =
        Type.forAll
          External
          (Var.named "a" :: Symbol)
          (Type.implicitArrow External (showOf a) (showOf (Type.app External (Type.ref External listRef) a)))
   in Elab.AmbientGiven
        { Elab.ambientName = showListRef,
          Elab.ambientType = sig
        }

testEndToEndChained :: Test ()
testEndToEndChained =
  scope "Show (List Nat) resolves via Show.list (Show.nat)" $
    let typeLookup' =
          mempty
            { TL.typeOfTerms = Map.fromList [(fShowRef, fShowType)]
            }
        term :: Term Symbol Ann
        term = Term.app External (Term.ref External fShowRef) (Term.nat External 7)
        ambient = Elab.ambientPool [showListGiven, showNatGiven]
        res =
          Context.synthesizeClosed
            PPE.empty
            Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled
            defaultVariances
            []
            Map.empty
            Set.empty
            typeLookup'
            (TypeVar.liftTerm term)
        infos = toList (Context.infoNotes res)
        enriched = Elab.elaborateInfoNotes ambient infos
        rewritten = fst (GA.applyGivenDecisions enriched typeLookup' term)
     in -- Expected: App (App fShow (App showListRef showNatRef)) 7
        case ABT.out rewritten of
          ABT.Tm (Term.App outerF _arg7) -> case ABT.out outerF of
            ABT.Tm (Term.App f dict) -> do
              -- f is fShow
              case ABT.out f of
                ABT.Tm (Term.Ref r) -> expectEqual fShowRef r
                o -> crash ("expected fShow, got: " <> show o)
              -- dict is App showList showNat
              case ABT.out dict of
                ABT.Tm (Term.App showListT showNatT) -> do
                  case ABT.out showListT of
                    ABT.Tm (Term.Ref r) -> expectEqual showListRef r
                    o -> crash ("expected showList, got: " <> show o)
                  case ABT.out showNatT of
                    ABT.Tm (Term.Ref r) -> expectEqual showNatRef r
                    o -> crash ("expected showNat, got: " <> show o)
                o -> crash ("expected App for chained dict, got: " <> show o)
            o -> crash ("expected outer App, got: " <> show o)
          o -> crash ("expected outermost App, got: " <> show o)

------------------------------------------------------------------------------
-- 5. Override skips decision
------------------------------------------------------------------------------

-- This test simulates the `f @ d x` override syntax. We construct a
-- term that already has a *widened*-annotation argument at the
-- implicit slot, then verify the apply pass leaves the slot alone (no
-- dictionary inserted) even though the typechecker emitted a
-- constraint goal.
testOverrideSkipsDecision :: Test ()
testOverrideSkipsDecision =
  scope "f @userOverride 42 keeps user's override and skips elaborator" $
    let userOverrideRef = Reference.Builtin "UserOverride"
        userOverride =
          -- `(id userOverride)` style: a compound whose outer ann
          -- is widened to start before its first child.
          Term.app
            -- outer ann starts at col 3 (the `@`), the inner refs
            -- start at col 5.
            (Ann (L.Pos 1 3) (L.Pos 1 20))
            (Term.var (Ann (L.Pos 1 5) (L.Pos 1 7)) (Var.named "id" :: Symbol))
            (Term.ref (Ann (L.Pos 1 8) (L.Pos 1 20)) userOverrideRef)
        term :: Term Symbol Ann
        term =
          Term.app
            External
            ( Term.app
                External
                (Term.ref External fImplicitRef)
                userOverride
            )
            (Term.nat External 42)
        -- Even though we provide an ambient given, the apply pass
        -- should NOT consume it because the override-arg is widened.
        ambient = Elab.ambientPool [ambientGiven]
        res =
          Context.synthesizeClosed
            PPE.empty
            Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled
            defaultVariances
            []
            Map.empty
            Set.empty
            typeLookup
            (TypeVar.liftTerm term)
        infos = toList (Context.infoNotes res)
        enriched = Elab.elaborateInfoNotes ambient infos
        rewritten = fst (GA.applyGivenDecisions enriched typeLookup term)
     in -- The rewritten term should preserve userOverride at the
        -- implicit slot rather than substituting ambientGiven.
        case ABT.out rewritten of
          ABT.Tm (Term.App outerF _arg42) -> case ABT.out outerF of
            ABT.Tm (Term.App _f arg) ->
              -- arg should be the compound override expression, not a Ref to ambientGiven.
              case ABT.out arg of
                ABT.Tm (Term.App _ inner) -> case ABT.out inner of
                  ABT.Tm (Term.Ref r) -> expectEqual userOverrideRef r
                  o -> crash ("expected userOverride at slot, got: " <> show o)
                o -> crash ("expected compound override, got: " <> show o)
            o -> crash ("expected outer App, got: " <> show o)
          o -> crash ("expected outermost App, got: " <> show o)

------------------------------------------------------------------------------
-- 6. No implicits ⇒ term unchanged
------------------------------------------------------------------------------

testNoImplicitNoRewrite :: Test ()
testNoImplicitNoRewrite =
  scope "fPlain 42 emits no goals and is unchanged" $
    let term = Term.app External (Term.ref External fPlainRef) (Term.nat External 42)
        rewritten = elaborateAndApply term (GR.poolFromList [])
     in -- Same shape as input: App (Ref fPlainRef) (Nat 42).
        case ABT.out rewritten of
          ABT.Tm (Term.App f arg) -> do
            case ABT.out f of
              ABT.Tm (Term.Ref r) -> expectEqual fPlainRef r
              o -> crash ("expected fPlainRef, got: " <> show o)
            case ABT.out arg of
              ABT.Tm (Term.Nat 42) -> ok
              o -> crash ("expected Nat 42, got: " <> show o)
          o -> crash ("expected single App, got: " <> show o)

------------------------------------------------------------------------------
-- 7. Missing decision (typecheck error) leaves the term alone
------------------------------------------------------------------------------

testMissingDecision :: Test ()
testMissingDecision =
  scope "no ambient, no lexical given ⇒ term unchanged, error preserved on info notes" $
    let term = Term.app External (Term.ref External fImplicitRef) (Term.nat External 42)
        rewritten = elaborateAndApply term (GR.poolFromList [])
     in -- The apply pass leaves the unsuccessful slot alone; the
        -- term keeps its original arity.
        case ABT.out rewritten of
          ABT.Tm (Term.App f arg) -> do
            case ABT.out f of
              ABT.Tm (Term.Ref r) -> expectEqual fImplicitRef r
              o -> crash ("expected fImplicitRef, got: " <> show o)
            case ABT.out arg of
              ABT.Tm (Term.Nat 42) -> ok
              o -> crash ("expected Nat 42, got: " <> show o)
          o -> crash ("expected App, got: " <> show o)

------------------------------------------------------------------------------
-- Property: the apply pass does not strip the widened annotation on
-- override arguments.
------------------------------------------------------------------------------

testOverrideMarkerPreserved :: Test ()
testOverrideMarkerPreserved =
  scope "apply pass preserves widened annotations on override-arg slots" $
    let userOverrideRef = Reference.Builtin "UserOverride"
        widenedOuter = Ann (L.Pos 1 3) (L.Pos 1 20)
        userOverride =
          Term.app
            widenedOuter
            (Term.var (Ann (L.Pos 1 5) (L.Pos 1 7)) (Var.named "id" :: Symbol))
            (Term.ref (Ann (L.Pos 1 8) (L.Pos 1 20)) userOverrideRef)
        term :: Term Symbol Ann
        term =
          Term.app
            External
            ( Term.app
                External
                (Term.ref External fImplicitRef)
                userOverride
            )
            (Term.nat External 42)
        ambient = Elab.ambientPool [ambientGiven]
        rewritten = elaborateAndApply term ambient
     in case ABT.out rewritten of
          ABT.Tm (Term.App outerF _arg42) ->
            case ABT.out outerF of
              ABT.Tm (Term.App _f arg) ->
                -- The widened annotation must survive the rewrite.
                expectEqual widenedOuter (ABT.annotation arg)
              o -> crash ("expected outer App, got: " <> show o)
          o -> crash ("expected App, got: " <> show o)

------------------------------------------------------------------------------
-- Source-level end-to-end pin: parser ⇒ typechecker ⇒ resolver
--
-- The other end-to-end tests above ('testEndToEndSingle' /
-- 'testEndToEndChained') build their input terms programmatically with
-- 'Type.implicitArrow ()'. That bypasses the parser entirely, so a
-- regression in 'Unison.Syntax.TypeParser' (where '=>' is desugared to
-- 'Type.implicitArrow' on top of '~>'-and-friends) would go unnoticed
-- by them.
--
-- This test pins the wire from source down through the elaborator: it
-- feeds a small Unison source string through
-- 'parseAndSynthesizeAsFileWithGivens' and asserts that the
-- typechecker's info-note stream contains:
--
--   1. A 'ConstraintGoal' (proving the parser produced an
--      'ImplicitArrow' that the typechecker then saw at the apply-site).
--   2. A 'SolvedImplicit' whose decision is @Right ResolutionTree@
--      (proving the ambient pool reached the resolver, and that
--      resolution succeeded against the supplied ambient given).
--   3. The chosen given's reference matches what we supplied.
------------------------------------------------------------------------------

testSourceLevelEndToEnd :: Test ()
testSourceLevelEndToEnd =
  scope "parse=>synthesize=>resolve picks ambient given on the success path" $
    let src =
          unlines
            [ "useImplicit : Nat => Nat -> Nat",
              "useImplicit n = n",
              "",
              "main : Nat",
              "main = useImplicit 42"
            ]
        -- An ambient given whose conclusion type matches the
        -- 'Nat'-shaped constraint that the parsed function emits.
        natGivenRef = Reference.Builtin "Test.natWitness"
        natGiven =
          Elab.AmbientGiven
            { Elab.ambientName = natGivenRef,
              Elab.ambientType = Type.nat External
            }
        Result.Result notes _ =
          Common.parseAndSynthesizeAsFileWithGivens
            []
            [natGiven]
            "source-level-implicit.u"
            src
        notes' = toList notes
        constraintGoals =
          [ ty
          | Result.TypeInfo (Context.ConstraintGoal _ ty _) <- notes'
          ]
        solvedImplicitTrees =
          [ (ty, tree)
          | Result.TypeInfo (Context.SolvedImplicit _ ty (Right tree)) <- notes'
          ]
        unresolved =
          [ (loc, ty)
          | Result.UnresolvedImplicit loc ty _ <- notes'
          ]
     in tests
          [ scope "constraint-goal-emitted-from-parsed-=>" $
              -- The parser+typechecker together must produce at least
              -- one 'ConstraintGoal'. Zero would mean either the parser
              -- desugared '=>' to '->' (regression) or the typechecker
              -- failed to recurse into the implicit slot at the apply
              -- site. We accept any non-zero count: minimization /
              -- generalization may duplicate the goal across components.
              expect (not (null constraintGoals)),
            scope "solved-implicit-emitted-on-success-path" $
              -- The ambient pool reached the resolver, so at least one
              -- goal must have been solved with a 'ResolutionTree'
              -- decision.
              expect (not (null solvedImplicitTrees)),
            scope "no-unresolved-implicits-when-given-is-supplied" $
              -- With a satisfying ambient given supplied, no
              -- 'UnresolvedImplicit' note should be surfaced —
              -- namespace givens feed the resolver.
              expect (null unresolved),
            scope "chosen-given-is-the-supplied-one" $
              -- The resolver picked /our/ ambient given, not some
              -- spurious other reference. Asserting on every solved
              -- decision (rather than just one) keeps this honest if
              -- letrec-component layout duplicates the goal: every
              -- copy must pick the same given.
              for_ solvedImplicitTrees $ \(_, tree) ->
                expectEqual natGivenRef (GR.givenName (GR.rtGiven tree))
          ]

------------------------------------------------------------------------------
-- Source-level: `let given` shadows ambient
--
-- The same parse=>synthesize=>resolve pipeline as 'testSourceLevelEndToEnd',
-- but exercising a @let given@ binding inside a function body. Both
-- an ambient given (passed via 'parseAndSynthesizeAsFileWithGivens')
-- and a local @let given@ binding match the apply-site goal; the
-- local should win (lexical-inner-wins).
--
-- The synthetic reference for a local @let given@ is built as
-- @Reference.Builtin "Local.given.<varname>"@; see
-- 'extendLexicalGivenFromBinding' in 'Unison.Typechecker.Context'.
-- Asserting on the *prefix* of the chosen ref's name lets the test
-- pin the parser → resolver → apply chain end-to-end without
-- depending on hash-based references for the local binding.
------------------------------------------------------------------------------

testSourceLevelLetGivenShadowsAmbient :: Test ()
testSourceLevelLetGivenShadowsAmbient =
  scope "let given local with => shadows the ambient given at the same goal" $
    -- For a binding to register as a lexical given, the binding's
    -- declared type must begin with at least one 'ImplicitArrow' (the
    -- same signal the typechecker uses to emit 'GivenDecl' notes). To
    -- avoid the trivial @Nat => Nat@ cycle (where the local's premise
    -- is the same as the goal, so the resolver short-circuits the
    -- lexical branch and falls back to ambient), we make the local's
    -- premise a /different/ type — @Boolean@ — and supply an ambient
    -- Boolean given to satisfy that premise. The apply-site goal is
    -- still @Nat@; both an ambient @Nat@ and the lexical
    -- @Boolean => Nat@ produce a successful tree, and the
    -- lexical-inner-wins rule selects the local.
    let src =
          unlines
            [ "useImplicit : Nat => Nat -> Nat",
              "useImplicit n = n",
              "",
              "main : Nat",
              "main =",
              "  let given altNat : Boolean => Nat = 7",
              "      useImplicit 42"
            ]
        ambientNatRef :: Reference.Reference
        ambientNatRef = Reference.Builtin "Test.ambientNat"
        ambientNat =
          Elab.AmbientGiven
            { Elab.ambientName = ambientNatRef,
              Elab.ambientType = Type.nat External
            }
        ambientBoolRef :: Reference.Reference
        ambientBoolRef = Reference.Builtin "Test.ambientBool"
        ambientBool =
          Elab.AmbientGiven
            { Elab.ambientName = ambientBoolRef,
              Elab.ambientType = Type.boolean External
            }
        Result.Result notes _ =
          Common.parseAndSynthesizeAsFileWithGivens
            []
            [ambientNat, ambientBool]
            "let-given-shadows.u"
            src
        notes' = toList notes
        solvedTrees =
          [ tree
          | Result.TypeInfo (Context.SolvedImplicit _ _ (Right tree)) <- notes'
          ]
        unresolved =
          [ ()
          | Result.UnresolvedImplicit _ _ _ <- notes'
          ]
        chosenNames = map (GR.givenName . GR.rtGiven) solvedTrees
        isLocalGiven r = case r of
          Reference.Builtin t -> "Local.given." `Text.isPrefixOf` t
          _ -> False
     in tests
          [ scope "decision-emitted" $
              expect (not (null solvedTrees)),
            scope "no-unresolved-implicits" $
              expect (null unresolved),
            scope "local-let-given-was-chosen-over-ambient" $
              -- The TestC apply-site goal must be solved by the lexical
              -- local given. The synthetic ref (synthetic
              -- @Reference.Builtin "Local.given.<varname>"@) is the
              -- resolver's witness that the binding was threaded into
              -- the lexical scope. Asserting on the prefix pins the
              -- parser → resolver → apply chain end-to-end without
              -- depending on hash-based references.
              expect (any isLocalGiven chosenNames),
            scope "local-overrides-but-ambient-still-reachable" $
              -- With the local given present, every emitted goal at
              -- the apply-site must pick the local.
              expect $
                all isLocalGiven chosenNames
                  && ambientNatRef `notElem` chosenNames
          ]

------------------------------------------------------------------------------
-- Source-level: file-internal given.
--
-- The ambient pool only harvests givens from the *namespace*
-- (codebase-committed definitions). A given declared in the same .u
-- file as the consumer never reaches the ambient pool because it
-- isn't part of the codebase yet. The in-typechecker lexical-given
-- env handles this: 'annotateLetRecBindings'' registers every
-- given-shaped top-level binding into 'lexicalGivens', so consumers
-- in the same file see it.
--
-- This test exercises a self-contained .u file with no ambient pool
-- and no library deps. The file declares its own given and a function
-- that consumes it; resolution must succeed against the in-file
-- given alone.
------------------------------------------------------------------------------

testSourceLevelFileInternalGiven :: Test ()
testSourceLevelFileInternalGiven =
  scope "file-internal given resolves the consumer's implicit slot" $
    -- Uses the @given@ keyword: only bindings explicitly declared
    -- via @given@ become candidates (a plain @=>@ in the type does
    -- not on its own register a binding as a given).
    let src =
          unlines
            [ "given fileGiven : Boolean => Nat = 99",
              "",
              "useImplicit : Nat => Nat -> Nat",
              "useImplicit n = n",
              "",
              "main : Nat",
              "main = useImplicit 42"
            ]
        -- The /only/ ambient is Boolean (used as the file-internal
        -- given's premise). No ambient Nat is supplied — if the
        -- file-internal given isn't picked up by the lexical-given
        -- wiring, resolution fails with NoGiven and the test catches
        -- it via UnresolvedImplicit.
        ambientBoolRef :: Reference.Reference
        ambientBoolRef = Reference.Builtin "Test.ambientBoolFI"
        ambientBool =
          Elab.AmbientGiven
            { Elab.ambientName = ambientBoolRef,
              Elab.ambientType = Type.boolean External
            }
        Result.Result notes _ =
          Common.parseAndSynthesizeAsFileWithGivens
            []
            [ambientBool]
            "file-internal-given.u"
            src
        notes' = toList notes
        solvedTrees =
          [ tree
          | Result.TypeInfo (Context.SolvedImplicit _ _ (Right tree)) <- notes'
          ]
        unresolved =
          [ ()
          | Result.UnresolvedImplicit _ _ _ <- notes'
          ]
        chosenNames = map (GR.givenName . GR.rtGiven) solvedTrees
        isLocalGiven r = case r of
          Reference.Builtin t -> "Local.given." `Text.isPrefixOf` t
          _ -> False
     in tests
          [ scope "no-unresolved-implicits" $
              -- Confirms the resolver succeeded for every goal, which
              -- requires the file-internal @fileGiven@ to be in pool.
              expect (null unresolved),
            scope "decision-emitted" $
              expect (not (null solvedTrees)),
            scope "file-internal-given-was-chosen" $
              -- The Nat goal at @useImplicit 42@'s apply-site must
              -- resolve via @fileGiven@ — visible only because
              -- 'annotateLetRecBindings'' registered it as a
              -- lexical-scope given, not because it's in the ambient
              -- (codebase-derived) pool.
              expect (any isLocalGiven chosenNames)
          ]

------------------------------------------------------------------------------
-- Source-level: premise-free file-level @given@
--
-- The given's type has no @=>@ premises (just the conclusion). The
-- parser tags the @given@-bound variable name and the typechecker
-- registers the binding by name.
--
-- Source: a self-contained file declaring a premise-free given and a
-- consumer whose declared type uses @=>@. Resolution must succeed
-- against the in-file given alone (no ambients supplied).
------------------------------------------------------------------------------

testSourceLevelFileGivenPremiseFree :: Test ()
testSourceLevelFileGivenPremiseFree =
  scope "premise-free file-level given (no `=>`) resolves the consumer's slot" $
    -- Use a Nat-shaped goal to keep the test self-contained: we
    -- declare @given fileGiven : Nat = 99@ and a consumer
    -- @useImplicit : Nat => Nat -> Nat@, then call @useImplicit 42@.
    -- With no ambients, resolution must come from the file-internal
    -- premise-free given.
    let src =
          unlines
            [ "given fileGiven : Nat = 99",
              "",
              "useImplicit : Nat => Nat -> Nat",
              "useImplicit n = n",
              "",
              "main : Nat",
              "main = useImplicit 42"
            ]
        Result.Result notes _ =
          -- No ambient pool at all. Resolution must succeed via the
          -- file-internal premise-free given.
          Common.parseAndSynthesizeAsFileWithGivens
            []
            []
            "file-given-premise-free.u"
            src
        notes' = toList notes
        solvedTrees =
          [ tree
          | Result.TypeInfo (Context.SolvedImplicit _ _ (Right tree)) <- notes'
          ]
        unresolved =
          [ ()
          | Result.UnresolvedImplicit _ _ _ <- notes'
          ]
        chosenNames = map (GR.givenName . GR.rtGiven) solvedTrees
        isLocalGiven r = case r of
          Reference.Builtin t -> "Local.given." `Text.isPrefixOf` t
          _ -> False
     in tests
          [ scope "no-unresolved-implicits" $
              expect (null unresolved),
            scope "decision-emitted" $
              expect (not (null solvedTrees)),
            scope "file-internal-premise-free-given-was-chosen" $
              expect (any isLocalGiven chosenNames)
          ]

------------------------------------------------------------------------------
-- Source-level: premise-free @let given@
--
-- Like 'testSourceLevelFileGivenPremiseFree' but the given is bound
-- inside a @let@ block in the body of @main@. The variable @local@
-- has type @Nat@ — no @=>@ at all.
------------------------------------------------------------------------------

testSourceLevelLetGivenPremiseFree :: Test ()
testSourceLevelLetGivenPremiseFree =
  scope "premise-free `let given` (no `=>`) resolves the consumer's slot" $
    -- The local @given fileGiven : Nat = 7@ has no premises. The
    -- consumer's premise is @Nat@ (matches the local's conclusion).
    -- The parser-tagged side channel registers @local@ and the
    -- apply-site @Nat@ goal resolves through it. The ambient @Nat@ is
    -- supplied so the consumer's /own/ @Nat =>@ definition-site check
    -- (which happens outside the let scope) is also satisfied; the
    -- test then asserts that the apply-site /inside/ the let chooses
    -- the local (lexical-inner-wins).
    let src =
          unlines
            [ "useImplicit : Nat => Nat -> Nat",
              "useImplicit n = n",
              "",
              "main : Nat",
              "main =",
              "  let given local : Nat = 7",
              "      useImplicit 42"
            ]
        ambientNatRef :: Reference.Reference
        ambientNatRef = Reference.Builtin "Test.ambientNat.PF"
        ambientNat =
          Elab.AmbientGiven
            { Elab.ambientName = ambientNatRef,
              Elab.ambientType = Type.nat External
            }
        Result.Result notes _ =
          Common.parseAndSynthesizeAsFileWithGivens
            []
            [ambientNat]
            "let-given-premise-free.u"
            src
        notes' = toList notes
        solvedTrees =
          [ tree
          | Result.TypeInfo (Context.SolvedImplicit _ _ (Right tree)) <- notes'
          ]
        unresolved =
          [ ()
          | Result.UnresolvedImplicit _ _ _ <- notes'
          ]
        chosenNames = map (GR.givenName . GR.rtGiven) solvedTrees
        isLocalGiven r = case r of
          Reference.Builtin t -> "Local.given." `Text.isPrefixOf` t
          _ -> False
     in tests
          [ scope "no-unresolved-implicits" $
              expect (null unresolved),
            scope "decision-emitted" $
              expect (not (null solvedTrees)),
            scope "premise-free-let-given-was-chosen" $
              -- The Nat goal at the apply-site inside the let body
              -- must resolve to the lexical local (lexical-inner-wins).
              expect (any isLocalGiven chosenNames)
          ]

------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------

mkGiven ::
  Reference.Reference ->
  [Type.Type Symbol Ann] ->
  GR.Given Symbol Ann
mkGiven r premises =
  GR.Given
    { GR.givenName = r,
      GR.givenTyVars = [],
      GR.givenPremises = premises,
      GR.givenConclusion = Type.ref External (Reference.Builtin "Dummy"),
      GR.givenScope = GR.Ambient
    }
