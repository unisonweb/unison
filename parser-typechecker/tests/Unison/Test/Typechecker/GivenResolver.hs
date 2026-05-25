{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the implicit-resolution algorithm, targeting real
-- 'Unison.Type.Type' values.
--
-- Tests are organised into six success-criteria groups plus a small
-- "sanity" group.
module Unison.Test.Typechecker.GivenResolver (test) where

import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import EasyTest
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Symbol (Symbol)
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker.GivenResolver
  ( Given (..),
    ResolutionTree (..),
    ResolveError (..),
    ResolveOptions (..),
    Scope (..),
    defaultOptions,
    poolFromList,
    resolve,
    resolveCounted,
    resolveWith,
  )
import Unison.Var qualified as Var

------------------------------------------------------------------------------
-- Test entry point
------------------------------------------------------------------------------

test :: Test ()
test =
  scope "Typechecker.GivenResolver" $
    tests
      [ scope "1.cycles" testCycles,
        scope "2.specificity" testSpecificity,
        scope "3.hkt" testHkt,
        scope "4.ambiguity" testAmbiguity,
        scope "5.performance" testPerformance,
        scope "6.diamond" testDiamond,
        scope "sanity" testSanity
      ]

------------------------------------------------------------------------------
-- Type aliases for readable test data
------------------------------------------------------------------------------

type Ty = Type Symbol ()

ref :: Text -> Ty
ref name = Type.builtin () name

-- Type constructor builtins.
showC, ordC, functorC, mapC, listC, optionalC, natC, fooC, barC, selfC :: Ty
showC = ref "Show"
ordC = ref "Ord"
functorC = ref "Functor"
mapC = ref "Map"
listC = ref "List"
optionalC = ref "Optional"
natC = ref "Nat"
fooC = ref "Foo"
barC = ref "Bar"
selfC = ref "Self"

-- Type variables.
tvA, tvF, tvK, tvV :: Symbol
tvA = Var.named "a"
tvF = Var.named "f"
tvK = Var.named "k"
tvV = Var.named "v"

tv :: Symbol -> Ty
tv = Type.var ()

-- | Givens are keyed by 'Reference'; we use 'Builtin' so equality
-- comparisons use by-name semantics.
gname :: Text -> Reference
gname = Reference.Builtin

-- Convenience builders.
appOne :: Ty -> Ty -> Ty
appOne = Type.app ()

appTwo :: Ty -> Ty -> Ty -> Ty
appTwo c x y = Type.app () (Type.app () c x) y

mkGiven ::
  Text ->
  [Symbol] ->
  [Ty] ->
  Ty ->
  Scope ->
  Given Symbol ()
mkGiven name vars premises concl s =
  Given
    { givenName = gname name,
      givenTyVars = vars,
      givenPremises = premises,
      givenConclusion = concl,
      givenScope = s
    }

-- | The chosen given's builtin name (for assertion).
chosenName :: ResolutionTree Symbol () -> Text
chosenName = refName . givenName . rtGiven

refName :: Reference -> Text
refName (Reference.Builtin n) = n
refName r = error ("unexpected non-builtin reference: " <> show r)

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

------------------------------------------------------------------------------
-- 1. Cycles terminate.
------------------------------------------------------------------------------

testCycles :: Test ()
testCycles =
  tests
    [ scope "Foo<->Bar mutual recursion fails cleanly" $
        let pool =
              poolFromList
                [ mkGiven "fooFromBar" [] [barC] fooC Ambient,
                  mkGiven "barFromFoo" [] [fooC] barC Ambient
                ]
         in case resolve pool fooC of
              Left (NoGiven _ _) -> ok
              Left e -> crash ("expected NoGiven, got: " <> show e)
              Right t -> crash ("expected failure, got: " <> show t),
      scope "Cycle with escape hatch resolves via the escape" $
        -- Two ways to resolve Foo: a base instance, and a cyclic
        -- detour via Bar->Foo. The cyclic candidate fails per-branch,
        -- but the base instance succeeds. Result is success or
        -- ambiguity — *not* a hang.
        let pool =
              poolFromList
                [ mkGiven "baseFoo" [] [] fooC Ambient,
                  mkGiven "fooFromBar" [] [barC] fooC Ambient,
                  mkGiven "barFromFoo" [] [fooC] barC Ambient
                ]
         in case resolve pool fooC of
              Right _ -> ok
              Left (Ambiguous _ _) -> ok
              Left e -> crash ("unexpected error: " <> show e)
    ]

------------------------------------------------------------------------------
-- 2. Specificity ordering.
------------------------------------------------------------------------------

testSpecificity :: Test ()
testSpecificity =
  tests
    [ scope "Show (List Nat) is preferred over Show (List a)" $
        let showListA =
              mkGiven
                "Show.list"
                [tvA]
                [appOne showC (tv tvA)]
                (appOne showC (appOne listC (tv tvA)))
                Ambient
            showListNat =
              mkGiven
                "Show.listNat"
                []
                []
                (appOne showC (appOne listC natC))
                Ambient
            showNat =
              mkGiven "Show.nat" [] [] (appOne showC natC) Ambient
            pool = poolFromList [showListA, showListNat, showNat]
            goal = appOne showC (appOne listC natC)
         in case resolve pool goal of
              Right rt -> expectEqual ("Show.listNat" :: Text) (chosenName rt)
              Left e -> crash ("expected success, got: " <> show e),
      scope "Show (List a) wins when no Nat-specific given is available" $
        let showListA =
              mkGiven
                "Show.list"
                [tvA]
                [appOne showC (tv tvA)]
                (appOne showC (appOne listC (tv tvA)))
                Ambient
            showNat =
              mkGiven "Show.nat" [] [] (appOne showC natC) Ambient
            pool = poolFromList [showListA, showNat]
            goal = appOne showC (appOne listC natC)
         in case resolve pool goal of
              Right rt -> expectEqual ("Show.list" :: Text) (chosenName rt)
              Left e -> crash ("expected success, got: " <> show e)
    ]

------------------------------------------------------------------------------
-- 3. HKT support.
------------------------------------------------------------------------------

testHkt :: Test ()
testHkt =
  tests
    [ scope "Functor List resolves" $
        let pool =
              poolFromList
                [ mkGiven "Functor.list" [] [] (appOne functorC listC) Ambient,
                  mkGiven "Functor.optional" [] [] (appOne functorC optionalC) Ambient
                ]
            goal = appOne functorC listC
         in case resolve pool goal of
              Right rt -> expectEqual ("Functor.list" :: Text) (chosenName rt)
              Left e -> crash ("expected success, got: " <> show e),
      scope "Functor Optional resolves" $
        let pool =
              poolFromList
                [ mkGiven "Functor.list" [] [] (appOne functorC listC) Ambient,
                  mkGiven "Functor.optional" [] [] (appOne functorC optionalC) Ambient
                ]
            goal = appOne functorC optionalC
         in case resolve pool goal of
              Right rt -> expectEqual ("Functor.optional" :: Text) (chosenName rt)
              Left e -> crash ("expected success, got: " <> show e),
      scope "Polymorphic Functor f matches a fresh constructor" $
        -- Functor.id : forall f. Functor f
        -- Functor.list : Functor List
        -- Goal: Functor List should pick the more-specific one.
        let pool =
              poolFromList
                [ mkGiven "Functor.id" [tvF] [] (appOne functorC (tv tvF)) Ambient,
                  mkGiven "Functor.list" [] [] (appOne functorC listC) Ambient
                ]
         in case resolve pool (appOne functorC listC) of
              Right rt -> expectEqual ("Functor.list" :: Text) (chosenName rt)
              Left e -> crash ("expected success, got: " <> show e)
    ]

------------------------------------------------------------------------------
-- 4. Ambiguity.
------------------------------------------------------------------------------

testAmbiguity :: Test ()
testAmbiguity =
  scope "Two givens of identical type produce Ambiguous" $
    let g1 = mkGiven "Show.nat.A" [] [] (appOne showC natC) Ambient
        g2 = mkGiven "Show.nat.B" [] [] (appOne showC natC) Ambient
        pool = poolFromList [g1, g2]
     in case resolve pool (appOne showC natC) of
          Left (Ambiguous _ candidates) ->
            let names = sort (map (refName . givenName) candidates)
             in expectEqual (["Show.nat.A", "Show.nat.B"] :: [Text]) names
          other -> crash ("expected Ambiguous, got: " <> show other)

------------------------------------------------------------------------------
-- 5. Performance: ~1000 givens, depth-20 chain in <2s.
------------------------------------------------------------------------------

testPerformance :: Test ()
testPerformance =
  scope "1000 givens, depth-20 chain in <2s" $ do
    let chainCons :: Int -> Ty
        chainCons k = ref ("Cn" <> tshow k)
        chainGivens =
          [ mkGiven
              ("chain" <> tshow k)
              []
              [chainCons (k + 1)]
              (chainCons k)
              Ambient
          | k <- [0 .. 19 :: Int]
          ]
        baseGiven = mkGiven "chainBase" [] [] (chainCons 20) Ambient
        noise =
          [ mkGiven
              ("noise" <> tshow k)
              []
              []
              (ref ("Noise" <> tshow k))
              Ambient
          | k <- [0 .. 979 :: Int]
          ]
        pool = poolFromList (chainGivens ++ [baseGiven] ++ noise)
        goal = chainCons 0
    (res, elapsed) <- io $ do
      t0 <- getCurrentTime
      let !r = resolve pool goal
      t1 <- getCurrentTime
      pure (r, realToFrac (diffUTCTime t1 t0) :: Double)
    case res of
      Right _ -> pure ()
      Left e -> crash ("expected success, got: " <> show e)
    if elapsed < 2.0
      then ok
      else crash ("chain resolved too slowly: " <> show elapsed <> "s")

------------------------------------------------------------------------------
-- 6. Diamond dependencies don't blow up.
------------------------------------------------------------------------------

testDiamond :: Test ()
testDiamond =
  tests
    [ scope "Show (Map (List Nat) Nat) memoizes Show Nat exactly once" $
        -- Givens:
        --   Show.nat       : Show Nat
        --   Show.list <a>  : Show a => Show (List a)
        --   Show.map  <k v>: Show k => Show v => Show (Map k v)
        --
        -- Goal: Show (Map (List Nat) Nat). Three distinct sub-goals
        -- in the dependency tree:
        --   Show (Map (List Nat) Nat), Show (List Nat), Show Nat
        let pool =
              poolFromList
                [ mkGiven "Show.nat" [] [] (appOne showC natC) Ambient,
                  mkGiven
                    "Show.list"
                    [tvA]
                    [appOne showC (tv tvA)]
                    (appOne showC (appOne listC (tv tvA)))
                    Ambient,
                  mkGiven
                    "Show.map"
                    [tvK, tvV]
                    [ appOne showC (tv tvK),
                      appOne showC (tv tvV)
                    ]
                    (appOne showC (appTwo mapC (tv tvK) (tv tvV)))
                    Ambient
                ]
            goal =
              appOne showC (appTwo mapC (appOne listC natC) natC)
            (res, work) = resolveCounted defaultOptions pool goal
         in case res of
              Right rt -> do
                expectEqual ("Show.map" :: Text) (chosenName rt)
                expectEqual 3 work
              Left e -> crash ("expected success, got: " <> show e),
      scope "Wider diamond: Show (Map (List Nat) (List Nat)) memoizes Show (List Nat) once" $
        let pool =
              poolFromList
                [ mkGiven "Show.nat" [] [] (appOne showC natC) Ambient,
                  mkGiven
                    "Show.list"
                    [tvA]
                    [appOne showC (tv tvA)]
                    (appOne showC (appOne listC (tv tvA)))
                    Ambient,
                  mkGiven
                    "Show.map"
                    [tvK, tvV]
                    [ appOne showC (tv tvK),
                      appOne showC (tv tvV)
                    ]
                    (appOne showC (appTwo mapC (tv tvK) (tv tvV)))
                    Ambient
                ]
            goal =
              appOne
                showC
                (appTwo mapC (appOne listC natC) (appOne listC natC))
            (res, work) = resolveCounted defaultOptions pool goal
         in case res of
              Right _ ->
                -- Distinct sub-goals: top-level, Show (List Nat), Show Nat.
                expectEqual 3 work
              Left e -> crash ("expected success, got: " <> show e)
    ]

------------------------------------------------------------------------------
-- Sanity tests.
------------------------------------------------------------------------------

testSanity :: Test ()
testSanity =
  tests
    [ scope "Trivial: Show Nat" $
        let pool = poolFromList [mkGiven "Show.nat" [] [] (appOne showC natC) Ambient]
         in case resolve pool (appOne showC natC) of
              Right rt -> expectEqual ("Show.nat" :: Text) (chosenName rt)
              Left e -> crash ("expected success, got: " <> show e),
      scope "Chained: Show (List Nat) via Show.list & Show.nat" $
        let pool =
              poolFromList
                [ mkGiven "Show.nat" [] [] (appOne showC natC) Ambient,
                  mkGiven
                    "Show.list"
                    [tvA]
                    [appOne showC (tv tvA)]
                    (appOne showC (appOne listC (tv tvA)))
                    Ambient
                ]
            goal = appOne showC (appOne listC natC)
         in case resolve pool goal of
              Right rt -> do
                expectEqual ("Show.list" :: Text) (chosenName rt)
                case rtChildren rt of
                  [inner] ->
                    expectEqual ("Show.nat" :: Text) (chosenName inner)
                  ps ->
                    crash ("expected exactly one premise, got: " <> show (length ps))
              Left e -> crash ("expected success, got: " <> show e),
      scope "Depth limit triggers a failure" $
        -- Self requires Self with no base case: with a small depth
        -- limit, this must fail (either DepthExceeded or NoGiven via
        -- the per-branch cycle path — both are acceptable).
        let pool = poolFromList [mkGiven "selfFromSelf" [] [selfC] selfC Ambient]
            opts = ResolveOptions {optMaxDepth = 5}
         in case resolveWith opts pool selfC of
              Left _ -> ok
              Right t -> crash ("expected failure, got: " <> show t),
      scope "lexical-inner local given beats ambient" $
        let local =
              mkGiven "Ord.flip.local" [] [] (appOne ordC natC) (Lexical 0)
            ambient =
              mkGiven "Ord.nat" [] [] (appOne ordC natC) Ambient
            pool = poolFromList [ambient, local]
         in case resolve pool (appOne ordC natC) of
              Right rt -> expectEqual ("Ord.flip.local" :: Text) (chosenName rt)
              Left e -> crash ("expected success, got: " <> show e)
    ]
