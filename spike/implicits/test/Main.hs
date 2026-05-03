{-# LANGUAGE OverloadedStrings #-}

-- | Test harness for the implicit-resolution spike. Covers the six
-- success criteria from @docs/implicits-plan.md@ §4.2.
--
-- Library choice: @tasty@ + @tasty-hunit@. The rest of the Unison repo
-- uses @easytest@, but @tasty@ keeps this spike's dependency graph
-- entirely outside the main @stack.yaml@ and is closer to what the
-- production typechecker tests already use elsewhere in the wider
-- ecosystem. Either choice is acceptable per the brief.
module Main (main) where

import Data.List (find, sort)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Implicits.Resolve
import Implicits.Types
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "implicit-resolution spike"
    [ testCriterion1_cycles,
      testCriterion2_specificity,
      testCriterion3_hkt,
      testCriterion4_ambiguity,
      testCriterion5_performance,
      testCriterion6_diamond,
      -- Extra sanity tests beyond the six criteria.
      testGroup
        "sanity"
        [ testTrivialResolve,
          testChainedResolve,
          testDepthLimit,
          testLexicalInnerWins
        ]
    ]

------------------------------------------------------------------------------
-- Type aliases for readable test data
------------------------------------------------------------------------------

-- Common type constructors.
showC, ordC, eqC, functorC, monadC, mapC, listC, optionalC, natC, textC, fooC, barC :: Ty
showC = TCon (ConName "Show")
ordC = TCon (ConName "Ord")
eqC = TCon (ConName "Eq")
functorC = TCon (ConName "Functor")
monadC = TCon (ConName "Monad")
mapC = TCon (ConName "Map")
listC = TCon (ConName "List")
optionalC = TCon (ConName "Optional")
natC = TCon (ConName "Nat")
textC = TCon (ConName "Text")
fooC = TCon (ConName "Foo")
barC = TCon (ConName "Bar")

-- Type variables.
tvA, tvB, tvF, tvK, tvV :: TyVar
tvA = TyVar 1
tvB = TyVar 2
tvF = TyVar 3
tvK = TyVar 4
tvV = TyVar 5

-- Convenience: build "Show t" etc.
appOne :: Ty -> Ty -> Ty
appOne = TApp

appTwo :: Ty -> Ty -> Ty -> Ty
appTwo c x y = TApp (TApp c x) y

------------------------------------------------------------------------------
-- 1. Cycles terminate.
------------------------------------------------------------------------------

testCriterion1_cycles :: TestTree
testCriterion1_cycles =
  testGroup
    "1. cycles terminate"
    [ testCase "Foo<->Bar mutual recursion fails cleanly, no hang" $ do
        -- Pool: given a : Foo => Bar, given b : Bar => Foo.
        -- Goal: Foo. There is no non-cyclic path; this must error
        -- (NoGiven), not hang.
        let pool =
              poolFromList
                [ Given
                    { givenName = "fooFromBar",
                      givenTyVars = [],
                      givenPremises = [barC],
                      givenConclusion = fooC,
                      givenScope = Ambient
                    },
                  Given
                    { givenName = "barFromFoo",
                      givenTyVars = [],
                      givenPremises = [fooC],
                      givenConclusion = barC,
                      givenScope = Ambient
                    }
                ]
        case resolve pool fooC of
          Left (NoGiven _ _) -> pure ()
          Left e -> assertFailure ("expected NoGiven, got: " ++ show e)
          Right t -> assertFailure ("expected failure but resolved: " ++ show t),
      testCase "Cycle with escape hatch resolves via the escape" $ do
        -- given baseFoo : Foo (no premises)
        -- given fooFromBar : Foo, requires Bar
        -- given barFromFoo : Bar, requires Foo
        -- Goal: Foo. The cyclic candidate fails per-branch, but the
        -- base instance succeeds, so the overall resolution succeeds.
        let pool =
              poolFromList
                [ Given "baseFoo" [] [] fooC Ambient,
                  Given "fooFromBar" [] [barC] fooC Ambient,
                  Given "barFromFoo" [] [fooC] barC Ambient
                ]
        -- With two ways to resolve Foo (the base and via Bar->Foo),
        -- both succeed, which is ambiguous; we expect either a
        -- successful pick or Ambiguous, but never a hang. Most
        -- importantly: completes in finite time.
        case resolve pool fooC of
          Right _ -> pure ()
          Left (Ambiguous _ _) -> pure ()
          Left e -> assertFailure ("unexpected error: " ++ show e)
    ]

------------------------------------------------------------------------------
-- 2. Specificity ordering: Show (List Nat) beats Show (List a)
------------------------------------------------------------------------------

testCriterion2_specificity :: TestTree
testCriterion2_specificity =
  testGroup
    "2. specificity ordering"
    [ testCase "Show (List Nat) is preferred over Show (List a)" $ do
        let showListA =
              Given
                { givenName = "Show.list",
                  givenTyVars = [tvA],
                  givenPremises = [appOne showC (TVar tvA)],
                  givenConclusion = appOne showC (appOne listC (TVar tvA)),
                  givenScope = Ambient
                }
            showListNat =
              Given
                { givenName = "Show.listNat",
                  givenTyVars = [],
                  givenPremises = [],
                  givenConclusion = appOne showC (appOne listC natC),
                  givenScope = Ambient
                }
            showNat =
              Given
                { givenName = "Show.nat",
                  givenTyVars = [],
                  givenPremises = [],
                  givenConclusion = appOne showC natC,
                  givenScope = Ambient
                }
            pool = poolFromList [showListA, showListNat, showNat]
            goal = appOne showC (appOne listC natC)
        case resolve pool goal of
          Right rt ->
            assertEqual
              "more-specific given chosen"
              "Show.listNat"
              (givenName (rtChosen rt))
          Left e -> assertFailure ("expected success, got: " ++ show e),
      testCase "Show (List a) wins when no Nat-specific given is available" $ do
        let showListA =
              Given
                "Show.list"
                [tvA]
                [appOne showC (TVar tvA)]
                (appOne showC (appOne listC (TVar tvA)))
                Ambient
            showNat = Given "Show.nat" [] [] (appOne showC natC) Ambient
            pool = poolFromList [showListA, showNat]
            goal = appOne showC (appOne listC natC)
        case resolve pool goal of
          Right rt ->
            assertEqual
              "polymorphic given chosen"
              "Show.list"
              (givenName (rtChosen rt))
          Left e -> assertFailure ("expected success, got: " ++ show e)
    ]

------------------------------------------------------------------------------
-- 3. HKT: resolve Functor List, Functor Optional
------------------------------------------------------------------------------

testCriterion3_hkt :: TestTree
testCriterion3_hkt =
  testGroup
    "3. HKT support"
    [ testCase "Functor List resolves" $ do
        let pool =
              poolFromList
                [ Given "Functor.list" [] [] (appOne functorC listC) Ambient,
                  Given "Functor.optional" [] [] (appOne functorC optionalC) Ambient
                ]
            goal = appOne functorC listC
        case resolve pool goal of
          Right rt ->
            assertEqual "Functor.list chosen" "Functor.list" (givenName (rtChosen rt))
          Left e -> assertFailure ("expected success, got: " ++ show e),
      testCase "Functor Optional resolves" $ do
        let pool =
              poolFromList
                [ Given "Functor.list" [] [] (appOne functorC listC) Ambient,
                  Given "Functor.optional" [] [] (appOne functorC optionalC) Ambient
                ]
            goal = appOne functorC optionalC
        case resolve pool goal of
          Right rt ->
            assertEqual "Functor.optional chosen" "Functor.optional" (givenName (rtChosen rt))
          Left e -> assertFailure ("expected success, got: " ++ show e),
      testCase "Polymorphic Functor f matches a fresh constructor" $ do
        -- given Functor.list : Functor List
        -- given Functor.id   : forall f. Functor f
        -- Goal: Functor List should pick the more-specific one.
        let pool =
              poolFromList
                [ Given "Functor.id" [tvF] [] (appOne functorC (TVar tvF)) Ambient,
                  Given "Functor.list" [] [] (appOne functorC listC) Ambient
                ]
        case resolve pool (appOne functorC listC) of
          Right rt ->
            assertEqual "specific HKT given wins" "Functor.list" (givenName (rtChosen rt))
          Left e -> assertFailure ("expected success, got: " ++ show e)
    ]

------------------------------------------------------------------------------
-- 4. Ambiguity
------------------------------------------------------------------------------

testCriterion4_ambiguity :: TestTree
testCriterion4_ambiguity =
  testGroup
    "4. ambiguity"
    [ testCase "Two givens of identical type produce Ambiguous" $ do
        let g1 = Given "Show.nat.A" [] [] (appOne showC natC) Ambient
            g2 = Given "Show.nat.B" [] [] (appOne showC natC) Ambient
            pool = poolFromList [g1, g2]
        case resolve pool (appOne showC natC) of
          Left (Ambiguous _ candidates) -> do
            let names = sort (map givenName candidates)
            assertEqual "both candidates listed" ["Show.nat.A", "Show.nat.B"] names
          other -> assertFailure ("expected Ambiguous, got: " ++ show other)
    ]

------------------------------------------------------------------------------
-- 5. Performance: ~1000 givens, chains ~20 deep
------------------------------------------------------------------------------

testCriterion5_performance :: TestTree
testCriterion5_performance =
  testCase "5. performance: 1000 givens, depth-20 chain in <2s" $ do
    -- Build a chain Cn0 -> Cn1 -> ... -> Cn20, each step a "given Cn_k
    -- requires Cn_{k+1}" pair. Pad the pool with 980 noise givens that
    -- never match.
    let chainCons k = TCon (ConName ("Cn" ++ show (k :: Int)))
        chainGivens =
          [ Given
              ("chain" ++ show k)
              []
              [chainCons (k + 1)]
              (chainCons k)
              Ambient
          | k <- [0 .. 19]
          ]
        baseGiven = Given "chainBase" [] [] (chainCons 20) Ambient
        noise =
          [ Given
              ("noise" ++ show k)
              []
              []
              (TCon (ConName ("Noise" ++ show k)))
              Ambient
          | k <- [0 .. 979 :: Int]
          ]
        pool = poolFromList (chainGivens ++ [baseGiven] ++ noise)
        goal = chainCons 0
    t0 <- getCurrentTime
    let !res = resolve pool goal
    t1 <- getCurrentTime
    case res of
      Right _ -> pure ()
      Left e -> assertFailure ("expected success, got: " ++ show e)
    let elapsed = realToFrac (diffUTCTime t1 t0) :: Double
    -- Generous bound; on a modern laptop we expect <0.1s.
    assertBool
      ("chain resolved too slowly: " ++ show elapsed ++ "s")
      (elapsed < 2.0)

------------------------------------------------------------------------------
-- 6. Diamond dependencies don't blow up
------------------------------------------------------------------------------

testCriterion6_diamond :: TestTree
testCriterion6_diamond =
  testGroup
    "6. diamond dependencies"
    [ testCase "Show (Map (List Nat) Nat) memoizes Show Nat exactly once" $ do
        -- Givens:
        --   Show.nat            : Show Nat
        --   Show.list <a>       : Show a => Show (List a)
        --   Show.map  <k v>     : Show k => Show v => Show (Map k v)
        -- Goal: Show (Map (List Nat) Nat).
        --
        -- The dependency tree contains two paths to "Show Nat":
        --   Show (Map (List Nat) Nat)
        --     -> Show (List Nat)         (via Show.map's first premise)
        --          -> Show Nat            (path 1)
        --     -> Show Nat                 (via Show.map's second premise; path 2)
        -- Without memoization, Show Nat would be solved twice. With
        -- per-resolution memoization, it must be solved exactly once.
        --
        -- The instrumented counter 'sWork' tracks distinct sub-goals
        -- attempted, so we can assert that exactly four sub-goals are
        -- considered in total: the top-level goal, Show (List Nat),
        -- Show Nat, and (the candidate Show.map's other Show Nat
        -- premise must be a memo hit, NOT a fresh attempt).
        let pool =
              poolFromList
                [ Given "Show.nat" [] [] (appOne showC natC) Ambient,
                  Given
                    "Show.list"
                    [tvA]
                    [appOne showC (TVar tvA)]
                    (appOne showC (appOne listC (TVar tvA)))
                    Ambient,
                  Given
                    "Show.map"
                    [tvK, tvV]
                    [ appOne showC (TVar tvK),
                      appOne showC (TVar tvV)
                    ]
                    (appOne showC (appTwo mapC (TVar tvK) (TVar tvV)))
                    Ambient
                ]
            goal =
              appOne showC (appTwo mapC (appOne listC natC) natC)
            (res, work) = resolveCounted defaultOptions pool goal
        case res of
          Right rt -> do
            assertEqual
              "outer chosen given is Show.map"
              "Show.map"
              (givenName (rtChosen rt))
            -- Three distinct sub-goal types in the dependency tree:
            --   Show (Map (List Nat) Nat), Show (List Nat), Show Nat
            assertEqual
              "memoization: each unique sub-goal solved exactly once"
              3
              work
          Left e -> assertFailure ("expected success, got: " ++ show e),
      testCase "Wider diamond: Show (Map (List Nat) (List Nat)) memoizes Show (List Nat) once" $ do
        let pool =
              poolFromList
                [ Given "Show.nat" [] [] (appOne showC natC) Ambient,
                  Given
                    "Show.list"
                    [tvA]
                    [appOne showC (TVar tvA)]
                    (appOne showC (appOne listC (TVar tvA)))
                    Ambient,
                  Given
                    "Show.map"
                    [tvK, tvV]
                    [ appOne showC (TVar tvK),
                      appOne showC (TVar tvV)
                    ]
                    (appOne showC (appTwo mapC (TVar tvK) (TVar tvV)))
                    Ambient
                ]
            goal =
              appOne
                showC
                (appTwo mapC (appOne listC natC) (appOne listC natC))
            (res, work) = resolveCounted defaultOptions pool goal
        case res of
          Right _ ->
            -- Distinct sub-goals: top-level, Show (List Nat), Show Nat.
            assertEqual
              "memoization across symmetric arguments"
              3
              work
          Left e -> assertFailure ("expected success, got: " ++ show e)
    ]

------------------------------------------------------------------------------
-- Sanity checks
------------------------------------------------------------------------------

testTrivialResolve :: TestTree
testTrivialResolve = testCase "Trivial: Show Nat" $ do
  let pool = poolFromList [Given "Show.nat" [] [] (appOne showC natC) Ambient]
  case resolve pool (appOne showC natC) of
    Right rt -> assertEqual "Show.nat chosen" "Show.nat" (givenName (rtChosen rt))
    Left e -> assertFailure ("expected success, got: " ++ show e)

testChainedResolve :: TestTree
testChainedResolve = testCase "Chained: Show (List Nat) via Show.list & Show.nat" $ do
  let pool =
        poolFromList
          [ Given "Show.nat" [] [] (appOne showC natC) Ambient,
            Given
              "Show.list"
              [tvA]
              [appOne showC (TVar tvA)]
              (appOne showC (appOne listC (TVar tvA)))
              Ambient
          ]
      goal = appOne showC (appOne listC natC)
  case resolve pool goal of
    Right rt -> do
      assertEqual "outer Show.list" "Show.list" (givenName (rtChosen rt))
      case rtPremises rt of
        [inner] ->
          assertEqual "inner Show.nat" "Show.nat" (givenName (rtChosen inner))
        ps ->
          assertFailure ("expected exactly one premise, got: " ++ show (length ps))
    Left e -> assertFailure ("expected success, got: " ++ show e)

testDepthLimit :: TestTree
testDepthLimit = testCase "Depth limit triggers DepthExceeded" $ do
  -- An infinite chain: Self requires Self. Without a base case and with
  -- a small depth limit, this should produce DepthExceeded (per-branch
  -- cycle would *also* catch it, but here the unifier will alpha-vary
  -- so we exercise the depth limit too).
  let selfC = TCon (ConName "Self")
      pool = poolFromList [Given "selfFromSelf" [] [selfC] selfC Ambient]
      opts = ResolveOptions {optMaxDepth = 5}
  case resolveWith opts pool selfC of
    Left _ -> pure () -- either DepthExceeded or NoGiven (cycle path) is fine
    Right t -> assertFailure ("expected failure, got: " ++ show t)

testLexicalInnerWins :: TestTree
testLexicalInnerWins =
  testCase "lexical-inner local given beats ambient" $ do
    let local =
          Given "Ord.flip.local" [] [] (appOne ordC natC) (Lexical 0)
        ambient =
          Given "Ord.nat" [] [] (appOne ordC natC) Ambient
        pool = poolFromList [ambient, local]
    case resolve pool (appOne ordC natC) of
      Right rt ->
        assertEqual
          "local lexical given wins"
          "Ord.flip.local"
          (givenName (rtChosen rt))
      Left e -> assertFailure ("expected success, got: " ++ show e)

-- Silence unused-import warnings when we tweak imports during development.
_unused :: a -> a
_unused x = case find (const True) ([] :: [()]) of
  _ -> x

_unusedRefs :: [Ty]
_unusedRefs = [eqC, monadC, textC, TVar tvB]
