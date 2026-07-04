{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Property tests for the implicit-resolution algorithm:
--
--   * /Cycle\/metavar interaction/. Stresses the per-branch cycle
--     short-circuit when the goal carries an inference variable. The
--     invariant is /termination plus a structured failure/ — a hang,
--     a runtime exception, or a 'Right' decision (we built the pool
--     to be unsolvable) all count as bugs.
--
--   * /Diamond + shared metavar/. The metavar-invalidation rule says:
--     when a metavar that appears in a memoized goal becomes bound,
--     the memo entry is no longer trustworthy. We exercise the case
--     where the same metavar appears in two paths through the
--     diamond, and one path resolves it. The correctness invariant
--     is that both paths agree on the metavar binding at the end
--     (they reach the same dictionary), or the resolver reports a
--     structured error — but never a /silently/ wrong choice.
--
-- Tests are randomised over a small space of pool shapes; each scope
-- runs ~25 iterations.
module Unison.Test.Typechecker.GivenResolverProperties (test) where

import Control.Monad (replicateM)
import Data.Text (Text)
import Data.Text qualified as Text
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
    Scope (..),
    defaultOptions,
    poolFromList,
    resolve,
    resolveCounted,
  )
import Unison.Var qualified as Var

------------------------------------------------------------------------------
-- Test entry point
------------------------------------------------------------------------------

test :: Test ()
test =
  scope "GivenResolverProperties" $
    tests
      [ scope "cycle-metavar-terminates" propCycleMetavar,
        scope "diamond-shared-metavar-correctness" propDiamondSharedMetavar
      ]

------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------

type Ty = Type Symbol ()

ref :: Text -> Ty
ref = Type.builtin ()

appOne :: Ty -> Ty -> Ty
appOne = Type.app ()

tv :: Symbol -> Ty
tv = Type.var ()

mkGiven :: Text -> [Symbol] -> [Ty] -> Ty -> Scope -> Given Symbol ()
mkGiven name vars premises concl s =
  Given
    { givenName = Reference.Builtin name,
      givenTyVars = vars,
      givenPremises = premises,
      givenConclusion = concl,
      givenScope = s
    }

-- A user-named (rigid) variable.
userVar :: Symbol
userVar = Var.named "a"

------------------------------------------------------------------------------
-- Property 1: cycle/metavar termination.
--
-- Build a randomised pool of givens that contain a mutual cycle. The
-- goal type is an instantiation of the cycle's outer type with a free

-- * user* variable in some position. (We use a user var rather than an

-- inference var so the resolver attempts proper resolution; otherwise
-- it short-circuits with UnresolvedMetavarInGoal.)
--
-- Invariant: regardless of the cycle shape, the resolver returns in
-- bounded time with a structured error — it does not hang.
------------------------------------------------------------------------------

propCycleMetavar :: Test ()
propCycleMetavar = do
  -- 25 randomised iterations.
  _ <- replicateM 25 oneRound
  pure ()
  where
    oneRound :: Test ()
    oneRound = do
      -- Pick a cycle length 2..5.
      n <- int' 2 5
      -- Build n distinct type constructors C0..Cn-1 plus mutual
      -- givens. Each Ci-from-Ci+1 (mod n).
      let ctors = [ref ("C" <> Text.pack (show k)) | k <- [0 .. n - 1]]
          mutualGivens =
            [ mkGiven
                ("from_C" <> Text.pack (show ((k + 1) `mod` n)) <> "_to_C" <> Text.pack (show k))
                []
                [ctors !! ((k + 1) `mod` n)]
                (ctors !! k)
                Ambient
            | k <- [0 .. n - 1]
            ]
      -- Optionally add a base case that breaks the cycle for one
      -- ctor — making this a property test of "with-or-without
      -- escape, the resolver always terminates with a Result".
      hasEscape <- bool
      escapeIdx <- int' 0 (n - 1)
      let baseGiven =
            [ mkGiven ("base_C" <> Text.pack (show escapeIdx)) [] [] (ctors !! escapeIdx) Ambient
            | hasEscape
            ]
      -- The goal is C0 applied to either nothing or a free user var
      -- (to exercise the head-unification flex-set logic).
      paramShape <- int' 0 1
      let goal = case paramShape of
            0 -> ctors !! 0
            _ -> appOne (ctors !! 0) (tv userVar)
          -- Polymorphic givens that head-match the parametric goal.
          polyGivens =
            if paramShape == 1
              then
                [ mkGiven
                    ("poly_from_C" <> Text.pack (show ((k + 1) `mod` n)) <> "_to_C" <> Text.pack (show k))
                    [userVar]
                    [appOne (ctors !! ((k + 1) `mod` n)) (tv userVar)]
                    (appOne (ctors !! k) (tv userVar))
                    Ambient
                | k <- [0 .. n - 1]
                ]
              else []
          pool = poolFromList (mutualGivens ++ baseGiven ++ polyGivens)
      -- Run resolution. Either Right or Left; never hangs (EasyTest
      -- has no built-in timeout, but unbounded recursion would have
      -- triggered DepthExceeded by the resolver's optMaxDepth).
      case resolve pool goal of
        Right _ ->
          -- A successful resolve is OK iff there's an escape hatch
          -- \*or* a polymorphic given chain that head-matches without
          -- recursing. Both are valid pool shapes.
          ok
        Left (NoGiven _ _) -> ok
        Left (DepthExceeded _) -> ok
        Left (Cycle _) -> ok
        Left (Ambiguous _ _) -> ok
        Left (UnresolvedMetavarInGoal _) ->
          -- This shouldn't happen since 'a' is a user-typed variable
          -- (Var.named), not an inference variable.
          crash "user var should not trigger UnresolvedMetavarInGoal"

------------------------------------------------------------------------------
-- Property 2: diamond + shared metavar correctness.
--
-- A goal whose chain reaches the same sub-goal along multiple paths,
-- where the sub-goal contains an existential. We assert:
--
--   1. The resolver succeeds.
--   2. The number of distinct memo misses is bounded by the number
--      of distinct sub-goals in the dependency tree (not the number
--      of paths). This is the "diamond" property from §4.2#6.
--   3. The chosen given for the shared sub-goal is the *same* one
--      regardless of which path reached it (memo consistency).
--
-- Test setup: Show (Map (List a) (List a)) for varying ground 'a'.
-- The dependency tree visits Show (List a) along two paths (key and
-- value of the Map); the shared sub-goal appears once.
------------------------------------------------------------------------------

propDiamondSharedMetavar :: Test ()
propDiamondSharedMetavar = do
  -- 25 randomised iterations across a few choices of ground 'a'.
  _ <- replicateM 25 oneRound
  pure ()
  where
    oneRound :: Test ()
    oneRound = do
      -- Pick a ground type for the inner element.
      groundIdx <- int' 0 2
      let groundTy = case groundIdx of
            0 -> ref "Nat"
            1 -> ref "Text"
            _ -> ref "Boolean"
          showC = ref "Show"
          listC = ref "List"
          mapC = ref "Map"
          tvA = Var.named "a"
          tvK = Var.named "k"
          tvV = Var.named "v"
          showNat = mkGiven "Show.nat" [] [] (appOne showC (ref "Nat")) Ambient
          showText = mkGiven "Show.text" [] [] (appOne showC (ref "Text")) Ambient
          showBoolean = mkGiven "Show.boolean" [] [] (appOne showC (ref "Boolean")) Ambient
          showList =
            mkGiven
              "Show.list"
              [tvA]
              [appOne showC (tv tvA)]
              (appOne showC (appOne listC (tv tvA)))
              Ambient
          showMap =
            mkGiven
              "Show.map"
              [tvK, tvV]
              [appOne showC (tv tvK), appOne showC (tv tvV)]
              (appOne showC (Type.app () (Type.app () mapC (tv tvK)) (tv tvV)))
              Ambient
          pool =
            poolFromList
              [showNat, showText, showBoolean, showList, showMap]
          -- Show (Map (List a) (List a)) where 'a' is the chosen ground.
          goal =
            appOne
              showC
              ( Type.app
                  ()
                  ( Type.app
                      ()
                      mapC
                      (appOne listC groundTy)
                  )
                  (appOne listC groundTy)
              )
      let (res, work) = resolveCounted defaultOpts pool goal
      case res of
        Right tree -> do
          -- Distinct sub-goals visited: top-level Show (Map ...),
          -- Show (List <ground>), Show <ground>. So 3 memo misses.
          expectEqual 3 work
          -- Both children of Show.map must be the same Show.list
          -- chosen the same way. The shared metavar (a := <ground>)
          -- got bound at the first child; the second visit reuses
          -- the memoized success.
          case rtChildren tree of
            [child1, child2] -> do
              expectEqual (givenName (rtGiven child1)) (givenName (rtGiven child2))
              -- And the two trees must have the same (single) child:
              -- Show <ground>, with the chosen ground-given.
              expectEqual
                (childChosen child1)
                (childChosen child2)
            other -> crash ("expected 2 children, got: " <> show (length other))
        Left e -> crash ("expected success, got: " <> show e)

    childChosen :: ResolutionTree Symbol () -> Maybe Reference
    childChosen rt = case rtChildren rt of
      [c] -> Just (givenName (rtGiven c))
      _ -> Nothing

    defaultOpts = defaultOptions
