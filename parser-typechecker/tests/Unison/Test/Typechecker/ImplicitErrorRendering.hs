{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Message-rendering tests for the implicit-resolution error
-- categories. These tests exercise 'Unison.PrintError.printNoteWithSource'
-- on hand-built 'Result.UnresolvedImplicit' notes, and assert the
-- rendered output contains category-specific phrases plus the
-- relevant supporting detail (near-miss listing, candidate listing,
-- chain rendering, etc.).
--
-- Asserting on substrings rather than full byte-for-byte goldens
-- keeps the tests robust to color-text and word-wrap perturbations
-- while still verifying the user-visible categorical distinctions
-- are present.
module Unison.Test.Typechecker.ImplicitErrorRendering (test) where

import Data.List (isInfixOf)
import Data.Text qualified as Text
import EasyTest
import Unison.Parser.Ann (Ann (..))
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrintError qualified as PrintError
import Unison.Reference qualified as Reference
import Unison.Result qualified as Result
import Unison.Symbol (Symbol)
import Unison.Type qualified as Type
import Unison.Typechecker.GivenResolver
  ( Given (..),
    NearMiss (..),
    ResolveError (..),
    Scope (..),
  )
import Unison.Util.ColorText qualified as Color
import Unison.Util.Pretty qualified as Pr
import Unison.Var qualified as Var

------------------------------------------------------------------------------
-- Test entry point
------------------------------------------------------------------------------

test :: Test ()
test =
  scope "ImplicitErrorRendering" $
    tests
      [ scope "NoGiven-with-near-miss" testNoGivenNearMiss,
        scope "Ambiguous-lists-candidates" testAmbiguous,
        scope "DepthExceeded-shows-chain" testDepthExceeded,
        scope "Cycle-shows-cycle" testCycle,
        scope "UnresolvedMetavarInGoal-suggests-annotation" testUnresolvedMetavar
      ]

------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------

renderPlain :: Result.Note Symbol Ann -> String
renderPlain note =
  Text.unpack
    . Color.toPlain
    . Pr.render PrintError.defaultWidth
    $ PrintError.printNoteWithSource PPE.empty source note
  where
    -- A small bit of source so the location-hint code has something
    -- to point at. We don't assert on the source highlighting itself —
    -- only on the structured text the renderer emits — so any source
    -- string suffices.
    source = "x = 0\n"

-- | Check that each needle appears in the rendered output.
-- Whitespace in the haystack is normalised (newlines become spaces,
-- consecutive spaces collapse) so that word-wrap doesn't perturb
-- match expectations. Needles must therefore be written without
-- newlines.
containsAll :: String -> [String] -> Test ()
containsAll rendered needles = do
  let normalised = unwords (words rendered)
      missing = filter (not . (`isInfixOf` normalised)) needles
  if null missing
    then ok
    else
      crash $
        "rendered output missing expected substrings: "
          <> show missing
          <> "\nfull output:\n"
          <> rendered

------------------------------------------------------------------------------
-- Scenario builders
------------------------------------------------------------------------------

showC, listC, natC :: Type.Type Symbol Ann
showC = Type.builtin External "Show"
listC = Type.builtin External "List"
natC = Type.builtin External "Nat"

showListT :: Type.Type Symbol Ann
showListT = Type.app External showC listC

goalShowListNat :: Type.Type Symbol Ann
goalShowListNat = Type.app External showC (Type.app External listC natC)

mkGivenA :: Text.Text -> [Type.Type Symbol Ann] -> Type.Type Symbol Ann -> Given Symbol Ann
mkGivenA name premises concl =
  Given
    { givenName = Reference.Builtin name,
      givenTyVars = [],
      givenPremises = premises,
      givenConclusion = concl,
      givenScope = Ambient
    }

------------------------------------------------------------------------------
-- 1. NoGiven with one near-miss whose nested reason is also NoGiven.
------------------------------------------------------------------------------

testNoGivenNearMiss :: Test ()
testNoGivenNearMiss =
  let inner =
        NearMiss
          (mkGivenA "Show.list" [Type.app External showC natC] showListT)
          (NoGiven (Type.app External showC natC) [])
      err = NoGiven goalShowListNat [inner]
      note = Result.UnresolvedImplicit External goalShowListNat err
      rendered = renderPlain note
   in containsAll
        rendered
        [ "couldn't find a",
          "given",
          "Show.list",
          "failed because",
          "no given for"
        ]

------------------------------------------------------------------------------
-- 2. Ambiguous: two named candidates that both head-match.
------------------------------------------------------------------------------

testAmbiguous :: Test ()
testAmbiguous =
  let g1 = mkGivenA "Show.nat.A" [] (Type.app External showC natC)
      g2 = mkGivenA "Show.nat.B" [] (Type.app External showC natC)
      err = Ambiguous (Type.app External showC natC) [g1, g2]
      note = Result.UnresolvedImplicit External (Type.app External showC natC) err
      rendered = renderPlain note
   in containsAll
        rendered
        [ "multiple",
          "ambiguous",
          "Show.nat.A",
          "Show.nat.B"
        ]

------------------------------------------------------------------------------
-- 3. DepthExceeded: a long chain (truncated by the renderer if needed).
------------------------------------------------------------------------------

testDepthExceeded :: Test ()
testDepthExceeded =
  let chain =
        [ Type.builtin External (Text.pack ("Step" <> show k))
        | k <- [0 .. 14 :: Int]
        ]
      err :: ResolveError Symbol Ann
      err = DepthExceeded chain
      note = Result.UnresolvedImplicit External (head chain) err
      rendered = renderPlain note
   in containsAll
        rendered
        [ "exceeded the maximum depth",
          "Step0",
          -- The renderer shows head and tail; Step14 (last) should
          -- appear regardless of truncation.
          "Step14"
        ]

------------------------------------------------------------------------------
-- 4. Cycle: explicit hard-cycle diagnosis with the cycle path.
------------------------------------------------------------------------------

testCycle :: Test ()
testCycle =
  let cycleChain =
        [ Type.builtin External "Foo",
          Type.builtin External "Bar",
          Type.builtin External "Foo"
        ]
      err :: ResolveError Symbol Ann
      err = Cycle cycleChain
      note = Result.UnresolvedImplicit External (head cycleChain) err
      rendered = renderPlain note
   in containsAll
        rendered
        [ "cycle",
          "Foo",
          "Bar"
        ]

------------------------------------------------------------------------------
-- 5. UnresolvedMetavarInGoal: distinct user-visible message.
------------------------------------------------------------------------------

testUnresolvedMetavar :: Test ()
testUnresolvedMetavar =
  let inferVar :: Symbol
      inferVar = Var.inferOther
      goal = Type.app External showC (Type.var External inferVar)
      err :: ResolveError Symbol Ann
      err = UnresolvedMetavarInGoal goal
      note = Result.UnresolvedImplicit External goal err
      rendered = renderPlain note
   in containsAll
        rendered
        [ "unsolved type variable",
          "Add a type annotation"
        ]
