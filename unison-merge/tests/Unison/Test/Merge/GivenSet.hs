-- | Tests for given-set conflict resolution per ADR-021.
--
-- Each of the four ADR-021 cases has a dedicated scope:
--
--   * Case (a) — both branches mark the same hash given.
--   * Case (b) — one branch marks, the other does not.
--   * Case (c) — both branches rename the same given to different
--     names (post-resolution: the surviving name carries the mark).
--   * Case (d) — one branch marks, the other deletes the definition
--     (post-resolution: surviving definition carries the mark; if the
--     delete won, the mark is dropped).
module Unison.Test.Merge.GivenSet
  ( test,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import EasyTest
import Unison.Merge (MergeMode (..), ThreeWay (..))
import Unison.Merge.GivenSet qualified as GivenSet
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment.Internal qualified as NameSegment
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent

-- ---------------------------------------------------------------------------
-- Test fixtures
-- ---------------------------------------------------------------------------

-- Construct a 'Name' from a dot-separated string. We bypass the
-- syntax parser here because the test suite is intentionally lean on
-- dependencies; we only need names that participate in 'Eq'/'Ord'
-- comparisons, not parser-driven name validation.
n :: Text -> Name
n s =
  Name.fromSegments . NonEmpty.fromList . map seg . filter (not . nullText) $ splitDots s
  where
    seg :: Text -> NameSegment
    seg = NameSegment.NameSegment

    nullText :: Text -> Bool
    nullText t = t == ""

    splitDots :: Text -> [Text]
    splitDots = goDots []

    goDots :: [Text] -> Text -> [Text]
    goDots acc t =
      case Text.break (== '.') t of
        (a, "") -> reverse (a : acc)
        (a, rest) -> goDots (a : acc) (Text.drop 1 rest)

-- An arbitrary builtin term referent used as a stand-in for the
-- referent that's marked given. The merge logic only cares that the
-- referent is 'Eq'/'Ord'-able.
refA :: Referent
refA = Referent.Ref (Reference.Builtin "Test.refA")

-- Convenience: a one-element 'GivenMarks'.
single :: Text -> Referent -> GivenSet.GivenMarks
single name ref = GivenSet.fromList [(n name, ref)]

empty :: GivenSet.GivenMarks
empty = GivenSet.emptyGivenMarks

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

test :: Test ()
test =
  scope "given-set-merge" . tests $
    [ caseA,
      caseB,
      caseB_lcaHadMark,
      caseB_nonInteractive,
      caseC,
      caseD_deleteWon,
      caseD_markSurvives,
      regression_emptyIsNoOp
    ]

-- Case (a): both branches mark the same name. Per ADR-021 the merged
-- branch keeps the mark; no prompt is produced.
caseA :: Test ()
caseA =
  scope "case (a) — both mark same hash" do
    let outcome =
          GivenSet.mergeGivenSets
            Interactive
            ThreeWay
              { lca = empty,
                alice = single "Show.nat" refA,
                bob = single "Show.nat" refA
              }
    expect (outcome.merged == single "Show.nat" refA)
    expect (null outcome.prompts)

-- Case (b), interactive: only one side has the mark. The mark wins by
-- default and a prompt is emitted for caller confirmation.
caseB :: Test ()
caseB =
  scope "case (b) — one marks, the other doesn't (interactive)" do
    let outcome =
          GivenSet.mergeGivenSets
            Interactive
            ThreeWay
              { lca = empty,
                alice = single "Show.nat" refA,
                bob = empty
              }
    -- Default: mark wins.
    expect (outcome.merged == single "Show.nat" refA)
    -- One prompt, attributed to Alice.
    case outcome.prompts of
      [c] -> do
        expect (c.name == n "Show.nat")
        expect (c.def == refA)
        expect (c.side == GivenSet.AliceMarked)
      _ -> crash "expected exactly one prompt"

-- Case (b) variant: LCA had the mark, Bob explicitly removed it,
-- Alice kept it. Per ADR-021, dropping a deliberate mark is the
-- surprising direction; we keep the mark by default and surface a
-- prompt.
caseB_lcaHadMark :: Test ()
caseB_lcaHadMark =
  scope "case (b) — LCA had mark, Bob unmarked, Alice kept" do
    let outcome =
          GivenSet.mergeGivenSets
            Interactive
            ThreeWay
              { lca = single "Show.nat" refA,
                alice = single "Show.nat" refA,
                bob = empty
              }
    expect (outcome.merged == single "Show.nat" refA)
    case outcome.prompts of
      [c] -> do
        expect (c.name == n "Show.nat")
        expect (c.side == GivenSet.AliceMarked)
      _ -> crash "expected exactly one prompt"

-- Case (b), non-interactive (CI default): same input as caseB, but
-- mode is NonInteractive so prompts are auto-resolved silently. The
-- mark still wins.
caseB_nonInteractive :: Test ()
caseB_nonInteractive =
  scope "case (b) — non-interactive default is mark-wins (CI mode)" do
    let outcome =
          GivenSet.mergeGivenSets
            NonInteractive
            ThreeWay
              { lca = empty,
                alice = single "Show.nat" refA,
                bob = empty
              }
    expect (outcome.merged == single "Show.nat" refA)
    expect (null outcome.prompts)

-- Case (c): both branches rename the same given to different names.
-- The merge logic itself doesn't pick the winning name (that's the
-- existing rename-conflict path), but it must travel the mark with
-- the surviving hash. We model this by running 'mergeGivenSets'
-- followed by 'applyGivenSet' against a survivor map representing
-- the rename outcome.
caseC :: Test ()
caseC =
  scope "case (c) — rename conflict; mark travels with hash" do
    -- LCA: Show.nat = refA (marked given).
    -- Alice renames to ShowNat (still refA, still marked).
    -- Bob renames to Show.Nat (still refA, still marked).
    -- Pretend the rename-conflict path picks "ShowNat" as the winner.
    let outcome =
          GivenSet.mergeGivenSets
            NonInteractive
            ThreeWay
              { lca = single "Show.nat" refA,
                alice = single "ShowNat" refA,
                bob = single "Show.Nat" refA
              }
        survivors :: GivenSet.SurvivorMap
        survivors = Map.fromList [(n "ShowNat", refA)]
        pruned = GivenSet.applyGivenSet survivors outcome.merged
    -- After applying, the mark lives at the surviving name with the
    -- original hash.
    expect (pruned == single "ShowNat" refA)
    -- Symmetric check: if "Show.Nat" had won instead, the mark would
    -- live there.
    let survivors' = Map.fromList [(n "Show.Nat", refA)]
        pruned' = GivenSet.applyGivenSet survivors' outcome.merged
    expect (pruned' == single "Show.Nat" refA)

-- Case (d), delete-won variant: Alice marks, Bob deletes, the user
-- (or merge policy) accepts the delete. The mark is dropped because
-- there's no definition to attach it to.
caseD_deleteWon :: Test ()
caseD_deleteWon =
  scope "case (d) — mark vs delete; delete wins, mark is dropped" do
    let outcome =
          GivenSet.mergeGivenSets
            NonInteractive
            ThreeWay
              { lca = empty,
                alice = single "Show.nat" refA,
                bob = empty
              }
        -- No survivors: the definition was deleted.
        survivors :: GivenSet.SurvivorMap
        survivors = Map.empty
        pruned = GivenSet.applyGivenSet survivors outcome.merged
    expect (pruned == empty)

-- Case (d), mark-survives variant: Alice marks, Bob deletes, the
-- existing delete-conflict path restores Alice's definition. The
-- mark must travel with the restored hash.
caseD_markSurvives :: Test ()
caseD_markSurvives =
  scope "case (d) — mark vs delete; restore wins, mark survives" do
    let outcome =
          GivenSet.mergeGivenSets
            NonInteractive
            ThreeWay
              { lca = empty,
                alice = single "Show.nat" refA,
                bob = empty
              }
        survivors :: GivenSet.SurvivorMap
        survivors = Map.fromList [(n "Show.nat", refA)]
        pruned = GivenSet.applyGivenSet survivors outcome.merged
    expect (pruned == single "Show.nat" refA)

-- Regression: when no givens are involved anywhere, the merge is a
-- no-op and produces no prompts (per ADR-021 "regression test that
-- today's metadata-free merges still produce identical results when
-- no givens are involved").
regression_emptyIsNoOp :: Test ()
regression_emptyIsNoOp =
  scope "no givens anywhere is a no-op" do
    let outcome =
          GivenSet.mergeGivenSets
            Interactive
            ThreeWay {lca = empty, alice = empty, bob = empty}
    expect (outcome.merged == empty)
    expect (null outcome.prompts)
    -- And the identity holds for 'applyGivenSet' as well.
    expect (GivenSet.applyGivenSet Map.empty outcome.merged == empty)
