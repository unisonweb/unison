module Unison.Merge
  ( Diffblob (..),
    makeDiffblob,
    DiffblobLog (..),
    emptyDiffblobLog,
    Mergeblob (..),
    MergeblobError (..),
    makeMergeblob,

    -- * Types
    CombinedDiffOp (..),
    DiffOp (..),
    EitherWay (..),
    EitherWayI (..),
    GThreeWay (..),
    GUpdated (..),
    LibdepDiffOp (..),
    Rename (..),
    SimpleRenames (..),
    Synhashed (..),
    ThreeWay (..),
    TwoOrThreeWay (..),
    TwoWay (..),
    TwoWayI (..),
    Unconflicts (..),
    Updated,

    -- * Given-set conflict resolution (per ADR-021)
    GivenMarks,
    GivenSetConflict (..),
    GivenSetMergeOutcome (..),
    GivenSetSide (..),
    MergeMode (..),
    SurvivorMap,
    applyGivenSet,
    mergeGivenSets,
  )
where

import Unison.Merge.CombineDiffs (CombinedDiffOp (..))
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.Diffblob (Diffblob (..), DiffblobLog (..), emptyDiffblobLog, makeDiffblob)
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.EitherWayI (EitherWayI (..))
import Unison.Merge.GivenSet
  ( GivenMarks,
    GivenSetConflict (..),
    GivenSetMergeOutcome (..),
    GivenSetSide (..),
    MergeMode (..),
    SurvivorMap,
    applyGivenSet,
    mergeGivenSets,
  )
import Unison.Merge.Libdeps (LibdepDiffOp (..))
import Unison.Merge.Mergeblob (Mergeblob (..), MergeblobError (..), makeMergeblob)
import Unison.Merge.Rename (Rename (..), SimpleRenames (..))
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.ThreeWay (GThreeWay (..), ThreeWay (..))
import Unison.Merge.TwoOrThreeWay (TwoOrThreeWay (..))
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.TwoWayI (TwoWayI (..))
import Unison.Merge.Unconflicts (Unconflicts (..))
import Unison.Merge.Updated (GUpdated (..), Updated)
