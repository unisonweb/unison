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

    -- * Namespace metadata-mark merge (see "Unison.Merge.GivenSet")
    Marks,
    MarkConflict (..),
    MarksMergeOutcome (..),
    MarkSide (..),
    MergeMode (..),
    mergeMarks,
    applyMarks,
    -- ** Given-mark specialisation
    GivenMarks,
    GivenSetConflict,
    GivenSetMergeOutcome,
    GivenSetSide,
    SurvivorMap,
    applyGivenSet,
    mergeGivenSets,
    -- ** Class-mark specialisation
    ClassMarks,
    mergeClassSets,
    applyClassSet,
  )
where

import Unison.Merge.CombineDiffs (CombinedDiffOp (..))
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.Diffblob (Diffblob (..), DiffblobLog (..), emptyDiffblobLog, makeDiffblob)
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.EitherWayI (EitherWayI (..))
import Unison.Merge.GivenSet
  ( ClassMarks,
    GivenMarks,
    GivenSetConflict,
    GivenSetMergeOutcome,
    GivenSetSide,
    MarkConflict (..),
    MarkSide (..),
    Marks,
    MarksMergeOutcome (..),
    MergeMode (..),
    SurvivorMap,
    applyClassSet,
    applyGivenSet,
    applyMarks,
    mergeClassSets,
    mergeGivenSets,
    mergeMarks,
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
