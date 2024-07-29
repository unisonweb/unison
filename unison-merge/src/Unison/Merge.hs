module Unison.Merge
  ( -- * Decl coherency checks
    DeclNameLookup (..),
    PartialDeclNameLookup (..),
    IncoherentDeclReason (..),
    oldCheckDeclCoherency,
    checkDeclCoherency,
    oldLenientCheckDeclCoherency,
    lenientCheckDeclCoherency,
    IncoherentDeclReasons (..),
    checkAllDeclCoherency,

    -- * 3-way namespace diff
    DiffOp (..),
    oldNameBasedNamespaceDiff,
    nameBasedNamespaceDiff,

    -- * Combining namespace diffs
    CombinedDiffOp (..),
    combineDiffs,

    -- * Partitioning combined namespace diffs
    Unconflicts (..),
    partitionCombinedDiffs,

    -- * Merging libdeps
    mergeLibdeps,

    -- * Utility types
    EitherWay (..),
    ThreeWay (..),
    TwoOrThreeWay (..),
    EitherWayI (..),
    Synhashed (..),
    TwoWay (..),
    TwoWayI (..),
    Updated (..),
  )
where

import Unison.Merge.CombineDiffs (CombinedDiffOp (..), combineDiffs)
import Unison.Merge.DeclCoherencyCheck
  ( IncoherentDeclReason (..),
    IncoherentDeclReasons (..),
    checkAllDeclCoherency,
    checkDeclCoherency,
    lenientCheckDeclCoherency,
    oldCheckDeclCoherency,
    oldLenientCheckDeclCoherency,
  )
import Unison.Merge.DeclNameLookup (DeclNameLookup (..))
import Unison.Merge.Diff (nameBasedNamespaceDiff, oldNameBasedNamespaceDiff)
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.EitherWayI (EitherWayI (..))
import Unison.Merge.Libdeps (mergeLibdeps)
import Unison.Merge.PartialDeclNameLookup (PartialDeclNameLookup (..))
import Unison.Merge.PartitionCombinedDiffs (partitionCombinedDiffs)
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.ThreeWay (ThreeWay (..))
import Unison.Merge.TwoOrThreeWay (TwoOrThreeWay (..))
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.TwoWayI (TwoWayI (..))
import Unison.Merge.Unconflicts (Unconflicts (..))
import Unison.Merge.Updated (Updated (..))
