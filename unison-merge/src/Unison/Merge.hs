module Unison.Merge
  ( Mergeblob0 (..),
    makeMergeblob0,
    Mergeblob1 (..),
    makeMergeblob1,
    Mergeblob2 (..),
    Mergeblob2Error (..),
    makeMergeblob2,
    Mergeblob3 (..),
    makeMergeblob3,

    -- * Decl coherency checks
    DeclNameLookup (..),
    PartialDeclNameLookup (..),
    IncoherentDeclReason (..),
    checkDeclCoherency,
    lenientCheckDeclCoherency,
    IncoherentDeclReasons (..),
    checkAllDeclCoherency,

    -- * 3-way namespace diff

    -- DiffOp (..),
    -- nameBasedNamespaceDiff,

    -- * Finding conflicted aliases

    -- findConflictedAlias,

    -- * Combining namespace diffs

    -- CombinedDiffOp (..),
    -- combineDiffs,

    -- * Partitioning combined namespace diffs

    -- Unconflicts (..),
    -- partitionCombinedDiffs,
    -- narrowConflictsToNonBuiltins,

    -- * Merging libdeps

    -- LibdepDiffOp (..),
    -- diffLibdeps,
    -- applyLibdepsDiff,
    -- getTwoFreshLibdepNames,

    -- * Making a pretty-print environment

    -- makePrettyPrintEnvs,

    -- * Types
    CombinedDiffOp (..),
    DiffOp (..),
    EitherWay (..),
    EitherWayI (..),
    LibdepDiffOp (..),
    Synhashed (..),
    ThreeWay (..),
    TwoOrThreeWay (..),
    TwoWay (..),
    TwoWayI (..),
    Unconflicts (..),
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
  )
import Unison.Merge.DeclNameLookup (DeclNameLookup (..))
import Unison.Merge.Diff (nameBasedNamespaceDiff)
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.EitherWayI (EitherWayI (..))
import Unison.Merge.FindConflictedAlias (findConflictedAlias)
import Unison.Merge.Libdeps (LibdepDiffOp (..), applyLibdepsDiff, diffLibdeps, getTwoFreshLibdepNames)
import Unison.Merge.Mergeblob0 (Mergeblob0 (..), makeMergeblob0)
import Unison.Merge.Mergeblob1 (Mergeblob1 (..), makeMergeblob1)
import Unison.Merge.Mergeblob2 (Mergeblob2 (..), Mergeblob2Error (..), makeMergeblob2)
import Unison.Merge.Mergeblob3 (Mergeblob3 (..), makeMergeblob3)
import Unison.Merge.PartialDeclNameLookup (PartialDeclNameLookup (..))
import Unison.Merge.PartitionCombinedDiffs (narrowConflictsToNonBuiltins, partitionCombinedDiffs)
import Unison.Merge.PrettyPrintEnv (makePrettyPrintEnvs)
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.ThreeWay (ThreeWay (..))
import Unison.Merge.TwoOrThreeWay (TwoOrThreeWay (..))
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.TwoWayI (TwoWayI (..))
import Unison.Merge.Unconflicts (Unconflicts (..))
import Unison.Merge.Updated (Updated (..))
