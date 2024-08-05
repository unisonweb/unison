module Unison.Merge
  ( -- * Decl coherency checks
    DeclNameLookup (..),
    PartialDeclNameLookup (..),
    IncoherentDeclReason (..),
    checkDeclCoherency,
    lenientCheckDeclCoherency,
    IncoherentDeclReasons (..),
    checkAllDeclCoherency,

    -- * 3-way namespace diff
    DiffOp (..),
    nameBasedNamespaceDiff,

    -- * Finding conflicted aliases
    findConflictedAlias,

    -- * Combining namespace diffs
    CombinedDiffOp (..),
    combineDiffs,

    -- * Partitioning combined namespace diffs
    Unconflicts (..),
    partitionCombinedDiffs,
    narrowConflictsToNonBuiltins,

    -- * Merging libdeps
    LibdepDiffOp (..),
    diffLibdeps,
    applyLibdepsDiff,
    getTwoFreshLibdepNames,

    -- * Making a pretty-print environment
    makePrettyPrintEnvs,

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
  )
import Unison.Merge.DeclNameLookup (DeclNameLookup (..))
import Unison.Merge.Diff (nameBasedNamespaceDiff)
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.EitherWayI (EitherWayI (..))
import Unison.Merge.FindConflictedAlias (findConflictedAlias)
import Unison.Merge.Libdeps (LibdepDiffOp (..), applyLibdepsDiff, diffLibdeps, getTwoFreshLibdepNames)
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
