module Unison.Codebase.Branch.Names
  ( namesDiff,
    toNames,
  )
where

import Unison.Codebase.Branch
import Unison.Names (Names (..))
import Unison.NamesWithHistory qualified as Names
import Unison.Util.Relation qualified as R
import Prelude hiding (head, read, subtract)

toNames :: Branch0 m -> Names
toNames b =
  Names
    (R.swap . deepTerms $ b)
    (R.swap . deepTypes $ b)

namesDiff :: Branch m -> Branch m -> Names.Diff
namesDiff b1 b2 = Names.diff (toNames (head b1)) (toNames (head b2))
