module Unison.Codebase.Branch.Names
  ( namesDiff,
    toNames,
    toNames3,
    toPrettyPrintEnvDecl,
  )
where

import Unison.Codebase.Branch
import Unison.Codebase.Branch.Type qualified as Branch
import Unison.Namer qualified as Namer
import Unison.Names (Names (..))
import Unison.Names3 (Names3 (..))
import Unison.NamesWithHistory qualified as Names
import Unison.Prelude
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Suffixifier qualified as Suffixifier
import Unison.Util.Relation qualified as R
import Prelude hiding (head)

-- | Get the pretty-printing environment for names in the provided branch.
toPrettyPrintEnvDecl :: Int -> Branch0 m -> PPED.PrettyPrintEnvDecl
toPrettyPrintEnvDecl hashLength b =
  let names = toNames3 b
   in PPED.makePPED (Namer.makeHqNamer hashLength names) (Suffixifier.suffixifyByHash names)

-- | Get the names in the provided branch.
toNames :: Branch0 m -> Names
toNames b =
  Names
    (R.swap . Branch.deepTerms $ b)
    (R.swap . Branch.deepTypes $ b)

-- | Get the names in the provided branch.
--
-- TODO: actually classify direct and indirect deps as such
toNames3 :: Branch0 m -> Names3
toNames3 b =
  Names3
    { local = toNames b,
      directDeps = mempty,
      indirectDeps = mempty
    }

namesDiff :: Branch m -> Branch m -> Names.Diff
namesDiff b1 b2 = Names.diff (toNames (head b1)) (toNames (head b2))
