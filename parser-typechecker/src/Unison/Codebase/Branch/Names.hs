module Unison.Codebase.Branch.Names
  ( namesDiff,
    toNames,
    toPrettyPrintEnvDecl,
  )
where

import Unison.Codebase.Branch
import Unison.Codebase.Branch.Type qualified as Branch
import Unison.Names (Names (..))
import Unison.NamesWithHistory qualified as Names
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Prelude hiding (head, read, subtract)

-- | Get the pretty-printing environment for names in the provided branch.
toPrettyPrintEnvDecl :: Int -> Branch0 m -> PPED.PrettyPrintEnvDecl
toPrettyPrintEnvDecl hashLength b =
  let names = toNames b
   in PPED.makePPED (PPE.hqNamer hashLength names) (PPE.suffixifyByHash names)

-- | Get the names in the provided branch.
toNames :: Branch0 m -> Names
toNames = Branch.names

namesDiff :: Branch m -> Branch m -> Names.Diff
namesDiff b1 b2 = Names.diff (toNames (head b1)) (toNames (head b2))
