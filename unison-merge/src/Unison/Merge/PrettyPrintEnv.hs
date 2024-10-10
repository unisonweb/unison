module Unison.Merge.PrettyPrintEnv
  ( makePrettyPrintEnv,
  )
where

import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl.Names qualified as PPED

-- Create a PPE that uses Alice's names whenever possible, falling back to Bob's names only when Alice doesn't have any.
-- This results in a file that "looks familiar" to Alice (the one merging in Bob's changes), and avoids superfluous
-- textual conflicts that would arise from preferring Bob's names for Bob's code (where his names differ).
makePrettyPrintEnv :: TwoWay Names -> Names -> PrettyPrintEnvDecl
makePrettyPrintEnv names libdepsNames =
  PPED.makePPED (PPE.namer (Names.preferring names.alice names.bob <> libdepsNames)) suffixifier
  where
    suffixifier = PPE.suffixifyByName (fold names <> libdepsNames)
