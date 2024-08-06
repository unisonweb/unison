module Unison.Merge.PrettyPrintEnv
  ( makePrettyPrintEnvs,
  )
where

import Unison.Merge.TwoWay (TwoWay)
import Unison.Names (Names)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl.Names qualified as PPED

-- Make PPE for Alice that contains all of Alice's names, but suffixified against her names + Bob's names
makePrettyPrintEnvs :: TwoWay Names -> Names -> TwoWay PrettyPrintEnvDecl
makePrettyPrintEnvs names2 libdepsNames =
  names2 <&> \names -> PPED.makePPED (PPE.namer (names <> libdepsNames)) suffixifier
  where
    suffixifier = PPE.suffixifyByName (fold names2 <> libdepsNames)
