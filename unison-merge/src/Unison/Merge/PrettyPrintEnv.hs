module Unison.Merge.PrettyPrintEnv
  ( makePrettyPrintEnvs,
  )
where

import Unison.Merge.ThreeWay (ThreeWay)
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoWay (TwoWay)
import Unison.Names (Names)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.PrettyPrintEnvDecl.Names qualified as PPED

-- Make PPE for Alice that contains all of Alice's names, but suffixified against her names + Bob's names
makePrettyPrintEnvs :: ThreeWay Names -> TwoWay PrettyPrintEnvDecl
makePrettyPrintEnvs names3 =
  ThreeWay.forgetLca names3 <&> \names -> PPED.makePPED (PPE.namer (names <> names3.lca)) suffixifier
  where
    suffixifier = PPE.suffixifyByName (fold names3)
