module Unison.PrettyPrintEnvDecl.Names
  ( fromNamesDecl,
  )
where

import Unison.Names (Names)
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (PrettyPrintEnvDecl))

fromNamesDecl :: Int -> Names -> PrettyPrintEnvDecl
fromNamesDecl hashLength names =
  PrettyPrintEnvDecl
    (PPE.makePPE namer PPE.dontSuffixify)
    (PPE.makePPE namer (PPE.suffixify names))
  where
    namer = PPE.hqNamer hashLength names
