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
    (PPE.fromNames hashLength PPE.DontSuffixify names)
    (PPE.fromNames hashLength PPE.Suffixify names)
