module Unison.PrettyPrintEnvDecl.Names
  ( fromNamesSuffixifiedByHash,
    fromNamesSuffixifiedByName,
  )
where

import Unison.Names (Names)
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (PrettyPrintEnvDecl))

fromNamesSuffixifiedByHash :: Int -> Names -> PrettyPrintEnvDecl
fromNamesSuffixifiedByHash hashLength names =
  PrettyPrintEnvDecl
    (PPE.makePPE namer PPE.dontSuffixify)
    (PPE.makePPE namer (PPE.suffixifyByHash names))
  where
    namer = PPE.hqNamer hashLength names

fromNamesSuffixifiedByName :: Int -> Names -> PrettyPrintEnvDecl
fromNamesSuffixifiedByName hashLength names =
  PrettyPrintEnvDecl
    (PPE.makePPE namer PPE.dontSuffixify)
    (PPE.makePPE namer (PPE.suffixifyByName names))
  where
    namer = PPE.hqNamer hashLength names
