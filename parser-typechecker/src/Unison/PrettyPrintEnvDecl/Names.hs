module Unison.PrettyPrintEnvDecl.Names
  ( makePPED,
    makeFilePPED,
    makeCodebasePPED,
  )
where

import Unison.Names (Names)
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (PrettyPrintEnvDecl))

makePPED :: PPE.Namer -> PPE.Suffixifier -> PrettyPrintEnvDecl
makePPED namer suffixifier =
  PrettyPrintEnvDecl
    (PPE.makePPE namer PPE.dontSuffixify)
    (PPE.makePPE namer suffixifier)

-- | Make a PPED suitable for names in a Unison file.
--
-- Such names have special suffixification rules: aliases may *not* be referred to by a common suffix. For example, if
-- a file contains
--
--   one.foo = 6
--   two.foo = 6
--
-- then the suffix `foo` will *not* be accepted (currently). So, this PPE uses the "suffixify by name" strategy.
makeFilePPED :: Names -> PrettyPrintEnvDecl
makeFilePPED names =
  makePPED (PPE.namer names) (PPE.suffixifyByName names)

-- | Make a PPED suitable for names in the codebase. These names are hash qualified and suffixified by hash.
makeCodebasePPED :: Names -> PrettyPrintEnvDecl
makeCodebasePPED names =
  makePPED
    (PPE.hqNamer 10 names)
    (PPE.suffixifyByHash names)
