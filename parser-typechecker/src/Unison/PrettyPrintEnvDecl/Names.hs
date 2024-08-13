module Unison.PrettyPrintEnvDecl.Names
  ( makePPED,
    makeFilePPED,
    makeCodebasePPED,
  )
where

import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Namer (Namer)
import Unison.Namer qualified as Namer
import Unison.Names (Names)
import Unison.Names3 (Names3 (..))
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (PrettyPrintEnvDecl))
import Unison.Suffixifier (Suffixifier)
import Unison.Suffixifier qualified as Suffixifier

makePPED :: Namer (HQ'.HashQualified Name) -> Suffixifier -> PrettyPrintEnvDecl
makePPED namer suffixifier =
  PrettyPrintEnvDecl
    (PPE.makePPE namer Suffixifier.dontSuffixify)
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
  makePPED (Namer.mapNamer HQ'.NameOnly (Namer.makeNamer names3)) (Suffixifier.suffixifyByName names3)
  where
    names3 =
      Names3
        { local = names,
          directDeps = mempty,
          indirectDeps = mempty
        }

-- | Make a PPED suitable for names in the codebase. These names are hash qualified and suffixified by hash.
makeCodebasePPED :: Names3 -> PrettyPrintEnvDecl
makeCodebasePPED names =
  makePPED
    (Namer.makeHqNamer 10 names)
    (Suffixifier.suffixifyByHash names)
