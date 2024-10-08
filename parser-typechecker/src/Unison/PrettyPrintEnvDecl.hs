{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnvDecl
  ( PrettyPrintEnvDecl (..),
    biasTo,
    addFallback,
  )
where

import Unison.Name (Name)
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.PrettyPrintEnv qualified as PPE

-- A pair of PrettyPrintEnvs:
--   - suffixifiedPPE uses the shortest unique suffix
--   - unsuffixifiedPPE uses the shortest full name
--
-- Generally, we want declarations LHS (the `x` in `x = 23`) to use the
-- unsuffixified names, so the LHS is an accurate description of where in the
-- namespace the definition lives. For everywhere else, we can use the
-- suffixified version.
data PrettyPrintEnvDecl = PrettyPrintEnvDecl
  { unsuffixifiedPPE :: PrettyPrintEnv,
    suffixifiedPPE :: PrettyPrintEnv
  }
  deriving stock (Generic, Show)

-- | Lifts 'biasTo' over a PrettyPrintEnvDecl
biasTo :: [Name] -> PrettyPrintEnvDecl -> PrettyPrintEnvDecl
biasTo targets PrettyPrintEnvDecl {unsuffixifiedPPE, suffixifiedPPE} =
  PrettyPrintEnvDecl
    { unsuffixifiedPPE = PPE.biasTo targets unsuffixifiedPPE,
      suffixifiedPPE = PPE.biasTo targets suffixifiedPPE
    }

-- | Will use names from the fallback pped if no names were found in the primary.
-- @addFallback primary fallback@
addFallback :: PrettyPrintEnvDecl -> PrettyPrintEnvDecl -> PrettyPrintEnvDecl
addFallback (PrettyPrintEnvDecl unsuff1 suff1) (PrettyPrintEnvDecl unsuff2 suff2) =
  PrettyPrintEnvDecl (unsuff1 `PPE.addFallback` unsuff2) (suff1 `PPE.addFallback` suff2)
