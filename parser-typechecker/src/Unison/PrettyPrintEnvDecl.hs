{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# Language OverloadedStrings #-}

module Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl(..)) where

import Unison.PrettyPrintEnv (PrettyPrintEnv(..))

-- A pair of PrettyPrintEnvs:
--   - suffixifiedPPE uses the shortest unique suffix
--   - unsuffixifiedPPE uses the shortest full name
--
-- Generally, we want declarations LHS (the `x` in `x = 23`) to use the
-- unsuffixified names, so the LHS is an accurate description of where in the
-- namespace the definition lives. For everywhere else, we can use the
-- suffixified version.
data PrettyPrintEnvDecl = PrettyPrintEnvDecl {
  unsuffixifiedPPE :: PrettyPrintEnv,
  suffixifiedPPE :: PrettyPrintEnv
  } deriving Show

