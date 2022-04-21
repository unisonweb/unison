{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl, PrettyPrintEnvDeclM (..)) where

import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))

type PrettyPrintEnvDecl = PrettyPrintEnvDeclM Identity

-- A pair of PrettyPrintEnvs:
--   - suffixifiedPPE uses the shortest unique suffix
--   - unsuffixifiedPPE uses the shortest full name
--
-- Generally, we want declarations LHS (the `x` in `x = 23`) to use the
-- unsuffixified names, so the LHS is an accurate description of where in the
-- namespace the definition lives. For everywhere else, we can use the
-- suffixified version.
data PrettyPrintEnvDeclM m = PrettyPrintEnvDecl
  { unsuffixifiedPPE :: PrettyPrintEnv m,
    suffixifiedPPE :: PrettyPrintEnv m
  }
  deriving (Show)
