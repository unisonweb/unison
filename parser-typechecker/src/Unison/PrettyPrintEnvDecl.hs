{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..)) where

import Unison.PrettyPrintEnv (PrettyPrintEnv (..))

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
  deriving (Show)

instance Semigroup PrettyPrintEnvDecl where
  PrettyPrintEnvDecl unSuff1 suff1 <> PrettyPrintEnvDecl unSuff2 suff2 =
    PrettyPrintEnvDecl (unSuff1 <> unSuff2) (suff1 <> suff2)

instance Monoid PrettyPrintEnvDecl where
  mempty = PrettyPrintEnvDecl mempty mempty
