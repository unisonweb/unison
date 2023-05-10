{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnv.Util (declarationPPE, declarationPPEDecl) where

import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (suffixifiedPPE))
import Unison.Reference (Reference)

-- declarationPPE uses the full name for references that are
-- part the same cycle as the input reference, used to ensures
-- recursive definitions are printed properly, for instance:
--
-- foo.bar x = foo.bar x
-- and not
-- foo.bar x = bar x
declarationPPE :: PrettyPrintEnvDecl -> Reference -> PrettyPrintEnv
declarationPPE ppe _ref = suffixifiedPPE ppe

-- PrettyPrintEnv tm ty
-- where
--   rootH = hash ref
--   hash Reference.Builtin {} = Nothing
--   hash (Reference.Derived h _) = Just h
--   tm r0@(Referent.Ref r)
--     | hash r == rootH = PPE.termNames (unsuffixifiedPPE ppe) r0
--     | otherwise = PPE.termNames (suffixifiedPPE ppe) r0
--   tm r = PPE.termNames (suffixifiedPPE ppe) r
--   ty r
--     | hash r == rootH = PPE.typeNames (unsuffixifiedPPE ppe) r
--     | otherwise = PPE.typeNames (suffixifiedPPE ppe) r

-- The suffixed names uses the fully-qualified name for `r`
declarationPPEDecl :: PrettyPrintEnvDecl -> Reference -> PrettyPrintEnvDecl
declarationPPEDecl ppe _r = ppe

-- ppe {suffixifiedPPE = declarationPPE ppe r}
