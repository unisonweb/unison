{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnv.Util (declarationPPE, declarationPPEDecl) where

import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import qualified Unison.PrettyPrintEnv as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (suffixifiedPPE, unsuffixifiedPPE))
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent

-- declarationPPE uses the full name for references that are
-- part the same cycle as the input reference, used to ensures
-- recursive definitions are printed properly, for instance:
--
-- foo.bar x = foo.bar x
-- and not
-- foo.bar x = bar x
declarationPPE :: forall m. PrettyPrintEnvDecl m -> Reference -> PrettyPrintEnv m
declarationPPE ppe ref = PrettyPrintEnv tm ty
  where
    rootH = hash ref
    hash Reference.Builtin {} = Nothing
    hash (Reference.Derived h _) = Just h
    tm :: (Referent.Referent -> m [(HQ'.HashQualified Name, HQ'.HashQualified Name)])
    tm r0@(Referent.Ref r)
      | hash r == rootH = PPE.termNames' (unsuffixifiedPPE ppe) r0
      | otherwise = PPE.termNames' (suffixifiedPPE ppe) r0
    tm r = PPE.termNames' (suffixifiedPPE ppe) r
    ty :: Reference -> m [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
    ty r
      | hash r == rootH = PPE.typeNames' (unsuffixifiedPPE ppe) r
      | otherwise = PPE.typeNames' (suffixifiedPPE ppe) r

-- The suffixed names uses the fully-qualified name for `r`
declarationPPEDecl :: PrettyPrintEnvDecl m -> Reference -> PrettyPrintEnvDecl m
declarationPPEDecl ppe r =
  ppe {suffixifiedPPE = declarationPPE ppe r}
