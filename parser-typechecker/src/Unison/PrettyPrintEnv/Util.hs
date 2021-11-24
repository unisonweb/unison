{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnv.Util (declarationPPE, declarationPPEDecl) where

import qualified Data.Set as Set
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
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
declarationPPE :: PrettyPrintEnvDecl -> Reference -> PrettyPrintEnv
declarationPPE ppe rd = PrettyPrintEnv tm ty
  where
    comp = Reference.members (Reference.componentFor rd)
    tm r0@(Referent.Ref r) =
      if Set.member r comp
        then terms (unsuffixifiedPPE ppe) r0
        else terms (suffixifiedPPE ppe) r0
    tm r = terms (suffixifiedPPE ppe) r
    ty r =
      if Set.member r comp
        then types (unsuffixifiedPPE ppe) r
        else types (suffixifiedPPE ppe) r

-- The suffixed names uses the fully-qualified name for `r`
declarationPPEDecl ::  PrettyPrintEnvDecl -> Reference -> PrettyPrintEnvDecl
declarationPPEDecl ppe r = 
   ppe { suffixifiedPPE = declarationPPE ppe r }