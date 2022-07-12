{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnv.Util (declarationPPE, declarationPPEDecl) where

import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import qualified Unison.PrettyPrintEnv as PPE
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
declarationPPE :: PrettyPrintEnv -> Reference -> PrettyPrintEnv
declarationPPE ppe ref = ppe {termNames = tm, typeNames = ty}
  where
    rootH = hash ref
    hash Reference.Builtin {} = Nothing
    hash (Reference.Derived h _) = Just h
    tm restr p b s r0@(Referent.Ref r)
      | hash r == rootH = PPE.termNames (PPE.unsuffixifiedPPE ppe) restr p b s r0
      | otherwise = PPE.termNames (PPE.suffixifiedPPE ppe) restr p b s r0
    tm restr p b s r = PPE.termNames (PPE.suffixifiedPPE ppe) restr p b s r
    ty restr p b s r
      | hash r == rootH = PPE.typeNames (PPE.unsuffixifiedPPE ppe) restr p b s r
      | otherwise = PPE.typeNames (PPE.suffixifiedPPE ppe) restr p b s r

-- The suffixed names uses the fully-qualified name for `r`
declarationPPEDecl :: PrettyPrintEnv -> Reference -> PrettyPrintEnv
declarationPPEDecl ppe@(PrettyPrintEnv {bias, perspective, suffixify, restrictions}) ref =
  PrettyPrintEnv
    { termNames = \restr p b s r ->
        case s of
          PPE.Suffixify -> PPE.termNames (declarationPPE ppe ref) restr p b PPE.Suffixify r
          PPE.NoSuffixify -> PPE.termNames ppe restr p b PPE.Suffixify r,
      typeNames = \restr p b s r ->
        case s of
          PPE.Suffixify -> PPE.typeNames (declarationPPE ppe ref) restr p b PPE.Suffixify r
          PPE.NoSuffixify -> PPE.typeNames ppe restr p b PPE.Suffixify r,
      bias,
      perspective,
      suffixify,
      restrictions
    }
