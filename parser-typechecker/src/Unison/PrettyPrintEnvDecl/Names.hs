{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnvDecl.Names where

import Unison.Name (Name)
import Unison.NamesWithHistory (NamesWithHistory)
import Unison.PrettyPrintEnv.Names (fromNamesWithBias, fromSuffixNamesWithBias)
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (PrettyPrintEnvDecl))

-- | Creates a PPE decl which is biased towards a particular name.
--
-- This is helpful when printing a specific definition,
--
-- e.g. when pretty-printing for `view base.List.map`, we should prefer names which are close
-- to `base.List.map`.
biasedPPEDecl :: Int -> Maybe Name -> NamesWithHistory -> PrettyPrintEnvDecl
biasedPPEDecl hashLength mayBias names =
  PrettyPrintEnvDecl (fromNamesWithBias hashLength mayBias names) (fromSuffixNamesWithBias hashLength mayBias names)

fromNamesDecl :: Int -> NamesWithHistory -> PrettyPrintEnvDecl
fromNamesDecl hashLength names = biasedPPEDecl hashLength Nothing names
