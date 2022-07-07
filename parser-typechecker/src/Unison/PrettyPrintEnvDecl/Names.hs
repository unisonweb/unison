{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnvDecl.Names where

import Unison.Name (Name)
import Unison.NamesWithHistory (NamesWithHistory)
import Unison.PrettyPrintEnv.Names (fromNamesWithBias, fromSuffixNamesWithBias)
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (PrettyPrintEnvDecl))

biasedPPEDecl :: Int -> Maybe Name -> NamesWithHistory -> PrettyPrintEnvDecl
biasedPPEDecl hashLength mayBias names =
  PrettyPrintEnvDecl (fromNamesWithBias hashLength mayBias names) (fromSuffixNamesWithBias hashLength mayBias names)

fromNamesDecl :: Int -> NamesWithHistory -> PrettyPrintEnvDecl
fromNamesDecl hashLength names = biasedPPEDecl hashLength Nothing names
