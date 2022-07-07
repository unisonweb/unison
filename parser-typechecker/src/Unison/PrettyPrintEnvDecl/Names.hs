{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnvDecl.Names where

import Unison.Name (Name)
import Unison.NamesWithHistory (NamesWithHistory)
import Unison.PrettyPrintEnv.Names (fromNames, fromSuffixNames)
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (PrettyPrintEnvDecl))

fromNamesDecl :: Int -> Maybe Name -> NamesWithHistory -> PrettyPrintEnvDecl
fromNamesDecl hashLength mayBias names =
  PrettyPrintEnvDecl (fromNames hashLength mayBias names) (fromSuffixNames hashLength mayBias names)
