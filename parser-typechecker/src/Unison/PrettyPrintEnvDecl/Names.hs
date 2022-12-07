{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnvDecl.Names where

import Unison.NamesWithHistory (NamesWithHistory)
import Unison.PrettyPrintEnv.Names (fromNames, fromSuffixNames)
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (PrettyPrintEnvDecl))

fromNamesDecl :: Applicative m => Int -> NamesWithHistory -> PrettyPrintEnvDecl m
fromNamesDecl hashLength names =
  PrettyPrintEnvDecl (fromNames hashLength names) (fromSuffixNames hashLength names)
