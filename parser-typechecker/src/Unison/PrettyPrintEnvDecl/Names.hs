{-# LANGUAGE OverloadedStrings #-}

module Unison.PrettyPrintEnvDecl.Names where

import Unison.Names (Names)
import Unison.PrettyPrintEnv.Names (fromNames, fromSuffixNames)
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (PrettyPrintEnvDecl))

fromNamesDecl :: Int -> Names -> PrettyPrintEnvDecl
fromNamesDecl hashLength names =
  PrettyPrintEnvDecl (fromNames hashLength names) (fromSuffixNames hashLength names)
