{-# Language OverloadedStrings #-}

module Unison.PrettyPrintEnvDecl.Names where

import Unison.Names3 (Names)
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (PrettyPrintEnvDecl))
import Unison.PrettyPrintEnv.Names (fromNames, fromSuffixNames)

fromNamesDecl :: Int -> Names -> PrettyPrintEnvDecl
fromNamesDecl len names =
  PrettyPrintEnvDecl (fromNames len names) (fromSuffixNames len names)
