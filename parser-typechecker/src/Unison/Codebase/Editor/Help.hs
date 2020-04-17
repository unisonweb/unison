{-# LANGUAGE OverloadedStrings   #-}

module Unison.Codebase.Editor.Help where

import Unison.Prelude

import           Data.List (intercalate, find, sortOn)
import qualified Unison.CommandLine.InputPattern as I
import qualified Unison.CommandLine.InputPatterns as IP
import qualified Unison.Util.ColorText as CT
import qualified Unison.Util.Pretty as P
import           Unison.Util.Monoid (intercalateMap)

help :: P.Pretty CT.ColorText
help = intercalateMap "\n\n" showPatternHelp (sortOn I.patternName IP.validInputs)

helpFor :: String -> P.Pretty CT.ColorText
helpFor command = case findByName command of
  Just pat -> showPatternHelp pat
  Nothing -> help

findByName :: String -> Maybe I.InputPattern
findByName c = find (\pattern -> (I.patternName pattern) == c) IP.validInputs

showPatternHelp :: I.InputPattern -> P.Pretty CT.ColorText
showPatternHelp i = P.lines [
  P.bold (fromString $ I.patternName i) <> fromString
    (if not . null $ I.aliases i
     then " (or " <> intercalate ", " (I.aliases i) <> ")"
     else ""),
  P.wrap $ I.help i ]
