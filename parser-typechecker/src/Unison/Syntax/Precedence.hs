module Unison.Syntax.Precedence where

import Data.Map qualified as Map
import Unison.Prelude

-- Precedence rules for infix operators.
-- Lower number means higher precedence (tighter binding).
-- Operators not in this list have no precedence and will simply be parsed
-- left-to-right.
precedenceRules :: Map Text Int
precedenceRules =
  Map.fromList $ zip levels [0 ..] >>= \(ops, prec) -> map (,prec) ops

levels :: [[Text]]
levels =
  [ ["*", "/", "%"],
    ["+", "-"],
    ["<", ">", ">=", "<="],
    ["==", "!==", "!=", "==="],
    ["&&", "&"],
    ["^", "^^"],
    ["||", "|"]
  ]

-- | Returns the precedence of an infix operator, if it has one.
precedence :: Text -> Maybe Int
precedence op = Map.lookup op precedenceRules
