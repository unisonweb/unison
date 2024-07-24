{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.CommandLine.Helpers
  ( -- * Pretty Printing
    backtick,
    aside,
    bigproblem,
    note,
    nothingTodo,
    plural,
    plural',
    problem,
    tip,
    warn,
    warnNote,
  )
where

import Data.ListLike (ListLike)
import Unison.Prelude
import Unison.Util.Pretty qualified as P
import Prelude hiding (readFile, writeFile)

warnNote :: String -> String
warnNote s = "⚠️  " <> s

backtick :: (IsString s) => P.Pretty s -> P.Pretty s
backtick s = P.group ("`" <> s <> "`")

tip :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
tip s = P.column2 [("Tip:", P.wrap s)]

note :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
note s = P.column2 [("Note:", P.wrap s)]

aside :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s -> P.Pretty s
aside a b = P.column2 [(a <> ":", b)]

warn :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
warn = emojiNote "⚠️"

problem :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
problem = emojiNote "❗️"

bigproblem :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
bigproblem = emojiNote "‼️"

emojiNote :: (ListLike s Char, IsString s) => String -> P.Pretty s -> P.Pretty s
emojiNote lead s = P.group (fromString lead) <> "\n" <> P.wrap s

nothingTodo :: (ListLike s Char, IsString s) => P.Pretty s -> P.Pretty s
nothingTodo = emojiNote "😶"

-- `plural [] "cat" "cats" = "cats"`
-- `plural ["meow"] "cat" "cats" = "cat"`
-- `plural ["meow", "meow"] "cat" "cats" = "cats"`
plural :: (Foldable f) => f a -> b -> b -> b
plural items one other = case toList items of
  [_] -> one
  _ -> other

plural' :: (Integral a) => a -> b -> b -> b
plural' 1 one _other = one
plural' _ _one other = other
