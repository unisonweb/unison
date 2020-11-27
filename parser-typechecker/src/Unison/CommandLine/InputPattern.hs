{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.CommandLine.InputPattern where

import qualified System.Console.Haskeline as Line
import Unison.Codebase (Codebase)
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Editor.Input (Input (..))
import Unison.Codebase.Path as Path
import qualified Unison.Util.ColorText as CT
import qualified Unison.Util.Pretty as P

-- InputPatterns accept some fixed number of Required arguments of various
-- types, followed by a variable number of a single type of argument.
data IsOptional
  = Required -- 1, at the start
  | Optional -- 0 or 1, at the end
  | ZeroPlus -- 0 or more, at the end
  | OnePlus -- 1 or more, at the end
  deriving (Show)

data InputPattern = InputPattern
  { patternName :: String,
    aliases :: [String],
    args :: [(IsOptional, ArgumentType)],
    help :: P.Pretty CT.ColorText,
    parse :: [String] -> Either (P.Pretty CT.ColorText) Input
  }

data ArgumentType = ArgumentType
  { typeName :: String,
    suggestions ::
      forall m v a.
      Monad m =>
      String ->
      Codebase m v a ->
      Branch m ->
      Path.Absolute ->
      m [Line.Completion]
  }

instance Show ArgumentType where
  show at = "ArgumentType " <> typeName at

-- `argType` gets called when the user tries to autocomplete an `i`th argument (zero-indexed).
-- todo: would be nice if we could alert the user if they try to autocomplete
-- past the end.  It would also be nice if
argType :: InputPattern -> Int -> Maybe ArgumentType
argType ip i = go (i, args ip)
  where
    -- Strategy: all of these input patterns take some number of arguments.
    -- If it takes no arguments, then don't autocomplete.
    go (_, []) = Nothing
    -- If requesting the 0th of >=1 arguments, return it.
    go (0, (_, t) : _) = Just t
    -- Vararg parameters should appear at the end of the arg list, and work for
    -- any later argument number.
    go (_, [(ZeroPlus, t)]) = Just t
    go (_, [(OnePlus, t)]) = Just t
    -- Optional parameters only work at position 0, under this countdown scheme.
    go (_, [(Optional, _)]) = Nothing
    -- If requesting a later parameter, decrement and drop one.
    go (n, (Required, _) : args) = go (n - 1, args)
    -- The argument list spec is invalid if something follows optional or vararg
    go _ =
      error $
        "Input pattern " <> show (patternName ip)
          <> " has an invalid argument list: "
          <> (show . fmap fst) (args ip)

minArgs :: InputPattern -> Int
minArgs ip@(fmap fst . args -> args) = go args
  where
    go [] = 0
    go (Required : args) = 1 + go args
    go [_] = 0
    go _ =
      error $
        "Invalid args for InputPattern ("
          <> show (patternName ip)
          <> "): "
          <> show args

maxArgs :: InputPattern -> Maybe Int
maxArgs ip@(fmap fst . args -> args) = go args
  where
    go [] = Just 0
    go (Required : args) = (1 +) <$> go args
    go [Optional] = Just 0
    go [_] = Nothing
    go _ =
      error $
        "Invalid args for InputPattern ("
          <> show (patternName ip)
          <> "): "
          <> show args

noSuggestions ::
  Monad m =>
  String ->
  Codebase m v a ->
  Branch m ->
  Path.Absolute ->
  m [Line.Completion]
noSuggestions _ _ _ _ = pure []
