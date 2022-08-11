{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.CommandLine.InputPattern
  ( InputPattern (..),
    ArgumentType (..),
    argType,
    IsOptional (..),
    Visibility (..),

    -- * Currently Unused
    minArgs,
    maxArgs,
  )
where

import Data.Set (Set)
import qualified System.Console.Haskeline as Line
import Unison.Codebase (Codebase)
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Editor.Input (Input (..))
import Unison.Codebase.Path as Path
import qualified Unison.CommandLine.Globbing as Globbing
import qualified Unison.Util.ColorText as CT
import qualified Unison.Util.Pretty as P

-- InputPatterns accept some fixed number of Required arguments of various
-- types, followed by a variable number of a single type of argument.
data IsOptional
  = Required -- 1, at the start
  | Optional -- 0 or 1, at the end
  | ZeroPlus -- 0 or more, at the end
  | OnePlus -- 1 or more, at the end
  deriving (Show, Eq)

data Visibility = Hidden | Visible
  deriving (Show, Eq, Ord)

data InputPattern = InputPattern
  { patternName :: String,
    aliases :: [String],
    visibility :: Visibility, -- Allow hiding certain commands when debugging or work-in-progress
    argTypes :: [(IsOptional, ArgumentType)],
    help :: P.Pretty CT.ColorText,
    parse :: [String] -> Either (P.Pretty CT.ColorText) Input
  }

data ArgumentType = ArgumentType
  { typeName :: String,
    -- | Generate completion suggestions for this argument type
    suggestions ::
      forall m v a.
      Monad m =>
      String ->
      Codebase m v a ->
      Branch m -> -- Root Branch
      Path.Absolute -> -- Current path
      m [Line.Completion],
    -- | Select which targets glob patterns may expand into for this argument.
    -- An empty set disables globbing.
    globTargets :: Set Globbing.TargetType
  }

instance Show ArgumentType where
  show at = "ArgumentType " <> typeName at

-- `argType` gets called when the user tries to autocomplete an `i`th argument (zero-indexed).
-- todo: would be nice if we could alert the user if they try to autocomplete
-- past the end.  It would also be nice if
argType :: InputPattern -> Int -> Maybe ArgumentType
argType ip i = go (i, argTypes ip)
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
    -- If requesting a later parameter, decrement and drop one.
    go (n, (o, _) : argTypes)
      | o == Optional || o == Required = go (n - 1, argTypes)
    -- The argument list spec is invalid if something follows a vararg
    go args =
      error $
        "Input pattern " <> show (patternName ip)
          <> " has an invalid argument list: "
          <> show args

minArgs :: InputPattern -> Int
minArgs ip@(fmap fst . argTypes -> argTypes) = go argTypes
  where
    go [] = 0
    go (Required : argTypes) = 1 + go argTypes
    go [_] = 0
    go _ =
      error $
        "Invalid argTypes for InputPattern ("
          <> show (patternName ip)
          <> "): "
          <> show argTypes

maxArgs :: InputPattern -> Maybe Int
maxArgs ip@(fmap fst . argTypes -> args) = go args
  where
    go [] = Just 0
    go (Required : argTypes) = (1 +) <$> go argTypes
    go [Optional] = Just 0
    go [_] = Nothing
    go _ =
      error $
        "Invalid argTypes for InputPattern ("
          <> show (patternName ip)
          <> "): "
          <> show args
