{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE DoAndIfThenElse     #-}


module Unison.CommandLine.InputPattern where

import qualified System.Console.Haskeline       as Line
import           Unison.Codebase                (Codebase)
import           Unison.Codebase.Branch         (Branch)
import           Unison.Codebase.Editor         (Input (..))
import qualified Unison.Util.ColorText          as CT
import qualified Unison.Util.Pretty             as P

data IsOptional
  = Optional -- 0 or 1, at the end
  | Required -- 1, at the start
  | ZeroPlus -- 0 or more, at the end
  | OnePlus -- 1 or more, at the end
  deriving Show

data InputPattern = InputPattern
  { patternName :: String
  , aliases     :: [String]
  , args        :: [(IsOptional, ArgumentType)]
  , help        :: P.Pretty CT.ColorText
  , parse       :: [String] -> Either (P.Pretty CT.ColorText) Input
  }

data ArgumentType = ArgumentType
  { typeName :: String
  , suggestions :: forall m v a . Monad m
                => String
                -> Codebase m v a
                -> Branch
                -> m [Line.Completion]
  }
instance Show ArgumentType where
  show at = "ArgumentType " <> typeName at

-- `argType` gets called when the user tries to autocomplete an `i`th argument.
-- todo: would be nice if we could alert the user if they try to autocomplete
-- past the end.
argType :: InputPattern -> Int -> Maybe ArgumentType
argType ip i = go (i, args ip) where
  go (_, []) = Nothing
  go (0, (_, t) : _) = Just t
  go (n, (Required, _) : args) = go (n - 1, args)
  go (_, (ZeroPlus, t) : []) = Just t
  go (_, (OnePlus, t) : []) = Just t
  go _ = Nothing

minArgs :: InputPattern -> Int
minArgs ip@(fmap fst . args -> args) = go args where
  go [] = 0
  go (Required : args) = 1 + go args
  go (_ : []) = 0
  go _ = error $ "Invalid args for InputPattern ("
                  <> show (patternName ip) <> "): " <> show args

maxArgs :: InputPattern -> Maybe Int
maxArgs ip@(fmap fst . args -> args) = go args where
  go [] = Just 0
  go (Required : args) = (1 +) <$> go args
  go (Optional : []) = Just 0
  go (_ : []) = Nothing
  go _ = error $ "Invalid args for InputPattern ("
                  <> show (patternName ip) <> "): " <> show args



noSuggestions ::
  Monad m => String -> Codebase m v a -> Branch -> m [Line.Completion]
noSuggestions _ _ _ = pure []
