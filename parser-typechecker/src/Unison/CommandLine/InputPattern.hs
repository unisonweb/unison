{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
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

type IsOptional = Bool

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
