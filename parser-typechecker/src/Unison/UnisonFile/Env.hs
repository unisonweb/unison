{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.UnisonFile.Env (Env(..), datas) where

import Unison.Prelude

import           Data.Bifunctor         (first)
import           Unison.DataDeclaration (DataDeclaration)
import           Unison.DataDeclaration (EffectDeclaration(..))
import           Unison.Reference       (Reference)
import qualified Unison.Reference       as Reference
import Unison.Names (Names)

data Env v a = Env
  -- Data declaration name to hash and its fully resolved form
  { datasId   :: Map v (Reference.Id, DataDeclaration v a)
  -- Effect declaration name to hash and its fully resolved form
  , effectsId :: Map v (Reference.Id, EffectDeclaration v a)
  -- Naming environment
  , names   :: Names
}

datas :: Env v a -> Map v (Reference, DataDeclaration v a)
datas = fmap (first Reference.DerivedId) . datasId
