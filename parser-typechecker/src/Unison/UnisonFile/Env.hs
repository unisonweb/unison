{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.UnisonFile.Env (Env (..)) where

import Unison.DataDeclaration (DataDeclaration, EffectDeclaration (..))
import Unison.Names (Names)
import Unison.Prelude
import Unison.Reference qualified as Reference

data Env v a = Env
  -- Data declaration name to hash and its fully resolved form
  { datasId :: Map v (Reference.Id, DataDeclaration v a),
    -- Effect declaration name to hash and its fully resolved form
    effectsId :: Map v (Reference.Id, EffectDeclaration v a),
    -- Naming environment
    names :: Names
  }
