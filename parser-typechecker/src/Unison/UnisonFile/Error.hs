{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.UnisonFile.Error where

data Error v a
  = -- A free type variable that couldn't be resolved
    UnknownType v a
  | -- A variable which is both a data and an ability declaration
    DupDataAndAbility v a a
  deriving (Eq, Ord, Show)
