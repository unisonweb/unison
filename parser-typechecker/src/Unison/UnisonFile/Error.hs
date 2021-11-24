{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.UnisonFile.Error where

data Error v a
  -- A free type variable that couldn't be resolved
  = UnknownType v a
  -- A variable which is both a data and an ability declaration
  | DupDataAndAbility v a a
  deriving (Eq,Ord,Show)

