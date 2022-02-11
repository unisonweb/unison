{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE DeriveGeneric #-}

module Unison.ConstructorType where

import Unison.Prelude
import Unison.Hashable (Hashable, Token(Tag), tokens)

data ConstructorType = Data | Effect deriving (Eq, Ord, Show, Enum, Generic)

instance Hashable ConstructorType where
  tokens b = [Tag . fromIntegral $ fromEnum b]
