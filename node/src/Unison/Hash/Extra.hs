{-# OPTIONS_GHC -fno-warn-orphans #-}
module Unison.Hash.Extra where

import Unison.Hash
import Data.Bytes.Serial

instance Serial Hash where
  serialize h = serialize (hashBytes h)
  deserialize = fromBytes <$> deserialize
