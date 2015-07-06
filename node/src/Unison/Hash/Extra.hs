{-# OPTIONS_GHC -fno-warn-orphans #-}
module Unison.Hash.Extra where

import Control.Applicative
import Data.Bytes.Serial
import Unison.Hash

instance Serial Hash where
  serialize h = serialize (hashBytes h)
  deserialize = fromBytes <$> deserialize
