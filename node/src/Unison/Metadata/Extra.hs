{-# OPTIONS_GHC -fno-warn-orphans #-}
module Unison.Metadata.Extra where

import Data.Bytes.Serial
import Unison.Metadata

instance Serial Sort
instance (Serial v) => Serial (Names v)
instance (Serial v, Serial h) => Serial (Metadata v h)
