{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Reference.Extra where

import Data.Bytes.Serial
import Unison.Hash.Extra ()
import Unison.Reference

instance Serial Reference

