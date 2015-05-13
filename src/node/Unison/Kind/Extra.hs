{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Kind.Extra where

import Data.Bytes.Serial
import Unison.Kind

instance Serial Kind
