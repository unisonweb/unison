{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Unison.Kind where

import qualified Language.Haskell.TH.Syntax as TH
import Unison.Prelude

data Kind = Star | Arrow Kind Kind deriving (Eq, Ord, Read, Show, Generic, TH.Lift)
