{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Reference where

import Data.Aeson.TH
import GHC.Generics
import Data.Bytes.Serial
import qualified Data.Text as Text
import qualified Unison.Hash as H

data Reference = Builtin Text.Text | Derived H.Hash deriving (Eq,Ord,Generic)

instance Show Reference where
  show (Builtin t) = Text.unpack t
  show (Derived h) = show h

deriveJSON defaultOptions ''Reference
instance Serial Reference
