{-# LANGUAGE DeriveGeneric #-}

module Unison.Reference where

import GHC.Generics
import Unison.Hashable as Hashable
import qualified Data.Text as Text
import qualified Unison.Hash as H

data Reference = Builtin Text.Text | Derived H.Hash deriving (Eq,Ord,Generic)

instance Show Reference where
  show (Builtin t) = Text.unpack t
  show (Derived h) = "#" <> show h

instance Hashable.Hashable Reference where
  tokens (Builtin txt) = [Hashable.Tag 0, Hashable.Text txt]
  tokens (Derived h) = [Hashable.Tag 0, Hashable.Bytes (H.toBytes h)]
