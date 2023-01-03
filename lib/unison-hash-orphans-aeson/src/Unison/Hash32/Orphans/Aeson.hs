{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Hash32.Orphans.Aeson () where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Text (Text)
import U.Util.Base32Hex (Base32Hex (..))
import Unison.Hash32 (Hash32 (..))

deriving via Text instance FromJSON Hash32

deriving via Text instance FromJSONKey Hash32

deriving via Text instance ToJSON Hash32

deriving via Text instance ToJSONKey Hash32
