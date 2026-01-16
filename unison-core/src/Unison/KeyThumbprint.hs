module Unison.KeyThumbprint (KeyThumbprint (..)) where

import Data.Text (Text)

newtype KeyThumbprint = KeyThumbprint {thumbprintToText :: Text}
  deriving (Show, Eq, Ord)
