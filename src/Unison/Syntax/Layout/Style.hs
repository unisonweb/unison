module Unison.Syntax.Layout.Style where

import Data.Text

-- | Controls presentation of a cell in a 'Unison.Edit.Layout'
data Style = Style {
  name :: Text,
  nameVisible :: Bool,
  showEquals :: Bool,
  style :: Text
} deriving (Eq,Ord,Show,Read)
