{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.V1.Branch.NameSegment where

import qualified Data.Text as Text
import Data.String (IsString(..))
import Data.Text (Text)

-- Represents the parts of a name between the `.`s
newtype NameSegment = NameSegment {toText :: Text} deriving (Show, Eq, Ord)

instance IsString NameSegment where
  fromString = NameSegment . Text.pack
