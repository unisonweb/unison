{-# LANGUAGE OverloadedStrings #-}

module Unison.NameSegment where

import qualified Data.Text as Text
import qualified Unison.Hashable as H
import Unison.Prelude

-- Represents the parts of a name between the `.`s
newtype NameSegment = NameSegment {toText :: Text} deriving (Eq, Ord)

-- Split text into segments. A smarter version of `Text.splitOn` that handles
-- the name `.` properly.
segments' :: Text -> [Text]
segments' n = go split
  where
    split = Text.splitOn "." n
    go [] = []
    go ("" : "" : z) = "." : go z
    go ("" : z) = go z
    go (x : y) = x : go y

instance H.Hashable NameSegment where
  tokens s = [H.Text (toText s)]

isEmpty :: NameSegment -> Bool
isEmpty ns = toText ns == mempty

isPrefixOf :: NameSegment -> NameSegment -> Bool
isPrefixOf n1 n2 = Text.isPrefixOf (toText n1) (toText n2)

toString :: NameSegment -> String
toString = Text.unpack . toText

instance Show NameSegment where
  show = Text.unpack . toText

instance IsString NameSegment where
  fromString = NameSegment . Text.pack
