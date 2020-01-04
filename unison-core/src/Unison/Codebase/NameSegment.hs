{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.NameSegment
  ( NameSegment
  , unsafeFromText
  , isEmpty
  , isLower
  , isPrefixOf
  , toString
  , toText
  ) where

import Unison.Prelude

import qualified Data.Char                     as Char
import qualified Data.Text                     as Text
import qualified Unison.Hashable               as H

-- Represents the parts of a name between the `.`s
newtype NameSegment = NameSegment { toText :: Text } deriving (Eq, Ord)

instance H.Hashable NameSegment where
  tokens s = [H.Text (toText s)]

unsafeFromText :: Text -> NameSegment
unsafeFromText = NameSegment

isEmpty :: NameSegment -> Bool
isEmpty ns = toText ns == mempty

-- | Does this name segment contain only lowercase letters?
isLower :: NameSegment -> Bool
isLower = coerce (Text.all Char.isLower)

isPrefixOf :: NameSegment -> NameSegment -> Bool
isPrefixOf n1 n2 = Text.isPrefixOf (toText n1) (toText n2)

toString :: NameSegment -> String
toString = Text.unpack . toText

instance Show NameSegment where
  show = Text.unpack . toText

instance IsString NameSegment where
  fromString = NameSegment . Text.pack
