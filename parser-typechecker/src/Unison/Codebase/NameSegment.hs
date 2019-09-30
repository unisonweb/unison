{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.NameSegment where

import Unison.Prelude

import qualified Unison.Name                   as Name
import qualified Data.Text                     as Text
import qualified Unison.Hashable               as H
import qualified Unison.HashQualified'         as HQ'

-- Represents the parts of a name between the `.`s
newtype NameSegment = NameSegment { toText :: Text } deriving (Eq, Ord)
type HQSegment = HQ'.HashQualified' NameSegment

instance H.Hashable NameSegment where
  tokens s = [H.Text (toText s)]

isEmpty :: NameSegment -> Bool
isEmpty ns = toText ns == mempty

isPrefixOf :: NameSegment -> NameSegment -> Bool
isPrefixOf n1 n2 = Text.isPrefixOf (toText n1) (toText n2)

toString :: NameSegment -> String
toString = Text.unpack . toText

toName :: NameSegment -> Name.Name
toName = Name.Name . toText

segments :: Name.Name -> [NameSegment]
segments name = NameSegment <$> Text.splitOn "." (Name.toText name)

instance Show NameSegment where
  show = Text.unpack . toText

instance IsString NameSegment where
  fromString = NameSegment . Text.pack
