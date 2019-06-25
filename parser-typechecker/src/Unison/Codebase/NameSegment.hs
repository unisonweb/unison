{-# LANGUAGE PatternSynonyms   #-}

module Unison.Codebase.NameSegment where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Unison.Hashable               as H
import qualified Unison.HashQualified          as HQ
import qualified Unison.HashQualified'         as HQ'

-- Represents the parts of a name between the `.`s
newtype NameSegment = NameSegment { toText :: Text } deriving (Eq, Ord, Show)
type HQSegment = HQ.HashQualified' NameSegment
type HQ'Segment = HQ'.HashQualified' NameSegment

instance H.Hashable NameSegment where
  tokens s = [H.Text (toText s)]

isEmpty :: NameSegment -> Bool
isEmpty ns = toText ns == mempty

isPrefixOf :: NameSegment -> NameSegment -> Bool
isPrefixOf n1 n2 = Text.isPrefixOf (toText n1) (toText n2)

toString :: NameSegment -> String
toString = Text.unpack . toText
