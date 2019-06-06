{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.NameSegment where

import           Data.Text                      ( Text )
import qualified Unison.Hashable               as H
import qualified Unison.HashQualified          as HQ
import qualified Unison.HashQualified'         as HQ'

-- Represents the parts of a name between the `.`s
newtype NameSegment = NameSegment { toText :: Text } deriving (Eq, Ord, Show)
type HQSegment = HQ.HashQualified' NameSegment
type HQ'Segment = HQ'.HashQualified' NameSegment

instance H.Hashable NameSegment where
  tokens s = [H.Text (toText s)]
