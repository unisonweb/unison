{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns #-}

  -- , unsafeFromText
module Unison.Codebase.NameSegment where

import qualified Data.Foldable as Foldable
-- import           Data.String                    ( IsString
--                                                 , fromString
--                                                 )
import           Data.Text                      ( Text, intercalate )
import qualified Data.Text                     as Text
import           Data.Sequence                  (Seq((:<|),(:|>) ))
import qualified Data.Sequence                 as Seq
import qualified Unison.Hashable               as H
import           Unison.Name                    ( Name )
import qualified Unison.Name                   as Name
import qualified Unison.HashQualified          as HQ
import qualified Unison.HashQualified'         as HQ'
import Unison.Util.Monoid (intercalateMap)

-- Represents the parts of a name between the `.`s
newtype NameSegment = NameSegment { toText :: Text } deriving (Eq, Ord, Show)
type HQSegment = HQ.HashQualified' NameSegment
type HQ'Segment = HQ'.HashQualified' NameSegment

instance H.Hashable NameSegment where
  tokens s = [H.Text (toText s)]
