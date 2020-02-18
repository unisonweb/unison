{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Unison.Codebase.NameSegment where

import Unison.Prelude

import qualified Unison.Name                   as Name
import qualified Data.Text                     as Text
import qualified Unison.Hashable               as H
import qualified Unison.HashQualified'         as HQ'
import qualified Control.Lens as Lens
import Unison.Name (Name(Name))

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
toName = Name.unsafeFromText . toText

segments :: Name.Name -> [NameSegment]
segments name = NameSegment <$> Text.splitOn "." (Name.toText name)

instance Show NameSegment where
  show = Text.unpack . toText

instance IsString NameSegment where
  fromString = NameSegment . Text.pack

instance Lens.Snoc Name Name NameSegment NameSegment where
  _Snoc = Lens.prism snoc unsnoc
    where
    snoc :: (Name, NameSegment) -> Name
    snoc (n,s) = Name.joinDot n (toName s)
    unsnoc :: Name -> Either Name (Name, NameSegment)
    unsnoc n@(Name (Text.splitOn "." -> ns)) = case Lens.unsnoc ns of
      Nothing -> Left n
      Just ([],_) -> Left n
      Just (init, last) -> Right (Name (Text.intercalate "." init), NameSegment last)
