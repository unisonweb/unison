{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.ShortHash where

import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Safe (readMay)

-- Arya created this type to be able to query the Codebase for anonymous definitions.  The parsing functions can't fail, because they only try to pull apart the syntactic elements "#" and ".".  They don't necessarily produce a meaningful reference; you'll figure that out during base58 decoding.  We don't attempt base58 decoding here because the base58 prefix doesn't correspond to anything useful.  We'll just compare strings against the codebase or namespace later.
data ShortHash
  = Builtin Text
  | ShortHash { prefix :: Text, cycle :: Maybe Text, cid :: Maybe Text }
  deriving (Eq, Ord)

-- Parse a string like those described in Referent.fromText:
-- examples:
-- `##Text.take` — builtins don’t have cycles
-- `##FileIO#3` — builtins can have suffixes, constructor 3
-- `#2tWjVAuc7` — term ref, no cycle
-- `#y9ycWkiC1.y9` — term ref, part of cycle
-- `#cWkiC1x89#1` — constructor
-- `#DCxrnCAPS.WD#0` — constructor of a type in a cycle
-- Anything to the left of the first # is ignored.
fromText :: Text -> ShortHash
fromText t = case Text.split (=='#') t of
  [_, "", b] -> Builtin b -- builtin gets ##
  [_, h]     -> uncurry ShortHash (getCycle h) Nothing
  [_, h, c]  -> uncurry ShortHash (getCycle h) (Just c)
  where
  getCycle :: Text -> (Text, Maybe Text)
  getCycle h = case Text.split (=='.') h of
    [hash] -> (hash, Nothing)
    [hash, suffix] -> (hash, Just suffix)

toText :: ShortHash -> Text
toText (Builtin b) = "##" <> b
toText (ShortHash p i cid) = "#" <> p <> i' <> c' where
  i', c' :: Text
  i' = maybe "" ("."<>) i
  c' = maybe "" ("#" <>) cid

toString :: ShortHash -> String
toString = Text.unpack . toText

take :: Int -> ShortHash -> ShortHash
take _ b@(Builtin _) = b
take i s@(ShortHash{..}) = s { prefix = (Text.take i prefix) }

instance Show ShortHash where
  show = Text.unpack . toText
