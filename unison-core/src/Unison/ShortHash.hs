{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.ShortHash where

import Unison.Prelude

import qualified Data.Text as Text

-- Arya created this type to be able to query the Codebase for anonymous definitions.  The parsing functions can't fail,
-- because they only try to pull apart the syntactic elements "#" and ".".  They don't necessarily produce a meaningful
-- reference; you'll figure that out during base32 decoding.  We don't attempt base32 decoding here because the base32
-- prefix doesn't correspond to anything useful.  We'll just compare strings against the codebase or namespace later.
-- None of the punctuation is stored here.
data ShortHash
  = Builtin Text
  | ShortHash { prefix :: Text, cycle :: Maybe Text, cid :: Maybe Text }
  deriving (Eq, Ord, Show, Generic)

-- currently unused
isConstructor :: ShortHash -> Bool
isConstructor = \case
  ShortHash _ _ (Just _) -> True
  _ -> False

-- Parse a string like those described in Referent.fromText:
-- examples:
-- `##Text.take` — builtins don’t have cycles or cids
-- `#2tWjVAuc7` — term ref, no cycle
-- `#y9ycWkiC1.y9` — term ref, part of cycle
-- `#cWkiC1x89#1` — constructor
-- `#DCxrnCAPS.WD#0` — constructor of a type in a cycle
-- A constructor ID on a builtin is ignored:
--  e.g. ##FileIO#2 is parsed as ##FileIO
-- Anything to the left of the first # is
--   e.g. foo#abc is parsed as #abc
-- Anything including and following a third # is ignored.
--   e.g. foo#abc#2#hello is parsed as #abc#2
-- Anything after a second . before a second # is ignored.
--   e.g. foo#abc.1f.x is parsed as #abc.1f
fromText :: Text -> Maybe ShortHash
fromText t = case Text.split (=='#') t of
  [_, "", b] -> Just $ Builtin b -- builtin starts with ##
  _ : "" : b : _ -> -- builtin with a CID todo: could be rejected
    Just $ Builtin b
  [_, h]     -> Just $ uncurry ShortHash (getCycle h) Nothing
  [_, h, c] -> Just $ uncurry ShortHash (getCycle h) (Just c)
  _ : h : c : _garbage -> -- CID with more hash after todo: could be rejected
    Just $ uncurry ShortHash (getCycle h) (Just c)
  _ -> Nothing
  where
  getCycle :: Text -> (Text, Maybe Text)
  getCycle h = case Text.split (=='.') h of
    [] -> ("", Nothing) -- e.g. foo#.1j
    [hash] -> (hash, Nothing)
    hash : suffix : _garbage -> (hash, Just suffix)

unsafeFromText :: Text -> ShortHash
unsafeFromText t = fromMaybe
    (error . Text.unpack $ "can't parse ShortHash from: " <> t)
    (fromText t)

toText :: ShortHash -> Text
toText (Builtin b) = "##" <> b
toText (ShortHash p i cid) = "#" <> p <> i' <> c' where
  i', c' :: Text
  i' = maybe "" ("."<>) i
  c' = maybe "" ("#" <>) cid

toString :: ShortHash -> String
toString = Text.unpack . toText

fromString :: String -> Maybe ShortHash
fromString = fromText . Text.pack

take :: Int -> ShortHash -> ShortHash
take _ b@(Builtin _) = b
take i s@ShortHash{..} = s { prefix = Text.take i prefix }

-- x `isPrefixOf` y is True iff x might be a shorter version of y
-- if a constructor id is provided on the right-hand side, the left-hand side
-- needs to match exactly (as of this commit).
isPrefixOf :: ShortHash -> ShortHash -> Bool
isPrefixOf (Builtin t) (Builtin t2) = t `Text.isPrefixOf` t2
isPrefixOf (ShortHash h n cid) (ShortHash h2 n2 cid2) =
  Text.isPrefixOf h h2 && maybePrefixOf n n2 && maybePrefixOf cid cid2
  where
  Nothing `maybePrefixOf` Nothing = True
  Nothing `maybePrefixOf` Just _ = False
  Just _ `maybePrefixOf` Nothing = False
  Just a `maybePrefixOf` Just b = a == b
isPrefixOf _ _ = False

--instance Show ShortHash where
--  show = Text.unpack . toText
