{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.ShortHash
  ( ShortHash (..),
    ShortCausalHash (..),
    ShortNamespaceHash (..),
    isPrefixOf,
    shortenTo,

    -- * String conversions
    fromText,
    toText,
  )
where

import Data.Text qualified as Text
import Unison.Prelude

-- ##Text.++
--   ^^^^^^^-- builtin

-- #abc123.a#0
--  ^      ^ ^-cid
--  |      \-cycle
--  \-- prefix
data ShortHash
  = Builtin Text
  | ShortHash {prefix :: Text, cycle :: Maybe Word64, cid :: Maybe Word64}
  deriving (Eq, Ord, Show)

newtype ShortCausalHash = ShortCausalHash {shortCausalHashToText :: Text}
  deriving stock (Eq, Ord, Show)

newtype ShortNamespaceHash = ShortNamespaceHash {shortNamespaceHashToText :: Text}
  deriving stock (Eq, Ord, Show)

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

shortenTo :: Int -> ShortHash -> ShortHash
shortenTo _ b@(Builtin _) = b
shortenTo i s@ShortHash {..} = s {prefix = Text.take i prefix}

-- Parse a string like those described in Referent.fromText:
-- examples:
--
-- builtins don’t have cycles or cids
-- >>> fromText "##Text.take"
-- Just (Builtin "Text.take")
--
-- term ref, no cycle
-- >>> fromText "#2tWjVAuc7"
-- Just (ShortHash {prefix = "2tWjVAuc7", cycle = Nothing, cid = Nothing})
--
-- term ref, part of cycle
-- >>> fromText "#y9ycWkiC1.y9"
-- Just (ShortHash {prefix = "y9ycWkiC1", cycle = Just "y9", cid = Nothing})
--
-- constructor
-- >>> fromText "#cWkiC1x89#1"
-- Just (ShortHash {prefix = "cWkiC1x89", cycle = Nothing, cid = Just "1"})
--
-- constructor of a type in a cycle
-- >>> fromText "#DCxrnCAPS.WD#0"
-- Just (ShortHash {prefix = "DCxrnCAPS", cycle = Just "WD", cid = Just "0"})
--
-- A constructor ID on a builtin is ignored:
-- >>> fromText "##FileIO#2"
-- Just (Builtin "FileIO")
--
-- Anything to the left of the first # is
-- >>> fromText "foo#abc "
-- Just (ShortHash {prefix = "abc ", cycle = Nothing, cid = Nothing})
--
-- Anything including and following a third # is ignored.
-- >>> fromText "foo#abc#2#hello"
-- Just (ShortHash {prefix = "abc", cycle = Nothing, cid = Just "2"})
--
-- Anything after a second . before a second # is ignored.
-- >>> fromText "foo#abc.1f.x"
-- Just (ShortHash {prefix = "abc", cycle = Just "1f", cid = Nothing})
fromText :: Text -> Maybe ShortHash
fromText t =
  case Text.split (== '#') t of
    [_, "", b] -> Just $ Builtin b -- builtin starts with ##
    _ : "" : b : _ ->
      -- builtin with a CID todo: could be rejected
      Just $ Builtin b
    [_, h0] -> do
      (h, cid) <- getCycle h0
      Just (ShortHash h cid Nothing)
    [_, h0, readMaybe . Text.unpack -> Just c] -> do
      (h, cid) <- getCycle h0
      Just (ShortHash h cid (Just c))
    _ : h0 : (readMaybe . Text.unpack -> Just c) : _garbage -> do
      -- CID with more hash after todo: could be rejected
      (h, cid) <- getCycle h0
      Just (ShortHash h cid (Just c))
    _ -> Nothing
  where
    getCycle :: Text -> Maybe (Text, Maybe Word64)
    getCycle h =
      case Text.split (== '.') h of
        [] -> Just ("", Nothing) -- e.g. foo#.1j
        [hash] -> Just (hash, Nothing)
        hash : suffix : _garbage -> do
          cid <- readMaybe (Text.unpack suffix)
          Just (hash, Just cid)

toText :: ShortHash -> Text
toText (Builtin b) = "##" <> b
toText (ShortHash p i cid) = "#" <> p <> i' <> c'
  where
    i', c' :: Text
    i' = maybe "" (("." <>) . tShow) i
    c' = maybe "" (("#" <>) . tShow) cid
