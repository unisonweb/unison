{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module U.Codebase.ShortHash
  ( ShortHash (..),
    ShortCausalHash (..),
    ShortNamespaceHash (..),
    shortenTo,
    shortCausalHashToText,
    shortNamespaceHashToText,
    parseShortCausalHash,
    parseShortNamespaceHash,
  )
where

import qualified Data.Text as Text
import U.Util.Base32Hex as Base32
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

newtype ShortCausalHash = ShortCausalHash Text
  deriving stock (Eq, Ord, Show)

newtype ShortNamespaceHash = ShortNamespaceHash Text
  deriving stock (Eq, Ord, Show)

shortNamespaceHashToText :: ShortNamespaceHash -> Text
shortNamespaceHashToText (ShortNamespaceHash base32) = "#" <> base32

shortCausalHashToText :: ShortCausalHash -> Text
shortCausalHashToText (ShortCausalHash base32) = "#" <> base32

shortenTo :: Int -> ShortHash -> ShortHash
shortenTo _ b@(Builtin _) = b
shortenTo i s@ShortHash {..} = s {prefix = Text.take i prefix}

parseShortCausalHash :: Text -> Either Text ShortCausalHash
parseShortCausalHash = fmap ShortCausalHash . parseHashLike

parseShortNamespaceHash :: Text -> Either Text ShortNamespaceHash
parseShortNamespaceHash = fmap ShortNamespaceHash . parseHashLike

parseHashLike :: Text -> Either Text Text
parseHashLike txt =
  case Text.stripPrefix "#" txt <|> Text.stripPrefix "#" txt of
    Nothing -> Left $ "Hash arguments must begin with a # or @ prefix, but this did not: " <> (fromString . Text.unpack $ txt)
    Just h -> case Base32.fromText h of
      Nothing -> Left $ "Hash argument was invalid base32: " <> (fromString . Text.unpack $ txt)
      Just (Base32.UnsafeFromText bh) -> Right bh
