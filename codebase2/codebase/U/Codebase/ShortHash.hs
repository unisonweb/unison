{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module U.Codebase.ShortHash where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)

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

data ShortBranchHash = ShortBranchHash {toText :: Text} deriving (Eq, Ord, Show)

shortenTo :: Int -> ShortHash -> ShortHash
shortenTo _ b@(Builtin _) = b
shortenTo i s@ShortHash {..} = s {prefix = Text.take i prefix}
