{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module U.Codebase.ShortHash where

import Data.Text (Text)
import Data.Word (Word64)


-- ##Text.++
--   ^^^^^^^-- builtin

-- #abc123.a#0
--  ^      ^ ^-cid
--  |      \-cycle
--  \-- prefix
data ShortHash
  = Builtin Text
  | ShortHash { prefix :: Text, cycle :: Maybe Word64, cid :: Maybe Word64 }
  deriving (Eq, Ord, Show)

data ShortBranchHash = ShortBranchHash { toText :: Text } deriving (Eq, Ord, Show)
