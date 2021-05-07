{-# LANGUAGE ViewPatterns #-}

module U.Codebase.Reflog where

import Data.Text (Text)
import U.Codebase.HashTags (BranchHash)

data Entry = Entry {from :: BranchHash, to :: BranchHash, reason :: Text}

-- fromText :: Text -> Maybe Entry
-- fromText t =
--   case Text.words t of
--     (Hash.fromBase32Hex -> Just old) : (Hash.fromBase32Hex -> Just new) : (Text.unwords -> reason) ->
--       Just $ Entry (Causal.RawHash old) (Causal.RawHash new) reason
--     _ -> Nothing

-- toText :: Entry -> Text
-- toText (Entry old new reason) =
--   Text.unwords [ Hash.base32Hex . Causal.unRawHash $ old
--                , Hash.base32Hex . Causal.unRawHash $ new
--                , reason ]
