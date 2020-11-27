{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Reflog where

import Data.Text (Text)
import qualified Data.Text as Text
import Unison.Codebase.Branch (Hash)
import qualified Unison.Codebase.Causal as Causal
import qualified Unison.Hash as Hash

data Entry = Entry
  { from :: Hash,
    to :: Hash,
    reason :: Text
  }

fromText :: Text -> Maybe Entry
fromText t =
  case Text.words t of
    (Hash.fromBase32Hex -> Just old) : (Hash.fromBase32Hex -> Just new) : (Text.unwords -> reason) ->
      Just $ Entry (Causal.RawHash old) (Causal.RawHash new) reason
    _ -> Nothing

toText :: Entry -> Text
toText (Entry old new reason) =
  Text.unwords
    [ Hash.base32Hex . Causal.unRawHash $ old,
      Hash.base32Hex . Causal.unRawHash $ new,
      reason
    ]
