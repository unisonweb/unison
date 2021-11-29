{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Reflog (Entry(..), fromText, toText) where

import Data.Coerce (Coercible, coerce)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Unison.Hash as Hash

data Entry h = Entry
  { from :: h,
    to :: h,
    reason :: Text
  }

fromText :: Coercible h Hash.Hash => Text -> Maybe (Entry h)
fromText t =
  case Text.words t of
    (Hash.fromBase32Hex -> Just old) : (Hash.fromBase32Hex -> Just new) : (Text.unwords -> reason) ->
      Just $ Entry (coerce old) (coerce new) reason
    _ -> Nothing

toText :: Coercible h Hash.Hash => Entry h -> Text
toText (Entry old new reason) =
  Text.unwords
    [ Hash.base32Hex . coerce $ old,
      Hash.base32Hex . coerce $ new,
      reason
    ]
