{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Unison.Codebase.ShortBranchHash where

import Unison.Prelude
import qualified Unison.Hash as Hash
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Coerce (Coercible, coerce)

newtype ShortBranchHash =
  ShortBranchHash { toText :: Text } -- base32hex characters
  deriving (Eq, Ord, Generic)

toString :: ShortBranchHash -> String
toString = Text.unpack . toText

toHash :: Coercible Hash.Hash h => ShortBranchHash -> Maybe h
toHash = fmap coerce . Hash.fromBase32Hex . toText

fromHash :: Coercible h Hash.Hash => Int -> h -> ShortBranchHash
fromHash len =
  ShortBranchHash . Text.take len . Hash.base32Hex . coerce

fullFromHash :: Coercible h Hash.Hash => h -> ShortBranchHash
fullFromHash = ShortBranchHash . Hash.base32Hex . coerce

-- abc -> SBH abc
-- #abc -> SBH abc
fromText :: Text -> Maybe ShortBranchHash
fromText (Text.dropWhile (=='#') -> t)
  | Text.all (`Set.member` Hash.validBase32HexChars) t = Just
  $ ShortBranchHash t
fromText _ = Nothing

instance Show ShortBranchHash where
  show (ShortBranchHash h) = '#' : Text.unpack h
