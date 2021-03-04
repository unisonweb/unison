{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Unison.Codebase.ShortBranchHash where

import Unison.Prelude
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal
import qualified Unison.Hash as Hash
import qualified Data.Text as Text
import qualified Data.Set as Set

newtype ShortBranchHash =
  ShortBranchHash { toText :: Text } -- base32hex characters
  deriving (Eq, Ord, Generic)

toString :: ShortBranchHash -> String
toString = Text.unpack . toText

toHash :: ShortBranchHash -> Maybe Branch.Hash
toHash = fmap Causal.RawHash . Hash.fromBase32Hex . toText

fromHash :: Int -> Branch.Hash -> ShortBranchHash
fromHash len =
  ShortBranchHash . Text.take len . Hash.base32Hex . Causal.unRawHash

fullFromHash :: Branch.Hash -> ShortBranchHash
fullFromHash = ShortBranchHash . Hash.base32Hex . Causal.unRawHash

-- abc -> SBH abc
-- #abc -> SBH abc
fromText :: Text -> Maybe ShortBranchHash
fromText (Text.dropWhile (=='#') -> t)
  | Text.all (`Set.member` Hash.validBase32HexChars) t = Just
  $ ShortBranchHash t
fromText _ = Nothing

instance Show ShortBranchHash where
  show (ShortBranchHash h) = '#' : Text.unpack h
