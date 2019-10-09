module Unison.Codebase.ShortBranchHash where

import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Causal as Causal
import qualified Unison.Hash as Hash
import qualified Data.Text as Text
import Data.Text (Text)

newtype ShortBranchHash = 
  ShortBranchHash { toText :: Text } 
  deriving (Eq, Ord)

toString :: ShortBranchHash -> String
toString = Text.unpack . toText

toHash :: ShortBranchHash -> Maybe Branch.Hash
toHash = fmap Causal.RawHash . Hash.fromBase32Hex . toText

fromHash :: Int -> Branch.Hash -> ShortBranchHash
fromHash len =
  ShortBranchHash . Text.take len . Hash.base32Hex . Causal.unRawHash

-- abc -> SBH abc
-- #abc -> SBH abc
fromText :: Text -> Maybe ShortBranchHash
fromText t =
  let h0 = ShortBranchHash . Text.dropWhile (=='#') $ t
  in const h0 <$> toHash h0

instance Show ShortBranchHash where
  show (ShortBranchHash h) = '#' : Text.unpack h