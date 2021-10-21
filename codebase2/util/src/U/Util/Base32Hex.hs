module U.Util.Base32Hex
  ( Base32Hex (..),
    fromByteString,
    toByteString,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base32.Hex as Base32.Hex
import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

newtype Base32Hex = UnsafeBase32Hex {toText :: Text}
  deriving (Eq, Ord, Show)

-- | Return the lowercase unpadded base32Hex encoding of this 'ByteString'.
-- Multibase prefix would be 'v', see https://github.com/multiformats/multibase
fromByteString :: ByteString -> Base32Hex
fromByteString =
  UnsafeBase32Hex . Text.toLower . Base32.Hex.encodeBase32Unpadded

-- | Produce a 'Hash' from a base32hex-encoded version of its binary representation
toByteString :: Base32Hex -> ByteString
toByteString (UnsafeBase32Hex s) =
  case Base32.Hex.decodeBase32Unpadded (Text.encodeUtf8 s) of
    Left _ -> error ("not base32: " <> Text.unpack s)
    Right h -> h
