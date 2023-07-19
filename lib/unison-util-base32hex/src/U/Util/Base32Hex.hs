module U.Util.Base32Hex
  ( Base32Hex (UnsafeFromText),
    fromByteString,
    toByteString,
    fromText,
    toText,
    validChars,
  )
where

import Data.ByteString.Base32.Hex qualified as Base32.Hex
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Unison.Prelude

newtype Base32Hex = UnsafeFromText Text
  deriving (Eq, Ord, Show)

toText :: Base32Hex -> Text
toText (UnsafeFromText s) = s

-- | Return the lowercase unpadded base32Hex encoding of this 'ByteString'.
-- Multibase prefix would be 'v', see https://github.com/multiformats/multibase
fromByteString :: ByteString -> Base32Hex
fromByteString =
  UnsafeFromText . Text.toLower . Base32.Hex.encodeBase32Unpadded

-- | Produce a 'Hash' from a base32hex-encoded version of its binary representation
toByteString :: Base32Hex -> ByteString
toByteString (UnsafeFromText s) =
  case Base32.Hex.decodeBase32Unpadded (Text.encodeUtf8 s) of
    Left _ -> error ("not base32: " <> Text.unpack s)
    Right h -> h

fromText :: Text -> Maybe Base32Hex
fromText s =
  if Base32.Hex.isBase32Hex . Text.encodeUtf8 . Text.toUpper $ s
    then Just (UnsafeFromText s)
    else Nothing

validChars :: Set Char
validChars = Set.fromList $ ['0' .. '9'] ++ ['a' .. 'v']
