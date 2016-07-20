module Unison.Runtime.Address where

import Data.ByteString (ByteString)
import Data.Bytes.Serial
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Random
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64

data Address = Address ByteString deriving (Eq,Ord,Show)

fromBytes :: ByteString -> Address
fromBytes = Address

toBytes :: Address -> ByteString
toBytes (Address bs) = bs

toBase64 :: Address -> Text
toBase64 (Address bs) = decodeUtf8 (Base64.encode bs)

fromBase64 :: Text -> Address
fromBase64 = Address . Base64.decodeLenient . encodeUtf8

instance Serial Address where
  serialize h = serialize (toBytes h)
  deserialize = fromBytes <$> deserialize

instance Random Address where
  -- bounds are ignored
  randomR (_, _) gen =
    let rs = iterate (random . snd) (0, gen)
        rPairs = take 64 $ tail rs
        newGen = snd . head $ reverse rPairs
        bstring = B.pack $ map fst rPairs
    in (fromBytes bstring, newGen)
  random = randomR (undefined, undefined)
