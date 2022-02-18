{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
module Main where

import qualified Codec.Binary.Base32Hex as Sandi
import Criterion
import Criterion.Main
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified U.Util.Base32Hex as U.Base32Hex

main :: IO ()
main = do
  let textual = U.Base32Hex.UnsafeFromText "kccnret7m1895ta8ncs3ct5pqmguqntvjlcsr270ug8mbqvkh07v983i12obpgsii0gbga2esk1423t6evr03f62hkkfllrrj7iil30"
  let binary = "\163\EM}\187\167\176P\146\245H\187\&86t\185\213\161\237_\191\157Y\205\136\224\244\DC1e\235\244\136\SI\244\160r\b\176\188\195\146\144 \184(N\229\STXA\SI\166w\246\SOH\188\194\141(\250\215{\153\229*\140"

  defaultMain
    [ bench "base32 fromByteString" (nf (coerce @_ @(ByteString -> Text) U.Base32Hex.fromByteString) binary),
      bench "sandi fromByteString" (nf sandi_fromByteString binary),
      bench "base32 toByteString" (nf U.Base32Hex.toByteString textual),
      bench "sandi toByteString" (nf sandi_toByteString textual)
    ]

-- The old implementation of `fromByteString` which used `sandi`
sandi_fromByteString :: ByteString -> Text
sandi_fromByteString bs =
  Text.toLower (Text.dropWhileEnd (== '=') (Text.decodeUtf8 (Sandi.encode bs)))

-- The old implementation of `toByteString` which used `sandi`
sandi_toByteString :: U.Base32Hex.Base32Hex -> ByteString
sandi_toByteString (U.Base32Hex.UnsafeFromText txt) =
  case Sandi.decode (Text.encodeUtf8 (Text.toUpper txt <> paddingChars)) of
    Left (_, _rem) -> error ("not base32: " <> Text.unpack txt)
    Right h -> h
  where
    paddingChars :: Text
    paddingChars = case Text.length txt `mod` 8 of
      0 -> ""
      n -> Text.replicate (8 - n) "="
