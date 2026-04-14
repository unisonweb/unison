module Unison.Runtime.Crypto.P256
  ( derivePublicKey,
    signSha256,
    verifySha256,
  )
where

import Crypto.Hash qualified as Hash
import Crypto.PubKey.ECC.ECDSA qualified as ECDSA
import Crypto.PubKey.ECC.Prim qualified as ECC
import Crypto.PubKey.ECC.Types qualified as ECC
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Unison.Util.Text (Text)

curve :: ECC.Curve
curve = ECC.getCurveByName ECC.SEC_p256r1

curveOrder :: Integer
curveOrder = ECC.ecc_n (ECC.common_curve curve)

coordinateBytes :: Int
coordinateBytes = 32

uncompressedPrefix :: Word8
uncompressedPrefix = 0x04

derivePublicKey :: BS.ByteString -> Either Text BS.ByteString
derivePublicKey privateKeyBytes = do
  privateKey <- parsePrivateKey privateKeyBytes
  pure (encodePublicKey (toPublicKey privateKey))

signSha256 :: BS.ByteString -> BS.ByteString -> Either Text BS.ByteString
signSha256 privateKeyBytes message = do
  privateKey <- parsePrivateKey privateKeyBytes
  let digest = Hash.hash message :: Hash.Digest Hash.SHA256
      signature =
        ECDSA.deterministicNonce Hash.SHA256 privateKey digest $
          \nonce -> ECDSA.signDigestWith nonce privateKey digest
  pure (encodeSignature signature)

verifySha256 :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Either Text Bool
verifySha256 publicKeyBytes message signatureBytes = do
  publicKey <- parsePublicKey publicKeyBytes
  signature <- parseSignature signatureBytes
  pure (ECDSA.verify Hash.SHA256 publicKey signature message)

parsePrivateKey :: BS.ByteString -> Either Text ECDSA.PrivateKey
parsePrivateKey bytes
  | BS.length bytes /= coordinateBytes =
      Left "p256: private key must be 32 bytes"
  | d <= 0 =
      Left "p256: private key scalar must be non-zero"
  | d >= curveOrder =
      Left "p256: private key scalar is out of range"
  | otherwise =
      Right (ECDSA.PrivateKey curve d)
  where
    d = os2ip bytes

parsePublicKey :: BS.ByteString -> Either Text ECDSA.PublicKey
parsePublicKey bytes
  | BS.length bytes /= 1 + (2 * coordinateBytes) =
      Left "p256: public key must be 65 bytes in uncompressed SEC1 form"
  | BS.head bytes /= uncompressedPrefix =
      Left "p256: public key must start with 0x04 (uncompressed SEC1 form)"
  | point == ECC.PointO =
      Left "p256: public key point at infinity is invalid"
  | not (ECC.isPointValid curve point) =
      Left "p256: public key point is not on the curve"
  | ECC.pointMul curve curveOrder point /= ECC.PointO =
      Left "p256: public key point is not in the prime-order subgroup"
  | otherwise =
      Right (ECDSA.PublicKey curve point)
  where
    (xBytes, yBytes) = BS.splitAt coordinateBytes (BS.tail bytes)
    point = ECC.Point (os2ip xBytes) (os2ip yBytes)

parseSignature :: BS.ByteString -> Either Text ECDSA.Signature
parseSignature bytes
  | BS.length bytes /= 2 * coordinateBytes =
      Left "p256: signature must be 64 bytes"
  | r <= 0 || r >= curveOrder =
      Left "p256: signature r component is out of range"
  | s <= 0 || s >= curveOrder =
      Left "p256: signature s component is out of range"
  | otherwise =
      Right (ECDSA.Signature r s)
  where
    (rBytes, sBytes) = BS.splitAt coordinateBytes bytes
    r = os2ip rBytes
    s = os2ip sBytes

toPublicKey :: ECDSA.PrivateKey -> ECDSA.PublicKey
toPublicKey privateKey =
  ECDSA.PublicKey curve (ECC.pointBaseMul curve (ECDSA.private_d privateKey))

encodePublicKey :: ECDSA.PublicKey -> BS.ByteString
encodePublicKey publicKey =
  case ECDSA.public_q publicKey of
    ECC.Point x y ->
      BS.concat
        [ BS.singleton uncompressedPrefix,
          i2ospOf_ coordinateBytes x,
          i2ospOf_ coordinateBytes y
        ]
    ECC.PointO ->
      error "p256: attempted to encode point at infinity"

encodeSignature :: ECDSA.Signature -> BS.ByteString
encodeSignature signature =
  BS.concat
    [ i2ospOf_ coordinateBytes (ECDSA.sign_r signature),
      i2ospOf_ coordinateBytes (ECDSA.sign_s signature)
    ]
