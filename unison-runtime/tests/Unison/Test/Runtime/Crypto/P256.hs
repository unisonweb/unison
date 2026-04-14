module Unison.Test.Runtime.Crypto.P256 where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (fromJust)
import EasyTest
import Text.Hex (decodeHex)
import Unison.Runtime.Crypto.P256 qualified as P256

test :: Test ()
test =
  scope "p256" $
    tests
      [ scope "derivePublicKey" derivePublicKeyTest,
        scope "signSha256" signSha256Test,
        scope "verifySha256" verifySha256Test
      ]

privateKey :: ByteString
privateKey =
  fromJust $
    decodeHex
      "0000000000000000000000000000000000000000000000000000000000000001"

publicKey :: ByteString
publicKey =
  fromJust $
    decodeHex
      "046b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c2964fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5"

message :: ByteString
message = BC.pack "hello"

signature :: ByteString
signature =
  fromJust $
    decodeHex
      "fe485cf09056db3fec44540ce06a9abf8ac1498d5cae2fa5c36519afaff98487448fcd767dfeee08940317d1e01d4296b9bd323629771d118ea10674dbb47755"

derivePublicKeyTest :: Test ()
derivePublicKeyTest =
  expectEqual (Right publicKey) (P256.derivePublicKey privateKey)

signSha256Test :: Test ()
signSha256Test =
  expectEqual (Right signature) (P256.signSha256 privateKey message)

verifySha256Test :: Test ()
verifySha256Test =
  tests
    [ expectEqual (Right True) (P256.verifySha256 publicKey message signature),
      expectEqual
        (Right False)
        (P256.verifySha256 publicKey (BC.pack "hello!") signature)
    ]
