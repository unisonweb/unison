module Unison.Test.Cryptography where

import EasyTest
import Control.Monad
import qualified Unison.Cryptography as C
import Unison.Runtime.Cryptography
import qualified Data.ByteString as B
import Crypto.Noise.DH
import Crypto.Noise.DH.Curve25519
import Data.Either

test :: Test ()
test = scope "Cryptography" $ do
  keyPair <- io (dhGenKey :: IO (KeyPair Curve25519))
  let crypto = mkCrypto keyPair
  Just symkey <- symmetricKey <$> io (C.randomBytes crypto 32)
  bigSizes <- listOf 3 (int' 1000 9000)
  cleartexts <- map B.pack <$> listsOf ([0..100] ++ bigSizes) word8
  cleartexts `forM_` \cleartext -> do
    ciphertext <- io (C.encrypt crypto symkey [cleartext])
    let cleartext' = C.decrypt crypto symkey ciphertext
    case cleartext' of
      Left err -> crash err
      Right cleartext' -> expect (cleartext == cleartext')
