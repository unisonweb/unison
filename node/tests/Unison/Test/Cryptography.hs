module Unison.Test.Cryptography where

import EasyTest
import Control.Monad
import qualified Unison.Cryptography as C
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Unison.Runtime.Cryptography (mkCrypto, symmetricKey)
import Data.Either

test :: Test ()
test = scope "Cryptography" $ do
  let crypto = mkCrypto (BC.pack "dummypublickey")
  Just symkey <- symmetricKey <$> io (C.randomBytes crypto 32)
  bigSizes <- listOf 3 (int' 1000 9000)
  cleartexts <- map B.pack <$> listsOf ([0..100] ++ bigSizes) word8
  cleartexts `forM_` \cleartext -> do
    ciphertext <- io (C.encrypt crypto symkey [cleartext])
    let cleartext' = C.decrypt crypto symkey ciphertext
    case cleartext' of
      Left err -> crash err
      Right cleartext' -> expect (cleartext == cleartext')
