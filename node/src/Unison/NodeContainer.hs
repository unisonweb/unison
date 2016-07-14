{-# Language OverloadedStrings #-}

module Unison.NodeContainer where

import qualified Data.Bytes.Serial as S
import qualified Data.Bytes.Put as Put
import qualified Unison.BlockStore as BS
import qualified Unison.Cryptography as C
import qualified Unison.Remote as Remote
import qualified Unison.Runtime.Remote as Remote
import qualified Unison.Runtime.JournaledMap as JM

make :: (Ord h, S.Serial key)
     => BS.BlockStore h
     -> C.Cryptography key symmetricKey signKey signature hash Remote.Cleartext
     -> IO ()
make bs crypto = do
  knownNodes <- JM.fromEncryptedSeries crypto bs
    (BS.Series $ Put.runPutS (S.serialize (C.publicKey crypto)))
    (BS.Series $ Put.runPutS (S.serialize (C.publicKey crypto) >> Put.putByteString "updates"))
    :: IO (JM.JournaledMap Remote.Node BS.Series)
  undefined
