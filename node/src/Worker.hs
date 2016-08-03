module Main where

import Unison.NodeProtocol.V0 (protocol)
import Unison.NodeWorker as W
import qualified Unison.Cryptography as C
import Unison.SerializationAndHashing ()

main :: IO ()
main = W.make protocol crypto lang where
  crypto keypair = C.noop (W.public keypair)
  lang = undefined
