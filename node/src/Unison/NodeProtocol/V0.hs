{-# Language OverloadedStrings #-}

module Unison.NodeProtocol.V0 (protocol) where

import qualified Data.ByteString.Base64.URL as Base64
import qualified Unison.NodeProtocol as P
import qualified Unison.Runtime.Multiplex as Mux
import Unison.SerializationAndHashing (TermV)
import Data.ByteString
import Unison.Hash (Hash)

b64Channel :: ByteString -> Mux.Channel a
b64Channel bs = Mux.Channel Mux.Type (Base64.decodeLenient bs)

protocol :: P.Protocol TermV ByteString ByteString Hash
protocol = P.Protocol -- keeping channel ids human readable for debuggability
  (b64Channel "v0-destroyIn")
  (b64Channel "v0-destroyOut")
  (b64Channel "v0-spawn")
  (Mux.EncryptedChannel (b64Channel "v0-eval"))
  (b64Channel "v0-local-eval")
  (Mux.EncryptedChannel (b64Channel "v0-sync"))
  (b64Channel "v0-insert")
  (b64Channel "v0-lookup")
  (b64Channel "v0-declare")
  (b64Channel "v0-delete")
  (b64Channel "v0-update")
  (b64Channel "v0-append")
  (b64Channel "v0-resolve")
  (b64Channel "v0-resolves")
