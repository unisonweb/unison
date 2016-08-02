{-# Language OverloadedStrings #-}

module Unison.NodeProtocol.V0 where

import qualified Unison.NodeProtocol as P
import qualified Unison.Runtime.Multiplex as Mux
import qualified Unison.Node.BasicNode as BN
import Data.ByteString
import Unison.Term (Term)
import Unison.Hash (Hash)

protocol :: P.Protocol (Term BN.V) ByteString ByteString Hash
protocol = P.Protocol -- keeping channel ids human readable for debuggability
  (Mux.Channel Mux.Type "v0-destroyIn")
  (Mux.Channel Mux.Type "v0-destroyOut")
  (Mux.Channel Mux.Type "v0-spawn")
  (Mux.EncryptedChannel (Mux.Channel Mux.Type "v0-eval"))
  (Mux.EncryptedChannel (Mux.Channel Mux.Type "v0-sync"))
  (Mux.Channel Mux.Type "v0-insert")
  (Mux.Channel Mux.Type "v0-lookup")
  (Mux.Channel Mux.Type "v0-declare")
  (Mux.Channel Mux.Type "v0-delete")
  (Mux.Channel Mux.Type "v0-update")
  (Mux.Channel Mux.Type "v0-append")
  (Mux.Channel Mux.Type "v0-resolve")
  (Mux.Channel Mux.Type "v0-resolves")

