{-# Language OverloadedStrings #-}

module Main where

import Unison.NodeProtocol.V0 (protocol)
import Unison.NodeWorker as W
import qualified Unison.Cryptography as C
import Unison.SerializationAndHashing (TermV)
import qualified Unison.Term as Term
import qualified Unison.Runtime.Remote as R
import qualified Unison.Remote as RT
import Unison.Hash (Hash)

main :: IO ()
main = W.make protocol crypto (pure lang) where
  crypto keypair = C.noop (W.public keypair)
  lang blockstore = pure $
    (R.Language localDependencies eval apply node unit channel local unRemote remote
      :: R.Language TermV Hash)
    where
      localDependencies t = undefined
      eval t = undefined
      apply = Term.app
      node = Term.node
      unit = Term.builtin "()"
      channel = Term.channel
      local l = Term.remote (RT.Step (RT.Local l))
      unRemote (Term.Distributed' (Term.Remote r)) = Just r
      unRemote _ = Nothing
      remote = Term.remote
