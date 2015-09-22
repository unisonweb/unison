{-# LANGUAGE PatternSynonyms #-}

module Main where

import Unison.Reference (Reference)
import Unison.Symbol.Extra ()
import Unison.Term.Extra ()
import Unison.ABT.Extra as ABT'
import qualified Unison.Node.BasicNode as BasicNode
import qualified Unison.Node.Store as Store
import qualified Unison.NodeServer as NodeServer
import qualified Unison.Reference as Reference
import qualified Unison.Symbol as Symbol
import qualified Unison.Term as Term

hash :: Serial v => Term.Term v -> Reference
hash (Term.Ref' r) = r
hash t = ABT.hash t

main :: IO ()
main = do
  store <- Store.store "store" :: IO (Store IO (Symbol.Symbol ()))
  node <- BasicNode.make hash store
  NodeServer.server 8080 node
