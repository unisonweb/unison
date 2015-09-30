{-# LANGUAGE PatternSynonyms #-}

module Main where

import Data.Bytes.Serial (Serial)
import Unison.Reference (Reference)
import Unison.Symbol.Extra ()
import Unison.Term.Extra ()
import Unison.Hash.Extra ()
import Unison.Node.Store (Store)
import Unison.Var (Var)
import qualified Unison.ABT as ABT
import qualified Unison.Hash
import qualified Unison.Hash as Hash
import qualified Unison.Node.BasicNode as BasicNode
import qualified Unison.Node.FileStore as FileStore
import qualified Unison.NodeServer as NodeServer
import qualified Unison.Reference as Reference
import qualified Unison.Symbol as Symbol
import qualified Unison.Term as Term
import qualified Unison.View as View

hash :: Var v => Term.Term v -> Reference
hash (Term.Ref' r) = r
hash t = Reference.Derived (ABT.hash t)

main :: IO ()
main = do
  store <- FileStore.make "store" :: IO (Store IO (Symbol.Symbol View.DFO))
  node <- BasicNode.make hash store
  NodeServer.server 8080 node
