{-# LANGUAGE PatternSynonyms, CPP #-}

module Main where

import Unison.Reference (Reference)
import Unison.Symbol.Extra ()
import Unison.Term.Extra ()
import Unison.Hash.Extra ()
import Unison.Node.Store (Store)
import Unison.Var (Var)
import qualified Unison.ABT as ABT
import qualified Unison.Node.BasicNode as BasicNode
#ifdef leveldb
import qualified Unison.Node.LeveldbStore as DBStore
#else
import qualified Unison.Node.FileStore as FileStore
#endif
import qualified Unison.NodeServer as NodeServer
import qualified Unison.Reference as Reference
import qualified Unison.Symbol as Symbol
import qualified Unison.Term as Term
import qualified Unison.View as View

hash :: Var v => Term.Term v -> Reference
hash (Term.Ref' r) = r
hash t = Reference.Derived (ABT.hash t)

store :: IO (Store IO (Symbol.Symbol View.DFO))
#ifdef leveldb
store = DBStore.make "store"
#else
store = FileStore.make "store"
#endif

main :: IO ()
main = do
  store' <- store
  node <- BasicNode.make hash store'
  NodeServer.server 8080 node
