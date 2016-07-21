{-# LANGUAGE PatternSynonyms, CPP #-}

module Main where

import Unison.Hash.Extra ()
import Unison.Node.Store (Store)
import Unison.Reference (Reference)
import Unison.Runtime.Address
import Unison.Symbol.Extra ()
import Unison.Term.Extra ()
import Unison.Var (Var)
import qualified Unison.ABT as ABT
import qualified Unison.BlockStore.FileBlockStore as FBS
import qualified Unison.Cryptography as C
import qualified Unison.Node.BasicNode as BasicNode
import qualified Unison.Node.Builtin as Builtin
#ifdef leveldb
import qualified Unison.Node.LeveldbStore as DBStore
#else
import qualified Unison.Node.FileStore as FileStore
#endif
import qualified Unison.NodeServer as NodeServer
import qualified Unison.Reference as Reference
import qualified Unison.Runtime.ExtraBuiltins as EB
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

makeRandomAddress :: C.Cryptography k syk sk skp s h c -> IO Address
makeRandomAddress crypt = Address <$> C.randomBytes crypt 64

main :: IO ()
main = do
  store' <- store
  blockStore <- FBS.make' (makeRandomAddress C.noop) "Index"
  keyValueOps <- EB.makeAPI blockStore C.noop
  let makeBuiltins whnf = concat [Builtin.makeBuiltins whnf, keyValueOps whnf]
  node <- BasicNode.make hash store' makeBuiltins
  NodeServer.server 8080 node
