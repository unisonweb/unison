{-# LANGUAGE OverloadedStrings #-}
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
import qualified Unison.Util.Logger as L

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
  logger <- L.atomic (L.atInfo L.toStandardError)
  let crypto = C.noop "dummypublickey"
  blockStore <- FBS.make' (makeRandomAddress crypto) makeAddress "Index"
  keyValueOps <- EB.make logger blockStore crypto
  let makeBuiltins whnf = concat [Builtin.makeBuiltins logger whnf, keyValueOps whnf]
  node <- BasicNode.make hash store' makeBuiltins
  NodeServer.server 8080 node
