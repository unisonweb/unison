{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms, CPP #-}

module Main where

import System.IO
import Unison.Codebase.Store (Store)
import Unison.Reference (Reference)
import Unison.Runtime.Address
import Unison.SerializationAndHashing ()
import Unison.Var (Var)
import qualified Unison.ABT as ABT
import qualified Unison.BlockStore.FileBlockStore as FBS
import qualified Unison.Builtin as Builtin
import qualified Unison.Cryptography as C
#ifdef leveldb
import qualified Unison.Codebase.LeveldbStore as DBStore
#else
import qualified Unison.Codebase.FileStore as FileStore
#endif
import qualified Unison.Codebase as Codebase
import qualified Unison.CodebaseServer as CodebaseServer
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
  mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
  store' <- store
  logger <- L.atomic (L.atInfo L.toStandardError)
  let crypto = C.noop "dummypublickey"
  blockStore <- FBS.make' (makeRandomAddress crypto) makeAddress "Index"
  builtins0 <- pure $ Builtin.make logger
  builtins1 <- EB.make logger blockStore crypto
  codebase <- pure $ Codebase.make hash store'
  Codebase.addBuiltins (builtins0 ++ builtins1) store' codebase
  CodebaseServer.server 8080 codebase
