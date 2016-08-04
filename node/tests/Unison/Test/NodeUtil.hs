{-# Language OverloadedStrings #-}

module Unison.Test.NodeUtil where

import Unison.Hash (Hash)
import Unison.Node (Node)
import Unison.Reference (Reference)
import Unison.Runtime.Address
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Var (Var)
import qualified Unison.ABT as ABT
import qualified Unison.BlockStore.MemBlockStore as MBS
import qualified Unison.Cryptography as C
import qualified Unison.Hash as Hash
import qualified Unison.Node.BasicNode as BasicNode
import qualified Unison.Node.Builtin as Builtin
import qualified Unison.Node.UnisonBlockStore as UBS
import qualified Unison.Reference as R
import qualified Unison.Reference as Reference
import qualified Unison.Runtime.ExtraBuiltins as EB
import qualified Unison.Term as Term
import qualified Unison.View as View

type DFO = View.DFO
type V = Symbol DFO
type TestNode = Node IO V R.Reference (Type V) (Term V)

hash :: Var v => Term.Term v -> Reference
hash (Term.Ref' r) = r
hash t = Reference.Derived (ABT.hash t)

makeRandomAddress :: C.Cryptography k syk sk skp s h c -> IO Address
makeRandomAddress crypt = Address <$> C.randomBytes crypt 64

makeTestNode :: IO TestNode
makeTestNode = do
  let crypto = C.noop "dummypublickey"
  blockStore <- MBS.make' (makeRandomAddress crypto) makeAddress
  store' <- UBS.make blockStore
  keyValueOps <- EB.makeAPI blockStore crypto
  let makeBuiltins whnf = concat [Builtin.makeBuiltins whnf, keyValueOps whnf]
  BasicNode.make hash store' makeBuiltins
