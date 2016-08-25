{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Node.MemNode where

import Data.List
import Unison.Hash (Hash)
import Unison.Node (Node)
import Unison.Node.Store (Store)
import Unison.Reference (Reference(Derived))
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Util.Logger (Logger)
import Unison.Var (Var)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB
import qualified Data.Digest.Murmur64 as Murmur
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Unison.ABT as ABT
import qualified Unison.Hash as Hash
import qualified Unison.Hashable as Hashable
import qualified Unison.Node.BasicNode as BasicNode
import qualified Unison.Node.Builtin as Builtin
import qualified Unison.Node.MemStore as MemStore
import qualified Unison.Symbol as Symbol
import qualified Unison.Term as Term
import qualified Unison.View as View

hash :: (Var v) => Term.Term v -> Reference
hash (Term.Ref' r) = r
hash t = Derived (ABT.hash t)

instance Hashable.Accumulate Hash where
  accumulate tokens = finish $ foldl' step (Murmur.hash64 ()) tokens where
    step acc t = case t of
      Hashable.Tag i -> Murmur.hash64AddInt (fromIntegral i) acc
      Hashable.Bytes bs -> Murmur.hash64Add bs acc
      Hashable.VarInt i -> Murmur.hash64AddInt i acc
      Hashable.Text txt -> Murmur.hash64Add (Encoding.encodeUtf8 txt) acc
      Hashable.Hashed h -> Murmur.hash64Add (Hash.toBytes h) acc
      Hashable.Double d -> Murmur.hash64Add (Encoding.encodeUtf8 (Text.pack (show d))) acc
    finish h64 = (Hash.fromBytes . LB.toStrict . Builder.toLazyByteString . Builder.word64LE . Murmur.asWord64) h64
  fromBytes = Hash.fromBytes
  toBytes = Hash.toBytes

type V = Symbol.Symbol View.DFO

make :: Logger -> IO (Node IO V Reference (Type V) (Term V))
make logger = do
  store <- MemStore.make :: IO (Store IO V)
  BasicNode.make hash store (Builtin.makeBuiltins logger)
