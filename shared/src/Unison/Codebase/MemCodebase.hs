{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Codebase.MemCodebase where

import Data.List
import Unison.Codebase (Codebase)
import Unison.Codebase.Store (Store)
import Unison.Note (Noted)
import Unison.Hash (Hash)
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
import qualified Unison.Builtin as Builtin
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.MemStore as MemStore
import qualified Unison.Hash as Hash
import qualified Unison.Hashable as Hashable
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

make :: (Show v, Var v) => Logger
     -> IO (Codebase IO v Reference (Type v) (Term v), Term v -> Noted IO (Term v))
make logger = do
  store <- MemStore.make :: IO (Store IO v)
  code <- pure $ Codebase.make hash store
  let builtins = Builtin.makeBuiltins logger
  Codebase.addBuiltins builtins store code
  pure (code, Codebase.interpreter builtins code)
