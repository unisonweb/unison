{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.SerializationAndHashing
  (V, TermV, DFO, hash, hash', serializeTerm, deserializeTerm, deserializeTermFromBytes) where

import Control.Comonad.Cofree (Cofree((:<)))
import Data.ByteString (ByteString)
import Data.Bytes.Serial (serialize, Serial(..), Serial1(..))
import Data.Foldable (traverse_)
import Data.List
import Data.Serialize.Get (Get)
import Data.Vector (Vector)
import Unison.Hash (Hash)
import Unison.Literal (Literal)
import Unison.Reference (Reference)
import Unison.Symbol (Symbol)
import Unison.Var (Var)
import qualified Crypto.Hash as CH
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Data.Bytes.VarInt as VarInt
import qualified Data.Vector as Vector
import qualified Unison.ABT as ABT
import qualified Unison.Doc as Doc
import qualified Unison.Hash as Hash
import qualified Unison.Hashable as H
import qualified Unison.Kind as Kind
import qualified Unison.Pattern as Pattern
import qualified Unison.Reference as Reference
import qualified Unison.Remote as Remote
import qualified Unison.Term as Term
import qualified Unison.TermEdit as TermEdit
import qualified Unison.Type as Type
import qualified Unison.View as View

type DFO = View.DFO
type V = Symbol DFO
type TermV = Term.Term V

hash :: (Var v) => Term.Term v -> Reference
hash (Term.Ref' r) = r
hash t = Reference.Derived (ABT.hash t)

hash' :: Var v => Term.Term v -> ByteString
hash' = Put.runPutS . serialize . hash

serializeTerm :: (Var v, Serial v) => Term.Term v -> ByteString
serializeTerm = Put.runPutS . serialize

deserializeTerm :: (Var v, Serial v) => Get (Term.Term v)
deserializeTerm = deserialize

deserializeTermFromBytes :: (Var v, Serial v) => ByteString -> Either String (Term.Term v)
deserializeTermFromBytes = Get.runGetS deserializeTerm

-- boring serialization and hashing instances below

instance H.Accumulate Hash where
  accumulate = Hash.fromBytes . BA.convert . CH.hashFinalize . foldl' step CH.hashInit where
    step :: CH.Context CH.SHA3_512 -> H.Token Hash -> CH.Context CH.SHA3_512
    step acc (H.Tag b) = CH.hashUpdate acc (B.singleton b)
    step acc (H.Bytes bs) = CH.hashUpdate acc bs
    step acc (H.VarInt i) = CH.hashUpdate acc (Put.runPutS $ serialize (VarInt.VarInt i))
    step acc (H.Text txt) = CH.hashUpdate acc (Put.runPutS $ serialize txt)
    step acc (H.Double d) = CH.hashUpdate acc (Put.runPutS $ serialize d)
    step acc (H.Hashed h) = CH.hashUpdate acc (Hash.toBytes h)
  fromBytes = Hash.fromBytes
  toBytes = Hash.toBytes

instance Serial Hash where
  serialize h = serialize (Hash.toBytes h)
  deserialize = Hash.fromBytes <$> deserialize

instance Serial Reference
instance Serial a => Serial (Remote.Remote a)
instance Serial a => Serial (Remote.Step a)
instance Serial1 Remote.Remote
instance Serial1 Remote.Step
instance Serial a => Serial (Remote.Local a)
instance Serial1 Remote.Local
instance Serial Remote.Node
instance Serial Remote.Duration
instance Serial Remote.Channel
instance Serial Pattern.Pattern
instance Serial Kind.Kind
instance Serial a => Serial (Symbol a)
instance Serial1 Symbol

instance (Foldable f, Serial a, Serial v, Ord v, Serial1 f) => Serial (ABT.Term f v a) where
  serialize (ABT.Term _ a e) = serialize a *> case e of
    ABT.Var v -> Put.putWord8 0 *> serialize v
    ABT.Cycle body -> Put.putWord8 1 *> serialize body
    ABT.Abs v body -> Put.putWord8 2 *> serialize v *> serialize body
    ABT.Tm v -> Put.putWord8 3 *> serializeWith serialize v

  deserialize = do
    ann <- deserialize
    b <- Get.getWord8
    case b of
      0 -> ABT.annotatedVar ann <$> deserialize
      1 -> ABT.cycle' ann <$> deserialize
      2 -> ABT.abs' ann <$> deserialize <*> deserialize
      3 -> ABT.tm' ann <$> deserializeWith deserialize
      _ -> fail ("unknown byte tag, expected one of {0,1,2,3}, got: " ++ show b)

instance Serial Literal
instance Serial a => Serial (Term.Distributed a)
instance Serial1 Term.Distributed
instance (Serial v, Ord v) => Serial1 (Term.F v)

instance Serial1 Vector where
  serializeWith f vs = serialize (Vector.length vs) *> traverse_ f vs
  deserializeWith v = deserialize >>= \len -> sequence (Vector.replicate len v)

instance Serial Type.Literal
instance Serial1 Type.F

instance Serial v => Serial (TermEdit.Action v)

instance Serial View.Precedence
instance Serial View.Var
instance Serial View.Segment

instance Serial1 f => Serial1 (Cofree f) where
  serializeWith put (a :< f) = put a *> serializeWith (serializeWith put) f
  deserializeWith get = (:<) <$> get <*> deserializeWith (deserializeWith get)

instance Serial View.DFO where
  serialize (View.DFO d prec) = serializeWith serialize d *> serialize prec
  deserialize = View.DFO <$> deserializeWith deserialize <*> deserialize

instance (Serial e) => Serial1 (Doc.D e) where

  serializeWith _ Doc.Empty = Put.putWord8 0
  serializeWith _ (Doc.Embed b e) = Put.putWord8 1 *> serialize b *> serialize e
  serializeWith _ (Doc.Breakable e) = Put.putWord8 2 *> serialize e
  serializeWith _ (Doc.Fits b e) = Put.putWord8 3 *> serialize b *> serialize e
  serializeWith _ Doc.Linebreak = Put.putWord8 4
  serializeWith put (Doc.Group r) = Put.putWord8 5 *> put r
  serializeWith put (Doc.Nest e r) = Put.putWord8 6 *> serialize e *> put r
  serializeWith put (Doc.Append a b) = Put.putWord8 7 *> put a *> put b
  deserializeWith get = do
    b <- Get.getWord8
    case b of
      0 -> pure Doc.Empty
      1 -> Doc.Embed <$> deserialize <*> deserialize
      2 -> Doc.Breakable <$> deserialize
      3 -> Doc.Fits <$> deserialize <*> deserialize
      4 -> pure Doc.Linebreak
      5 -> Doc.Group <$> get
      6 -> Doc.Nest <$> deserialize <*> get
      7 -> Doc.Append <$> get <*> get
      _ -> fail ("unknown byte tag, expected [0-7], got: " ++ show b)
