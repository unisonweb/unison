{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Unison.SerializationAndHashing where

import Control.Comonad.Cofree (Cofree((:<)))
import Data.ByteString (ByteString)
import Data.Bytes.Put (MonadPut)
import Data.Bytes.Serial (serialize, serialize1, Serial(..), Serial1(..), GSerial1)
import Data.Bytes.VarInt
import Data.Serialize.Put (Put)
import Data.Serialize.Get (Get)
import GHC.Generics (Generic,Generic1,Rep1)
import Unison.ABT.Extra -- Serial instances
import Unison.Reference (Reference)
import Unison.Reference.Extra -- ..
import Unison.Symbol (Symbol)
import Unison.Symbol.Extra -- Serial instance
import Unison.Term.Extra -- Serial instances
import Unison.Var (Var)
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Unison.ABT as ABT
import qualified Unison.Doc as Doc
import qualified Unison.Reference as Reference
import qualified Unison.Term as Term
import qualified Unison.View as View

type DFO = View.DFO
type V = Symbol DFO
type TermV = Term.Term V

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

testTerm2 :: TermV
testTerm2 = undefined

testSerialize2 :: Put
testSerialize2 = serialize testTerm2

