{-# LANGUAGE TypeSynonymInstances #-}
module Unison.Runtime.SerializationAndHashing where

import Data.Bytes.Put (MonadPut)
import Data.Bytes.Serial (serialize, serialize1, Serial(..), Serial1(..))
import Data.Bytes.VarInt
import Data.Serialize.Put (Put)
import Unison.Term.Extra -- Serial instances
import Unison.Symbol.Extra -- Serial instance
import Unison.ABT.Extra -- Serial instances
import Unison.Reference (Reference)
import Unison.Reference.Extra -- ..
import Unison.Symbol (Symbol)
import Unison.Var (Var)
import qualified Data.Bytes.Get as Get
import qualified Data.Bytes.Put as Put
import qualified Unison.ABT as ABT
import qualified Unison.Reference as Reference
import qualified Unison.Term as Term
import qualified Unison.View as View

type DFO = View.DFO
type V = Symbol DFO
type TermV = Term.Term V

instance Serial View.Precedence where
  serialize (View.Precedence n) = serialize (VarInt n)
  deserialize = View.Precedence <$> (unVarInt <$> deserialize)

instance Serial View.Var where
  serialize (View.Arg n) = serialize (VarInt n)
  deserialize = View.Arg <$> (unVarInt <$> deserialize)

{--
instance Serial View.Segment where
  serialize (View.Slot v p) = Put.putWord8 0 *> serialize v *> serialize p
  serialize (View.Text t) = Put.putWord8 1 *> serialize t
  deserialize = do
    b <- Get.getWord8
    case b of
      0 -> -- TODO here
--}

  {--
instance Serial DFO where
  serialize ()
--}

hash :: (Var v) => Term.Term v -> Reference
hash (Term.Ref' r) = r
hash t = Reference.Derived (ABT.hash t)

-- Term v -- ABT.Term (F v) v ()
--testTerm :: (Foldable f, Serial a, Serial v, Ord v, Serial1 f) => ABT.Term f v a
--testTerm :: (Serial v, Ord v) => ABT.Term (Term.F v) v ()
testTerm :: (Serial v, Ord v) => Term.Term v
testTerm = undefined

testTerm2 :: TermV
testTerm2 = undefined

testS :: (MonadPut m, Serial s) => s -> m ()
testS = serialize

--testSerialize2 = serialize testTerm2
--testThing = serialize _

 {--
tSerialize :: Put
tSerialize = serialize testTerm
--}


