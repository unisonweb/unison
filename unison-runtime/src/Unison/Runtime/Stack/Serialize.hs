module Unison.Runtime.Stack.Serialize (putGClosure, getGClosure) where

import Data.Bytes.Get
import Data.Bytes.Put
import Unison.Runtime.Serialize
import Unison.Runtime.Stack (Closure (..), GClosure (..))

data GClosureT
  = GPApT
  | GEnumT
  | GDataU1T
  | GDataU2T
  | GDataB1T
  | GDataB2T
  | GDataUBT
  | GDataGT
  | GCapturedT
  | GForeignT
  | GBlackHoleT

instance Tag GClosureT where
  tag2word = \case
    GPApT -> 0
    GEnumT -> 1
    GDataU1T -> 2
    GDataU2T -> 3
    GDataB1T -> 4
    GDataB2T -> 5
    GDataUBT -> 6
    GDataGT -> 7
    GCapturedT -> 8
    GForeignT -> 9
    GBlackHoleT -> 10
  word2tag = \case
    0 -> pure GPApT
    1 -> pure GEnumT
    2 -> pure GDataU1T
    3 -> pure GDataU2T
    4 -> pure GDataB1T
    5 -> pure GDataB2T
    6 -> pure GDataUBT
    7 -> pure GDataGT
    8 -> pure GCapturedT
    9 -> pure GForeignT
    10 -> pure GBlackHoleT
    n -> unknownTag "GClosureT" n

putGClosure :: (MonadPut m) => (comb -> m ()) -> GClosure comb -> m ()
putGClosure putComb = \case
  GPAp comb uargs bargs ->
    putTag GPApT *> putComb comb *> putByteArray uargs *> putArray (putGClosure putComb) bargs
  GEnum r w -> putTag GEnumT *> putReference r *> putNat w
  GDataU1 r w i -> putTag GDataU1T *> putReference r *> putNat w *> putI i
  GDataU2 r w i j -> putTag GDataU2T *> putReference r *> putNat w *> putI i *> putInt j
  GDataB1 r w clos -> putTag GDataB1T *> putReference r *> putNat w *> putGClosure putComb clos
  GDataB2 r w c1 c2 -> putTag GDataB2T *> putReference r *> putNat w *> putGClosure putComb c1 *> putGClosure putComb c2
  GDataUB r w i c -> putTag GDataUBT *> putReference r *> putNat w *> putI i *> putGClosure putComb c
  GDataG r w usegs bsegs -> putTag GDataGT *> putReference r *> putNat w *> putByteArray usegs *> putArray (putGClosure putComb) bsegs
  GCaptured k i j s1 s2 -> putTag GCapturedT *> putInt k *> putInt i *> putInt j *> putGCaptured putComb s1 *> putGCaptured putComb s2
  GForeign f -> _
  GBlackHole -> _
  where
    putI = putInt . fromIntegral

getGClosure :: (MonadGet m) => m Closure
getGClosure = error "getClosure not implemented"
