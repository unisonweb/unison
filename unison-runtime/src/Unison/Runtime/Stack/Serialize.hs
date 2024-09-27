module Unison.Runtime.Stack.Serialize (putClosure, getClosure) where

import Data.Bytes.Get
import Data.Bytes.Put
import Unison.Runtime.Foreign (Foreign (..))
import Unison.Runtime.MCode.Serialize (putCombIx)
import Unison.Runtime.Serialize
import Unison.Runtime.Stack (Closure (..), GClosure (..), K (..))

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

putClosure :: (MonadPut m) => Closure -> m ()
putClosure (Closure gclos) = case gclos of
  GPAp cix _comb uargs bargs ->
    putTag GPApT *> putCombIx cix *> putByteArray uargs *> putArray putClosure bargs
  GEnum r w -> putTag GEnumT *> putReference r *> putNat w
  GDataU1 r w i -> putTag GDataU1T *> putReference r *> putNat w *> putI i
  GDataU2 r w i j -> putTag GDataU2T *> putReference r *> putNat w *> putI i *> putI j
  GDataB1 r w clos -> putTag GDataB1T *> putReference r *> putNat w *> putClosure (Closure clos)
  GDataB2 r w c1 c2 -> putTag GDataB2T *> putReference r *> putNat w *> putClosure (Closure c1) *> putClosure (Closure c2)
  GDataUB r w i c -> putTag GDataUBT *> putReference r *> putNat w *> putI i *> putClosure (Closure c)
  GDataG r w usegs bsegs -> putTag GDataGT *> putReference r *> putNat w *> putByteArray usegs *> putArray putClosure bsegs
  GCaptured k i j s1 s2 -> putTag GCapturedT *> putK k *> putI i *> putI j *> putByteArray s1 *> putArray putClosure s2
  GForeign (Wrap ref _) -> error $ "putClosure: Cannot serialize foreign, ref: " <> show ref
  GBlackHole -> putTag GBlackHoleT
  where
    putI = putInt . fromIntegral

getClosure :: (MonadGet m) => m Closure
getClosure = error "getClosure not implemented"

data KTag
  = KET
  | CBT
  | MarkT
  | PushT

instance Tag KTag where
  tag2word = \case
    KET -> 0
    CBT -> 1
    MarkT -> 2
    PushT -> 3
  word2tag = \case
    0 -> pure KET
    1 -> pure CBT
    2 -> pure MarkT
    3 -> pure PushT
    n -> unknownTag "KTag" n

putK :: (MonadPut m) => K -> m ()
putK = \case
  KE {} -> putTag KET
  CB {} -> error "putK: Cannot serialize Callback"
  Mark puarg pbarg ws cs k ->
    putTag MarkT
      *> putI puarg
      *> putI pbarg
      *> putEnumSet putNat ws
      *> putEnumMap putNat putClosure cs
      *> putK k
  Push ufsz bfsz puarg pbarg cix _comb k ->
    putTag PushT
      *> putI ufsz
      *> putI bfsz
      *> putI puarg
      *> putI pbarg
      *> putCombIx cix
      *> putK k
  where
    putI = putInt . fromIntegral
