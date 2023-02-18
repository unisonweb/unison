{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Runtime.MCode.SerializeV4
  ( putComb,
    getComb,
  )
where

import Control.Monad.State (MonadState)
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Data.Bytes.VarInt
import Data.Primitive.PrimArray
import Data.Word (Word64)
import GHC.Exts (IsList (..))
import Unison.Runtime.MCode hiding (MatchT)
import Unison.Runtime.SerializeV4
import qualified Unison.Util.Text as Util.Text

putComb :: (MonadState EncodeState m, MonadPut m) => Comb -> m ()
putComb (Lam ua ba uf bf body) =
  pInt ua *> pInt ba *> pInt uf *> pInt bf *> putSection body

getComb :: (MonadState DecodeState m, MonadGet m) => m Comb
getComb = Lam <$> gInt <*> gInt <*> gInt <*> gInt <*> getSection

data SectionT
  = AppT
  | CallT
  | JumpT
  | MatchT
  | YieldT
  | InsT
  | LetT
  | DieT
  | ExitT

instance Tag SectionT where
  tag2word AppT = 0
  tag2word CallT = 1
  tag2word JumpT = 2
  tag2word MatchT = 3
  tag2word YieldT = 4
  tag2word InsT = 5
  tag2word LetT = 6
  tag2word DieT = 7
  tag2word ExitT = 8

  word2tag 0 = pure AppT
  word2tag 1 = pure CallT
  word2tag 2 = pure JumpT
  word2tag 3 = pure MatchT
  word2tag 4 = pure YieldT
  word2tag 5 = pure InsT
  word2tag 6 = pure LetT
  word2tag 7 = pure DieT
  word2tag 8 = pure ExitT
  word2tag i = unknownTag "SectionT" i

putSection :: (MonadState EncodeState m, MonadPut m) => Section -> m ()
putSection (App b r a) =
  putTag AppT *> serialize b *> putRef r *> putArgs a
putSection (Call b w a) =
  putTag CallT *> serialize b *> pWord w *> putArgs a
putSection (Jump i a) =
  putTag JumpT *> pInt i *> putArgs a
putSection (Match i b) =
  putTag MatchT *> pInt i *> putBranch b
putSection (Yield a) =
  putTag YieldT *> putArgs a
putSection (Ins i s) =
  putTag InsT *> putInstr i *> putSection s
putSection (Let s ci) =
  putTag LetT *> putSection s *> putCombIx ci
putSection (Die s) =
  putTag DieT *> serialize s
putSection Exit =
  putTag ExitT

getSection :: (MonadState DecodeState m, MonadGet m) => m Section
getSection =
  getTag >>= \case
    AppT -> App <$> deserialize <*> getRef <*> getArgs
    CallT -> Call <$> deserialize <*> gWord <*> getArgs
    JumpT -> Jump <$> gInt <*> getArgs
    MatchT -> Match <$> gInt <*> getBranch
    YieldT -> Yield <$> getArgs
    InsT -> Ins <$> getInstr <*> getSection
    LetT -> Let <$> getSection <*> getCombIx
    DieT -> Die <$> deserialize
    ExitT -> pure Exit

data InstrT
  = UPrim1T
  | UPrim2T
  | BPrim1T
  | BPrim2T
  | ForeignCallT
  | SetDynT
  | CaptureT
  | NameT
  | InfoT
  | PackT
  | UnpackT
  | LitT
  | PrintT
  | ResetT
  | ForkT
  | AtomicallyT
  | SeqT
  | TryForceT

instance Tag InstrT where
  tag2word UPrim1T = 0
  tag2word UPrim2T = 1
  tag2word BPrim1T = 2
  tag2word BPrim2T = 3
  tag2word ForeignCallT = 4
  tag2word SetDynT = 5
  tag2word CaptureT = 6
  tag2word NameT = 7
  tag2word InfoT = 8
  tag2word PackT = 9
  tag2word UnpackT = 10
  tag2word LitT = 11
  tag2word PrintT = 12
  tag2word ResetT = 13
  tag2word ForkT = 14
  tag2word AtomicallyT = 15
  tag2word SeqT = 16
  tag2word TryForceT = 17

  word2tag 0 = pure UPrim1T
  word2tag 1 = pure UPrim2T
  word2tag 2 = pure BPrim1T
  word2tag 3 = pure BPrim2T
  word2tag 4 = pure ForeignCallT
  word2tag 5 = pure SetDynT
  word2tag 6 = pure CaptureT
  word2tag 7 = pure NameT
  word2tag 8 = pure InfoT
  word2tag 9 = pure PackT
  word2tag 10 = pure UnpackT
  word2tag 11 = pure LitT
  word2tag 12 = pure PrintT
  word2tag 13 = pure ResetT
  word2tag 14 = pure ForkT
  word2tag 15 = pure AtomicallyT
  word2tag 16 = pure SeqT
  word2tag 17 = pure TryForceT
  word2tag n = unknownTag "InstrT" n

putInstr :: (MonadState EncodeState m, MonadPut m) => Instr -> m ()
putInstr (UPrim1 up i) =
  putTag UPrim1T *> putTag up *> pInt i
putInstr (UPrim2 up i j) =
  putTag UPrim2T *> putTag up *> pInt i *> pInt j
putInstr (BPrim1 bp i) =
  putTag BPrim1T *> putTag bp *> pInt i
putInstr (BPrim2 bp i j) =
  putTag BPrim2T *> putTag bp *> pInt i *> pInt j
putInstr (ForeignCall b w a) =
  putTag ForeignCallT *> serialize b *> pWord w *> putArgs a
putInstr (SetDyn w i) =
  putTag SetDynT *> pWord w *> pInt i
putInstr (Capture w) =
  putTag CaptureT *> pWord w
putInstr (Name r a) =
  putTag NameT *> putRef r *> putArgs a
putInstr (Info s) =
  putTag InfoT *> serialize s
putInstr (Pack r w a) =
  putTag PackT *> putReference r *> pWord w *> putArgs a
putInstr (Unpack mr i) =
  putTag UnpackT *> putMaybe mr putReference *> pInt i
putInstr (Lit l) =
  putTag LitT *> putLit l
putInstr (Print i) =
  putTag PrintT *> pInt i
putInstr (Reset s) =
  putTag ResetT *> putEnumSet pWord s
putInstr (Fork i) =
  putTag ForkT *> pInt i
putInstr (Atomically i) =
  putTag AtomicallyT *> pInt i
putInstr (Seq a) =
  putTag SeqT *> putArgs a
putInstr (TryForce i) =
  putTag TryForceT *> pInt i

getInstr :: (MonadState DecodeState m, MonadGet m) => m Instr
getInstr =
  getTag >>= \case
    UPrim1T -> UPrim1 <$> getTag <*> gInt
    UPrim2T -> UPrim2 <$> getTag <*> gInt <*> gInt
    BPrim1T -> BPrim1 <$> getTag <*> gInt
    BPrim2T -> BPrim2 <$> getTag <*> gInt <*> gInt
    ForeignCallT -> ForeignCall <$> deserialize <*> gWord <*> getArgs
    SetDynT -> SetDyn <$> gWord <*> gInt
    CaptureT -> Capture <$> gWord
    NameT -> Name <$> getRef <*> getArgs
    InfoT -> Info <$> deserialize
    PackT -> Pack <$> getReference <*> gWord <*> getArgs
    UnpackT -> Unpack <$> getMaybe getReference <*> gInt
    LitT -> Lit <$> getLit
    PrintT -> Print <$> gInt
    ResetT -> Reset <$> getEnumSet gWord
    ForkT -> Fork <$> gInt
    AtomicallyT -> Atomically <$> gInt
    SeqT -> Seq <$> getArgs
    TryForceT -> TryForce <$> gInt

data ArgsT
  = ZArgsT
  | UArg1T
  | UArg2T
  | BArg1T
  | BArg2T
  | DArg2T
  | UArgRT
  | BArgRT
  | DArgRT
  | BArgNT
  | UArgNT
  | DArgNT
  | DArgVT

instance Tag ArgsT where
  tag2word ZArgsT = 0
  tag2word UArg1T = 1
  tag2word UArg2T = 2
  tag2word BArg1T = 3
  tag2word BArg2T = 4
  tag2word DArg2T = 5
  tag2word UArgRT = 6
  tag2word BArgRT = 7
  tag2word DArgRT = 8
  tag2word BArgNT = 9
  tag2word UArgNT = 10
  tag2word DArgNT = 11
  tag2word DArgVT = 12

  word2tag 0 = pure ZArgsT
  word2tag 1 = pure UArg1T
  word2tag 2 = pure UArg2T
  word2tag 3 = pure BArg1T
  word2tag 4 = pure BArg2T
  word2tag 5 = pure DArg2T
  word2tag 6 = pure UArgRT
  word2tag 7 = pure BArgRT
  word2tag 8 = pure DArgRT
  word2tag 9 = pure BArgNT
  word2tag 10 = pure UArgNT
  word2tag 11 = pure DArgNT
  word2tag 12 = pure DArgVT
  word2tag n = unknownTag "ArgsT" n

putArgs :: (MonadState EncodeState m, MonadPut m) => Args -> m ()
putArgs ZArgs = putTag ZArgsT
putArgs (UArg1 i) = putTag UArg1T *> pInt i
putArgs (UArg2 i j) = putTag UArg1T *> pInt i *> pInt j
putArgs (BArg1 i) = putTag BArg1T *> pInt i
putArgs (BArg2 i j) = putTag BArg2T *> pInt i *> pInt j
putArgs (DArg2 i j) = putTag DArg2T *> pInt i *> pInt j
putArgs (UArgR i j) = putTag UArgRT *> pInt i *> pInt j
putArgs (BArgR i j) = putTag BArgRT *> pInt i *> pInt j
putArgs (DArgR i j k l) =
  putTag DArgRT *> pInt i *> pInt j *> pInt k *> pInt l
putArgs (BArgN pa) = putTag BArgNT *> putIntArr pa
putArgs (UArgN pa) = putTag UArgNT *> putIntArr pa
putArgs (DArgN ua ba) =
  putTag DArgNT *> putIntArr ua *> putIntArr ba
putArgs (DArgV i j) = putTag DArgVT *> pInt i *> pInt j

getArgs :: (MonadState DecodeState m, MonadGet m) => m Args
getArgs =
  getTag >>= \case
    ZArgsT -> pure ZArgs
    UArg1T -> UArg1 <$> gInt
    UArg2T -> UArg2 <$> gInt <*> gInt
    BArg1T -> BArg1 <$> gInt
    BArg2T -> BArg2 <$> gInt <*> gInt
    DArg2T -> DArg2 <$> gInt <*> gInt
    UArgRT -> UArgR <$> gInt <*> gInt
    BArgRT -> BArgR <$> gInt <*> gInt
    DArgRT -> DArgR <$> gInt <*> gInt <*> gInt <*> gInt
    BArgNT -> BArgN <$> getIntArr
    UArgNT -> UArgN <$> getIntArr
    DArgNT -> DArgN <$> getIntArr <*> getIntArr
    DArgVT -> DArgV <$> gInt <*> gInt

data RefT = StkT | EnvT | DynT

instance Tag RefT where
  tag2word StkT = 0
  tag2word EnvT = 1
  tag2word DynT = 2

  word2tag 0 = pure StkT
  word2tag 1 = pure EnvT
  word2tag 2 = pure DynT
  word2tag n = unknownTag "RefT" n

putRef :: (MonadState EncodeState m, MonadPut m) => Ref -> m ()
putRef (Stk i) = putTag StkT *> pInt i
putRef (Env i j) = putTag EnvT *> pWord i *> pWord j
putRef (Dyn i) = putTag DynT *> pWord i

getRef :: (MonadState DecodeState m, MonadGet m) => m Ref
getRef =
  getTag >>= \case
    StkT -> Stk <$> gInt
    EnvT -> Env <$> gWord <*> gWord
    DynT -> Dyn <$> gWord

putCombIx :: (MonadState EncodeState m, MonadPut m) => CombIx -> m ()
putCombIx (CIx r n i) = putReference r *> pWord n *> pWord i

getCombIx :: (MonadState DecodeState m, MonadGet m) => m CombIx
getCombIx = CIx <$> getReference <*> gWord <*> gWord

data MLitT = MIT | MDT | MTT | MMT | MYT

instance Tag MLitT where
  tag2word MIT = 0
  tag2word MDT = 1
  tag2word MTT = 2
  tag2word MMT = 3
  tag2word MYT = 4

  word2tag 0 = pure MIT
  word2tag 1 = pure MDT
  word2tag 2 = pure MTT
  word2tag 3 = pure MMT
  word2tag 4 = pure MYT
  word2tag n = unknownTag "MLitT" n

putLit :: (MonadState EncodeState m, MonadPut m) => MLit -> m ()
putLit (MI i) = putTag MIT *> pInt i
putLit (MD d) = putTag MDT *> putFloat d
putLit (MT t) = putTag MTT *> putText (Util.Text.toText t)
putLit (MM r) = putTag MMT *> putReferent r
putLit (MY r) = putTag MYT *> putReference r

getLit :: (MonadState DecodeState m, MonadGet m) => m MLit
getLit =
  getTag >>= \case
    MIT -> MI <$> gInt
    MDT -> MD <$> getFloat
    MTT -> MT . Util.Text.fromText <$> getText
    MMT -> MM <$> getReferent
    MYT -> MY <$> getReference

data BranchT = Test1T | Test2T | TestWT | TestTT

instance Tag BranchT where
  tag2word Test1T = 0
  tag2word Test2T = 1
  tag2word TestWT = 2
  tag2word TestTT = 3

  word2tag 0 = pure Test1T
  word2tag 1 = pure Test2T
  word2tag 2 = pure TestWT
  word2tag 3 = pure TestTT
  word2tag n = unknownTag "BranchT" n

putBranch :: (MonadState EncodeState m, MonadPut m) => Branch -> m ()
putBranch (Test1 w s d) =
  putTag Test1T *> pWord w *> putSection s *> putSection d
putBranch (Test2 a sa b sb d) =
  putTag Test2T
    *> pWord a
    *> putSection sa
    *> pWord b
    *> putSection sb
    *> putSection d
putBranch (TestW d m) =
  putTag TestWT *> putSection d *> putEnumMap pWord putSection m
putBranch (TestT d m) =
  putTag TestTT *> putSection d *> putMap (putText . Util.Text.toText) putSection m

getBranch :: (MonadState DecodeState m, MonadGet m) => m Branch
getBranch =
  getTag >>= \case
    Test1T -> Test1 <$> gWord <*> getSection <*> getSection
    Test2T ->
      Test2
        <$> gWord
        <*> getSection
        <*> gWord
        <*> getSection
        <*> getSection
    TestWT -> TestW <$> getSection <*> getEnumMap gWord getSection
    TestTT -> TestT <$> getSection <*> getMap (Util.Text.fromText <$> getText) getSection

gInt :: (MonadState DecodeState m, MonadGet m) => m Int
gInt = unVarInt <$> deserialize

pInt :: (MonadState EncodeState m, MonadPut m) => Int -> m ()
pInt i = serialize (VarInt i)

gWord :: (MonadState DecodeState m, MonadGet m) => m Word64
gWord = unVarInt <$> deserialize

pWord :: (MonadState EncodeState m, MonadPut m) => Word64 -> m ()
pWord w = serialize (VarInt w)

putIntArr :: (MonadState EncodeState m, MonadPut m) => PrimArray Int -> m ()
putIntArr pa = putFoldable pInt $ toList pa

getIntArr :: (MonadState DecodeState m, MonadGet m) => m (PrimArray Int)
getIntArr = fromList <$> getList gInt
