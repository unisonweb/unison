{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Runtime.MCode.Serialize
  ( putComb,
    getComb,
    putCombIx,
    getCombIx,
  )
where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BU
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Exts (IsList (..))
import Unison.Runtime.ANF (PackedTag (..), RecordRef (..))
import Unison.Runtime.Array (PrimArray)
import Unison.Runtime.Foreign.Function.Type (ForeignFunc)
import Unison.Runtime.MCode hiding (MatchT)
import Unison.Runtime.Serialize hiding (getFieldTag, putFieldTag)
import Unison.Runtime.Serialize.Get
import Unison.Runtime.TypeTags (FieldTag (..))
import Unison.Util.Text qualified as Util.Text
import Prelude hiding (getChar, putChar)

data CombT = LamT | CachedClosureT

instance Tag CombT where
  tag2word LamT = 0
  tag2word CachedClosureT = 1

  word2tag 0 = pure LamT
  word2tag 1 = pure CachedClosureT
  word2tag n = unknownTag "CombT" n

putPackedTag :: PackedTag -> Builder
putPackedTag (PackedTag w) = pWord w

getPackedTag :: (PrimBase m) => Get m PackedTag
getPackedTag = PackedTag <$> gWord

putComb :: (clos -> Builder) -> GComb clos comb -> Builder
putComb pClos = \case
  (Lam a f body) ->
    putTag LamT <> pInt a <> pInt f <> putSection body
  (CachedVal w v) ->
    putTag CachedClosureT <> putNat w <> pClos v

getComb :: (PrimBase m) => Get m (GComb Void CombIx)
getComb =
  getTag >>= \case
    LamT ->
      Lam <$> gInt <*> gInt <*> getSection
    CachedClosureT -> error "getComb: Unexpected serialized Cached Closure"

getMForeignFunc :: (PrimBase m) => Get m ForeignFunc
getMForeignFunc = do
  toEnum <$> gInt

putMForeignFunc :: ForeignFunc -> Builder
putMForeignFunc = pInt . fromEnum

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
  | DMatchT
  | NMatchT
  | RMatchT

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
  tag2word DMatchT = 9
  tag2word NMatchT = 10
  tag2word RMatchT = 11

  word2tag 0 = pure AppT
  word2tag 1 = pure CallT
  word2tag 2 = pure JumpT
  word2tag 3 = pure MatchT
  word2tag 4 = pure YieldT
  word2tag 5 = pure InsT
  word2tag 6 = pure LetT
  word2tag 7 = pure DieT
  word2tag 8 = pure ExitT
  word2tag 9 = pure DMatchT
  word2tag 10 = pure NMatchT
  word2tag 11 = pure RMatchT
  word2tag i = unknownTag "SectionT" i

putSection :: GSection cix -> Builder
putSection = \case
  App b r a -> putTag AppT <> putBool b <> putRef r <> putArgs a
  Call b cix _comb a -> putTag CallT <> putBool b <> putCombIx cix <> putArgs a
  Jump i a -> putTag JumpT <> pInt i <> putArgs a
  Match i b -> putTag MatchT <> pInt i <> putBranch b
  Yield a -> putTag YieldT <> putArgs a
  Ins i s -> putTag InsT <> putInstr i <> putSection s
  Let s ci f bd ->
    putTag LetT
      <> putSection s
      <> putCombIx ci
      <> pInt f
      <> putSection bd
  Die s -> putTag DieT <> putString s
  Exit -> putTag ExitT
  DMatch mr i b -> putTag DMatchT <> putMaybe mr putReference <> pInt i <> putBranch b
  NMatch mr i b -> putTag NMatchT <> putMaybe mr putReference <> pInt i <> putBranch b
  RMatch i pu bs ->
    putTag RMatchT
      <> pInt i
      <> putSection pu
      <> putEnumMap pWord putBranch bs

getSection :: (PrimBase m) => Get m Section
getSection =
  getTag >>= \case
    AppT -> App <$> getBool <*> getRef <*> getArgs
    CallT -> do
      skipCheck <- getBool
      cix <- getCombIx
      args <- getArgs
      pure $ Call skipCheck cix cix args
    JumpT -> Jump <$> gInt <*> getArgs
    MatchT -> Match <$> gInt <*> getBranch
    YieldT -> Yield <$> getArgs
    InsT -> Ins <$> getInstr <*> getSection
    LetT ->
      Let <$> getSection <*> getCombIx <*> gInt <*> getSection
    DieT -> Die <$> getString
    ExitT -> pure Exit
    DMatchT -> DMatch <$> getMaybe getReference <*> gInt <*> getBranch
    NMatchT -> NMatch <$> getMaybe getReference <*> gInt <*> getBranch
    RMatchT ->
      RMatch <$> gInt <*> getSection <*> getEnumMap gWord getBranch

data InstrT
  = Prim1T
  | Prim2T
  | ForeignCallT
  | SetAffT
  | CaptureT
  | NameT
  | InfoT
  | PackT
  | LitT
  | PrintT
  | ResetT
  | ForkT
  | AtomicallyT
  | SeqT
  | TryForceT
  | RefCAST
  | SandboxingFailureT
  | DiscardT
  | InLocalT
  | KeepAliveT
  | RecPackT

instance Tag InstrT where
  tag2word Prim1T = 0
  tag2word Prim2T = 1
  tag2word ForeignCallT = 4
  tag2word SetAffT = 5
  tag2word CaptureT = 6
  tag2word NameT = 7
  tag2word InfoT = 8
  tag2word PackT = 9
  tag2word LitT = 10
  tag2word PrintT = 11
  tag2word ResetT = 12
  tag2word ForkT = 13
  tag2word AtomicallyT = 14
  tag2word SeqT = 15
  tag2word TryForceT = 16
  tag2word RefCAST = 17
  tag2word SandboxingFailureT = 18
  tag2word DiscardT = 19
  tag2word InLocalT = 20
  tag2word KeepAliveT = 21
  tag2word RecPackT = 22

  word2tag 0 = pure Prim1T
  word2tag 1 = pure Prim2T
  word2tag 4 = pure ForeignCallT
  word2tag 5 = pure SetAffT
  word2tag 6 = pure CaptureT
  word2tag 7 = pure NameT
  word2tag 8 = pure InfoT
  word2tag 9 = pure PackT
  word2tag 10 = pure LitT
  word2tag 11 = pure PrintT
  word2tag 12 = pure ResetT
  word2tag 13 = pure ForkT
  word2tag 14 = pure AtomicallyT
  word2tag 15 = pure SeqT
  word2tag 16 = pure TryForceT
  word2tag 17 = pure RefCAST
  word2tag 18 = pure SandboxingFailureT
  word2tag 19 = pure DiscardT
  word2tag 20 = pure InLocalT
  word2tag 21 = pure KeepAliveT
  word2tag 22 = pure RecPackT
  word2tag n = unknownTag "InstrT" n

putInstr :: GInstr cix -> Builder
putInstr = \case
  (Prim1 up i) -> putTag Prim1T <> putTag up <> pInt i
  (Prim2 up i j) -> putTag Prim2T <> putTag up <> pInt i <> pInt j
  (RefCAS i j k) -> putTag RefCAST <> pInt i <> pInt j <> pInt k
  (ForeignCall b ff a) -> putTag ForeignCallT <> putBool b <> putMForeignFunc ff <> putArgs a
  (SetAff u i j) -> putTag SetAffT <> pBool u <> pInt i <> pInt j
  (Capture w) -> putTag CaptureT <> pWord w
  (Discard i) -> putTag DiscardT <> pInt i
  (Name r a) -> putTag NameT <> putRef r <> putArgs a
  (Info s) -> putTag InfoT <> putString s
  (Pack r w a) -> putTag PackT <> putReference r <> putPackedTag w <> putArgs a
  (RecPack rr args) -> putTag RecPackT <> putRecordRef rr <> putArgs args
  (Lit l) -> putTag LitT <> putLit l
  (Print i) -> putTag PrintT <> pInt i
  (Reset s nh ah) ->
    putTag ResetT
      <> putEnumSet pWord s
      <> pInt nh
      <> putMaybe ah pInt
  (Fork i) -> putTag ForkT <> pInt i
  (Atomically i) -> putTag AtomicallyT <> pInt i
  (Seq a) -> putTag SeqT <> putArgs a
  (TryForce i) -> putTag TryForceT <> pInt i
  (InLocal i) -> putTag InLocalT <> pInt i
  (KeepAlive i) -> putTag KeepAliveT <> pInt i
  (SandboxingFailure {}) ->
    -- Sandboxing failures should only exist in code we're actively running, it shouldn't be serialized.
    error "putInstr: Unexpected serialized Sandboxing Failure"
  DLLCall ->
    -- same for DLL calls; those happen exclusively at runtime
    error "putInstr: Unexpected serialized DLLCall"

_putFieldTag :: FieldTag -> Builder
_putFieldTag (FieldTag name) = putText name

_getFieldTag :: (PrimBase m) => Get m FieldTag
_getFieldTag = FieldTag <$> getText

getInstr :: (PrimBase m) => Get m Instr
getInstr =
  getTag >>= \case
    Prim1T -> Prim1 <$> getTag <*> gInt
    Prim2T -> Prim2 <$> getTag <*> gInt <*> gInt
    RefCAST -> RefCAS <$> gInt <*> gInt <*> gInt
    ForeignCallT -> ForeignCall <$> getBool <*> getMForeignFunc <*> getArgs
    SetAffT -> SetAff <$> gBool <*> gInt <*> gInt
    CaptureT -> Capture <$> gWord
    DiscardT -> Discard <$> gInt
    NameT -> Name <$> getRef <*> getArgs
    InfoT -> Info <$> getString
    PackT -> Pack <$> getReference <*> getPackedTag <*> getArgs
    LitT -> Lit <$> getLit
    PrintT -> Print <$> gInt
    ResetT -> Reset <$> getEnumSet gWord <*> gInt <*> getMaybe gInt
    ForkT -> Fork <$> gInt
    AtomicallyT -> Atomically <$> gInt
    SeqT -> Seq <$> getArgs
    TryForceT -> TryForce <$> gInt
    InLocalT -> InLocal <$> gInt
    KeepAliveT -> KeepAlive <$> gInt
    SandboxingFailureT -> error "getInstr: Unexpected serialized Sandboxing Failure"
    RecPackT -> RecPack <$> getRecordRef <*> getArgs

data ArgsT
  = ZArgsT
  | Arg1T
  | Arg2T
  | ArgRT
  | ArgNT
  | ArgVT

instance Tag ArgsT where
  tag2word ZArgsT = 0
  tag2word Arg1T = 1
  tag2word Arg2T = 2
  tag2word ArgRT = 3
  tag2word ArgNT = 4
  tag2word ArgVT = 5

  word2tag 0 = pure ZArgsT
  word2tag 1 = pure Arg1T
  word2tag 2 = pure Arg2T
  word2tag 3 = pure ArgRT
  word2tag 4 = pure ArgNT
  word2tag 5 = pure ArgVT
  word2tag n = unknownTag "ArgsT" n

putArgs :: Args -> Builder
putArgs ZArgs = putTag ZArgsT
putArgs (VArg1 i) = putTag Arg1T <> pInt i
putArgs (VArg2 i j) = putTag Arg2T <> pInt i <> pInt j
putArgs (VArgR i j) = putTag ArgRT <> pInt i <> pInt j
putArgs (VArgN pa) = putTag ArgNT <> putIntArr pa
putArgs (VArgV i) = putTag ArgVT <> pInt i

getArgs :: (PrimBase m) => Get m Args
getArgs =
  getTag >>= \case
    ZArgsT -> pure ZArgs
    Arg1T -> VArg1 <$> gInt
    Arg2T -> VArg2 <$> gInt <*> gInt
    ArgRT -> VArgR <$> gInt <*> gInt
    ArgNT -> VArgN <$> getIntArr
    ArgVT -> VArgV <$> gInt

-- getRecordRef :: (PrimBase m) => Get m RecordRef
-- getRecordRef = RecordRef <$> getWord64be

-- putRecordRef :: RecordRef -> Builder
-- putRecordRef (RecordRef r) = BU.word64BE r

data RefT = StkT | EnvT | DynT

instance Tag RefT where
  tag2word StkT = 0
  tag2word EnvT = 1
  tag2word DynT = 2

  word2tag 0 = pure StkT
  word2tag 1 = pure EnvT
  word2tag 2 = pure DynT
  word2tag n = unknownTag "RefT" n

putRef :: GRef cix -> Builder
putRef (Stk i) = putTag StkT <> pInt i
putRef (Env cix _) = putTag EnvT <> putCombIx cix
putRef (Dyn i) = putTag DynT <> pWord i

getRef :: (PrimBase m) => Get m Ref
getRef =
  getTag >>= \case
    StkT -> Stk <$> gInt
    EnvT -> do
      cix <- getCombIx
      pure $ Env cix cix
    DynT -> Dyn <$> gWord

putCombIx :: CombIx -> Builder
putCombIx (CIx r n i) = putReference r <> pWord n <> pWord i

getCombIx :: (PrimBase m) => Get m CombIx
getCombIx = CIx <$> getReference <*> gWord <*> gWord

data MLitT = MIT | MNT | MCT | MDT | MTT | MMT | MYT

instance Tag MLitT where
  tag2word MIT = 0
  tag2word MNT = 1
  tag2word MCT = 2
  tag2word MDT = 3
  tag2word MTT = 4
  tag2word MMT = 5
  tag2word MYT = 6

  word2tag 0 = pure MIT
  word2tag 1 = pure MNT
  word2tag 2 = pure MCT
  word2tag 3 = pure MDT
  word2tag 4 = pure MTT
  word2tag 5 = pure MMT
  word2tag 6 = pure MYT
  word2tag n = unknownTag "MLitT" n

putLit :: MLit -> Builder
putLit (MI i) = putTag MIT <> pInt i
putLit (MN n) = putTag MNT <> pWord n
putLit (MC c) = putTag MCT <> putChar c
putLit (MD d) = putTag MDT <> putFloat d
putLit (MT t) = putTag MTT <> putText (Util.Text.toText t)
putLit (MM r) = putTag MMT <> putReferent r
putLit (MY r) = putTag MYT <> putReference r

getLit :: (PrimBase m) => Get m MLit
getLit =
  getTag >>= \case
    MIT -> MI <$> gInt
    MNT -> MN <$> gWord
    MCT -> MC <$> getChar
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

putBranch :: GBranch cix -> Builder
putBranch (Test1 w s d) =
  putTag Test1T <> pWord w <> putSection s <> putSection d
putBranch (Test2 a sa b sb d) =
  putTag Test2T
    <> pWord a
    <> putSection sa
    <> pWord b
    <> putSection sb
    <> putSection d
putBranch (TestW d m) =
  putTag TestWT <> putSection d <> putEnumMap pWord putSection m
putBranch (TestT d m) =
  putTag TestTT <> putSection d <> putMap (putText . Util.Text.toText) putSection m

getBranch :: (PrimBase m) => Get m Branch
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

gInt :: (PrimBase m) => Get m Int
gInt = getVarInt

pInt :: Int -> Builder
pInt i = putVarInt i

gBool :: (PrimBase m) => Get m Bool
gBool =
  getWord8 >>= \case
    0 -> pure False
    1 -> pure True
    n -> fail $ "bad byte `" ++ show n ++ "` while deserializing Bool"

pBool :: Bool -> Builder
pBool False = BU.word8 0
pBool True = BU.word8 1

gWord :: (PrimBase m) => Get m Word64
gWord = getVarInt

pWord :: Word64 -> Builder
pWord w = putVarInt w

putIntArr :: PrimArray Int -> Builder
putIntArr pa = putFoldable pInt $ toList pa

getIntArr :: (PrimBase m) => Get m (PrimArray Int)
getIntArr = fromList <$> getList gInt
