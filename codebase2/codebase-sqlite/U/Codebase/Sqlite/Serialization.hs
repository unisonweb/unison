{-# LANGUAGE RecordWildCards #-}

module U.Codebase.Sqlite.Serialization
  ( decomposeBranchFormat,
    decomposeDeclFormat,
    decomposePatchFormat,
    decomposeTermFormat,
    decomposeWatchFormat,
    getBranchFormat,
    getLocalBranch,
    getDeclElement,
    getDeclElementNumConstructors,
    getDeclFormat,
    getPatchFormat,
    getLocalPatch,
    getTempCausalFormat,
    getTempDeclFormat,
    getTempNamespaceFormat,
    getTempPatchFormat,
    getTempTermFormat,
    getTermAndType,
    getTypeFromTermAndType,
    getTermFormat,
    getWatchResultFormat,
    lookupDeclElement,
    lookupDeclElementNumConstructors,
    lookupTermElement,
    lookupTermElementDiscardingTerm,
    lookupTermElementDiscardingType,
    putBranchFormat,
    putDeclFormat,
    putPatchFormat,
    putTempEntity,
    putTermFormat,
    putWatchResultFormat,
    recomposeBranchFormat,
    recomposeDeclFormat,
    recomposePatchFormat,
    recomposeTermFormat,
    recomposeWatchFormat,

    -- * Exported for Share
    putTermAndType,
    putSingleTerm,
    putDeclElement,
    getSingleTerm,
    putLocalIdsWith,
    getLocalIdsWith,
    putLocalBranch,
    putLocalPatch,
  )
where

import Data.Bits (Bits)
import Data.ByteString qualified as BS
import Data.Bytes.Get (MonadGet, getByteString, getWord8, runGetS)
import Data.Bytes.Put (MonadPut, putByteString, putWord8)
import Data.Bytes.Serial (SerialEndian (serializeBE), deserialize, deserializeBE, serialize)
import Data.Bytes.VarInt (VarInt (VarInt), unVarInt)
import Data.List (elemIndex)
import Data.Set qualified as Set
import Data.Vector (Vector)
import U.Codebase.Decl (Modifier)
import U.Codebase.Decl qualified as Decl
import U.Codebase.Kind (Kind)
import U.Codebase.Kind qualified as Kind
import U.Codebase.Reference (Reference' (ReferenceBuiltin, ReferenceDerived))
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent')
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.Branch.Diff qualified as BranchDiff
import U.Codebase.Sqlite.Branch.Format qualified as BranchFormat
import U.Codebase.Sqlite.Branch.Full qualified as BranchFull
import U.Codebase.Sqlite.Causal qualified as Causal
import U.Codebase.Sqlite.DbId (ObjectId, TextId)
import U.Codebase.Sqlite.Decl.Format qualified as DeclFormat
import U.Codebase.Sqlite.Entity qualified as Entity
import U.Codebase.Sqlite.LocalIds (LocalIds, LocalIds' (..), LocalTextId, WatchLocalIds)
import U.Codebase.Sqlite.Patch.Diff qualified as PatchDiff
import U.Codebase.Sqlite.Patch.Format qualified as PatchFormat
import U.Codebase.Sqlite.Patch.Full qualified as PatchFull
import U.Codebase.Sqlite.Patch.TermEdit qualified as TermEdit
import U.Codebase.Sqlite.Patch.TypeEdit qualified as TypeEdit
import U.Codebase.Sqlite.Symbol (Symbol (..))
import U.Codebase.Sqlite.TempEntity (TempEntity)
import U.Codebase.Sqlite.TempEntity qualified as TempEntity
import U.Codebase.Sqlite.Term.Format qualified as TermFormat
import U.Codebase.Term qualified as Term
import U.Codebase.Type qualified as Type
import U.Core.ABT qualified as ABT
import U.Util.Base32Hex qualified as Base32Hex
import U.Util.Serialization hiding (debug)
import Unison.Hash32 (Hash32)
import Unison.Hash32 qualified as Hash32
import Unison.Prelude
import Unison.Util.Monoid qualified as Monoid
import Prelude hiding (getChar, putChar)

debug :: Bool
debug = False

putABT ::
  (MonadPut m, Foldable f, Functor f, Ord v) =>
  (v -> m ()) ->
  (a -> m ()) ->
  (forall x. (x -> m ()) -> f x -> m ()) ->
  ABT.Term f v a ->
  m ()
putABT putVar putA putF abt =
  putFoldable putVar fvs *> go (annotateBound abt)
  where
    fvs = Set.toList $ ABT.freeVars abt
    go (ABT.Term _ (a, env) abt) =
      putA a *> case abt of
        ABT.Var v -> putWord8 0 *> putVarRef env v
        ABT.Tm f -> putWord8 1 *> putF go f
        ABT.Abs v body -> putWord8 2 *> putVar v *> go body
        ABT.Cycle body -> putWord8 3 *> go body
    annotateBound :: (Ord v, Functor f, Foldable f) => ABT.Term f v a -> ABT.Term f v (a, [v])
    annotateBound = go []
      where
        go env t =
          let a = (ABT.annotation t, env)
           in case ABT.out t of
                ABT.Abs v body -> ABT.abs a v (go (v : env) body)
                ABT.Cycle body -> ABT.cycle a (go env body)
                ABT.Tm f -> ABT.tm a (go env <$> f)
                ABT.Var v -> ABT.var a v
    putVarRef env v = case v `elemIndex` env of
      Just i -> putWord8 0 *> putVarInt i
      Nothing -> case v `elemIndex` fvs of
        Just i -> putWord8 1 *> putVarInt i
        Nothing -> error "impossible: var not free or bound"

getABT ::
  (MonadGet m, Foldable f, Functor f, Ord v) =>
  m v ->
  m a ->
  (forall x. m x -> m (f x)) ->
  m (ABT.Term f v a)
getABT getVar getA getF = getList getVar >>= go []
  where
    go env fvs = do
      a <- getA
      tag <- getWord8
      case tag of
        0 -> do
          tag <- getWord8
          case tag of
            0 -> ABT.var a . (env !!) <$> getVarInt
            1 -> ABT.var a . (fvs !!) <$> getVarInt
            _ -> unknownTag "getABT.Var" tag
        1 -> ABT.tm a <$> getF (go env fvs)
        2 -> do
          v <- getVar
          body <- go (v : env) fvs
          pure $ ABT.abs a v body
        3 -> ABT.cycle a <$> go env fvs
        _ -> unknownTag "getABT" tag

putLocalIds :: (MonadPut m, Integral t, Bits t, Integral d, Bits d) => LocalIds' t d -> m ()
putLocalIds = putLocalIdsWith putVarInt putVarInt

putLocalIdsWith :: (MonadPut m) => (t -> m ()) -> (d -> m ()) -> LocalIds' t d -> m ()
putLocalIdsWith putText putDefn LocalIds {textLookup, defnLookup} = do
  putFoldable putText textLookup
  putFoldable putDefn defnLookup

getLocalIds :: (MonadGet m, Num t, Bits t, Num h, Bits h) => m (LocalIds' t h)
getLocalIds = getLocalIdsWith getVarInt getVarInt

skipLocalIds :: (MonadGet m) => m ()
skipLocalIds = skipLocalIdsWith @TextId @ObjectId getVarInt getVarInt

getWatchLocalIds :: (MonadGet m) => m WatchLocalIds
getWatchLocalIds = getLocalIdsWith getVarInt getVarInt

getLocalIdsWith :: (MonadGet m) => m t -> m d -> m (LocalIds' t d)
getLocalIdsWith getText getDefn =
  LocalIds <$> getVector getText <*> getVector getDefn

skipLocalIdsWith :: forall t d m. (MonadGet m) => m t -> m d -> m ()
skipLocalIdsWith skipText skipDefn = do
  skipVector skipText
  skipVector skipDefn

putUnit :: (Applicative m) => () -> m ()
putUnit _ = pure ()

getUnit :: (Applicative m) => m ()
getUnit = pure ()

putWatchResultFormat :: (MonadPut m) => TermFormat.WatchResultFormat -> m ()
putWatchResultFormat = \case
  TermFormat.WatchResult ids t -> do
    putWord8 0
    putLocalIds ids
    putSingleTerm t

getWatchResultFormat :: (MonadGet m) => m TermFormat.WatchResultFormat
getWatchResultFormat =
  getWord8 >>= \case
    0 -> TermFormat.WatchResult <$> getWatchLocalIds <*> getSingleTerm
    other -> unknownTag "getWatchResultFormat" other

putTermFormat :: (MonadPut m) => TermFormat.TermFormat -> m ()
putTermFormat = \case
  TermFormat.Term c -> putWord8 0 *> putTermComponent c

getTermFormat :: (MonadGet m) => m TermFormat.TermFormat
getTermFormat =
  getWord8 >>= \case
    0 -> TermFormat.Term <$> getTermComponent
    other -> unknownTag "getTermFormat" other

putTermComponent ::
  (MonadPut m) =>
  TermFormat.LocallyIndexedComponent ->
  m ()
putTermComponent t | debug && trace ("putTermComponent " ++ show t) False = undefined
putTermComponent (TermFormat.LocallyIndexedComponent v) =
  putFramedArray
    ( \(localIds, term, typ) ->
        putLocalIds localIds >> putTermAndType (term, typ)
    )
    v

putTermAndType :: (MonadPut m) => (TermFormat.Term, TermFormat.Type) -> m ()
putTermAndType (term, typ) = putFramed putSingleTerm term >> putTType typ

-- | Encode a single term without its type or component.
-- Don't use this on its own unless you're encoding a watch result.
putSingleTerm :: (MonadPut m) => TermFormat.Term -> m ()
putSingleTerm _t | debug && trace "putSingleTerm" False = undefined
putSingleTerm t = putABT putSymbol putUnit putF t
  where
    putF :: (MonadPut m) => (a -> m ()) -> TermFormat.F a -> m ()
    putF putChild = \case
      Term.Int n ->
        putWord8 0 *> putInt n
      Term.Nat n ->
        putWord8 1 *> putNat n
      Term.Float n ->
        putWord8 2 *> putFloat n
      Term.Boolean b ->
        putWord8 3 *> putBoolean b
      Term.Text t ->
        putWord8 4 *> putVarInt t
      Term.Ref r ->
        putWord8 5 *> putRecursiveReference r
      Term.Constructor r cid ->
        putWord8 6 *> putReference r *> putVarInt cid
      Term.Request r cid ->
        putWord8 7 *> putReference r *> putVarInt cid
      Term.Handle h a ->
        putWord8 8 *> putChild h *> putChild a
      Term.App f arg ->
        putWord8 9 *> putChild f *> putChild arg
      Term.Ann e t ->
        putWord8 10 *> putChild e *> putTType t
      Term.List vs ->
        putWord8 11 *> putFoldable putChild vs
      Term.If cond t f ->
        putWord8 12 *> putChild cond *> putChild t *> putChild f
      Term.And x y ->
        putWord8 13 *> putChild x *> putChild y
      Term.Or x y ->
        putWord8 14 *> putChild x *> putChild y
      Term.Lam body ->
        putWord8 15 *> putChild body
      Term.LetRec bs body ->
        putWord8 16 *> putFoldable putChild bs *> putChild body
      Term.Let b body ->
        putWord8 17 *> putChild b *> putChild body
      Term.Match s cases ->
        putWord8 18 *> putChild s *> putFoldable (putMatchCase putChild) cases
      Term.Char c ->
        putWord8 19 *> putChar c
      Term.TermLink r ->
        putWord8 20 *> putReferent' putRecursiveReference putReference r
      Term.TypeLink r ->
        putWord8 21 *> putReference r
    putMatchCase :: (MonadPut m) => (a -> m ()) -> Term.MatchCase LocalTextId TermFormat.TypeRef a -> m ()
    putMatchCase putChild (Term.MatchCase pat guard body) =
      putPattern pat *> putMaybe putChild guard *> putChild body
    putPattern :: (MonadPut m) => Term.Pattern LocalTextId TermFormat.TypeRef -> m ()
    putPattern p = case p of
      Term.PUnbound -> putWord8 0
      Term.PVar -> putWord8 1
      Term.PBoolean b -> putWord8 2 *> putBoolean b
      Term.PInt n -> putWord8 3 *> putInt n
      Term.PNat n -> putWord8 4 *> putNat n
      Term.PFloat n -> putWord8 5 *> putFloat n
      Term.PConstructor r cid ps ->
        putWord8 6
          *> putReference r
          *> putVarInt cid
          *> putFoldable putPattern ps
      Term.PAs p -> putWord8 7 *> putPattern p
      Term.PEffectPure p -> putWord8 8 *> putPattern p
      Term.PEffectBind r cid args k ->
        putWord8 9
          *> putReference r
          *> putVarInt cid
          *> putFoldable putPattern args
          *> putPattern k
      Term.PSequenceLiteral ps ->
        putWord8 10 *> putFoldable putPattern ps
      Term.PSequenceOp l op r ->
        putWord8 11
          *> putPattern l
          *> putSeqOp op
          *> putPattern r
      Term.PText t -> putWord8 12 *> putVarInt t
      Term.PChar c -> putWord8 13 *> putChar c
    putSeqOp :: (MonadPut m) => Term.SeqOp -> m ()
    putSeqOp Term.PCons = putWord8 0
    putSeqOp Term.PSnoc = putWord8 1
    putSeqOp Term.PConcat = putWord8 2

getTermComponent :: (MonadGet m) => m TermFormat.LocallyIndexedComponent
getTermComponent =
  TermFormat.LocallyIndexedComponent
    <$> getFramedArray (getTuple3 getLocalIds (getFramed getSingleTerm) getTermElementType)

getTermAndType :: (MonadGet m) => m (TermFormat.Term, TermFormat.Type)
getTermAndType = (,) <$> getFramed getSingleTerm <*> getTermElementType

-- | Decode ONLY the type of a term-component element.
-- This is useful during sync and when we need the type of a term component element but don't
-- want to decode the whole term (which can be expensive).
getTypeFromTermAndType :: (MonadGet m) => m (TermFormat.Type)
getTypeFromTermAndType = skipFramed *> getTermElementType

getSingleTerm :: (MonadGet m) => m TermFormat.Term
getSingleTerm = getABT getSymbol getUnit getF
  where
    getF :: (MonadGet m) => m a -> m (TermFormat.F a)
    getF getChild =
      getWord8 >>= \case
        0 -> Term.Int <$> getInt
        1 -> Term.Nat <$> getNat
        2 -> Term.Float <$> getFloat
        3 -> Term.Boolean <$> getBoolean
        4 -> Term.Text <$> getVarInt
        5 -> Term.Ref <$> getRecursiveReference
        6 -> Term.Constructor <$> getReference <*> getVarInt
        7 -> Term.Request <$> getReference <*> getVarInt
        8 -> Term.Handle <$> getChild <*> getChild
        9 -> Term.App <$> getChild <*> getChild
        10 -> Term.Ann <$> getChild <*> getType getReference
        11 -> Term.List <$> getSequence getChild
        12 -> Term.If <$> getChild <*> getChild <*> getChild
        13 -> Term.And <$> getChild <*> getChild
        14 -> Term.Or <$> getChild <*> getChild
        15 -> Term.Lam <$> getChild
        16 -> Term.LetRec <$> getList getChild <*> getChild
        17 -> Term.Let <$> getChild <*> getChild
        18 ->
          Term.Match
            <$> getChild
            <*> getList
              (Term.MatchCase <$> getPattern <*> getMaybe getChild <*> getChild)
        19 -> Term.Char <$> getChar
        20 -> Term.TermLink <$> getReferent
        21 -> Term.TypeLink <$> getReference
        tag -> unknownTag "getSingleTerm" tag
      where
        getReferent :: (MonadGet m) => m (Referent' TermFormat.TermRef TermFormat.TypeRef)
        getReferent =
          getWord8 >>= \case
            0 -> Referent.Ref <$> getRecursiveReference
            1 -> Referent.Con <$> getReference <*> getVarInt
            x -> unknownTag "getTermComponent" x
        getPattern :: (MonadGet m) => m (Term.Pattern LocalTextId TermFormat.TypeRef)
        getPattern =
          getWord8 >>= \case
            0 -> pure Term.PUnbound
            1 -> pure Term.PVar
            2 -> Term.PBoolean <$> getBoolean
            3 -> Term.PInt <$> getInt
            4 -> Term.PNat <$> getNat
            5 -> Term.PFloat <$> getFloat
            6 -> Term.PConstructor <$> getReference <*> getVarInt <*> getList getPattern
            7 -> Term.PAs <$> getPattern
            8 -> Term.PEffectPure <$> getPattern
            9 ->
              Term.PEffectBind
                <$> getReference
                <*> getVarInt
                <*> getList getPattern
                <*> getPattern
            10 -> Term.PSequenceLiteral <$> getList getPattern
            11 ->
              Term.PSequenceOp
                <$> getPattern
                <*> getSeqOp
                <*> getPattern
            12 -> Term.PText <$> getVarInt
            13 -> Term.PChar <$> getChar
            x -> unknownTag "Pattern" x
          where
            getSeqOp :: (MonadGet m) => m Term.SeqOp
            getSeqOp =
              getWord8 >>= \case
                0 -> pure Term.PCons
                1 -> pure Term.PSnoc
                2 -> pure Term.PConcat
                tag -> unknownTag "SeqOp" tag

lookupTermElement :: (MonadGet m) => Reference.Pos -> m (LocalIds, TermFormat.Term, TermFormat.Type)
lookupTermElement i =
  getWord8 >>= \case
    0 -> unsafeFramedArrayLookup (getTuple3 getLocalIds (getFramed getSingleTerm) getTermElementType) $ fromIntegral i
    tag -> unknownTag "lookupTermElement" tag

lookupTermElementDiscardingType :: (MonadGet m) => Reference.Pos -> m (LocalIds, TermFormat.Term)
lookupTermElementDiscardingType i =
  getWord8 >>= \case
    0 -> unsafeFramedArrayLookup ((,) <$> getLocalIds <*> getFramed getSingleTerm) $ fromIntegral i
    tag -> unknownTag "lookupTermElementDiscardingType" tag

lookupTermElementDiscardingTerm :: (MonadGet m) => Reference.Pos -> m (LocalIds, TermFormat.Type)
lookupTermElementDiscardingTerm i =
  getWord8 >>= \case
    0 -> unsafeFramedArrayLookup ((,) <$> getLocalIds <* skipFramed <*> getTermElementType) $ fromIntegral i
    tag -> unknownTag "lookupTermElementDiscardingTerm" tag

-- | Decode a type which is stored alongisde a term-component element.
getTermElementType :: (MonadGet m) => m TermFormat.Type
getTermElementType = getType getReference

getType :: forall m r. (MonadGet m) => m r -> m (Type.TypeR r Symbol)
getType getReference = getABT getSymbol getUnit go
  where
    go :: m x -> m (Type.F' r x)
    go getChild =
      getWord8 >>= \case
        0 -> Type.Ref <$> getReference
        1 -> Type.Arrow <$> getChild <*> getChild
        2 -> Type.Ann <$> getChild <*> getKind
        3 -> Type.App <$> getChild <*> getChild
        4 -> Type.Effect <$> getChild <*> getChild
        5 -> Type.Effects <$> getList getChild
        6 -> Type.Forall <$> getChild
        7 -> Type.IntroOuter <$> getChild
        tag -> unknownTag "getType" tag
    getKind :: (MonadGet m) => m Kind
    getKind =
      getWord8 >>= \case
        0 -> pure Kind.Star
        1 -> Kind.Arrow <$> getKind <*> getKind
        tag -> unknownTag "getKind" tag

putDeclFormat :: (MonadPut m) => DeclFormat.DeclFormat -> m ()
putDeclFormat = \case
  DeclFormat.Decl c -> putWord8 0 *> putDeclComponent c
  where
    -- These use a framed array for randomer access
    putDeclComponent :: (MonadPut m) => DeclFormat.LocallyIndexedComponent -> m ()
    putDeclComponent t | debug && trace ("putDeclComponent " ++ show t) False = undefined
    putDeclComponent (DeclFormat.LocallyIndexedComponent v) =
      putFramedArray (putPair putLocalIds putDeclElement) v

putDeclElement :: (MonadPut m) => Decl.DeclR DeclFormat.TypeRef Symbol -> m ()
putDeclElement Decl.DataDeclaration {..} = do
  putDeclType declType
  putModifier modifier
  putFoldable putSymbol bound
  putFoldable putDType constructorTypes
  where
    putDeclType Decl.Data = putWord8 0
    putDeclType Decl.Effect = putWord8 1
    putModifier Decl.Structural = putWord8 0
    putModifier (Decl.Unique t) = putWord8 1 *> putText t

getDeclFormat :: (MonadGet m) => m DeclFormat.DeclFormat
getDeclFormat =
  getWord8 >>= \case
    0 -> DeclFormat.Decl <$> getDeclComponent
    other -> unknownTag "DeclFormat" other
  where
    getDeclComponent :: (MonadGet m) => m DeclFormat.LocallyIndexedComponent
    getDeclComponent =
      DeclFormat.LocallyIndexedComponent
        <$> getFramedArray (getPair getLocalIds getDeclElement)

getDeclElement :: (MonadGet m) => m (DeclFormat.Decl Symbol)
getDeclElement =
  Decl.DataDeclaration
    <$> getDeclType
    <*> getModifier
    <*> getList getSymbol
    <*> getList (getType getRecursiveReference)
  where
    getDeclType =
      getWord8 >>= \case
        0 -> pure Decl.Data
        1 -> pure Decl.Effect
        other -> unknownTag "DeclType" other

getModifier :: (MonadGet m) => m Modifier
getModifier =
  getWord8 >>= \case
    0 -> pure Decl.Structural
    1 -> Decl.Unique <$> getText
    other -> unknownTag "DeclModifier" other

-- | Get the number of constructors in a decl element.
getDeclElementNumConstructors :: (MonadGet m) => m Int
getDeclElementNumConstructors = do
  skipDeclType
  skipDeclModifier
  skipDeclTypeVariables
  getListLength
  where
    skipDeclType = void getWord8
    skipDeclModifier = void getModifier
    skipDeclTypeVariables = void (getList skipSymbol)

lookupDeclElement ::
  (MonadGet m) => Reference.Pos -> m (LocalIds, DeclFormat.Decl Symbol)
lookupDeclElement i =
  lookupDeclElementWith i (getPair getLocalIds getDeclElement)

lookupDeclElementNumConstructors :: (MonadGet m) => Reference.Pos -> m Int
lookupDeclElementNumConstructors i =
  lookupDeclElementWith i (skipLocalIds *> getDeclElementNumConstructors)

-- Note: the caller is responsible for either consuming the whole decl, or not
-- parsing anything after a partially-parsed decl
lookupDeclElementWith :: (MonadGet m) => Reference.Pos -> m a -> m a
lookupDeclElementWith i get =
  getWord8 >>= \case
    0 -> unsafeFramedArrayLookup get $ fromIntegral @Reference.Pos @Int i
    other -> unknownTag "lookupDeclElementWith" other

putBranchFormat :: (MonadPut m) => BranchFormat.BranchFormat -> m ()
putBranchFormat b | debug && trace ("putBranchFormat " ++ show b) False = undefined
putBranchFormat b = case b of
  BranchFormat.Full li b -> do
    putWord8 0
    putBranchLocalIds li
    putLocalBranch b
  BranchFormat.Diff r li d -> do
    putWord8 1
    putVarInt r
    putBranchLocalIds li
    putBranchDiff d
  where
    putBranchDiff (BranchDiff.Diff terms types patches children) = do
      putMap putVarInt (putMap putReferent putDiffOp) terms
      putMap putVarInt (putMap putReference putDiffOp) types
      putMap putVarInt putPatchOp patches
      putMap putVarInt putChildOp children
      where
        putAddRemove put map = do
          let (adds, removes) = BranchDiff.addsRemoves map
          putFoldable put adds
          putFoldable put removes
        putPatchOp = \case
          BranchDiff.PatchRemove -> putWord8 0
          BranchDiff.PatchAddReplace pId -> putWord8 1 *> putVarInt pId
        putDiffOp = \case
          BranchDiff.RemoveDef -> putWord8 0
          BranchDiff.AddDefWithMetadata md -> putWord8 1 *> putFoldable putReference md
          BranchDiff.AlterDefMetadata md -> putWord8 2 *> putAddRemove putReference md
        putChildOp = \case
          BranchDiff.ChildRemove -> putWord8 0
          BranchDiff.ChildAddReplace b -> putWord8 1 *> putVarInt b

putLocalBranch :: (MonadPut m) => BranchFull.LocalBranch -> m ()
putLocalBranch (BranchFull.Branch terms types patches children) = do
  putMap putVarInt (putMap putReferent putMetadataSetFormat) terms
  putMap putVarInt (putMap putReference putMetadataSetFormat) types
  putMap putVarInt putVarInt patches
  putMap putVarInt putVarInt children
  where
    putMetadataSetFormat (BranchFull.Inline s) =
      putWord8 0 *> putFoldable putReference s

putBranchLocalIds :: (MonadPut m) => BranchFormat.BranchLocalIds -> m ()
putBranchLocalIds (BranchFormat.LocalIds ts os ps cs) = do
  putFoldable putVarInt ts
  putFoldable putVarInt os
  putFoldable putVarInt ps
  putFoldable (putPair putVarInt putVarInt) cs

putPatchFormat :: (MonadPut m) => PatchFormat.PatchFormat -> m ()
putPatchFormat = \case
  PatchFormat.Full ids p -> do
    putWord8 0
    putPatchLocalIds ids
    putLocalPatch p
  PatchFormat.Diff r ids p -> do
    putWord8 1
    putVarInt r
    putPatchLocalIds ids
    putPatchDiff p

getPatchFormat :: (MonadGet m) => m PatchFormat.PatchFormat
getPatchFormat =
  getWord8 >>= \case
    0 -> PatchFormat.Full <$> getPatchLocalIds <*> getLocalPatch
    1 -> PatchFormat.Diff <$> getVarInt <*> getPatchLocalIds <*> getPatchDiff
    x -> unknownTag "getPatchFormat" x
  where
    getPatchDiff :: (MonadGet m) => m PatchDiff.LocalPatchDiff
    getPatchDiff =
      PatchDiff.PatchDiff
        <$> getMap getReferent (getSet getTermEdit)
        <*> getMap getReference (getSet getTypeEdit)
        <*> getMap getReferent (getSet getTermEdit)
        <*> getMap getReference (getSet getTypeEdit)

getLocalPatch :: (MonadGet m) => m PatchFull.LocalPatch
getLocalPatch =
  PatchFull.Patch
    <$> getMap getReferent (getSet getTermEdit)
    <*> getMap getReference (getSet getTypeEdit)

getTermEdit :: (MonadGet m) => m TermEdit.LocalTermEdit
getTermEdit =
  getWord8 >>= \case
    0 -> pure TermEdit.Deprecate
    1 -> TermEdit.Replace <$> getReferent <*> getTyping
    x -> unknownTag "getTermEdit" x

getTyping :: (MonadGet m) => m TermEdit.Typing
getTyping =
  getWord8 >>= \case
    0 -> pure TermEdit.Same
    1 -> pure TermEdit.Subtype
    2 -> pure TermEdit.Different
    x -> unknownTag "getTyping" x

getTypeEdit :: (MonadGet m) => m TypeEdit.LocalTypeEdit
getTypeEdit =
  getWord8 >>= \case
    0 -> pure TypeEdit.Deprecate
    1 -> TypeEdit.Replace <$> getReference
    x -> unknownTag "getTypeEdit" x

getPatchLocalIds :: (MonadGet m) => m PatchFormat.PatchLocalIds
getPatchLocalIds =
  PatchFormat.LocalIds
    <$> getVector getVarInt
    <*> getVector getVarInt
    <*> getVector getVarInt

putLocalPatch :: (MonadPut m) => PatchFull.LocalPatch -> m ()
putLocalPatch (PatchFull.Patch termEdits typeEdits) = do
  putMap putReferent (putFoldable putTermEdit) termEdits
  putMap putReference (putFoldable putTypeEdit) typeEdits

putPatchDiff :: (MonadPut m) => PatchDiff.LocalPatchDiff -> m ()
putPatchDiff (PatchDiff.PatchDiff atm atp rtm rtp) = do
  putMap putReferent (putFoldable putTermEdit) atm
  putMap putReference (putFoldable putTypeEdit) atp
  putMap putReferent (putFoldable putTermEdit) rtm
  putMap putReference (putFoldable putTypeEdit) rtp

putPatchLocalIds :: (MonadPut m) => PatchFormat.PatchLocalIds -> m ()
putPatchLocalIds (PatchFormat.LocalIds ts hs os) = do
  putFoldable putVarInt ts
  putFoldable putVarInt hs
  putFoldable putVarInt os

putTermEdit :: (MonadPut m) => TermEdit.LocalTermEdit -> m ()
putTermEdit TermEdit.Deprecate = putWord8 0
putTermEdit (TermEdit.Replace r t) = putWord8 1 *> putReferent r *> putTyping t
  where
    putTyping TermEdit.Same = putWord8 0
    putTyping TermEdit.Subtype = putWord8 1
    putTyping TermEdit.Different = putWord8 2

putTypeEdit :: (MonadPut m) => TypeEdit.LocalTypeEdit -> m ()
putTypeEdit TypeEdit.Deprecate = putWord8 0
putTypeEdit (TypeEdit.Replace r) = putWord8 1 *> putReference r

getBranchFormat :: (MonadGet m) => m BranchFormat.BranchFormat
getBranchFormat = getBranchFormat' getBranchFull getBranchDiff
  where
    getBranchFull = getBranchFull' getBranchLocalIds
    getBranchDiff = getBranchDiff' getBranchRef getBranchLocalIds
    getBranchRef = getVarInt

getBranchFormat' ::
  forall text defRef patchRef childRef branchRef m.
  (MonadGet m) =>
  m (BranchFormat.BranchFormat' text defRef patchRef childRef branchRef) ->
  m (BranchFormat.BranchFormat' text defRef patchRef childRef branchRef) ->
  m (BranchFormat.BranchFormat' text defRef patchRef childRef branchRef)
getBranchFormat' getBranchFull getBranchDiff =
  getWord8 >>= \case
    0 -> getBranchFull
    1 -> getBranchDiff
    x -> unknownTag "getBranchFormat" x

getBranchFull' ::
  forall text defRef patchRef childRef branchRef m.
  (MonadGet m) =>
  m (BranchFormat.BranchLocalIds' text defRef patchRef childRef) ->
  m (BranchFormat.BranchFormat' text defRef patchRef childRef branchRef)
getBranchFull' getBranchLocalIds =
  BranchFormat.Full <$> getBranchLocalIds <*> getLocalBranch

getLocalBranch :: (MonadGet m) => m BranchFull.LocalBranch
getLocalBranch =
  BranchFull.Branch
    <$> getMap getVarInt (getMap getReferent getMetadataSetFormat)
    <*> getMap getVarInt (getMap getReference getMetadataSetFormat)
    <*> getMap getVarInt getVarInt
    <*> getMap getVarInt getVarInt
  where
    getMetadataSetFormat :: (MonadGet m) => m BranchFull.LocalMetadataSet
    getMetadataSetFormat =
      getWord8 >>= \case
        0 -> BranchFull.Inline <$> getSet getReference
        x -> unknownTag "getMetadataSetFormat" x

getBranchDiff' ::
  (MonadGet m) =>
  m branchRef ->
  m (BranchFormat.BranchLocalIds' text defRef patchRef childRef) ->
  m (BranchFormat.BranchFormat' text defRef patchRef childRef branchRef)
getBranchDiff' getBranchRef getBranchLocalIds =
  BranchFormat.Diff
    <$> getBranchRef
    <*> getBranchLocalIds
    <*> getLocalBranchDiff

getLocalBranchDiff :: (MonadGet m) => m BranchDiff.LocalDiff
getLocalBranchDiff =
  BranchDiff.Diff
    <$> getMap getVarInt (getMap getReferent getDiffOp)
    <*> getMap getVarInt (getMap getReference getDiffOp)
    <*> getMap getVarInt getPatchOp
    <*> getMap getVarInt getChildOp
  where
    getDiffOp :: (MonadGet m) => m BranchDiff.LocalDefinitionOp
    getDiffOp =
      getWord8 >>= \case
        0 -> pure BranchDiff.RemoveDef
        1 -> BranchDiff.AddDefWithMetadata <$> getSet getReference
        2 -> BranchDiff.AlterDefMetadata <$> getAddRemove getReference
        x -> unknownTag "getDiffOp" x
    getAddRemove get = do
      adds <- getMap get (pure True)
      -- and removes:
      addToExistingMap get (pure False) adds
    getPatchOp :: (MonadGet m) => m BranchDiff.LocalPatchOp
    getPatchOp =
      getWord8 >>= \case
        0 -> pure BranchDiff.PatchRemove
        1 -> BranchDiff.PatchAddReplace <$> getVarInt
        x -> unknownTag "getPatchOp" x
    getChildOp :: (MonadGet m) => m BranchDiff.LocalChildOp
    getChildOp =
      getWord8 >>= \case
        0 -> pure BranchDiff.ChildRemove
        1 -> BranchDiff.ChildAddReplace <$> getVarInt
        x -> unknownTag "getChildOp" x

getBranchLocalIds :: (MonadGet m) => m BranchFormat.BranchLocalIds
getBranchLocalIds =
  BranchFormat.LocalIds
    <$> getVector getVarInt
    <*> getVector getVarInt
    <*> getVector getVarInt
    <*> getVector (getPair getVarInt getVarInt)

decomposeTermFormat :: (MonadGet m) => m TermFormat.SyncTermFormat
decomposeTermFormat =
  getWord8 >>= \case
    0 ->
      TermFormat.SyncTerm
        . TermFormat.SyncLocallyIndexedComponent
        <$> decomposeComponent
    tag -> error $ "todo: unknown term format tag " ++ show tag

decomposeDeclFormat :: (MonadGet m) => m DeclFormat.SyncDeclFormat
decomposeDeclFormat =
  getWord8 >>= \case
    0 ->
      DeclFormat.SyncDecl
        . DeclFormat.SyncLocallyIndexedComponent
        <$> decomposeComponent
    tag -> error $ "todo: unknown term format tag " ++ show tag

decomposeComponent :: (MonadGet m) => m (Vector (LocalIds, BS.ByteString))
decomposeComponent = do
  offsets <- getList (getVarInt @_ @Int)
  componentBytes <- getByteString (last offsets)
  let get1 (start, end) = do
        let bytes = BS.drop start $ BS.take end componentBytes
        either fail (pure . pure) $ runGetS split bytes
      split = (,) <$> getLocalIds <*> getRemainingByteString
  Monoid.foldMapM get1 (zip offsets (tail offsets))

recomposeTermFormat :: (MonadPut m) => TermFormat.SyncTermFormat -> m ()
recomposeTermFormat = \case
  TermFormat.SyncTerm (TermFormat.SyncLocallyIndexedComponent x) ->
    putWord8 0 >> recomposeComponent x

recomposeDeclFormat :: (MonadPut m) => DeclFormat.SyncDeclFormat -> m ()
recomposeDeclFormat = \case
  DeclFormat.SyncDecl (DeclFormat.SyncLocallyIndexedComponent x) ->
    putWord8 0 >> recomposeComponent x

recomposeComponent :: (MonadPut m) => Vector (LocalIds, BS.ByteString) -> m ()
recomposeComponent = putFramedArray \(localIds, bytes) -> do
  putLocalIds localIds
  putByteString bytes

decomposeWatchFormat :: (MonadGet m) => m TermFormat.SyncWatchResultFormat
decomposeWatchFormat =
  getWord8 >>= \case
    0 -> TermFormat.SyncWatchResult <$> getWatchLocalIds <*> getRemainingByteString
    x -> unknownTag "decomposeWatchFormat" x

recomposeWatchFormat :: (MonadPut m) => TermFormat.SyncWatchResultFormat -> m ()
recomposeWatchFormat (TermFormat.SyncWatchResult wli bs) =
  putWord8 0 *> putLocalIds wli *> putByteString bs

decomposePatchFormat :: (MonadGet m) => m PatchFormat.SyncPatchFormat
decomposePatchFormat =
  getWord8 >>= \case
    0 -> PatchFormat.SyncFull <$> getPatchLocalIds <*> getRemainingByteString
    1 -> PatchFormat.SyncDiff <$> getVarInt <*> getPatchLocalIds <*> getRemainingByteString
    x -> unknownTag "decomposePatchFormat" x

recomposePatchFormat :: (MonadPut m) => PatchFormat.SyncPatchFormat -> m ()
recomposePatchFormat = \case
  PatchFormat.SyncFull li bs ->
    putWord8 0 *> putPatchLocalIds li *> putByteString bs
  PatchFormat.SyncDiff id li bs ->
    putWord8 1 *> putVarInt id *> putPatchLocalIds li *> putByteString bs

decomposeBranchFormat :: (MonadGet m) => m BranchFormat.SyncBranchFormat
decomposeBranchFormat =
  getWord8 >>= \case
    0 -> BranchFormat.SyncFull <$> getBranchLocalIds <*> (BranchFormat.LocalBranchBytes <$> getRemainingByteString)
    1 -> BranchFormat.SyncDiff <$> getVarInt <*> getBranchLocalIds <*> (BranchFormat.LocalBranchBytes <$> getRemainingByteString)
    x -> unknownTag "decomposeBranchFormat" x

recomposeBranchFormat :: (MonadPut m) => BranchFormat.SyncBranchFormat -> m ()
recomposeBranchFormat = \case
  BranchFormat.SyncFull li (BranchFormat.LocalBranchBytes bs) ->
    putWord8 0 *> putBranchLocalIds li *> putByteString bs
  BranchFormat.SyncDiff id li (BranchFormat.LocalBranchBytes bs) ->
    putWord8 1 *> putVarInt id *> putBranchLocalIds li *> putByteString bs

putTempEntity :: (MonadPut m) => TempEntity -> m ()
putTempEntity = \case
  Entity.TC tc -> case tc of
    TermFormat.SyncTerm term ->
      putWord8 0 *> putSyncTerm term
  Entity.DC dc -> case dc of
    DeclFormat.SyncDecl decl ->
      putWord8 0 *> putSyncDecl decl
  Entity.P p -> case p of
    PatchFormat.SyncFull lids bytes ->
      putWord8 0 *> putSyncFullPatch lids bytes
    PatchFormat.SyncDiff parent lids bytes ->
      putWord8 1 *> putSyncDiffPatch parent lids bytes
  Entity.N n -> case n of
    BranchFormat.SyncFull lids (BranchFormat.LocalBranchBytes bytes) ->
      putWord8 0 *> putSyncFullNamespace lids bytes
    BranchFormat.SyncDiff parent lids (BranchFormat.LocalBranchBytes bytes) ->
      putWord8 1 *> putSyncDiffNamespace parent lids bytes
  Entity.C gdc ->
    putSyncCausal gdc
  where
    putHash32 = putText . Hash32.toText
    putPatchLocalIds PatchFormat.LocalIds {patchTextLookup, patchHashLookup, patchDefnLookup} = do
      putFoldable putText patchTextLookup
      putFoldable putHash32 patchHashLookup
      putFoldable putHash32 patchDefnLookup
    putNamespaceLocalIds BranchFormat.LocalIds {branchTextLookup, branchDefnLookup, branchPatchLookup, branchChildLookup} = do
      putFoldable putText branchTextLookup
      putFoldable putHash32 branchDefnLookup
      putFoldable putHash32 branchPatchLookup
      putFoldable (putPair putHash32 putHash32) branchChildLookup
    putSyncCausal Causal.SyncCausalFormat {valueHash, parents} = do
      putHash32 valueHash
      putFoldable putHash32 parents
    putSyncFullPatch lids bytes = do
      putPatchLocalIds lids
      putFramedByteString bytes
    putSyncDiffPatch parent lids bytes = do
      putHash32 parent
      putPatchLocalIds lids
      putFramedByteString bytes
    putSyncFullNamespace lids bytes = do
      putNamespaceLocalIds lids
      putFramedByteString bytes
    putSyncDiffNamespace parent lids bytes = do
      putHash32 parent
      putNamespaceLocalIds lids
      putFramedByteString bytes
    putSyncTerm (TermFormat.SyncLocallyIndexedComponent vec) =
      -- we're not leaving ourselves the ability to skip over the localIds
      -- when deserializing, because we don't think we need to (and it adds a
      -- little overhead.)
      flip putFoldable vec \(localIds, bytes) -> do
        putLocalIdsWith putText putHash32 localIds
        putFramedByteString bytes
    putSyncDecl (DeclFormat.SyncLocallyIndexedComponent vec) =
      flip putFoldable vec \(localIds, bytes) -> do
        putLocalIdsWith putText putHash32 localIds
        putFramedByteString bytes

getHash32 :: (MonadGet m) => m Hash32
getHash32 = Hash32.UnsafeFromBase32Hex . Base32Hex.UnsafeFromText <$> getText

getTempTermFormat :: (MonadGet m) => m TempEntity.TempTermFormat
getTempTermFormat =
  getWord8 >>= \case
    0 ->
      TermFormat.SyncTerm . TermFormat.SyncLocallyIndexedComponent
        <$> getVector
          ( getPair
              (getLocalIdsWith getText getHash32)
              getFramedByteString
          )
    tag -> unknownTag "getTempTermFormat" tag

getTempDeclFormat :: (MonadGet m) => m TempEntity.TempDeclFormat
getTempDeclFormat =
  getWord8 >>= \case
    0 ->
      DeclFormat.SyncDecl . DeclFormat.SyncLocallyIndexedComponent
        <$> getVector
          ( getPair
              (getLocalIdsWith getText getHash32)
              getFramedByteString
          )
    tag -> unknownTag "getTempDeclFormat" tag

getTempPatchFormat :: (MonadGet m) => m TempEntity.TempPatchFormat
getTempPatchFormat =
  getWord8 >>= \case
    0 -> PatchFormat.SyncFull <$> getPatchLocalIds <*> getFramedByteString
    1 -> PatchFormat.SyncDiff <$> getHash32 <*> getPatchLocalIds <*> getFramedByteString
    tag -> unknownTag "getTempPatchFormat" tag
  where
    getPatchLocalIds =
      PatchFormat.LocalIds
        <$> getVector getText
        <*> getVector getHash32
        <*> getVector getHash32

getTempNamespaceFormat :: (MonadGet m) => m TempEntity.TempNamespaceFormat
getTempNamespaceFormat =
  getWord8 >>= \case
    0 -> BranchFormat.SyncFull <$> getBranchLocalIds <*> (BranchFormat.LocalBranchBytes <$> getFramedByteString)
    1 -> BranchFormat.SyncDiff <$> getHash32 <*> getBranchLocalIds <*> (BranchFormat.LocalBranchBytes <$> getFramedByteString)
    tag -> unknownTag "getTempNamespaceFormat" tag
  where
    getBranchLocalIds =
      BranchFormat.LocalIds
        <$> getVector getText
        <*> getVector getHash32
        <*> getVector getHash32
        <*> getVector (getPair getHash32 getHash32)

getTempCausalFormat :: (MonadGet m) => m TempEntity.TempCausalFormat
getTempCausalFormat =
  Causal.SyncCausalFormat
    <$> getHash32
    <*> getVector getHash32

getSymbol :: (MonadGet m) => m Symbol
getSymbol = Symbol <$> getVarInt <*> getText

skipSymbol :: (MonadGet m) => m ()
skipSymbol = do
  _ :: Word64 <- getVarInt
  skipText

putSymbol :: (MonadPut m) => Symbol -> m ()
putSymbol (Symbol n t) = putVarInt n >> putText t

putReferent ::
  ( MonadPut m,
    Integral t1,
    Integral h1,
    Bits t1,
    Bits h1,
    Integral t2,
    Integral h2,
    Bits t2,
    Bits h2
  ) =>
  Referent' (Reference' t1 h1) (Reference' t2 h2) ->
  m ()
putReferent = putReferent' putReference putReference

putReferent' :: (MonadPut m) => (r1 -> m ()) -> (r2 -> m ()) -> Referent' r1 r2 -> m ()
putReferent' putRefRef putConRef = \case
  Referent.Ref r -> do
    putWord8 0
    putRefRef r
  Referent.Con r i -> do
    putWord8 1
    putConRef r
    putVarInt i

putReference ::
  (MonadPut m, Integral t, Bits t, Integral r, Bits r) =>
  Reference' t r ->
  m ()
putReference = \case
  ReferenceBuiltin t ->
    putWord8 0 *> putVarInt t
  ReferenceDerived (Reference.Id r index) ->
    putWord8 1 *> putVarInt r *> putVarInt index

getReferent' :: (MonadGet m) => m r1 -> m r2 -> m (Referent' r1 r2)
getReferent' getRefRef getConRef =
  getWord8 >>= \case
    0 -> Referent.Ref <$> getRefRef
    1 -> Referent.Con <$> getConRef <*> getVarInt
    x -> unknownTag "getReferent" x

getReferent ::
  ( MonadGet m,
    Integral t1,
    Integral h1,
    Bits t1,
    Bits h1,
    Integral t2,
    Integral h2,
    Bits t2,
    Bits h2
  ) =>
  m (Referent' (Reference' t1 h1) (Reference' t2 h2))
getReferent = getReferent' getReference getReference

getReference ::
  (MonadGet m, Integral t, Bits t, Integral r, Bits r) =>
  m (Reference' t r)
getReference =
  getWord8 >>= \case
    0 -> ReferenceBuiltin <$> getVarInt
    1 -> ReferenceDerived <$> (Reference.Id <$> getVarInt <*> getVarInt)
    x -> unknownTag "getRecursiveReference" x

putRecursiveReference ::
  (MonadPut m, Integral t, Bits t, Integral r, Bits r) =>
  Reference' t (Maybe r) ->
  m ()
putRecursiveReference = \case
  ReferenceBuiltin t ->
    putWord8 0 *> putVarInt t
  ReferenceDerived (Reference.Id r index) ->
    putWord8 1 *> putMaybe putVarInt r *> putVarInt index

getRecursiveReference ::
  (MonadGet m, Integral t, Bits t, Integral r, Bits r) =>
  m (Reference' t (Maybe r))
getRecursiveReference =
  getWord8 >>= \case
    0 -> ReferenceBuiltin <$> getVarInt
    1 -> ReferenceDerived <$> (Reference.Id <$> getMaybe getVarInt <*> getVarInt)
    x -> unknownTag "getRecursiveReference" x

putInt :: (MonadPut m) => Int64 -> m ()
putInt = serializeBE

getInt :: (MonadGet m) => m Int64
getInt = deserializeBE

putNat :: (MonadPut m) => Word64 -> m ()
putNat = serializeBE

getNat :: (MonadGet m) => m Word64
getNat = deserializeBE

putFloat :: (MonadPut m) => Double -> m ()
putFloat = serializeBE

getFloat :: (MonadGet m) => m Double
getFloat = deserializeBE

putBoolean :: (MonadPut m) => Bool -> m ()
putBoolean False = putWord8 0
putBoolean True = putWord8 1

getBoolean :: (MonadGet m) => m Bool
getBoolean =
  getWord8 >>= \case
    0 -> pure False
    1 -> pure True
    x -> unknownTag "Boolean" x

putTType :: (MonadPut m) => TermFormat.Type -> m ()
putTType = putType putReference putSymbol

putDType :: (MonadPut m) => DeclFormat.Type Symbol -> m ()
putDType = putType putRecursiveReference putSymbol

putType ::
  forall m r v.
  (MonadPut m, Ord v) =>
  (r -> m ()) ->
  (v -> m ()) ->
  Type.TypeR r v ->
  m ()
putType putReference putVar = putABT putVar putUnit go
  where
    go :: (x -> m ()) -> Type.F' r x -> m ()
    go putChild t = case t of
      Type.Ref r -> putWord8 0 *> putReference r
      Type.Arrow i o -> putWord8 1 *> putChild i *> putChild o
      Type.Ann t k -> putWord8 2 *> putChild t *> putKind k
      Type.App f x -> putWord8 3 *> putChild f *> putChild x
      Type.Effect e t -> putWord8 4 *> putChild e *> putChild t
      Type.Effects es -> putWord8 5 *> putFoldable putChild es
      Type.Forall body -> putWord8 6 *> putChild body
      Type.IntroOuter body -> putWord8 7 *> putChild body
    putKind :: (MonadPut m) => Kind -> m ()
    putKind k = case k of
      Kind.Star -> putWord8 0
      Kind.Arrow i o -> putWord8 1 *> putKind i *> putKind o

putChar :: (MonadPut m) => Char -> m ()
putChar = serialize . VarInt . fromEnum

getChar :: (MonadGet m) => m Char
getChar = toEnum . unVarInt <$> deserialize

putMaybe :: (MonadPut m) => (a -> m ()) -> Maybe a -> m ()
putMaybe putA = \case
  Nothing -> putWord8 0
  Just a -> putWord8 1 *> putA a

getMaybe :: (MonadGet m) => m a -> m (Maybe a)
getMaybe getA =
  getWord8 >>= \tag -> case tag of
    0 -> pure Nothing
    1 -> Just <$> getA
    _ -> unknownTag "Maybe" tag

unknownTag :: (MonadGet m, Show a) => String -> a -> m x
unknownTag msg tag =
  fail $ "unknown tag " ++ show tag ++ " while deserializing: " ++ msg
