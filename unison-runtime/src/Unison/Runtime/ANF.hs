{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.ANF
  ( pattern TVar,
    pattern TLit,
    pattern TBLit,
    pattern TApp,
    pattern TApv,
    pattern TCom,
    pattern TCon,
    pattern UFalse,
    pattern UTrue,
    pattern TKon,
    pattern TReq,
    pattern TPrm,
    pattern TFOp,
    pattern THnd,
    pattern TLet,
    pattern TLetD,
    pattern TFrc,
    pattern TLets,
    pattern TName,
    pattern TBind,
    pattern TBinds,
    pattern TShift,
    pattern TMatch,
    pattern TDiscard,
    pattern TLocal,
    pattern TUpdate,
    pattern TBox,
    Mem (..),
    Lit (..),
    Cacheability (..),
    Direction (..),
    SuperNormal (..),
    arity,
    SuperGroup (..),
    arities,
    entryArity,
    POp (..),
    litRef,
    ANormalF (.., AApv, ACom, ACon, AKon, AReq, APrm, AFOp),
    ANormal,
    RTag,
    CTag,
    PackedTag (..),
    Tag (..),
    RecordRef (..),
    RecordSchema (..),
    FieldName,
    GroupRef (..),
    Code (..),
    ValList,
    Value (..),
    Cont (..),
    BLit (..),
    packTags,
    unpackTags,
    maskTags,
    ANFM,
    Branched (.., MatchDataCover),
    Func (..),
    SGEqv (..),
    equivocate,
    codeGroup,
    valueTermLinks,
    collectValueLinks,
    valueLinks,
    groupTermLinks,
    replaceConstructors,
    replaceFunctions,
    foldGroup,
    foldGroupLinks,
    groupRecordSchemas,
    overGroup,
    overGroupLinks,
    traverseGroup,
    traverseGroupLinks,
    normalLinks,
    prettyGroup,
    prettySuperNormal,
    prettyANF,

    -- * For the translation from terms (Unison.Runtime.ANF.Convert)
    ANFD,
    DNormal,
    Ctx,
    Cte,
    CTE (..),
    Directed,
    BranchAccum (..),
    SeqEnd (..),
    pattern Lit',
    pattern ST1,
    pattern TRec,
    pattern TRecSet,
    cteVars,
    directed,
  )
where

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (State)
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Compose (Compose (..))
import Data.List hiding (and, or, unsnoc, unzip)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as Data.Text
import Data.Text qualified as Text
import Numeric.Natural (Natural)
import Unison.ABT qualified as ABT
import Unison.ABT.Normalized qualified as ABTN
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.Prelude
import Unison.Reference (Reference, toShortHash)
import Unison.ReferentPrime qualified as Rfn
import Unison.Runtime.ANF.POp (POp (..))
import Unison.Runtime.Array qualified as PA
import Unison.Runtime.Foreign.Function.Type (ForeignFunc (..))
import Unison.Runtime.InternalError (internalBug)
import Unison.Runtime.Referenced (Referential (..))
import Unison.Runtime.TypeTags (CTag (..), PackedTag (..), RTag (..), Tag (..), maskTags, packTags, unpackTags)
import Unison.ShortHash (shortenTo)
import Unison.ShortHash qualified as SH
import Unison.Symbol (Symbol)
import Unison.Term hiding (Char, Float, List, Record, Ref, Text, arity, float, fresh, record, resolve)
import Unison.Type qualified as Ty
import Unison.Util.Bytes (Bytes)
import Unison.Util.EnumContainers as EC
import Unison.Util.Text qualified as Util.Text
import Unison.Var (Var)
import Unison.Var qualified as Var
import Prelude hiding (abs, and, or, seq, unzip)

replaceConstructors ::
  (Ord ref, Var v) =>
  Map ref (Map CTag ForeignFunc) ->
  SuperGroup ref v ->
  SuperGroup ref v
replaceConstructors reps (Rec bs entry) =
  Rec (fmap go0 <$> bs) (go0 entry)
  where
    go0 (Lambda ccs body) = Lambda ccs $ ABTN.visitPure f body

    f (TApp (FCon r c) as) = do
      cs <- Map.lookup r reps
      ff <- Map.lookup c cs
      pure $ TApp (FPrim (Right ff)) as
    f _ = Nothing

replaceFunctions ::
  (Ord ref, Var v) =>
  Map ref ref ->
  SuperGroup ref v ->
  SuperGroup ref v
replaceFunctions reps (Rec bs entry) =
  Rec (fmap go0 <$> bs) (go0 entry)
  where
    go0 (Lambda ccs body) = Lambda ccs $ ABTN.visitPure f body

    f (TApp (FComb r) as) =
      Map.lookup r reps <&> \r -> TApp (FComb r) as
    f _ = Nothing

-- | How a let's binder, a parameter or a match arm's field is held:
-- unboxed, boxed, or wide. A wide binder (Unison.Runtime.Wide) has
-- 'wideWidth' slots, its boxed value on top; only a let of one variable
-- has one, marked by Unison.Runtime.MCode.Wide in a batch's code after it
-- is loaded, so nothing stored has it (the serializers refuse it).
data Mem = UN | BX | WD deriving (Eq, Ord, Show, Enum)

-- Context entries with evaluation strategy
data CTE v s
  = ST (Direction Word16) [v] [Mem] s
  | LZ v (Either Reference v) [v]
  deriving (Show)

pattern ST1 :: Direction Word16 -> v -> Mem -> s -> CTE v s
pattern ST1 d v m s = ST d [v] [m] s

-- All variables, both bound and free occurring in a CTE. This is
-- useful for avoiding both free and bound variables when
-- freshening.
cteVars :: (Ord v) => Cte v -> Set v
cteVars (ST _ vs _ e) = Set.fromList vs `Set.union` ABTN.freeVars e
cteVars (LZ v r as) = Set.fromList (either (const id) (:) r $ v : as)

data ANormalF ref v e
  = ALet (Direction Word16) [Mem] e e
  | AName (Either ref v) [v] e
  | ALit (Lit ref)
  | ABLit (Lit ref) -- direct boxed literal
  | AMatch v (Branched ref e)
  | AShift ref e
  | AHnd [ref] v (Maybe v) e
  | AApp (Func ref v) [v]
  | AFrc v
  | AVar v
  | -- Affine handler support
    ADiscard v
  | ALocal v e
  | -- Boolean indicates whether there are indirect calls afterward
    AUpdate Bool v v
  | -- | Box a wide variable in place (Unison.Runtime.Wide): if its value
    -- slot holds an unboxed constructor, the heap object built from its
    -- slots replaces it, so from here on the variable is an ordinary
    -- value. Binds nothing, like a discard. Placed by
    -- Unison.Runtime.MCode.Wide before the first escape of the variable
    -- on each path; only in loaded code, never stored.
    ABox v
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Bifunctor (ANormalF ref) where
  bimap f _ (AVar v) = AVar (f v)
  bimap _ _ (ALit l) = ALit l
  bimap _ _ (ABLit l) = ABLit l
  bimap _ g (ALet d m bn bo) = ALet d m (g bn) (g bo)
  bimap f g (AName n as bo) = AName (f <$> n) (f <$> as) $ g bo
  bimap f g (AMatch v br) = AMatch (f v) $ fmap g br
  bimap f g (AHnd rs nh ah e) = AHnd rs (f nh) (fmap f ah) $ g e
  bimap _ g (AShift i e) = AShift i $ g e
  bimap f _ (AFrc v) = AFrc (f v)
  bimap f _ (AApp fu args) = AApp (fmap f fu) $ fmap f args
  bimap f _ (ADiscard v) = ADiscard (f v)
  bimap f g (ALocal v bo) = ALocal (f v) (g bo)
  bimap f _ (AUpdate b r v) = AUpdate b (f r) (f v)
  bimap f _ (ABox v) = ABox (f v)

instance Bifoldable (ANormalF ref) where
  bifoldMap f _ (AVar v) = f v
  bifoldMap _ _ (ALit _) = mempty
  bifoldMap _ _ (ABLit _) = mempty
  bifoldMap _ g (ALet _ _ b e) = g b <> g e
  bifoldMap f g (AName n as e) = foldMap f n <> foldMap f as <> g e
  bifoldMap f g (AMatch v br) = f v <> foldMap g br
  bifoldMap f g (AHnd _ nh ah e) = f nh <> foldMap f ah <> g e
  bifoldMap _ g (AShift _ e) = g e
  bifoldMap f _ (AFrc v) = f v
  bifoldMap f _ (AApp func args) = foldMap f func <> foldMap f args
  bifoldMap f _ (ADiscard v) = f v
  bifoldMap f g (ALocal v bo) = f v <> g bo
  bifoldMap f _ (AUpdate _ r v) = f r <> f v
  bifoldMap f _ (ABox v) = f v

instance (Ord ref) => ABTN.Align (ANormalF ref) where
  align f _ (AVar u) (AVar v) = Just $ AVar <$> f u v
  align _ _ (ALit l) (ALit r)
    | l == r = Just $ pure (ALit l)
  align _ _ (ABLit l) (ABLit r)
    | l == r = Just $ pure (ABLit l)
  align _ g (ALet dl ccl bl el) (ALet dr ccr br er)
    | dl == dr,
      ccl == ccr =
        Just $ ALet dl ccl <$> g bl br <*> g el er
  align f g (AName hl asl el) (AName hr asr er)
    | length asl == length asr,
      Just hs <- alignEither f hl hr =
        Just $
          AName
            <$> hs
            <*> traverse (uncurry f) (zip asl asr)
            <*> g el er
  align f g (AMatch vl bsl) (AMatch vr bsr)
    | Just bss <- alignBranch g bsl bsr =
        Just $ AMatch <$> f vl vr <*> bss
  align f g (AHnd rl nhl ahl bl) (AHnd rr nhr ahr br)
    | rl == rr,
      Just ah <- alignMaybe f ahl ahr =
        Just $ AHnd rl <$> f nhl nhr <*> ah <*> g bl br
  align _ g (AShift rl bl) (AShift rr br)
    | rl == rr = Just $ AShift rl <$> g bl br
  align f _ (AFrc u) (AFrc v) = Just $ AFrc <$> f u v
  align f _ (AApp hl asl) (AApp hr asr)
    | Just hs <- alignFunc f hl hr,
      length asl == length asr =
        Just $ AApp <$> hs <*> traverse (uncurry f) (zip asl asr)
  align f _ (ADiscard u) (ADiscard v) = Just $ ADiscard <$> f u v
  align f g (ALocal u bl) (ALocal v br) =
    Just $ ALocal <$> f u v <*> g bl br
  align f _ (AUpdate b r u) (AUpdate c s v)
    | b == c = Just $ AUpdate b <$> f r s <*> f u v
  align f _ (ABox u) (ABox v) = Just $ ABox <$> f u v
  align _ _ _ _ = Nothing

alignEither ::
  (Eq ref, Applicative f) =>
  (l -> r -> f s) ->
  Either ref l ->
  Either ref r ->
  Maybe (f (Either ref s))
alignEither _ (Left rl) (Left rr) | rl == rr = Just . pure $ Left rl
alignEither f (Right u) (Right v) = Just $ Right <$> f u v
alignEither _ _ _ = Nothing

alignMaybe ::
  (Applicative f) =>
  (l -> r -> f s) ->
  Maybe l ->
  Maybe r ->
  Maybe (f (Maybe s))
alignMaybe f (Just l) (Just r) = Just $ Just <$> f l r
alignMaybe _ Nothing Nothing = Just (pure Nothing)
alignMaybe _ _ _ = Nothing

alignFunc ::
  (Eq ref, Applicative f) =>
  (vl -> vr -> f vs) ->
  Func ref vl ->
  Func ref vr ->
  Maybe (f (Func ref vs))
alignFunc f (FVar u) (FVar v) = Just $ FVar <$> f u v
alignFunc _ (FComb rl) (FComb rr) | rl == rr = Just . pure $ FComb rl
alignFunc f (FCont u) (FCont v) = Just $ FCont <$> f u v
alignFunc _ (FCon rl tl) (FCon rr tr)
  | rl == rr, tl == tr = Just . pure $ FCon rl tl
alignFunc _ (FReq rl tl) (FReq rr tr)
  | rl == rr, tl == tr = Just . pure $ FReq rl tl
alignFunc _ (FPrim ol) (FPrim or)
  | ol == or = Just . pure $ FPrim ol
alignFunc _ (FRec rl) (FRec rr)
  | rl == rr = Just . pure $ FRec rl
alignFunc _ (FRecSet fl) (FRecSet fr)
  | fl == fr = Just . pure $ FRecSet fl
alignFunc _ (FWorker rl il) (FWorker rr ir)
  | rl == rr, il == ir = Just . pure $ FWorker rl il
alignFunc _ FYield FYield = Just . pure $ FYield
alignFunc _ _ _ = Nothing

alignBranch ::
  (Ord ref, Applicative f) =>
  (el -> er -> f es) ->
  Branched ref el ->
  Branched ref er ->
  Maybe (f (Branched ref es))
alignBranch _ MatchEmpty MatchEmpty = Just $ pure MatchEmpty
alignBranch f (MatchIntegral bl dl) (MatchIntegral br dr)
  | keysSet bl == keysSet br,
    Just ds <- alignMaybe f dl dr =
      Just $
        MatchIntegral
          <$> interverse f bl br
          <*> ds
alignBranch f (MatchText bl dl) (MatchText br dr)
  | Map.keysSet bl == Map.keysSet br,
    Just ds <- alignMaybe f dl dr =
      Just $
        MatchText
          <$> traverse id (Map.intersectionWith f bl br)
          <*> ds
alignBranch f (MatchBytes bl dl) (MatchBytes br dr)
  | Map.keysSet bl == Map.keysSet br,
    Just ds <- alignMaybe f dl dr =
      Just $
        MatchBytes
          <$> traverse id (Map.intersectionWith f bl br)
          <*> ds
alignBranch f (MatchRequest bl pl) (MatchRequest br pr)
  | Just bs <- alignAscList h bl br =
      Just $ MatchRequest <$> bs <*> f pl pr
  where
    h csl csr
      | keysSet csl == keysSet csr,
        all q (keys csl) =
          Just $ interverse (alignCCs f) csl csr
      | otherwise = Nothing
      where
        q t = fst (csl ! t) == fst (csr ! t)
alignBranch f (MatchData rfl bl dl) (MatchData rfr br dr)
  | rfl == rfr,
    keysSet bl == keysSet br,
    all (\t -> fst (bl ! t) == fst (br ! t)) (keys bl),
    Just ds <- alignMaybe f dl dr =
      Just $ MatchData rfl <$> interverse (alignCCs f) bl br <*> ds
alignBranch f (MatchRec rsl bdl) (MatchRec rsr bdr)
  | rsl == rsr = Just $ MatchRec rsl <$> f bdl bdr
alignBranch f (MatchSum bl) (MatchSum br)
  | keysSet bl == keysSet br,
    all (\w -> fst (bl ! w) == fst (br ! w)) (keys bl) =
      Just $ MatchSum <$> interverse (alignCCs f) bl br
alignBranch f (MatchNumeric rl bl dl) (MatchNumeric rr br dr)
  | rl == rr,
    keysSet bl == keysSet br,
    Just ds <- alignMaybe f dl dr =
      Just $
        MatchNumeric rl
          <$> interverse f bl br
          <*> ds
alignBranch _ _ _ = Nothing

alignAscList ::
  (Applicative f, Ord k) =>
  (a -> b -> Maybe (f c)) ->
  [(k, a)] ->
  [(k, b)] ->
  Maybe (f [(k, c)])
alignAscList f ls0 rs0
  | ll /= lr = Nothing
  | otherwise = getCompose $ zipped ls rs
  where
    (ll, ls) = case prep 0 ls0 of
      Left n -> (n, sortBy (comparing fst) ls0)
      Right n -> (n, ls0)

    (lr, rs) = case prep 0 rs0 of
      Left n -> (n, sortBy (comparing fst) rs0)
      Right n -> (n, rs0)

    prep !n ((k0, _) : xs@((k1, _) : _))
      | k0 <= k1 = prep (n + 1) xs
    prep n [_] = Right (n + 1)
    prep n [] = Right n
    prep n xs = Left (n + length xs)

    zipped [] [] = Compose . Just $ pure []
    zipped ((lk, lv) : lkvs) ((rk, rv) : rkvs)
      | lk == rk = (:) . (lk,) <$> Compose (f lv rv) <*> zipped lkvs rkvs
    zipped _ _ = Compose Nothing

alignCCs :: (Functor f) => (l -> r -> f s) -> (a, l) -> (a, r) -> f (a, s)
alignCCs f (ccs, l) (_, r) = (,) ccs <$> f l r

matchLit :: Term v a -> Maybe (Lit Reference)
matchLit (Int' i) = Just $ I i
matchLit (Nat' n) = Just $ N n
matchLit (Float' f) = Just $ F f
matchLit (Text' t) = Just $ T (Util.Text.fromText t)
matchLit (Char' c) = Just $ C c
matchLit _ = Nothing

pattern TLet ::
  (ABT.Var v) =>
  Direction Word16 ->
  v ->
  Mem ->
  ANormal ref v ->
  ANormal ref v ->
  ANormal ref v
pattern TLet d v m bn bo = ABTN.TTm (ALet d [m] bn (ABTN.TAbs v bo))

pattern TLetD ::
  (ABT.Var v) =>
  v ->
  Mem ->
  ANormal ref v ->
  ANormal ref v ->
  ANormal ref v
pattern TLetD v m bn bo = ABTN.TTm (ALet Direct [m] bn (ABTN.TAbs v bo))

pattern TLets ::
  (ABT.Var v) =>
  Direction Word16 ->
  [v] ->
  [Mem] ->
  ANormal ref v ->
  ANormal ref v ->
  ANormal ref v
pattern TLets d vs ms bn bo = ABTN.TTm (ALet d ms bn (ABTN.TAbss vs bo))

pattern TName ::
  (ABT.Var v) =>
  v ->
  Either ref v ->
  [v] ->
  ANormal ref v ->
  ANormal ref v
pattern TName v f as bo = ABTN.TTm (AName f as (ABTN.TAbs v bo))

pattern Lit' :: Lit Reference -> Term v a
pattern Lit' l <- (matchLit -> Just l)

pattern TLit ::
  (ABT.Var v) =>
  Lit ref ->
  ANormal ref v
pattern TLit l = ABTN.TTm (ALit l)

pattern TBLit ::
  (ABT.Var v) =>
  Lit ref ->
  ANormal ref v
pattern TBLit l = ABTN.TTm (ABLit l)

pattern TApp ::
  (ABT.Var v) =>
  Func ref v ->
  [v] ->
  ANormal ref v
pattern TApp f args = ABTN.TTm (AApp f args)

pattern AApv :: v -> [v] -> ANormalF ref v e
pattern AApv v args = AApp (FVar v) args

pattern TApv ::
  (ABT.Var v) =>
  v ->
  [v] ->
  ANormal ref v
pattern TApv v args = TApp (FVar v) args

pattern ACom :: ref -> [v] -> ANormalF ref v e
pattern ACom r args = AApp (FComb r) args

pattern TCom ::
  (ABT.Var v) =>
  ref ->
  [v] ->
  ANormal ref v
pattern TCom r args = TApp (FComb r) args

pattern ACon :: ref -> CTag -> [v] -> ANormalF ref v e
pattern ACon r t args = AApp (FCon r t) args

pattern TCon ::
  (ABT.Var v) =>
  ref ->
  CTag ->
  [v] ->
  ANormal ref v
pattern TCon r t args = TApp (FCon r t) args

pattern TRec ::
  (ABT.Var v) =>
  RecordSchema ->
  [v] ->
  ANormal ref v
pattern TRec rs args = TApp (FRec rs) args

pattern TRecSet ::
  (ABT.Var v) =>
  FieldName ->
  [v] ->
  ANormal ref v
pattern TRecSet field args = TApp (FRecSet field) args

pattern AKon :: v -> [v] -> ANormalF ref v e
pattern AKon v args = AApp (FCont v) args

pattern TKon ::
  (ABT.Var v) =>
  v ->
  [v] ->
  ANormal ref v
pattern TKon v args = TApp (FCont v) args

pattern AReq :: ref -> CTag -> [v] -> ANormalF ref v e
pattern AReq r t args = AApp (FReq r t) args

pattern TReq ::
  (ABT.Var v) =>
  ref ->
  CTag ->
  [v] ->
  ANormal ref v
pattern TReq r t args = TApp (FReq r t) args

pattern APrm :: POp -> [v] -> ANormalF ref v e
pattern APrm p args = AApp (FPrim (Left p)) args

pattern TPrm ::
  (ABT.Var v) =>
  POp ->
  [v] ->
  ANormal ref v
pattern TPrm p args = TApp (FPrim (Left p)) args

pattern AFOp :: ForeignFunc -> [v] -> ANormalF ref v e
pattern AFOp p args = AApp (FPrim (Right p)) args

pattern TFOp ::
  (ABT.Var v) =>
  ForeignFunc ->
  [v] ->
  ANormal ref v
pattern TFOp p args = TApp (FPrim (Right p)) args

pattern THnd ::
  (ABT.Var v) =>
  [ref] ->
  v ->
  Maybe v ->
  ANormal ref v ->
  ANormal ref v
pattern THnd rs nh ah b = ABTN.TTm (AHnd rs nh ah b)

pattern TShift ::
  (ABT.Var v) =>
  ref ->
  v ->
  ANormal ref v ->
  ANormal ref v
pattern TShift i v e = ABTN.TTm (AShift i (ABTN.TAbs v e))

pattern TMatch ::
  (ABT.Var v) =>
  v ->
  Branched ref (ANormal ref v) ->
  ANormal ref v
pattern TMatch v cs = ABTN.TTm (AMatch v cs)

pattern TFrc :: (ABT.Var v) => v -> ANormal ref v
pattern TFrc v = ABTN.TTm (AFrc v)

pattern TVar :: (ABT.Var v) => v -> ANormal ref v
pattern TVar v = ABTN.TTm (AVar v)

pattern TDiscard :: (ABT.Var v) => v -> ANormal ref v
pattern TDiscard v = ABTN.TTm (ADiscard v)

pattern TLocal ::
  (ABT.Var v) => v -> ANormal ref v -> ANormal ref v
pattern TLocal v e = ABTN.TTm (ALocal v e)

pattern TUpdate ::
  (ABT.Var v) => Bool -> v -> v -> ABTN.Term (ANormalF ref) v
pattern TUpdate ind u v = ABTN.TTm (AUpdate ind u v)

pattern TBox :: (ABT.Var v) => v -> ANormal ref v
pattern TBox v = ABTN.TTm (ABox v)

{-# COMPLETE
  TLets,
  TName,
  TVar,
  TApp,
  TFrc,
  TLit,
  TBLit,
  THnd,
  TShift,
  TMatch,
  TDiscard,
  TLocal,
  TUpdate,
  TBox,
  ABTN.TAbs
  #-}

{-# COMPLETE
  TLets,
  TName,
  TVar,
  TFrc,
  TApv,
  TCom,
  TCon,
  TKon,
  TReq,
  TPrm,
  TFOp,
  TLit,
  TBLit,
  THnd,
  TShift,
  TMatch,
  TDiscard,
  TLocal,
  TUpdate,
  TBox,
  ABTN.TAbs
  #-}

bind :: (Var v) => Cte v -> ANormal Reference v -> ANormal Reference v
bind (ST d us ms bu) = TLets d us ms bu
bind (LZ u f as) = TName u f as

unbind ::
  (Var v) => ANormal Reference v -> Maybe (Cte v, ANormal Reference v)
unbind (TLets d us ms bu bd) = Just (ST d us ms bu, bd)
unbind (TName u f as bd) = Just (LZ u f as, bd)
unbind _ = Nothing

unbinds ::
  (Var v) => ANormal Reference v -> ([Cte v], ANormal Reference v)
unbinds (TLets d us ms bu (unbinds -> (ctx, bd))) =
  (ST d us ms bu : ctx, bd)
unbinds (TName u f as (unbinds -> (ctx, bd))) = (LZ u f as : ctx, bd)
unbinds tm = ([], tm)

pattern TBind ::
  (Var v) =>
  Cte v ->
  ANormal Reference v ->
  ANormal Reference v
pattern TBind bn bd <-
  (unbind -> Just (bn, bd))
  where
    TBind bn bd = bind bn bd

pattern TBinds ::
  (Var v) => [Cte v] -> ANormal Reference v -> ANormal Reference v
pattern TBinds ctx bd <-
  (unbinds -> (ctx, bd))
  where
    TBinds ctx bd = foldr bind bd ctx

{-# COMPLETE TBinds #-}

data SeqEnd = SLeft | SRight
  deriving (Eq, Ord, Enum, Show)

-- Note: MatchNumeric is a new form for matching directly on boxed
-- numeric data. This leaves MatchIntegral around so that builtins can
-- continue to use it. But interchanged code can be free of unboxed
-- details.
data Branched ref e
  = MatchIntegral (EnumMap Word64 e) (Maybe e)
  | MatchText (Map.Map Util.Text.Text e) (Maybe e)
  | MatchBytes (Map.Map Bytes e) (Maybe e)
  | MatchRequest [(ref, (EnumMap CTag ([Mem], e)))] e
  | MatchEmpty
  | MatchData ref (EnumMap CTag ([Mem], e)) (Maybe e)
  | MatchRec RecordSchema e
  | MatchSum (EnumMap Word64 ([Mem], e))
  | MatchNumeric ref (EnumMap Word64 e) (Maybe e)
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- Data cases expected to cover all constructors
pattern MatchDataCover ::
  ref -> EnumMap CTag ([Mem], e) -> Branched ref e
pattern MatchDataCover r m = MatchData r m Nothing

data BranchAccum v
  = AccumEmpty
  | AccumIntegral
      Reference
      (Maybe (ANormal Reference v))
      (EnumMap Word64 (ANormal Reference v))
  | AccumText
      (Maybe (ANormal Reference v))
      (Map.Map Util.Text.Text (ANormal Reference v))
  | AccumBytes
      (Maybe (ANormal Reference v))
      (Map.Map Bytes (ANormal Reference v))
  | AccumDefault (ANormal Reference v)
  | AccumPure (ANormal Reference v)
  | AccumRequest
      (Map Reference (EnumMap CTag ([Mem], ANormal Reference v)))
      (Maybe (ANormal Reference v))
  | AccumData
      Reference
      (Maybe (ANormal Reference v))
      (EnumMap CTag ([Mem], ANormal Reference v))
  | AccumRec RecordSchema (ANormal Reference v)
  | AccumSeqEmpty (ANormal Reference v)
  | AccumSeqView
      SeqEnd
      (Maybe (ANormal Reference v)) -- empty
      (ANormal Reference v) -- cons/snoc
  | AccumSeqSplit
      SeqEnd
      Int -- split at
      (Maybe (ANormal Reference v)) -- default
      (ANormal Reference v) -- split

instance Semigroup (BranchAccum v) where
  AccumEmpty <> r = r
  l <> AccumEmpty = l
  AccumIntegral rl dl cl <> AccumIntegral rr dr cr
    | rl == rr = AccumIntegral rl (dl <|> dr) $ cl <> cr
  AccumText dl cl <> AccumText dr cr =
    AccumText (dl <|> dr) (cl <> cr)
  AccumBytes dl cl <> AccumBytes dr cr =
    AccumBytes (dl <|> dr) (cl <> cr)
  AccumData rl dl cl <> AccumData rr dr cr
    | rl == rr = AccumData rl (dl <|> dr) (cl <> cr)
  AccumDefault dl <> AccumIntegral r _ cr =
    AccumIntegral r (Just dl) cr
  AccumDefault dl <> AccumText _ cr =
    AccumText (Just dl) cr
  AccumDefault dl <> AccumBytes _ cr =
    AccumBytes (Just dl) cr
  AccumDefault dl <> AccumData rr _ cr =
    AccumData rr (Just dl) cr
  AccumIntegral r dl cl <> AccumDefault dr =
    AccumIntegral r (dl <|> Just dr) cl
  AccumText dl cl <> AccumDefault dr =
    AccumText (dl <|> Just dr) cl
  AccumBytes dl cl <> AccumDefault dr =
    AccumBytes (dl <|> Just dr) cl
  AccumData rl dl cl <> AccumDefault dr =
    AccumData rl (dl <|> Just dr) cl
  l@(AccumPure _) <> AccumPure _ = l
  AccumPure dl <> AccumRequest hr _ = AccumRequest hr (Just dl)
  AccumRequest hl dl <> AccumPure dr =
    AccumRequest hl (dl <|> Just dr)
  AccumRequest hl dl <> AccumRequest hr dr =
    AccumRequest hm $ dl <|> dr
    where
      hm = Map.unionWith (<>) hl hr
  l@(AccumSeqEmpty _) <> AccumSeqEmpty _ = l
  AccumSeqEmpty eml <> AccumSeqView er _ cnr =
    AccumSeqView er (Just eml) cnr
  AccumSeqView el eml cnl <> AccumSeqEmpty emr =
    AccumSeqView el (eml <|> Just emr) cnl
  AccumSeqView el eml cnl <> AccumSeqView er emr _
    | el /= er =
        internalBug [] "AccumSeqView: trying to merge views of opposite ends"
    | otherwise = AccumSeqView el (eml <|> emr) cnl
  AccumSeqView _ _ _ <> AccumDefault _ =
    internalBug [] "seq views may not have defaults"
  AccumDefault _ <> AccumSeqView _ _ _ =
    internalBug [] "seq views may not have defaults"
  AccumSeqSplit el nl dl bl <> AccumSeqSplit er nr dr _
    | el /= er =
        internalBug [] "AccumSeqSplit: trying to merge splits at opposite ends"
    | nl /= nr =
        internalBug [] "AccumSeqSplit: trying to merge splits at different positions"
    | otherwise =
        AccumSeqSplit el nl (dl <|> dr) bl
  AccumDefault dl <> AccumSeqSplit er nr _ br =
    AccumSeqSplit er nr (Just dl) br
  AccumSeqSplit el nl dl bl <> AccumDefault dr =
    AccumSeqSplit el nl (dl <|> Just dr) bl
  _ <> _ = internalBug [] "cannot merge data cases for different types"

instance Monoid (BranchAccum e) where
  mempty = AccumEmpty

newtype RecordRef = RecordRef Word64
  deriving (Show, Eq, Ord)

newtype RecordSchema = RecordSchema (Set FieldName)
  deriving (Show, Eq, Ord)

type FieldName = Text

data Func ref v
  = -- variable
    FVar v
  | -- top-level combinator
    FComb !ref
  | -- continuation jump
    FCont v
  | -- data constructor
    FCon !ref !CTag
  | -- ability request
    FReq !ref !CTag
  | -- prim op
    FPrim (Either POp ForeignFunc)
  | -- record constructor
    FRec RecordSchema
  | -- record update: replace one field's value, keeping the record's shape
    FRecSet FieldName
  | -- a worker of a function's group (Unison.Runtime.MCode.Explode): the
    -- combinator at this index, called with exactly the arguments it
    -- takes. Only in a batch's code after it is loaded; never stored.
    FWorker !ref !Word64
  | -- a return of the arguments as that many values (the tails of a
    -- product-return worker, Unison.Runtime.MCode.Explode). Only in a
    -- batch's code after it is loaded; never stored.
    FYield
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Lit ref
  = I Int64
  | N Word64
  | F Double
  | T Util.Text.Text
  | C Char
  | LM (Rfn.Referent' ref) -- Term Link
  | LY ref -- Type Link
  deriving (Show, Eq)

litRef :: Lit ref -> Reference
litRef (I _) = Ty.intRef
litRef (N _) = Ty.natRef
litRef (F _) = Ty.floatRef
litRef (T _) = Ty.textRef
litRef (C _) = Ty.charRef
litRef (LM _) = Ty.termLinkRef
litRef (LY _) = Ty.typeLinkRef

type ANormal ref = ABTN.Term (ANormalF ref)

type Cte v = CTE v (ANormal Reference v)

-- A block's bindings so far, in order. A Seq, because a left-nested
-- application appends the left operand's whole context at each level
-- (`anfArg`, `anfArgs`), which with a list copied it each time: quadratic
-- in the nesting, and the live heap of a wide expression's normalization
-- was almost all list cells.
type Ctx v = Directed () (Seq (Cte v))

data Direction a = Indirect a | Direct
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

directed :: (Foldable f) => f (Cte v) -> Ctx v
directed x = (foldMap f x, Seq.fromList (toList x))
  where
    f (ST d _ _ _) = () <$ d
    f _ = Direct

instance (Semigroup a) => Semigroup (Direction a) where
  Indirect l <> Indirect r = Indirect $ l <> r
  Direct <> r = r
  l <> Direct = l

instance (Semigroup a) => Monoid (Direction a) where
  mempty = Direct

type Directed a = (,) (Direction a)

type DNormal v = Directed () (ANormal Reference v)

-- Should be a completely closed term
data SuperNormal ref v = Lambda {conventions :: [Mem], bound :: ANormal ref v}
  deriving (Show, Eq)

data SuperGroup ref v = Rec
  { group :: [(v, SuperNormal ref v)],
    entry :: SuperNormal ref v
  }
  deriving (Show)

-- | Whether the evaluation of a given definition is cacheable or not.
-- i.e. it's a top-level pure value.
data Cacheability = Cacheable | Uncacheable
  deriving stock (Eq, Show)

instance (Ord ref, Var v) => Eq (SuperGroup ref v) where
  g0 == g1 | Left _ <- equivocate g0 g1 = False | otherwise = True

-- Failure modes for SuperGroup alpha equivalence test
data SGEqv ref v
  = -- mismatch number of definitions in group
    NumDefns (SuperGroup ref v) (SuperGroup ref v)
  | -- mismatched SuperNormal calling conventions
    DefnConventions (SuperNormal ref v) (SuperNormal ref v)
  | -- mismatched subterms in corresponding definition
    Subterms (ANormal ref v) (ANormal ref v)

-- Yields the number of arguments directly accepted by a combinator.
arity :: SuperNormal ref v -> Int
arity (Lambda ccs _) = length ccs

-- Yields the numbers of arguments directly accepted by the
-- combinators in a group. The main entry is the first element, and
-- local bindings follow in their original order.
arities :: SuperGroup ref v -> [Int]
arities (Rec bs e) = arity e : fmap (arity . snd) bs

-- | The number of arguments the group's main entry accepts.
entryArity :: SuperGroup ref v -> Int
entryArity (Rec _ e) = arity e

-- Checks if two SuperGroups are equivalent up to renaming. The rest
-- of the structure must match on the nose. If the two groups are not
-- equivalent, an example of conflicting structure is returned.
equivocate ::
  (Ord ref, Var v) =>
  SuperGroup ref v ->
  SuperGroup ref v ->
  Either (SGEqv ref v) ()
equivocate g0@(Rec bs0 e0) g1@(Rec bs1 e1)
  | length bs0 == length bs1 =
      traverse_ eqvSN (zip ns0 ns1) *> eqvSN (e0, e1)
  | otherwise = Left $ NumDefns g0 g1
  where
    (vs0, ns0) = unzip bs0
    (vs1, ns1) = unzip bs1
    vm = Map.fromList (zip vs1 vs0)

    promote (Left (l, r)) = Left $ Subterms l r
    promote (Right v) = Right v

    eqvSN (Lambda ccs0 e0, Lambda ccs1 e1)
      | ccs0 == ccs1 = promote $ ABTN.alpha vm e0 e1
    eqvSN (n0, n1) = Left $ DefnConventions n0 n1

type ANFM v =
  ReaderT
    (Set v)
    (State (Word64, Word16, [(v, SuperNormal Reference v)]))

type ANFD v = Compose (ANFM v) (Directed ())

data GroupRef ref = GR ref Word64
  deriving (Functor, Foldable, Traversable, Show, Eq)

-- | A list of either unboxed or boxed values.
-- Each slot is one of unboxed or boxed but not both.
type ValList ref = [Value ref]

data Value ref
  = Partial (GroupRef ref) (ValList ref)
  | Data ref Word64 (ValList ref)
  | Record RecordSchema (ValList ref)
  | Cont (ValList ref) (Cont ref)
  | BLit (BLit ref)
  deriving (Show, Eq)

-- Since we can now track cacheability of supergroups, this type
-- pairs the two together. This is the type that should be used
-- as the representation of unison Code values rather than the
-- previous `SuperGroup Symbol`.
data Code ref = CodeRep (SuperGroup ref Symbol) Cacheability
  deriving (Show)

codeGroup :: Code ref -> SuperGroup ref Symbol
codeGroup (CodeRep sg _) = sg

instance (Ord ref) => Eq (Code ref) where
  CodeRep sg1 _ == CodeRep sg2 _ = sg1 == sg2

overGroup ::
  (SuperGroup ref0 Symbol -> SuperGroup ref1 Symbol) ->
  Code ref0 ->
  Code ref1
overGroup f (CodeRep sg ch) = CodeRep (f sg) ch

foldGroup ::
  (Monoid m) => (SuperGroup ref Symbol -> m) -> Code ref -> m
foldGroup f (CodeRep sg _) = f sg

traverseGroup ::
  (Applicative f) =>
  (SuperGroup ref0 Symbol -> f (SuperGroup ref1 Symbol)) ->
  Code ref0 ->
  f (Code ref1)
traverseGroup f (CodeRep sg ch) = flip CodeRep ch <$> f sg

instance Referential Code where
  overRefs f (CodeRep sg ch) = CodeRep (overGroupLinks f sg) ch
  foldMapRefs f (CodeRep sg _) = foldGroupLinks f sg
  traverseRefs f (CodeRep sg ch) =
    flip CodeRep ch <$> traverseGroupLinks f sg

data Cont ref
  = KE
  | Mark
      Word64 -- pending args
      [ref]
      [(ref, Value ref)]
      (Cont ref)
  | Push
      Word64 -- Frame size
      Word64 -- Pending args
      (GroupRef ref)
      (Cont ref)
  deriving (Show, Eq)

data BLit ref
  = Text Util.Text.Text
  | List (Seq (Value ref))
  | TmLink (Rfn.Referent' ref)
  | TyLink ref
  | Bytes Bytes
  | Quote (Value ref)
  | Code (Code ref)
  | BArr PA.ByteArray
  | Arr (PA.Array (Value ref))
  | -- Despite the following being in the Boxed Literal type, they all represent unboxed values
    Pos Word64
  | Neg Word64
  | Char Char
  | Float Double
  | -- special cases for newer formats
    Map [(Value ref, Value ref)]
  | -- arbitrary precision numbers
    BigInt Integer
  | BigNat Natural
  deriving (Show, Eq)

pattern UFalse <- TCon ((== Ty.booleanRef) -> True) 0 []
  where
    UFalse = TCon Ty.booleanRef 0 []

pattern UTrue <- TCon ((== Ty.booleanRef) -> True) 1 []
  where
    UTrue = TCon Ty.booleanRef 1 []

valueTermLinks :: (Ord ref) => Value ref -> [ref]
valueTermLinks = Set.toList . valueLinks f
  where
    f False r = Set.singleton r
    f _ _ = Set.empty

collectValueLinks :: (Ord ref) => Value ref -> (Set ref, Set ref)
collectValueLinks = valueLinks f
  where
    f False r = (mempty, Set.singleton r)
    f True r = (Set.singleton r, mempty)

-- Folds over the references necessary to _load_ a `Value`. This does
-- not include references in quoted code or values, or literal
-- term/type links.
valueLinks :: (Monoid a) => (Bool -> ref -> a) -> Value ref -> a
valueLinks f = go
  where
    go (Partial (GR cr _) vs) = f False cr <> foldMap go vs
    go (Data dr _ vs) = f True dr <> foldMap go vs
    go (Cont vs k) = foldMap go vs <> contLinks f k
    go (BLit l) = blitLinks f l
    go (Record _rs vs) = foldMap go vs
{-# INLINE valueLinks #-}

-- Traversals of _all_ references in a `Value`, for e.g.
-- canonicalization.
instance Referential Value where
  overRefs h = \case
    Partial gr vs ->
      Partial (h False <$> gr) (fmap (overRefs h) vs)
    Data r t vs ->
      Data (h True r) t (fmap (overRefs h) vs)
    Cont vs k ->
      Cont (fmap (overRefs h) vs) (overRefs h k)
    BLit l -> BLit (overRefs h l)
    Record rs vs -> Record rs (fmap (overRefs h) vs)

  foldMapRefs h = \case
    Partial (GR r _) vs -> h False r <> foldMap (foldMapRefs h) vs
    Data r _ vs -> h True r <> foldMap (foldMapRefs h) vs
    Cont vs k -> foldMap (foldMapRefs h) vs <> foldMapRefs h k
    BLit l -> foldMapRefs h l
    Record _rs vs -> foldMap (foldMapRefs h) vs

  traverseRefs h = \case
    Partial gr vs ->
      Partial
        <$> traverse (h False) gr
        <*> traverse (traverseRefs h) vs
    Data r t vs ->
      flip Data t
        <$> h True r
        <*> traverse (traverseRefs h) vs
    Cont vs k ->
      Cont
        <$> traverse (traverseRefs h) vs
        <*> traverseRefs h k
    BLit l -> BLit <$> traverseRefs h l
    Record rs vs -> Record rs <$> traverse (traverseRefs h) vs

contLinks :: (Monoid a) => (Bool -> ref -> a) -> Cont ref -> a
contLinks f = go
  where
    go (Push _ _ (GR cr _) k) =
      f False cr <> go k
    go (Mark _ ps de k) =
      foldMap (f True) ps
        <> foldMap (\(k, c) -> f True k <> valueLinks f c) de
        <> go k
    go KE = mempty
{-# INLINE contLinks #-}

-- Traversals over references in a cont.
--
-- This traverses _all_ references in the continuation, not just the
-- ones necessary to load it.
instance Referential Cont where
  overRefs h = \case
    KE -> KE
    Mark asz rs env k ->
      Mark
        asz
        (fmap (h True) rs)
        (fmap (bimap (h True) (overRefs h)) env)
        (overRefs h k)
    Push fsz asz gr k ->
      Push fsz asz (h False <$> gr) (overRefs h k)

  foldMapRefs h = \case
    KE -> mempty
    Push _ _ (GR r _) k -> h False r <> foldMapRefs h k
    Mark _ rs env k ->
      foldMap (h True) rs
        <> foldMap (bifoldMap (h True) (foldMapRefs h)) env
        <> foldMapRefs h k

  traverseRefs h = \case
    KE -> pure KE
    Mark asz rs env k ->
      Mark asz
        <$> traverse (h True) rs
        <*> traverse (bitraverse (h True) (traverseRefs h)) env
        <*> traverseRefs h k
    Push fsz asz gr k ->
      Push fsz asz
        <$> traverse (h False) gr
        <*> traverseRefs h k

blitLinks :: (Monoid a) => (Bool -> ref -> a) -> BLit ref -> a
blitLinks f = go
  where
    go (List s) = foldMap (valueLinks f) s
    go (Arr a) = foldMap (valueLinks f) a
    go (Map m) =
      foldMap (\(k, v) -> valueLinks f k <> valueLinks f v) m
    go _ = mempty
{-# INLINE blitLinks #-}

instance Referential BLit where
  overRefs h = \case
    List vs -> List (fmap (overRefs h) vs)
    TmLink rn -> TmLink (overRefs h rn)
    TyLink r -> TyLink $ h True r
    Quote v -> Quote $ overRefs h v
    Code co -> Code $ overRefs h co
    Arr a -> Arr $ fmap (overRefs h) a
    Map kvs -> Map $ fmap (bimap (overRefs h) (overRefs h)) kvs
    Text t -> Text t
    Bytes b -> Bytes b
    BArr ba -> BArr ba
    Pos n -> Pos n
    Neg n -> Neg n
    Char c -> Char c
    Float f -> Float f
    BigInt i -> BigInt i
    BigNat n -> BigNat n

  foldMapRefs h = \case
    List vs -> foldMap (foldMapRefs h) vs
    TmLink rn -> foldMapRefs h rn
    TyLink r -> h True r
    Quote v -> foldMapRefs h v
    Code co -> foldMapRefs h co
    Arr a -> foldMap (foldMapRefs h) a
    Map kvs -> foldMap (bifoldMap (foldMapRefs h) (foldMapRefs h)) kvs
    _ -> mempty

  traverseRefs h = \case
    List vs -> List <$> traverse (traverseRefs h) vs
    TmLink rn -> TmLink <$> traverseRefs h rn
    TyLink r -> TyLink <$> h True r
    Quote v -> Quote <$> traverseRefs h v
    Code co -> Code <$> traverseRefs h co
    Arr a -> Arr <$> traverse (traverseRefs h) a
    Map kvs ->
      Map
        <$> traverse (bitraverse (traverseRefs h) (traverseRefs h)) kvs
    Text t -> pure $ Text t
    Bytes b -> pure $ Bytes b
    BArr ba -> pure $ BArr ba
    Pos n -> pure $ Pos n
    Neg n -> pure $ Neg n
    Char c -> pure $ Char c
    Float f -> pure $ Float f
    BigInt i -> pure $ BigInt i
    BigNat n -> pure $ BigNat n

groupTermLinks :: (Ord ref, Var v) => SuperGroup ref v -> [ref]
groupTermLinks = Set.toList . foldGroupLinks f
  where
    f False r = Set.singleton r
    f _ _ = Set.empty

overGroupLinks ::
  (Var v) =>
  (Bool -> ref0 -> ref1) ->
  SuperGroup ref0 v ->
  SuperGroup ref1 v
overGroupLinks f =
  runIdentity . traverseGroupLinks (\b -> Identity . f b)

traverseGroupLinks ::
  (Applicative f, Var v) =>
  (Bool -> ref0 -> f ref1) ->
  SuperGroup ref0 v ->
  f (SuperGroup ref1 v)
traverseGroupLinks f (Rec bs e) =
  Rec <$> (traverse . traverse) (normalLinks f) bs <*> normalLinks f e

foldGroupLinks ::
  (Monoid r, Var v) =>
  (Bool -> ref -> r) ->
  SuperGroup ref v ->
  r
foldGroupLinks f = getConst . traverseGroupLinks (\b -> Const . f b)

-- | Every record schema constructed or matched on anywhere in a group.
--
-- Record schemas are interned by the runtime like type and term references
-- are, so this is the analogue of 'foldGroupLinks' for them: it tells the code
-- cache which schemas a group needs before its code is emitted.
groupRecordSchemas :: SuperGroup ref v -> Set RecordSchema
groupRecordSchemas (Rec bs e) =
  foldMap (normalRecordSchemas . snd) bs <> normalRecordSchemas e

normalRecordSchemas :: SuperNormal ref v -> Set RecordSchema
normalRecordSchemas (Lambda _ e) = anfRecordSchemas e

anfRecordSchemas :: ANormal ref v -> Set RecordSchema
anfRecordSchemas = \case
  ABTN.Term _ (ABTN.Abs _ e) -> anfRecordSchemas e
  ABTN.Term _ (ABTN.Tm e) -> case e of
    AApp (FRec rs) _ -> Set.singleton rs
    AMatch _ bs -> branchRecordSchemas bs
    -- Everything else just recurses; `ANormalF` is Foldable in its
    -- subexpressions, and no other constructor mentions a schema.
    _ -> foldMap anfRecordSchemas e

branchRecordSchemas :: Branched ref (ANormal ref v) -> Set RecordSchema
branchRecordSchemas = \case
  MatchRec rs e -> Set.insert rs (anfRecordSchemas e)
  bs -> foldMap anfRecordSchemas bs

normalLinks ::
  (Applicative f, Var v) =>
  (Bool -> ref0 -> f ref1) ->
  SuperNormal ref0 v ->
  f (SuperNormal ref1 v)
normalLinks f (Lambda ccs e) = Lambda ccs <$> anfLinks f e

anfLinks ::
  (Applicative f, Var v) =>
  (Bool -> ref0 -> f ref1) ->
  ANormal ref0 v ->
  f (ANormal ref1 v)
anfLinks f (ABTN.Term _ (ABTN.Abs v e)) =
  ABTN.TAbs v <$> anfLinks f e
anfLinks f (ABTN.Term _ (ABTN.Tm e)) =
  ABTN.TTm <$> anfFLinks f (anfLinks f) e

anfFLinks ::
  (Applicative f) =>
  (Bool -> ref0 -> f ref1) ->
  (e0 -> f e1) ->
  ANormalF ref0 v e0 ->
  f (ANormalF ref1 v e1)
anfFLinks _ g (ALet d ccs b e) = ALet d ccs <$> g b <*> g e
anfFLinks f g (AName er vs e) =
  flip AName vs <$> bitraverse (f False) pure er <*> g e
anfFLinks f g (AMatch v bs) =
  AMatch v <$> branchLinks (f True) g bs
anfFLinks f g (AShift r e) =
  AShift <$> f True r <*> g e
anfFLinks f g (AHnd rs nh ah e) =
  (\rs -> AHnd rs nh ah) <$> traverse (f True) rs <*> g e
anfFLinks f _ (AApp fu vs) = flip AApp vs <$> funcLinks f fu
anfFLinks f _ (ALit l) = ALit <$> litLinks f l
anfFLinks f _ (ABLit l) = ABLit <$> litLinks f l
anfFLinks _ _ (AFrc v) = pure $ AFrc v
anfFLinks _ _ (AVar v) = pure $ AVar v
anfFLinks _ _ (ADiscard v) = pure $ ADiscard v
anfFLinks _ g (ALocal v e) = ALocal v <$> g e
anfFLinks _ _ (AUpdate b u v) = pure $ AUpdate b u v
anfFLinks _ _ (ABox v) = pure $ ABox v

litLinks ::
  (Applicative f) =>
  (Bool -> ref0 -> f ref1) ->
  Lit ref0 ->
  f (Lit ref1)
litLinks f (LY r) = LY <$> f True r
litLinks f (LM (Rfn.Con' (ConstructorReference r i) t)) =
  LM . flip Rfn.Con' t . flip ConstructorReference i <$> f True r
litLinks f (LM (Rfn.Ref' r)) = LM . Rfn.Ref' <$> f False r
litLinks _ (I i) = pure $ I i
litLinks _ (N n) = pure $ N n
litLinks _ (F d) = pure $ F d
litLinks _ (T t) = pure $ T t
litLinks _ (C c) = pure $ C c

branchLinks ::
  (Applicative f) =>
  (ref0 -> f ref1) ->
  (e0 -> f e1) ->
  Branched ref0 e0 ->
  f (Branched ref1 e1)
branchLinks f g (MatchRequest m e) =
  MatchRequest <$> traverse h m <*> g e
  where
    h (r, cs) = (,) <$> f r <*> (traverse . traverse) g cs
branchLinks f g (MatchData r m e) =
  MatchData <$> f r <*> (traverse . traverse) g m <*> traverse g e
branchLinks _ g (MatchText m e) =
  MatchText <$> traverse g m <*> traverse g e
branchLinks _ g (MatchBytes m e) =
  MatchBytes <$> traverse g m <*> traverse g e
branchLinks _ g (MatchIntegral m e) =
  MatchIntegral <$> traverse g m <*> traverse g e
branchLinks f g (MatchNumeric r m e) =
  MatchNumeric <$> f r <*> traverse g m <*> traverse g e
branchLinks _ g (MatchSum m) =
  MatchSum <$> (traverse . traverse) g m
branchLinks _ g (MatchRec rs e) =
  MatchRec rs <$> g e
branchLinks _ _ MatchEmpty = pure MatchEmpty

funcLinks ::
  (Applicative f) =>
  (Bool -> ref0 -> f ref1) ->
  Func ref0 v ->
  f (Func ref1 v)
funcLinks f (FComb r) = FComb <$> f False r
funcLinks f (FCon r t) = flip FCon t <$> f True r
funcLinks f (FReq r t) = flip FReq t <$> f True r
funcLinks _ (FVar v) = pure $ FVar v
funcLinks _ (FCont v) = pure $ FCont v
funcLinks _ (FPrim e) = pure $ FPrim e
funcLinks _ (FRec rr) = pure $ FRec rr
funcLinks _ (FRecSet field) = pure $ FRecSet field
funcLinks f (FWorker r i) = flip FWorker i <$> f False r
funcLinks _ FYield = pure FYield

indent :: Int -> ShowS
indent ind = showString (replicate (ind * 2) ' ')

prettyGroup :: (Var v) => String -> SuperGroup Reference v -> ShowS
prettyGroup s (Rec grp ent) =
  showString ("let rec[" ++ s ++ "]\n")
    . foldr f id grp
    . showString "entry"
    . prettySuperNormal 1 ent
  where
    f (v, sn) r =
      indent 1
        . pvar v
        . prettySuperNormal 2 sn
        . showString "\n"
        . r

pvar :: (Var v) => v -> ShowS
pvar v = showString . Data.Text.unpack $ Var.name v

prettyVars :: (Var v) => [v] -> ShowS
prettyVars =
  foldr (\v r -> showString " " . pvar v . r) id

prettyLVars :: (Var v) => [Mem] -> [v] -> ShowS
prettyLVars [] [] = showString " "
prettyLVars (c : cs) (v : vs) =
  showString " "
    . showParen True (pvar v . showString ":" . shows c)
    . prettyLVars cs vs
prettyLVars [] (_ : _) = internalBug [] "more variables than conventions"
prettyLVars (_ : _) [] = internalBug [] "more conventions than variables"

prettyRBind :: (Var v) => [v] -> ShowS
prettyRBind [] = showString "()"
prettyRBind [v] = pvar v
prettyRBind (v : vs) =
  showParen True $
    pvar v . foldr (\v r -> shows v . showString "," . r) id vs

prettySuperNormal ::
  (Var v) => Int -> SuperNormal Reference v -> ShowS
prettySuperNormal ind (Lambda ccs (ABTN.TAbss vs tm)) =
  prettyLVars ccs vs
    . showString "="
    . prettyANF False (ind + 1) tm

reqSpace :: (Var v) => Bool -> ANormal ref v -> Bool
reqSpace _ TLets {} = True
reqSpace _ TName {} = True
reqSpace _ TLocal {} = True
reqSpace b _ = b

prettyANF :: (Var v) => Bool -> Int -> ANormal Reference v -> ShowS
prettyANF m ind tm =
  prettySpace (reqSpace m tm) ind . case tm of
    TLets _ vs _ bn bo ->
      prettyRBind vs
        . showString " ="
        . prettyANF False (ind + 1) bn
        . prettyANF True ind bo
    TName v f vs bo ->
      prettyRBind [v]
        . showString " := "
        . prettyLZF f
        . prettyVars vs
        . prettyANF True ind bo
    TLit l -> shows l
    TBLit l -> shows l
    TFrc v -> showString "!" . pvar v
    TVar v -> pvar v
    TApp f vs -> prettyFunc f . prettyVars vs
    TMatch v bs ->
      showString "match "
        . pvar v
        . showString " with"
        . prettyBranches (ind + 1) bs
    TShift r v bo ->
      showString "shift["
        . showsShort r
        . showString "]"
        . prettyVars [v]
        . showString "."
        . prettyANF False (ind + 1) bo
    THnd rs nh ah bo ->
      showString "handle"
        . prettyRefs rs
        . prettyANF False (ind + 1) bo
        . prettySpace True ind
        . showString "with "
        . pvar nh
        . maybe id (\v -> showString " with affine " . pvar v) ah
    TLocal hr bo ->
      showString "in-local "
        . pvar hr
        . prettyANF True (ind + 1) bo
    TDiscard hr ->
      showString "discard[" . pvar hr . showString "]"
    TUpdate _ hr v ->
      showString "update["
        . pvar hr
        . showString ", "
        . pvar v
        . showString "]"
    TBox v ->
      showString "box[" . pvar v . showString "]"
    ABTN.TAbs v (ABTN.TAbss vs bo) ->
      prettyVars (v : vs)
        . showString " ->"
        . prettyANF True (ind + 1) bo

prettySpace :: Bool -> Int -> ShowS
prettySpace False _ = showString " "
prettySpace True ind = showString "\n" . indent ind

prettyLZF :: (Var v) => Either Reference v -> ShowS
prettyLZF (Left w) = showString "ENV(" . showsShort w . showString ") "
prettyLZF (Right v) = pvar v . showString " "

prettyRefs :: [Reference] -> ShowS
prettyRefs [] = showString "{}"
prettyRefs (r : rs) =
  showString "{"
    . showsShort r
    . foldr (\t r -> showString "," . showsShort t . r) id rs
    . showString "}"

prettyFunc :: (Var v) => Func Reference v -> ShowS
prettyFunc (FVar v) = pvar v . showString " "
prettyFunc (FCont v) = pvar v . showString " "
prettyFunc (FComb w) = showString "ENV(" . showsShort w . showString ")"
prettyFunc (FCon r t) =
  showString "CON("
    . showsShort r
    . showString ","
    . shows t
    . showString ")"
prettyFunc (FReq r t) =
  showString "REQ("
    . showsShort r
    . showString ","
    . shows t
    . showString ")"
prettyFunc (FPrim op) = either shows shows op . showString " "
prettyFunc (FRec r) =
  showString "REC(" . shows r . showString ") "
prettyFunc (FRecSet field) =
  showString "RECSET(" . shows field . showString ") "
prettyFunc (FWorker r i) =
  showString "WORKER(" . showsShort r . showString "," . shows i . showString ")"
prettyFunc FYield = showString "YIELD "

showsShort :: Reference -> ShowS
showsShort =
  showString . Text.unpack . SH.toText . shortenTo 10 . toShortHash

prettyBranches ::
  (Var v) => Int -> Branched Reference (ANormal Reference v) -> ShowS
prettyBranches ind bs = case bs of
  MatchEmpty -> showString "{}"
  MatchIntegral bs df ->
    maybe id (\e -> prettyCase ind (showString "_") e id) df
      . foldr (uncurry $ prettyCase ind . shows) id (mapToList bs)
  MatchText bs df ->
    maybe id (\e -> prettyCase ind (showString "_") e id) df
      . foldr (uncurry $ prettyCase ind . shows) id (Map.toList bs)
  MatchBytes bs df ->
    maybe id (\e -> prettyCase ind (showString "_") e id) df
      . foldr (uncurry $ prettyCase ind . shows) id (Map.toList bs)
  MatchData r bs df ->
    maybe id (\e -> prettyCase ind (showString "_") e id) df
      . foldr
        (uncurry $ prettyCase ind . prettyTag r)
        id
        (mapToList $ snd <$> bs)
  MatchRec (RecordSchema rs) bd ->
    let fields =
          Set.toList rs
            & Text.intercalate ", "
            & Text.unpack
     in prettyCase ind (showString "REC{" . showString fields . showString "}") bd id
  MatchRequest bs df ->
    foldr
      ( \(r, m) s ->
          foldr
            (\(c, e) -> prettyCase ind (prettyReq r c) e)
            s
            (mapToList $ snd <$> m)
      )
      (prettyCase ind (showString "REQ(0,0)") df id)
      bs
  MatchSum bs ->
    foldr
      (uncurry $ prettyCase ind . shows)
      id
      (mapToList $ snd <$> bs)
  MatchNumeric _ bs df ->
    maybe id (\e -> prettyCase ind (showString "_") e id) df
      . foldr (uncurry $ prettyCase ind . shows) id (mapToList bs)
  where
    -- _ -> error "prettyBranches: todo"

    -- prettyReq :: Reference -> CTag -> ShowS
    prettyReq r c =
      showString "REQ("
        . showsShort r
        . showString ","
        . shows c
        . showString ")"

    prettyTag r c =
      showString "CON("
        . showsShort r
        . showString ","
        . shows c
        . showString ")"

prettyCase ::
  (Var v) => Int -> ShowS -> ANormal Reference v -> ShowS -> ShowS
prettyCase ind sc (ABTN.TAbss vs e) r =
  showString "\n"
    . indent ind
    . sc
    . prettyVars vs
    . showString " ->"
    . prettyANF True (ind + 1) e
    . r
