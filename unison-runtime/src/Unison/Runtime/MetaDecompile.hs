{-# LANGUAGE PatternSynonyms #-}

-- | Conversion from Unison's source-level 'Term.Term' representation
-- (the Haskell ADT produced by "Unison.Runtime.Decompile") into a
-- runtime 'Val' shaped as a value of the @meta.Term meta.TermF@ type
-- defined in "Unison.Runtime.MetaSource".
--
-- This is the engine behind the @MDCM@ primop (surface name
-- @Meta.decompile@). The high-level flow is:
--
-- 1. The primop receives a 'Val' (any Unison value).
-- 2. It calls the existing @Decompile.decompile@ to obtain a
--    'Term.Term'.
-- 3. This module's 'convertTerm' walks that 'Term.Term' and emits a
--    'Val' that the runtime can hand back to user Unison code as a
--    @meta.Term meta.TermF@.
module Unison.Runtime.MetaDecompile
  ( convertTerm,
  )
where

import Data.Foldable (toList)
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Unison.ABT qualified as ABT
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.Hash qualified as Hash
import Unison.Pattern (Pattern, SeqOp)
import Unison.Pattern qualified as Pat
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Runtime.MetaSource qualified as Meta
import Unison.Runtime.Stack
  ( Closure (..),
    Foreign (..),
    Val (..),
    segFromList,
    pattern BoxedVal,
    pattern CharVal,
    pattern DoubleVal,
    pattern IntVal,
    pattern NatVal,
  )
import Unison.Runtime.TypeTags qualified as TT
import Unison.Symbol (Symbol)
import Unison.Term (MatchCase (..))
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Util.Bytes qualified as Bytes
import Unison.Util.Text qualified as Util.Text
import Unison.Var qualified as Var

-- ---------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------

-- | Convert a source-level 'Term.Term' into a runtime 'Val' whose
-- shape is @meta.Term meta.TermF@.
convertTerm :: Term.Term Symbol () -> Val
convertTerm = termVal

-- ---------------------------------------------------------------
-- meta.Term f
--
-- @Term f = Term (Set Name) (ABT f (Term f))@
-- ---------------------------------------------------------------

termVal :: Term.Term Symbol () -> Val
termVal tm =
  BoxedVal $
    Data2
      Meta.termRef
      TT.metaTermTermTag
      (freeVarsVal (Term.freeVars tm))
      (abtVal tm)

-- ---------------------------------------------------------------
-- meta.ABT f a
--
-- @ABT f a = Var Name | Abs Name a | Cycle a | Tm (f a)@
-- ---------------------------------------------------------------

abtVal :: Term.Term Symbol () -> Val
abtVal tm = case ABT.out tm of
  ABT.Var v ->
    BoxedVal $ Data1 Meta.abtRef TT.metaAbtVarTag (nameVal v)
  ABT.Abs v body ->
    BoxedVal $ Data2 Meta.abtRef TT.metaAbtAbsTag (nameVal v) (termVal body)
  ABT.Cycle body ->
    BoxedVal $ Data1 Meta.abtRef TT.metaAbtCycleTag (termVal body)
  ABT.Tm f ->
    BoxedVal $ Data1 Meta.abtRef TT.metaAbtTmTag (termFVal f)

-- ---------------------------------------------------------------
-- meta.TermF a — the Unison-term base functor.
-- ---------------------------------------------------------------

termFVal :: Term.F Symbol () () (Term.Term Symbol ()) -> Val
termFVal = \case
  Term.Int n -> litTermF (mkLit TT.metaLitIntTag (IntVal (fromIntegral n)))
  Term.Nat n -> litTermF (mkLit TT.metaLitNatTag (NatVal n))
  Term.Float f -> litTermF (mkLit TT.metaLitFloatTag (DoubleVal f))
  Term.Boolean b -> litTermF (mkLit TT.metaLitBooleanTag (boolVal b))
  Term.Text t -> litTermF (mkLit TT.metaLitTextTag (textVal t))
  Term.Char c -> litTermF (mkLit TT.metaLitCharTag (CharVal c))
  -- Blanks shouldn't appear in decompiled terms; fall back to a
  -- placeholder text rather than crash.
  Term.Blank _ -> litTermF (mkLit TT.metaLitTextTag (textVal "<Blank>"))
  Term.Ref r ->
    BoxedVal $ Data1 Meta.termFRef TT.metaTermFRefTag (referenceVal r)
  Term.Constructor cr ->
    BoxedVal $ Data1 Meta.termFRef TT.metaTermFConstructorTag (constructorReferenceVal cr)
  Term.Request cr ->
    BoxedVal $ Data1 Meta.termFRef TT.metaTermFRequestTag (constructorReferenceVal cr)
  Term.Handle h e ->
    BoxedVal $ Data2 Meta.termFRef TT.metaTermFHandleTag (termVal h) (termVal e)
  Term.App f a ->
    BoxedVal $ Data2 Meta.termFRef TT.metaTermFAppTag (termVal f) (termVal a)
  Term.Ann a typ ->
    BoxedVal $ Data2 Meta.termFRef TT.metaTermFAnnTag (termVal a) (typeTermVal typ)
  Term.List xs ->
    BoxedVal $ Data1 Meta.termFRef TT.metaTermFListTag (seqVal (termVal <$> toList xs))
  Term.If c t e ->
    BoxedVal $
      DataG Meta.termFRef TT.metaTermFIfTag (segFromList [termVal c, termVal t, termVal e])
  -- meta.TermF lacks And/Or; lower them to If for fidelity.
  Term.And a b ->
    BoxedVal $
      DataG
        Meta.termFRef
        TT.metaTermFIfTag
        (segFromList [termVal a, termVal b, termVal (Term.boolean () False)])
  Term.Or a b ->
    BoxedVal $
      DataG
        Meta.termFRef
        TT.metaTermFIfTag
        (segFromList [termVal a, termVal (Term.boolean () True), termVal b])
  Term.Lam body ->
    BoxedVal $ Data1 Meta.termFRef TT.metaTermFLamTag (termVal body)
  Term.LetRec _ bs body ->
    BoxedVal $
      Data2
        Meta.termFRef
        TT.metaTermFLetRecTag
        (seqVal (termVal <$> bs))
        (termVal body)
  Term.Let _ b body ->
    BoxedVal $ Data2 Meta.termFRef TT.metaTermFLetTag (termVal b) (termVal body)
  Term.Match scrut cases ->
    BoxedVal $
      Data2
        Meta.termFRef
        TT.metaTermFMatchTag
        (termVal scrut)
        (seqVal (matchCaseVal <$> cases))
  Term.TermLink r ->
    BoxedVal $ Data1 Meta.termFRef TT.metaTermFTermLinkTag (referentVal r)
  Term.TypeLink r ->
    BoxedVal $ Data1 Meta.termFRef TT.metaTermFTypeLinkTag (referenceVal r)
  where
    litTermF litV = BoxedVal $ Data1 Meta.termFRef TT.metaTermFLitTag litV

-- ---------------------------------------------------------------
-- meta.MatchCase a
-- ---------------------------------------------------------------

matchCaseVal :: MatchCase () (Term.Term Symbol ()) -> Val
matchCaseVal (MatchCase pat mguard body) =
  BoxedVal $
    DataG
      Meta.matchCaseRef
      TT.metaMatchCaseTag
      ( segFromList
          [ patternVal pat,
            optionalVal termVal mguard,
            termVal body
          ]
      )

-- ---------------------------------------------------------------
-- meta.Pattern
-- ---------------------------------------------------------------

patternVal :: Pattern () -> Val
patternVal = \case
  Pat.Unbound _ -> BoxedVal $ Enum Meta.patternRef TT.metaPatternPUnboundTag
  Pat.Var _ -> BoxedVal $ Enum Meta.patternRef TT.metaPatternPVarTag
  Pat.Boolean _ b ->
    BoxedVal $ Data1 Meta.patternRef TT.metaPatternPBooleanTag (boolVal b)
  Pat.Int _ i ->
    BoxedVal $ Data1 Meta.patternRef TT.metaPatternPIntTag (IntVal (fromIntegral i))
  Pat.Nat _ n ->
    BoxedVal $ Data1 Meta.patternRef TT.metaPatternPNatTag (NatVal n)
  Pat.Float _ f ->
    BoxedVal $ Data1 Meta.patternRef TT.metaPatternPFloatTag (DoubleVal f)
  Pat.Text _ t ->
    BoxedVal $ Data1 Meta.patternRef TT.metaPatternPTextTag (textVal t)
  Pat.Char _ c ->
    BoxedVal $ Data1 Meta.patternRef TT.metaPatternPCharTag (CharVal c)
  Pat.Bytes _ b ->
    BoxedVal $ Data1 Meta.patternRef TT.metaPatternPBytesTag (bytesVal b)
  Pat.As _ p ->
    BoxedVal $ Data1 Meta.patternRef TT.metaPatternPAsTag (patternVal p)
  Pat.EffectPure _ p ->
    BoxedVal $ Data1 Meta.patternRef TT.metaPatternPEffectPureTag (patternVal p)
  Pat.EffectBind _ (ConstructorReference r cid) ps cont ->
    BoxedVal $
      DataG
        Meta.patternRef
        TT.metaPatternPEffectBindTag
        ( segFromList
            [ referenceVal r,
              NatVal (fromIntegral cid),
              seqVal (patternVal <$> ps),
              patternVal cont
            ]
        )
  Pat.Constructor _ (ConstructorReference r cid) ps ->
    BoxedVal $
      DataG
        Meta.patternRef
        TT.metaPatternPConstructorTag
        ( segFromList
            [ referenceVal r,
              NatVal (fromIntegral cid),
              seqVal (patternVal <$> ps)
            ]
        )
  Pat.SequenceLiteral _ ps ->
    BoxedVal $
      Data1
        Meta.patternRef
        TT.metaPatternPSequenceLiteralTag
        (seqVal (patternVal <$> ps))
  Pat.SequenceOp _ l op r ->
    BoxedVal $
      DataG
        Meta.patternRef
        TT.metaPatternPSequenceOpTag
        ( segFromList
            [ patternVal l,
              seqOpVal op,
              patternVal r
            ]
        )

seqOpVal :: SeqOp -> Val
seqOpVal = \case
  Pat.Cons -> BoxedVal $ Enum Meta.seqOpRef TT.metaSeqOpPConsTag
  Pat.Snoc -> BoxedVal $ Enum Meta.seqOpRef TT.metaSeqOpPSnocTag
  Pat.Concat -> BoxedVal $ Enum Meta.seqOpRef TT.metaSeqOpPConcatTag

-- ---------------------------------------------------------------
-- meta.Term meta.TypeF — Type annotations on terms.
-- ---------------------------------------------------------------

typeTermVal :: Type.Type Symbol () -> Val
typeTermVal ty =
  BoxedVal $
    Data2
      Meta.termRef
      TT.metaTermTermTag
      (freeVarsVal (Type.freeVars ty))
      (typeAbtVal ty)

typeAbtVal :: Type.Type Symbol () -> Val
typeAbtVal ty = case ABT.out ty of
  ABT.Var v ->
    BoxedVal $ Data1 Meta.abtRef TT.metaAbtVarTag (nameVal v)
  ABT.Abs v body ->
    BoxedVal $ Data2 Meta.abtRef TT.metaAbtAbsTag (nameVal v) (typeTermVal body)
  ABT.Cycle body ->
    BoxedVal $ Data1 Meta.abtRef TT.metaAbtCycleTag (typeTermVal body)
  ABT.Tm f ->
    BoxedVal $ Data1 Meta.abtRef TT.metaAbtTmTag (typeFVal f)

typeFVal :: Type.F (Type.Type Symbol ()) -> Val
typeFVal = \case
  Type.Ref r -> BoxedVal $ Data1 Meta.typeFRef TT.metaTypeFRefTag (referenceVal r)
  Type.Arrow a b ->
    BoxedVal $ Data2 Meta.typeFRef TT.metaTypeFArrowTag (typeTermVal a) (typeTermVal b)
  Type.ImplicitArrow a b ->
    BoxedVal $ Data2 Meta.typeFRef TT.metaTypeFImplicitArrowTag (typeTermVal a) (typeTermVal b)
  Type.App a b ->
    BoxedVal $ Data2 Meta.typeFRef TT.metaTypeFAppTag (typeTermVal a) (typeTermVal b)
  Type.Effect a b ->
    BoxedVal $ Data2 Meta.typeFRef TT.metaTypeFEffectTag (typeTermVal a) (typeTermVal b)
  Type.Effects es ->
    BoxedVal $ Data1 Meta.typeFRef TT.metaTypeFEffectsTag (seqVal (typeTermVal <$> es))
  Type.Forall body ->
    BoxedVal $ Data1 Meta.typeFRef TT.metaTypeFForallTag (typeTermVal body)
  Type.IntroOuter body ->
    BoxedVal $ Data1 Meta.typeFRef TT.metaTypeFIntroOuterTag (typeTermVal body)
  -- Type.Ann carries (a, Kind). We don't currently round-trip the
  -- exact Kind structure; placeholder it as KStar.
  Type.Ann a _kind ->
    BoxedVal $ Data2 Meta.typeFRef TT.metaTypeFAnnTag (typeTermVal a) kStarVal

kStarVal :: Val
kStarVal = BoxedVal $ Enum Meta.kindRef TT.metaKindKStarTag

-- ---------------------------------------------------------------
-- Leaf types
-- ---------------------------------------------------------------

nameVal :: Symbol -> Val
nameVal v =
  BoxedVal $ Data1 Meta.nameRef TT.metaNameNameTag (textVal (Var.name v))

referenceVal :: Reference -> Val
referenceVal = \case
  Reference.Builtin t ->
    BoxedVal $ Data1 Meta.referenceRef TT.metaReferenceBuiltinTag (textVal t)
  Reference.DerivedId (Reference.Id h i) ->
    BoxedVal $
      Data2
        Meta.referenceRef
        TT.metaReferenceDerivedTag
        (hashVal h)
        (NatVal (fromIntegral i))

hashVal :: Hash.Hash -> Val
hashVal h =
  BoxedVal $
    Data1
      Meta.hashRef
      TT.metaHashHashTag
      (bytesVal (Bytes.fromByteString (Hash.toByteString h)))

constructorReferenceVal :: GConstructorReference Reference -> Val
constructorReferenceVal (ConstructorReference r cid) =
  BoxedVal $
    Data2
      Meta.constructorReferenceRef
      TT.metaConstructorReferenceTag
      (referenceVal r)
      (NatVal (fromIntegral cid))

referentVal :: Referent -> Val
referentVal = \case
  Referent.Ref r ->
    BoxedVal $ Data1 Meta.referentRef TT.metaReferentRefRefTag (referenceVal r)
  Referent.Con cr _ct ->
    BoxedVal $ Data1 Meta.referentRef TT.metaReferentRefConTag (constructorReferenceVal cr)

-- ---------------------------------------------------------------
-- Small helpers
-- ---------------------------------------------------------------

mkLit :: TT.PackedTag -> Val -> Val
mkLit tag inner = BoxedVal $ Data1 Meta.literalRef tag inner

boolVal :: Bool -> Val
boolVal b
  | b = BoxedVal $ Enum (Reference.Builtin "Boolean") TT.trueTag
  | otherwise = BoxedVal $ Enum (Reference.Builtin "Boolean") TT.falseTag

textVal :: Text -> Val
textVal t = BoxedVal $ Foreign (WrapText (Util.Text.fromText t))

bytesVal :: Bytes.Bytes -> Val
bytesVal b = BoxedVal $ Foreign (WrapBytes b)

seqVal :: [Val] -> Val
seqVal xs = BoxedVal $ Foreign (WrapSeq (Seq.fromList xs))

optionalVal :: (a -> Val) -> Maybe a -> Val
optionalVal _ Nothing =
  BoxedVal $ Enum (Reference.Builtin "Optional") TT.noneTag
optionalVal f (Just a) =
  BoxedVal $ Data1 (Reference.Builtin "Optional") TT.someTag (f a)

-- | Build a meta.Term's cached @Set Name@ field from a Symbol set.
freeVarsVal :: Set Symbol -> Val
freeVarsVal vs =
  let entries =
        [ (nameVal v, BoxedVal (Enum (Reference.Builtin "Unit") TT.unitTag))
        | v <- Set.toList vs
        ]
   in BoxedVal $
        Data1
          (Reference.Builtin "Set")
          TT.setWrapTag
          (BoxedVal (Foreign (WrapMap (Map.fromList entries))))
