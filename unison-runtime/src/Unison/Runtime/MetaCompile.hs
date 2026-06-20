{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Inverse of "Unison.Runtime.MetaDecompile": decode a runtime 'Val'
-- shaped as @meta.Term meta.TermF@ back into a source-level
-- 'Term Symbol ()'.
--
-- This is the substrate for the @Meta.typecheck@ builtin. The
-- decoded term is what gets handed to "Unison.Typechecker".
--
-- Covers every @TermF@/@TypeF@ shape currently in 'MetaSource':
-- variables, lambdas, applications, references, constructors,
-- requests, literals (including bytes), lists, term/type links,
-- conditionals, lets, letrec, match (full pattern coverage),
-- handle, and ann. Anything new added to @MetaSource.TermF@ needs
-- a matching decoder branch here.
module Unison.Runtime.MetaCompile
  ( compileTerm,
    decodeReference,
    typecheckTerm,
    typecheckVal,
  )
where

import Data.Foldable (toList)
import Data.Functor.Identity (runIdentity)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import Unison.ABT qualified as ABT
import Unison.Builtin qualified as Builtin
import Unison.Kind qualified as Kind
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Result qualified as Result
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker (Env (..))
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.GivenResolver qualified as GR
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.Hash qualified as Hash
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Runtime.MetaSource qualified as Meta
import Unison.Pattern (Pattern, SeqOp)
import Unison.Pattern qualified as Pat
import Unison.Runtime.Stack
  ( Closure (..),
    Foreign (..),
    Val (..),
    pattern BoxedVal,
    pattern CharVal,
    pattern DataC,
    pattern DoubleVal,
    pattern IntVal,
    pattern NatVal,
  )
import Unison.Runtime.TypeTags qualified as TT
import Unison.Symbol (Symbol)
import Unison.Term (MatchCase (..), Term)
import Unison.Term qualified as Term
import Unison.Util.Bytes qualified as Bytes
import Unison.Util.Text qualified as Util.Text
import Unison.Var qualified as Var

-- ---------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------

-- | Decode a runtime 'Val' (expected to be a @meta.Term meta.TermF@
-- closure) into a source-level 'Term Symbol ()'.
compileTerm :: Val -> Either Text (Term Symbol ())
compileTerm = decodeMetaTerm

-- ---------------------------------------------------------------
-- meta.Term f wrapper
-- ---------------------------------------------------------------

decodeMetaTerm :: Val -> Either Text (Term Symbol ())
decodeMetaTerm v = case v of
  BoxedVal (Data2 ref tag _freeVars abtVal)
    | ref == Meta.termRef && tag == TT.metaTermTermTag ->
        decodeAbt abtVal
  _ -> shapeError "meta.Term" v

-- ---------------------------------------------------------------
-- meta.ABT f a
--
-- @ABT f a = Var Name | Abs Name a | Cycle a | Tm (f a)@
-- ---------------------------------------------------------------

decodeAbt :: Val -> Either Text (Term Symbol ())
decodeAbt = \case
  BoxedVal (Data1 ref tag nameVal)
    | ref == Meta.abtRef && tag == TT.metaAbtVarTag -> do
        name <- decodeName nameVal
        pure (Term.var () (nameToSymbol name))
  BoxedVal (Data2 ref tag nameVal bodyVal)
    | ref == Meta.abtRef && tag == TT.metaAbtAbsTag -> do
        name <- decodeName nameVal
        body <- decodeMetaTerm bodyVal
        pure (ABT.abs' () (nameToSymbol name) body)
  BoxedVal (Data1 ref tag bodyVal)
    | ref == Meta.abtRef && tag == TT.metaAbtCycleTag -> do
        body <- decodeMetaTerm bodyVal
        pure (ABT.cycle' () body)
  BoxedVal (Data1 ref tag fVal)
    | ref == Meta.abtRef && tag == TT.metaAbtTmTag ->
        decodeTermF fVal
  v -> shapeError "meta.ABT" v

-- ---------------------------------------------------------------
-- meta.TermF — Unison-term base functor.
-- ---------------------------------------------------------------

decodeTermF :: Val -> Either Text (Term Symbol ())
decodeTermF = \case
  -- App fn arg
  BoxedVal (Data2 ref tag fnVal argVal)
    | ref == Meta.termFRef && tag == TT.metaTermFAppTag -> do
        fn <- decodeMetaTerm fnVal
        arg <- decodeMetaTerm argVal
        pure (Term.app () fn arg)
  -- Lam wraps an Abs Term
  BoxedVal (Data1 ref tag absVal)
    | ref == Meta.termFRef && tag == TT.metaTermFLamTag -> do
        absT <- decodeMetaTerm absVal
        pure (ABT.tm' () (Term.Lam absT))
  -- Let bnd body (body is an Abs)
  BoxedVal (Data2 ref tag bndVal absVal)
    | ref == Meta.termFRef && tag == TT.metaTermFLetTag -> do
        bnd <- decodeMetaTerm bndVal
        absT <- decodeMetaTerm absVal
        pure (ABT.tm' () (Term.Let False bnd absT))
  -- If c t e — uses DataG (3 args)
  BoxedVal (DataC ref tag [cVal, tVal, eVal])
    | ref == Meta.termFRef && tag == TT.metaTermFIfTag -> do
        c <- decodeMetaTerm cVal
        t <- decodeMetaTerm tVal
        e <- decodeMetaTerm eVal
        pure (Term.iff () c t e)
  -- Handle h e
  BoxedVal (Data2 ref tag hVal eVal)
    | ref == Meta.termFRef && tag == TT.metaTermFHandleTag -> do
        h <- decodeMetaTerm hVal
        e <- decodeMetaTerm eVal
        pure (ABT.tm' () (Term.Handle h e))
  -- Ann a (Term TypeF)
  BoxedVal (Data2 ref tag tmVal tyVal)
    | ref == Meta.termFRef && tag == TT.metaTermFAnnTag -> do
        tm <- decodeMetaTerm tmVal
        ty <- decodeMetaType tyVal
        pure (Term.ann () tm ty)
  -- Ref Reference
  BoxedVal (Data1 ref tag refVal)
    | ref == Meta.termFRef && tag == TT.metaTermFRefTag -> do
        r <- decodeReference refVal
        pure (Term.ref () r)
  -- Constructor ConstructorReference
  BoxedVal (Data1 ref tag crVal)
    | ref == Meta.termFRef && tag == TT.metaTermFConstructorTag -> do
        cr <- decodeConstructorReference crVal
        pure (Term.constructor () cr)
  -- Request ConstructorReference
  BoxedVal (Data1 ref tag crVal)
    | ref == Meta.termFRef && tag == TT.metaTermFRequestTag -> do
        cr <- decodeConstructorReference crVal
        pure (Term.request () cr)
  -- Lit Literal
  BoxedVal (Data1 ref tag litVal)
    | ref == Meta.termFRef && tag == TT.metaTermFLitTag ->
        decodeLiteral litVal
  -- List [a]
  BoxedVal (Data1 ref tag seqVal)
    | ref == Meta.termFRef && tag == TT.metaTermFListTag -> do
        xs <- decodeList decodeMetaTerm seqVal
        pure (Term.list () xs)
  -- TermLink Referent
  BoxedVal (Data1 ref tag rVal)
    | ref == Meta.termFRef && tag == TT.metaTermFTermLinkTag -> do
        r <- decodeReferent rVal
        pure (Term.termLink () r)
  -- TypeLink Reference
  BoxedVal (Data1 ref tag rVal)
    | ref == Meta.termFRef && tag == TT.metaTermFTypeLinkTag -> do
        r <- decodeReference rVal
        pure (Term.typeLink () r)
  -- LetRec [bindings] body — bindings and body are sub-Terms (the
  -- ABT.Cycle binders live above this LetRec node, so what we get
  -- here is the F-level constructor only).
  BoxedVal (Data2 ref tag bsVal bodyVal)
    | ref == Meta.termFRef && tag == TT.metaTermFLetRecTag -> do
        bs <- decodeList decodeMetaTerm bsVal
        body <- decodeMetaTerm bodyVal
        -- isTop = False is the conservative default; the typechecker
        -- and runtime don't distinguish top vs nested let-rec for
        -- meta-decoded terms.
        pure (ABT.tm' () (Term.LetRec False bs body))
  BoxedVal (Data2 ref tag scrutVal casesVal)
    | ref == Meta.termFRef && tag == TT.metaTermFMatchTag -> do
        scrut <- decodeMetaTerm scrutVal
        cases <- decodeList decodeMatchCase casesVal
        pure (ABT.tm' () (Term.Match scrut cases))
  v -> shapeError "meta.TermF" v

-- ---------------------------------------------------------------
-- meta.Term meta.TypeF — inverse of MetaDecompile.typeTermVal.
--
-- A 'meta.Term meta.TypeF' is the same @Term f = Term (Set Name)
-- (ABT f (Term f))@ wrapper as a meta.Term meta.TermF, just
-- parameterised by 'meta.TypeF' instead of 'meta.TermF'. The decode
-- pipeline mirrors 'decodeMetaTerm'/'decodeAbt'/'decodeTermF' but
-- produces 'Type.Type Symbol ()' values instead of source terms.
-- ---------------------------------------------------------------

decodeMetaType :: Val -> Either Text (Type.Type Symbol ())
decodeMetaType v = case v of
  BoxedVal (Data2 ref tag _freeVars abtVal)
    | ref == Meta.termRef && tag == TT.metaTermTermTag ->
        decodeTypeAbt abtVal
  _ -> shapeError "meta.Term meta.TypeF" v

decodeTypeAbt :: Val -> Either Text (Type.Type Symbol ())
decodeTypeAbt = \case
  BoxedVal (Data1 ref tag nameVal)
    | ref == Meta.abtRef && tag == TT.metaAbtVarTag -> do
        name <- decodeName nameVal
        pure (Type.var () (nameToSymbol name))
  BoxedVal (Data2 ref tag nameVal bodyVal)
    | ref == Meta.abtRef && tag == TT.metaAbtAbsTag -> do
        name <- decodeName nameVal
        body <- decodeMetaType bodyVal
        pure (ABT.abs' () (nameToSymbol name) body)
  BoxedVal (Data1 ref tag bodyVal)
    | ref == Meta.abtRef && tag == TT.metaAbtCycleTag -> do
        body <- decodeMetaType bodyVal
        pure (ABT.cycle' () body)
  BoxedVal (Data1 ref tag fVal)
    | ref == Meta.abtRef && tag == TT.metaAbtTmTag ->
        decodeTypeF fVal
  v -> shapeError "meta.ABT (TypeF)" v

decodeTypeF :: Val -> Either Text (Type.Type Symbol ())
decodeTypeF = \case
  BoxedVal (Data1 ref tag refVal)
    | ref == Meta.typeFRef && tag == TT.metaTypeFRefTag -> do
        r <- decodeReference refVal
        pure (Type.ref () r)
  BoxedVal (Data2 ref tag aVal bVal)
    | ref == Meta.typeFRef && tag == TT.metaTypeFArrowTag -> do
        a <- decodeMetaType aVal
        b <- decodeMetaType bVal
        pure (Type.arrow () a b)
  BoxedVal (Data2 ref tag aVal bVal)
    | ref == Meta.typeFRef && tag == TT.metaTypeFImplicitArrowTag -> do
        a <- decodeMetaType aVal
        b <- decodeMetaType bVal
        pure (ABT.tm' () (Type.ImplicitArrow a b))
  BoxedVal (Data2 ref tag aVal bVal)
    | ref == Meta.typeFRef && tag == TT.metaTypeFAppTag -> do
        a <- decodeMetaType aVal
        b <- decodeMetaType bVal
        pure (Type.app () a b)
  BoxedVal (Data2 ref tag aVal bVal)
    | ref == Meta.typeFRef && tag == TT.metaTypeFEffectTag -> do
        a <- decodeMetaType aVal
        b <- decodeMetaType bVal
        pure (ABT.tm' () (Type.Effect a b))
  BoxedVal (Data1 ref tag seqVal)
    | ref == Meta.typeFRef && tag == TT.metaTypeFEffectsTag -> do
        es <- decodeList decodeMetaType seqVal
        pure (ABT.tm' () (Type.Effects es))
  BoxedVal (Data1 ref tag bodyVal)
    | ref == Meta.typeFRef && tag == TT.metaTypeFForallTag -> do
        body <- decodeMetaType bodyVal
        pure (ABT.tm' () (Type.Forall body))
  BoxedVal (Data1 ref tag bodyVal)
    | ref == Meta.typeFRef && tag == TT.metaTypeFIntroOuterTag -> do
        body <- decodeMetaType bodyVal
        pure (ABT.tm' () (Type.IntroOuter body))
  BoxedVal (Data2 ref tag aVal kVal)
    | ref == Meta.typeFRef && tag == TT.metaTypeFAnnTag -> do
        a <- decodeMetaType aVal
        k <- decodeKind kVal
        pure (ABT.tm' () (Type.Ann a k))
  v -> shapeError "meta.TypeF" v

-- meta.Kind = KStar | KArrow Kind Kind
decodeKind :: Val -> Either Text Kind.Kind
decodeKind = \case
  BoxedVal (Enum ref tag)
    | ref == Meta.kindRef && tag == TT.metaKindKStarTag ->
        pure Kind.Star
  BoxedVal (Data2 ref tag aVal bVal)
    | ref == Meta.kindRef && tag == TT.metaKindKArrowTag -> do
        a <- decodeKind aVal
        b <- decodeKind bVal
        pure (Kind.Arrow a b)
  v -> shapeError "meta.Kind" v

-- ---------------------------------------------------------------
-- meta.MatchCase — inverse of MetaDecompile.matchCaseVal.
-- ---------------------------------------------------------------

decodeMatchCase :: Val -> Either Text (MatchCase () (Term Symbol ()))
decodeMatchCase = \case
  BoxedVal (DataC ref tag [pVal, gVal, bVal])
    | ref == Meta.matchCaseRef && tag == TT.metaMatchCaseTag -> do
        pat <- decodePattern pVal
        guard <- decodeOptional decodeMetaTerm gVal
        body <- decodeMetaTerm bVal
        pure (MatchCase pat guard body)
  v -> shapeError "meta.MatchCase" v

-- ---------------------------------------------------------------
-- meta.Pattern — inverse of MetaDecompile.patternVal.
-- ---------------------------------------------------------------

decodePattern :: Val -> Either Text (Pattern ())
decodePattern = \case
  BoxedVal (Enum ref tag)
    | ref == Meta.patternRef && tag == TT.metaPatternPUnboundTag ->
        pure (Pat.Unbound ())
    | ref == Meta.patternRef && tag == TT.metaPatternPVarTag ->
        pure (Pat.Var ())
  BoxedVal (Data1 ref tag inner)
    | ref == Meta.patternRef ->
        if
          | tag == TT.metaPatternPBooleanTag -> do
              b <- decodeBoolean inner
              pure (Pat.Boolean () b)
          | tag == TT.metaPatternPIntTag, IntVal i <- inner ->
              pure (Pat.Int () (fromIntegral i))
          | tag == TT.metaPatternPNatTag, NatVal n <- inner ->
              pure (Pat.Nat () n)
          | tag == TT.metaPatternPFloatTag, DoubleVal f <- inner ->
              pure (Pat.Float () f)
          | tag == TT.metaPatternPTextTag -> do
              t <- decodeText inner
              pure (Pat.Text () t)
          | tag == TT.metaPatternPCharTag, CharVal c <- inner ->
              pure (Pat.Char () c)
          | tag == TT.metaPatternPBytesTag -> do
              b <- decodeBytes inner
              pure (Pat.Bytes () b)
          | tag == TT.metaPatternPAsTag -> do
              p <- decodePattern inner
              pure (Pat.As () p)
          | tag == TT.metaPatternPEffectPureTag -> do
              p <- decodePattern inner
              pure (Pat.EffectPure () p)
          | tag == TT.metaPatternPSequenceLiteralTag -> do
              ps <- decodeList decodePattern inner
              pure (Pat.SequenceLiteral () ps)
          | otherwise ->
              Left ("Meta.compile: unknown Pattern Data1 tag: " <> Text.pack (show tag))
  BoxedVal (DataC ref tag [rVal, cidVal, psVal])
    | ref == Meta.patternRef && tag == TT.metaPatternPConstructorTag -> do
        r <- decodeReference rVal
        cid <- decodeNat cidVal
        ps <- decodeList decodePattern psVal
        pure (Pat.Constructor () (ConstructorReference r (fromIntegral cid)) ps)
  BoxedVal (DataC ref tag [rVal, cidVal, psVal, contVal])
    | ref == Meta.patternRef && tag == TT.metaPatternPEffectBindTag -> do
        r <- decodeReference rVal
        cid <- decodeNat cidVal
        ps <- decodeList decodePattern psVal
        cont <- decodePattern contVal
        pure (Pat.EffectBind () (ConstructorReference r (fromIntegral cid)) ps cont)
  BoxedVal (DataC ref tag [lVal, opVal, rVal])
    | ref == Meta.patternRef && tag == TT.metaPatternPSequenceOpTag -> do
        l <- decodePattern lVal
        op <- decodeSeqOp opVal
        r <- decodePattern rVal
        pure (Pat.SequenceOp () l op r)
  v -> shapeError "meta.Pattern" v

decodeSeqOp :: Val -> Either Text SeqOp
decodeSeqOp = \case
  BoxedVal (Enum ref tag)
    | ref == Meta.seqOpRef ->
        if
          | tag == TT.metaSeqOpPConsTag -> pure Pat.Cons
          | tag == TT.metaSeqOpPSnocTag -> pure Pat.Snoc
          | tag == TT.metaSeqOpPConcatTag -> pure Pat.Concat
          | otherwise -> Left ("Meta.compile: unknown SeqOp tag: " <> Text.pack (show tag))
  v -> shapeError "meta.SeqOp" v

decodeOptional :: (Val -> Either Text a) -> Val -> Either Text (Maybe a)
decodeOptional f = \case
  BoxedVal (Enum _ tag)
    | tag == TT.noneTag -> pure Nothing
  BoxedVal (Data1 _ tag inner)
    | tag == TT.someTag -> Just <$> f inner
  v -> shapeError "Optional" v

decodeNat :: Val -> Either Text Word64
decodeNat = \case
  NatVal n -> pure n
  v -> shapeError "Nat" v

-- ---------------------------------------------------------------
-- meta.Literal
-- ---------------------------------------------------------------

decodeLiteral :: Val -> Either Text (Term Symbol ())
decodeLiteral = \case
  BoxedVal (Data1 ref tag inner)
    | ref == Meta.literalRef ->
        if
          | tag == TT.metaLitNatTag, NatVal n <- inner -> pure (Term.nat () n)
          | tag == TT.metaLitIntTag, IntVal i <- inner -> pure (Term.int () (fromIntegral i))
          | tag == TT.metaLitFloatTag, DoubleVal f <- inner -> pure (Term.float () f)
          | tag == TT.metaLitCharTag, CharVal c <- inner -> pure (Term.char () c)
          | tag == TT.metaLitBooleanTag -> do
              b <- decodeBoolean inner
              pure (Term.boolean () b)
          | tag == TT.metaLitTextTag -> do
              t <- decodeText inner
              pure (Term.text () t)
          | tag == TT.metaLitBytesTag -> do
              b <- decodeBytes inner
              -- Bytes literals have no dedicated Term constructor;
              -- mirror Decompile.decompileBytes by lowering to a
              -- `Bytes.fromList [n0, n1, …]` call. The typechecker
              -- and runtime both understand this form.
              let nats = Term.list () (Term.nat () . fromIntegral <$> Bytes.toWord8s b)
              pure (Term.app () (Term.builtin () (fromString "Bytes.fromList")) nats)
          | otherwise ->
              Left ("Meta.compile: unknown Literal tag: " <> Text.pack (show tag))
  v -> shapeError "meta.Literal" v

-- ---------------------------------------------------------------
-- Leaf decoders
-- ---------------------------------------------------------------

decodeName :: Val -> Either Text Text
decodeName = \case
  BoxedVal (Data1 ref tag inner)
    | ref == Meta.nameRef && tag == TT.metaNameNameTag -> decodeText inner
  v -> shapeError "meta.Name" v

decodeReference :: Val -> Either Text Reference
decodeReference = \case
  BoxedVal (Data1 ref tag inner)
    | ref == Meta.referenceRef && tag == TT.metaReferenceBuiltinTag -> do
        t <- decodeText inner
        pure (Reference.Builtin t)
  BoxedVal (Data2 ref tag hashVal posVal)
    | ref == Meta.referenceRef && tag == TT.metaReferenceDerivedTag -> do
        h <- decodeHash hashVal
        case posVal of
          NatVal n -> pure (Reference.DerivedId (Reference.Id h n))
          _ -> shapeError "Reference position (Nat)" posVal
  v -> shapeError "meta.Reference" v

decodeHash :: Val -> Either Text Hash.Hash
decodeHash = \case
  BoxedVal (Data1 ref tag inner)
    | ref == Meta.hashRef && tag == TT.metaHashHashTag -> do
        bs <- decodeBytes inner
        pure (Hash.fromByteString (Bytes.toByteString bs))
  v -> shapeError "meta.Hash" v

decodeConstructorReference :: Val -> Either Text (GConstructorReference Reference)
decodeConstructorReference = \case
  BoxedVal (Data2 ref tag rVal posVal)
    | ref == Meta.constructorReferenceRef && tag == TT.metaConstructorReferenceTag -> do
        r <- decodeReference rVal
        case posVal of
          NatVal n -> pure (ConstructorReference r (fromIntegral n))
          _ -> shapeError "ConstructorReference index (Nat)" posVal
  v -> shapeError "meta.ConstructorReference" v

decodeReferent :: Val -> Either Text Referent
decodeReferent = \case
  BoxedVal (Data1 ref tag inner)
    | ref == Meta.referentRef && tag == TT.metaReferentRefRefTag -> do
        r <- decodeReference inner
        pure (Referent.Ref r)
    | ref == Meta.referentRef && tag == TT.metaReferentRefConTag -> do
        cr <- decodeConstructorReference inner
        -- We don't have ConstructorType info; default to Data. The
        -- typechecker mostly cares about the reference, not whether
        -- it's a data ctor vs effect ctor, when looking up types.
        pure (Referent.Con cr CT.Data)
  v -> shapeError "meta.Referent" v

-- ---------------------------------------------------------------
-- Container/primitive helpers
-- ---------------------------------------------------------------

decodeText :: Val -> Either Text Text
decodeText = \case
  BoxedVal (Foreign (WrapText t)) -> pure (Util.Text.toText t)
  v -> shapeError "Text" v

decodeBytes :: Val -> Either Text Bytes.Bytes
decodeBytes = \case
  BoxedVal (Foreign (WrapBytes b)) -> pure b
  v -> shapeError "Bytes" v

decodeBoolean :: Val -> Either Text Bool
decodeBoolean = \case
  BoxedVal (Enum _ tag)
    | tag == TT.trueTag -> pure True
    | tag == TT.falseTag -> pure False
  v -> shapeError "Boolean" v

decodeList :: (Val -> Either Text a) -> Val -> Either Text [a]
decodeList f = \case
  BoxedVal (Foreign (WrapSeq sq)) -> traverse f (toList sq)
  v -> shapeError "List" v

-- ---------------------------------------------------------------
-- Symbol / Var helpers
-- ---------------------------------------------------------------

nameToSymbol :: Text -> Symbol
nameToSymbol = Var.named

-- ---------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------

shapeError :: Text -> Val -> Either Text a
shapeError expected _v =
  Left ("Meta.compile: expected " <> expected <> ", got mismatched runtime shape")

-- ---------------------------------------------------------------
-- Typechecker call
-- ---------------------------------------------------------------

-- | Run Unison's typechecker on a source-level term using only the
-- builtin type environment plus the caller-supplied 'TypeLookup'.
-- Returns either an explanation of the typecheck failure or the
-- inferred 'Type'.
--
-- The caller is responsible for assembling a 'TypeLookup' covering
-- the transitive type/decl dependencies of @tm@ — see
-- 'Unison.Runtime.Interface.typeLookupForMetaTerm' for the canonical
-- IO-side dep walk over a 'CodeLookup'. Builtins are always merged
-- in; pass 'mempty' if the term only references builtins.
--
-- The constructed 'Env' has no TDNR, no ambient abilities, and no
-- namespace givens — sufficient for typechecking fully-elaborated
-- terms that came back through 'Meta.decompile' or were constructed
-- programmatically. Adding any of those is straightforward when a
-- future meta surface needs them.
typecheckTerm :: TL.TypeLookup Symbol () -> Term Symbol () -> Either Text (Type Symbol ())
typecheckTerm extraTL tm =
  let env :: Env Symbol ()
      env =
        Env
          { ambientAbilities = [],
            typeLookup = extraTL <> (() <$ Builtin.typeLookup),
            termsByShortname = Map.empty,
            freeNameToFuzzyTermsByShortName = Map.empty,
            topLevelComponents = Map.empty,
            variances = Map.empty,
            ambientGivens = GR.Pool [],
            givenBindings = Set.empty
          }
      ppe = PPE.empty
      pmccSwitch = Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Enabled
      (maybeTy, notes) =
        runIdentity (Result.runResultT (Typechecker.synthesize ppe pmccSwitch env tm))
   in case maybeTy of
        Just ty -> Right ty
        Nothing -> Left ("Meta.typecheck: failed: " <> Text.pack (show notes))

-- | Combined entry point used by the @MTYC@ primop: decode the
-- runtime 'Val', then run 'typecheckTerm' on the result. Returns
-- the inferred 'Type' on success, or an explanation otherwise.
--
-- The 'TypeLookup' argument extends 'Builtin.typeLookup' with any
-- codebase-resident dependencies the term may reference.
typecheckVal :: TL.TypeLookup Symbol () -> Val -> Either Text (Type Symbol ())
typecheckVal extraTL v = compileTerm v >>= typecheckTerm extraTL
