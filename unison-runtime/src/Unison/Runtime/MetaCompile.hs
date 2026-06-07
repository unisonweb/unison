{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Inverse of "Unison.Runtime.MetaDecompile": decode a runtime 'Val'
-- shaped as @meta.Term meta.TermF@ back into a source-level
-- 'Term Symbol ()'.
--
-- This is the substrate for the @Meta.typecheck@ builtin (Step 4 of
-- metaprogramming-design.md). The decoded term is what gets handed
-- to "Unison.Typechecker".
--
-- For the MVP we handle the core shapes — variables, lambdas (with
-- their wrapping @Abs@), applications, references, constructors,
-- requests, literals, lists, term/type links, conditionals, lets,
-- handle, and ann. Match/Pattern/LetRec/Effects/Forall are flagged
-- with explicit @Left@ failures until the typecheck call surface
-- actually needs them.
module Unison.Runtime.MetaCompile
  ( compileTerm,
    typecheckTerm,
    typecheckVal,
  )
where

import Data.Foldable (toList)
import Data.Functor.Identity (runIdentity)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Unison.ABT qualified as ABT
import Unison.Builtin qualified as Builtin
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Result qualified as Result
import Unison.Type (Type)
import Unison.Typechecker (Env (..))
import Unison.Typechecker qualified as Typechecker
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.GivenResolver qualified as GR
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.Hash qualified as Hash
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Runtime.MetaSource qualified as Meta
import Unison.Runtime.Stack
  ( Closure (..),
    Foreign (..),
    Val (..),
    pattern BoxedVal,
    pattern CharVal,
    pattern DoubleVal,
    pattern IntVal,
    pattern NatVal,
  )
import Unison.Runtime.TypeTags qualified as TT
import Unison.Symbol (Symbol)
import Unison.Term (Term)
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
  BoxedVal (DataG ref tag _seg)
    | ref == Meta.termFRef && tag == TT.metaTermFIfTag ->
        -- TODO: decode DataG segment; for now flag as unsupported.
        Left "Meta.compile: If decoding not yet implemented (DataG seg)"
  -- Handle h e
  BoxedVal (Data2 ref tag hVal eVal)
    | ref == Meta.termFRef && tag == TT.metaTermFHandleTag -> do
        h <- decodeMetaTerm hVal
        e <- decodeMetaTerm eVal
        pure (ABT.tm' () (Term.Handle h e))
  -- Ann a (Term TypeF)
  BoxedVal (Data2 ref tag _ _)
    | ref == Meta.termFRef && tag == TT.metaTermFAnnTag ->
        Left "Meta.compile: Ann decoding not yet implemented (needs Term TypeF reader)"
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
  -- LetRec / Match — defer.
  BoxedVal (Data2 ref tag _ _)
    | ref == Meta.termFRef && tag == TT.metaTermFLetRecTag ->
        Left "Meta.compile: LetRec decoding not yet implemented"
  BoxedVal (Data2 ref tag _ _)
    | ref == Meta.termFRef && tag == TT.metaTermFMatchTag ->
        Left "Meta.compile: Match decoding not yet implemented"
  v -> shapeError "meta.TermF" v

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
          | tag == TT.metaLitBytesTag ->
              Left "Meta.compile: Bytes literal decode not yet implemented"
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
-- builtin type environment. Returns either an explanation of the
-- typecheck failure or the inferred 'Type'.
--
-- For the MVP we set up a minimal 'Env' — no TDNR, no ambient
-- abilities, no namespace givens — sufficient to typecheck closed
-- terms whose only free 'Ref's resolve via 'Builtin.typeLookup'.
-- Derived term references (user code) will fail at the typecheck
-- step until we plumb a richer lookup through.
typecheckTerm :: Term Symbol () -> Either Text (Type Symbol ())
typecheckTerm tm =
  let env :: Env Symbol ()
      env =
        Env
          { ambientAbilities = [],
            typeLookup = () <$ Builtin.typeLookup,
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
typecheckVal :: Val -> Either Text (Type Symbol ())
typecheckVal v = compileTerm v >>= typecheckTerm
