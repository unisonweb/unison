{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Builtin Unison source for the meta-programming "tree" types
-- (the @meta/main@ project's @Term@, @TermF@, @TypeF@, @ABT@, and
-- supporting leaf types).
--
-- This module mirrors @Unison.Runtime.IOSource@. It embeds a Unison
-- source string carrying the 14 meta types with @unique[guid]@
-- annotations pinning their hashes to match what @meta/main@ has
-- on disk. Parsing + typechecking happens at runtime startup; the
-- @typeNamed@/@constructorNamed@ helpers expose the resulting
-- 'Reference's and 'ConstructorId's for the @decompile@ builtin
-- (and any other runtime-side machinery that needs to construct
-- values of these types).
--
-- ⚠️  HASH STABILITY — DO NOT CASUALLY EDIT  ⚠️
-- Each @unique[…]@ literal below is the GUID that was assigned to
-- that type when it was first @add@-ed to the @meta/main@ project.
-- Changing the GUID or the structural shape of a type changes the
-- hash, which immediately invalidates every reference in any
-- codebase that already has these types. If you need to evolve
-- one of these types, do the change in @meta/main@, capture the
-- new GUID via @debug.type.abt@, and update the corresponding
-- entry below in lockstep.
module Unison.Runtime.MetaSource where

import Control.Monad.Morph (hoist)
import Data.List (elemIndex)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Text.RawString.QQ (r)
import Unison.Builtin qualified as Builtin
import Unison.Codebase.CodeLookup (CodeLookup (..))
import Unison.Codebase.CodeLookup.Util qualified as CL
import Unison.DataDeclaration qualified as DD
import Unison.DataDeclaration.ConstructorId qualified as DD
import Unison.FileParsers (ShouldUseTndr (..), computeTypecheckingEnvironment, synthesizeFile)
import Unison.Parser.Ann (Ann (..))
import Unison.Parsers qualified as Parsers
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrintError qualified as PrintError
import Unison.Reference qualified as R
import Unison.Result qualified as Result
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser qualified as Parser
import Unison.Typechecker qualified as Typechecker
import Unison.UnisonFile qualified as UF
import Unison.Util.Monoid (intercalateMap)
import Unison.Util.Pretty qualified as Pretty
import Unison.Var qualified as Var

parsingEnv :: Parser.ParsingEnv Identity
parsingEnv =
  Parser.ParsingEnv
    { uniqueNames = mempty,
      uniqueTypeGuid = \_ -> pure Nothing,
      names = Builtin.names,
      maybeNamespace = Nothing,
      localNamespacePrefixedTypesAndConstructors = mempty
    }

typecheckingEnv :: Typechecker.Env Symbol Ann
typecheckingEnv =
  runIdentity do
    computeTypecheckingEnvironment
      (ShouldUseTndr'Yes parsingEnv)
      []
      (\_ -> pure (External <$ Builtin.typeLookup))
      []
      parsedFile

parsedFile :: UF.UnisonFile Symbol Ann
parsedFile =
  case runIdentity (Parsers.parseFile "<meta builtin>" sourceString parsingEnv) of
    Left err -> error (Text.unpack $ Pretty.toANSI 0 (PrintError.prettyParseError sourceString err))
    Right file -> file

typecheckedFile :: UF.TypecheckedUnisonFile Symbol Ann
typecheckedFile =
  case synthesizeFile typecheckingEnv parsedFile of
    Result.Result notes Nothing -> error (showNotes sourceString ppEnv notes)
    Result.Result _ (Just file) -> file

codeLookup :: CodeLookup Symbol Identity Ann
codeLookup = CL.fromTypecheckedUnisonFile typecheckedFile

codeLookupM :: (Applicative m) => CodeLookup Symbol m Ann
codeLookupM = hoist (pure . runIdentity) codeLookup

typeNamedId :: String -> R.Id
typeNamedId s =
  case Map.lookup (Var.nameds s) (UF.dataDeclarationsId' typecheckedFile) of
    Nothing -> error $ "No meta-builtin type called: " <> s
    Just (rid, _) -> rid

typeNamed :: String -> R.Reference
typeNamed = R.DerivedId . typeNamedId

constructorNamed :: R.Reference -> Text -> DD.ConstructorId
constructorNamed ref name =
  case runIdentity . getTypeDeclaration codeLookup $ R.unsafeId ref of
    Nothing ->
      error $
        "Unison runtime bug. Couldn't find meta type "
          <> show ref
    Just decl ->
      fromIntegral
        . fromMaybe
          ( error $
              "Unison runtime bug. The meta type "
                <> show ref
                <> " has no constructor named "
                <> show name
          )
        . elemIndex name
        . DD.constructorNames
        $ DD.asDataDecl decl

-- ---------------------------------------------------------------
-- Type references
-- ---------------------------------------------------------------

nameRef, hashRef, referenceRef, constructorReferenceRef, referentRef :: R.Reference
nameRef = typeNamed "Name"
hashRef = typeNamed "Hash"
referenceRef = typeNamed "Reference"
constructorReferenceRef = typeNamed "ConstructorReference"
referentRef = typeNamed "Referent"

literalRef, seqOpRef, patternRef, kindRef, matchCaseRef :: R.Reference
literalRef = typeNamed "Literal"
seqOpRef = typeNamed "SeqOp"
patternRef = typeNamed "Pattern"
kindRef = typeNamed "Kind"
matchCaseRef = typeNamed "MatchCase"

abtRef, termRef, termFRef, typeFRef :: R.Reference
abtRef = typeNamed "ABT"
termRef = typeNamed "Term"
termFRef = typeNamed "TermF"
typeFRef = typeNamed "TypeF"

-- | All 14 meta-type references in declaration order. Exposed so
-- 'Unison.Runtime.Builtin.Types' can fold them into the runtime's
-- @builtinTypeNumbering@ map (which is what @mkTags@/@packTags@
-- in "Unison.Runtime.TypeTags" consult).
typeReferences :: [R.Reference]
typeReferences =
  [ nameRef,
    hashRef,
    referenceRef,
    constructorReferenceRef,
    referentRef,
    literalRef,
    seqOpRef,
    patternRef,
    kindRef,
    matchCaseRef,
    abtRef,
    termRef,
    termFRef,
    typeFRef
  ]

-- ---------------------------------------------------------------
-- Constructor IDs (looked up by name; canonical-order-agnostic).
-- ---------------------------------------------------------------

nameNameId :: DD.ConstructorId
nameNameId = constructorNamed nameRef "Name.Name"

hashHashId :: DD.ConstructorId
hashHashId = constructorNamed hashRef "Hash.Hash"

referenceBuiltinId, referenceDerivedId :: DD.ConstructorId
referenceBuiltinId = constructorNamed referenceRef "Reference.ReferenceBuiltin"
referenceDerivedId = constructorNamed referenceRef "Reference.ReferenceDerived"

constructorReferenceCtorId :: DD.ConstructorId
constructorReferenceCtorId =
  constructorNamed constructorReferenceRef "ConstructorReference.ConstructorReference"

referentRefRefId, referentRefConId :: DD.ConstructorId
referentRefRefId = constructorNamed referentRef "Referent.RefRef"
referentRefConId = constructorNamed referentRef "Referent.RefCon"

litNatId, litIntId, litTextId, litCharId, litFloatId, litBooleanId, litBytesId :: DD.ConstructorId
litNatId = constructorNamed literalRef "Literal.LitNat"
litIntId = constructorNamed literalRef "Literal.LitInt"
litTextId = constructorNamed literalRef "Literal.LitText"
litCharId = constructorNamed literalRef "Literal.LitChar"
litFloatId = constructorNamed literalRef "Literal.LitFloat"
litBooleanId = constructorNamed literalRef "Literal.LitBoolean"
litBytesId = constructorNamed literalRef "Literal.LitBytes"

seqOpPConsId, seqOpPSnocId, seqOpPConcatId :: DD.ConstructorId
seqOpPConsId = constructorNamed seqOpRef "SeqOp.PCons"
seqOpPSnocId = constructorNamed seqOpRef "SeqOp.PSnoc"
seqOpPConcatId = constructorNamed seqOpRef "SeqOp.PConcat"

patternPUnboundId,
  patternPVarId,
  patternPBooleanId,
  patternPIntId,
  patternPNatId,
  patternPFloatId,
  patternPTextId,
  patternPCharId,
  patternPBytesId ::
    DD.ConstructorId
patternPUnboundId = constructorNamed patternRef "Pattern.PUnbound"
patternPVarId = constructorNamed patternRef "Pattern.PVar"
patternPBooleanId = constructorNamed patternRef "Pattern.PBoolean"
patternPIntId = constructorNamed patternRef "Pattern.PInt"
patternPNatId = constructorNamed patternRef "Pattern.PNat"
patternPFloatId = constructorNamed patternRef "Pattern.PFloat"
patternPTextId = constructorNamed patternRef "Pattern.PText"
patternPCharId = constructorNamed patternRef "Pattern.PChar"
patternPBytesId = constructorNamed patternRef "Pattern.PBytes"

patternPConstructorId,
  patternPAsId,
  patternPEffectPureId,
  patternPEffectBindId,
  patternPSequenceLiteralId,
  patternPSequenceOpId ::
    DD.ConstructorId
patternPConstructorId = constructorNamed patternRef "Pattern.PConstructor"
patternPAsId = constructorNamed patternRef "Pattern.PAs"
patternPEffectPureId = constructorNamed patternRef "Pattern.PEffectPure"
patternPEffectBindId = constructorNamed patternRef "Pattern.PEffectBind"
patternPSequenceLiteralId = constructorNamed patternRef "Pattern.PSequenceLiteral"
patternPSequenceOpId = constructorNamed patternRef "Pattern.PSequenceOp"

kindKStarId, kindKArrowId :: DD.ConstructorId
kindKStarId = constructorNamed kindRef "Kind.KStar"
kindKArrowId = constructorNamed kindRef "Kind.KArrow"

matchCaseCtorId :: DD.ConstructorId
matchCaseCtorId = constructorNamed matchCaseRef "MatchCase.MatchCase"

abtVarId, abtAbsId, abtCycleId, abtTmId :: DD.ConstructorId
abtVarId = constructorNamed abtRef "ABT.Var"
abtAbsId = constructorNamed abtRef "ABT.Abs"
abtCycleId = constructorNamed abtRef "ABT.Cycle"
abtTmId = constructorNamed abtRef "ABT.Tm"

termTermId :: DD.ConstructorId
termTermId = constructorNamed termRef "Term.Term"

termFAppId,
  termFLamId,
  termFLetId,
  termFLetRecId,
  termFIfId,
  termFMatchId,
  termFHandleId,
  termFAnnId ::
    DD.ConstructorId
termFAppId = constructorNamed termFRef "TermF.App"
termFLamId = constructorNamed termFRef "TermF.Lam"
termFLetId = constructorNamed termFRef "TermF.Let"
termFLetRecId = constructorNamed termFRef "TermF.LetRec"
termFIfId = constructorNamed termFRef "TermF.If"
termFMatchId = constructorNamed termFRef "TermF.Match"
termFHandleId = constructorNamed termFRef "TermF.Handle"
termFAnnId = constructorNamed termFRef "TermF.Ann"

termFRefId,
  termFConstructorId,
  termFRequestId,
  termFLitId,
  termFListId,
  termFTermLinkId,
  termFTypeLinkId ::
    DD.ConstructorId
termFRefId = constructorNamed termFRef "TermF.Ref"
termFConstructorId = constructorNamed termFRef "TermF.Constructor"
termFRequestId = constructorNamed termFRef "TermF.Request"
termFLitId = constructorNamed termFRef "TermF.Lit"
termFListId = constructorNamed termFRef "TermF.List"
termFTermLinkId = constructorNamed termFRef "TermF.TermLink"
termFTypeLinkId = constructorNamed termFRef "TermF.TypeLink"

typeFArrowId,
  typeFImplicitArrowId,
  typeFAppId,
  typeFEffectId,
  typeFEffectsId,
  typeFForallId,
  typeFIntroOuterId,
  typeFRefId,
  typeFAnnId ::
    DD.ConstructorId
typeFArrowId = constructorNamed typeFRef "TypeF.Arrow"
typeFImplicitArrowId = constructorNamed typeFRef "TypeF.ImplicitArrow"
typeFAppId = constructorNamed typeFRef "TypeF.App"
typeFEffectId = constructorNamed typeFRef "TypeF.Effect"
typeFEffectsId = constructorNamed typeFRef "TypeF.Effects"
typeFForallId = constructorNamed typeFRef "TypeF.Forall"
typeFIntroOuterId = constructorNamed typeFRef "TypeF.IntroOuter"
typeFRefId = constructorNamed typeFRef "TypeF.Ref"
typeFAnnId = constructorNamed typeFRef "TypeF.Ann"

-- ---------------------------------------------------------------
-- Note formatting (for parse / typecheck errors).
-- ---------------------------------------------------------------

type Note = Result.Note Symbol Ann

showNotes :: (Foldable f) => String -> PrintError.Env -> f Note -> String
showNotes src env notes =
  Text.unpack $ intercalateMap "\n\n" (PrintError.renderNoteAsANSI 60 env src) notes

ppEnv :: PPE.PrettyPrintEnv
ppEnv = PPE.makePPE (PPE.hqNamer 10 Builtin.names) PPE.dontSuffixify

-- ---------------------------------------------------------------
-- The Unison source. Hashes are pinned by the @unique[…]@ literals;
-- see the warning at the top of this module before editing.
-- ---------------------------------------------------------------

sourceString :: String
sourceString = Text.unpack source

source :: Text
source =
  fromString
    [r|

unique[t2fl4li6s59vha7q9ccq9gtose5ue420] type Name = Name Text

unique[evog4t6mvrq0tcpclc161g0107k0vo3e] type Hash = Hash Bytes

unique[maf6nft0n3eaci4s0m6jdmbiau9c31cf] type Reference
  = ReferenceBuiltin Text
  | ReferenceDerived Hash Nat

unique[lh68ps3ira5hrd3ku95d0gr9596utmis] type ConstructorReference =
  ConstructorReference Reference Nat

unique[h86vovm1ssp5ouuians13rqvjir9gu6g] type Referent
  = RefRef Reference
  | RefCon ConstructorReference

unique[hh3e1jv3rc1d6s10fghha314embgfg62] type Literal
  = LitNat Nat
  | LitInt Int
  | LitText Text
  | LitChar Char
  | LitFloat Float
  | LitBoolean Boolean
  | LitBytes Bytes

unique[e5j087ioc7sfa7ul1epkmrchbsta4udp] type SeqOp = PCons | PSnoc | PConcat

unique[fhev77u0iqgr0uuqfa5kbba2cnvfbrb6] type Pattern
  = PUnbound
  | PVar
  | PBoolean Boolean
  | PInt Int
  | PNat Nat
  | PFloat Float
  | PText Text
  | PChar Char
  | PBytes Bytes
  | PConstructor Reference Nat [Pattern]
  | PAs Pattern
  | PEffectPure Pattern
  | PEffectBind Reference Nat [Pattern] Pattern
  | PSequenceLiteral [Pattern]
  | PSequenceOp Pattern SeqOp Pattern

unique[g97m4j0ovcl5s1ptllnvkrqi54ioikdj] type Kind
  = KStar
  | KArrow Kind Kind

unique[v4okrq0ttha7if61102qsl69rk50sm54] type MatchCase a =
  MatchCase Pattern (Optional a) a

unique[vf7u61ih3hqiie7tcllu65j97jhv1hvc] type ABT f a
  = Var Name
  | Abs Name a
  | Cycle a
  | Tm (f a)

unique[h18fbo09gcagjvueom878lurtosfq5q0] type Term f =
  Term (Set Name) (ABT f (Term f))

unique[a3c5nm68h7v8hb8ngbb87fgijjp16nha] type TermF a
  = App a a
  | Lam a
  | Let a a
  | LetRec [a] a
  | If a a a
  | Match a [MatchCase a]
  | Handle a a
  | Ann a (Term TypeF)
  | Ref Reference
  | Constructor ConstructorReference
  | Request ConstructorReference
  | Lit Literal
  | List [a]
  | TermLink Referent
  | TypeLink Reference

unique[jju6gn1j4easdp7sf3681ui4iha1fhde] type TypeF a
  = Arrow a a
  | ImplicitArrow a a
  | App a a
  | Effect a a
  | Effects [a]
  | Forall a
  | IntroOuter a
  | Ref Reference
  | Ann a Kind

|]
