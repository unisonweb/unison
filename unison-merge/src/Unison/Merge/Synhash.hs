{-# LANGUAGE DataKinds #-}

-- | Utilities for computing the "syntactic hash" of a decl or term, which is a hash that is computed after substituting
-- references to other terms and decls with names from a pretty-print environment.
--
-- Thus, syntactic hashes can be compared for equality to answer questions like "would these definitions look the same
-- when rendered for a human (even if their underlying references are different)?".
--
-- The merge algorithm currently uses syntactic hashes for determining whether an update was performed by a human, or
-- was the result of auto-propagation. (Critically, this cannot handle renames very well). For example, consider
-- comparing two definitions on Alice's branch; one old one from somewhere in its history, and one new:
--
--   old namespace        new namespace
--   ----------------     ---------------
--   foo = #bar + 3       foo = #bar2 + 3
--
-- Either Alice manually updated #bar to #bar2, or else a dependency of #bar was updated, inducing an update to #bar2.
-- Computing the syntactic hash can help answer that question. Let's combine a pretty-print environment for the old
-- and new namespaces together, substitute references with it, and look again at the terms:
--
--   old namespace        new namespace
--   ----------------     ----------------
--   foo = helper + 3     foo = helper + 3
--
-- We see now that our pretty-print environment has mapped both #bar and #bar2 to the name "helper", so each version of
-- "foo" would have the same syntactic hash. This indicates (to our merge algorithm) that this was an auto-propagated
-- update.
module Unison.Merge.Synhash
  ( synhashType,
    synhashTerm,
    synhashBuiltinDecl,
    synhashDerivedDecl,

    -- * Exported for debugging
    hashBuiltinTermTokens,
    hashDerivedTermTokens,
  )
where

import Data.Char (ord)
import Data.List qualified as List
import Data.Text qualified as Text
import U.Codebase.Reference (TypeReference)
import Unison.ABT qualified as ABT
import Unison.ConstructorType (ConstructorType)
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration (DataDeclaration, Decl)
import Unison.DataDeclaration qualified as DD
import Unison.Hash (Hash)
import Unison.HashQualified as HQ
import Unison.Hashable qualified as H
import Unison.Kind qualified as K
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference (Reference' (..), TypeReferenceId)
import Unison.Reference qualified as V1
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Syntax.Name qualified as Name (toText, unsafeParseVar)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Var (Var)
import Witch (unsafeFrom)

type Token = H.Token Hash

-- A few tags for readability

isBuiltinTag, isNotBuiltinTag :: H.Token Hash
isBuiltinTag = H.Tag 0
isNotBuiltinTag = H.Tag 1

isDeclTag, isTermTag :: H.Token Hash
isDeclTag = H.Tag 0
isTermTag = H.Tag 1

synhashBuiltinDecl :: Text -> Hash
synhashBuiltinDecl name =
  H.accumulate [isBuiltinTag, isDeclTag, H.Text name]

hashBuiltinTerm :: Text -> Hash
hashBuiltinTerm =
  H.accumulate . hashBuiltinTermTokens

hashBuiltinTermTokens :: Text -> [Token]
hashBuiltinTermTokens name =
  [isBuiltinTag, isTermTag, H.Text name]

hashCaseTokens :: PrettyPrintEnv -> Term.MatchCase loc a -> [Token]
hashCaseTokens ppe (Term.MatchCase pat Nothing _) = H.Tag 0 : hashPatternTokens ppe pat
hashCaseTokens ppe (Term.MatchCase pat (Just _) _) = H.Tag 1 : hashPatternTokens ppe pat

-- | The hash of a constructor name determined by how it looks when rendered, i.e. without the decl name prefix.
--
-- For example, in the decl "Maybe" with constructors "Maybe.Just" and "Maybe.Nothing", the hash of constructor
-- "Maybe.Nothing" is hash of the string "Nothing".
hashConstructorNameToken :: Name -> Name -> Token
hashConstructorNameToken declName conName =
  let strippedConName =
        Name.stripNamePrefix declName conName
          & fromMaybe
            ( error $
                reportBug
                  "E784201"
                  ( "constructor "
                      ++ Text.unpack (Name.toText conName)
                      ++ " not under decl "
                      ++ Text.unpack (Name.toText declName)
                  )
            )
   in H.Text (Name.toText strippedConName)

hashDerivedTerm :: Var v => PrettyPrintEnv -> Term v a -> Hash
hashDerivedTerm ppe term =
  H.accumulate (hashDerivedTermTokens ppe term)

hashDerivedTermTokens :: forall a v. Var v => PrettyPrintEnv -> Term v a -> [Token]
hashDerivedTermTokens ppe =
  (isNotBuiltinTag :) . (isTermTag :) . go []
  where
    go :: [v] -> Term v a -> [Token]
    go bound t =
      H.Tag 255 : case ABT.out t of
        ABT.Var v -> [H.Tag 0, hashVarToken bound v]
        -- trick: encode the structure, followed the children as a flat list
        ABT.Tm f -> H.Tag 1 : hashTermFTokens ppe (void f) <> (toList f >>= go bound)
        ABT.Cycle c -> H.Tag 2 : go bound c
        ABT.Abs v body -> H.Tag 3 : go (v : bound) body

hashConstructorType :: ConstructorType -> Token
hashConstructorType = \case
  CT.Effect -> H.Tag 0
  CT.Data -> H.Tag 1

hashDataDeclTokens :: Var v => PrettyPrintEnv -> Name -> DataDeclaration v a -> [Token]
hashDataDeclTokens ppe declName (DD.DataDeclaration modifier _ bound ctors) =
  hashModifierTokens modifier <> (ctors >>= hashConstructorTokens ppe declName bound)

-- separating constructor types with tag of 99, which isn't used elsewhere
hashConstructorTokens :: Var v => PrettyPrintEnv -> Name -> [v] -> (a, v, Type v a) -> [Token]
hashConstructorTokens ppe declName bound (_, conName, ty) =
  H.Tag 99
    : hashConstructorNameToken declName (Name.unsafeParseVar conName)
    : hashTypeTokens ppe bound ty

hashDeclTokens :: Var v => PrettyPrintEnv -> Name -> Decl v a -> [Token]
hashDeclTokens ppe name decl =
  hashConstructorType (DD.constructorType decl) : hashDataDeclTokens ppe name (DD.asDataDecl decl)

-- | Syntactically hash a decl, using reference names rather than hashes. Two decls will have the same syntactic hash if
-- they they are the same sort of decl (both are data decls or both are effect decls), the unique type guid is the same,
-- the constructors appear in the same order and have the same names, and the constructors' types have the same
-- syntactic hashes.
synhashDerivedDecl :: Var v => PrettyPrintEnv -> Name -> Decl v a -> Hash
synhashDerivedDecl ppe name decl =
  H.accumulate $ isNotBuiltinTag : isDeclTag : hashDeclTokens ppe name decl

hashHQNameToken :: HashQualified Name -> Token
hashHQNameToken =
  H.Text . HQ.toTextWith Name.toText

hashKindTokens :: K.Kind -> [Token]
hashKindTokens k = case k of
  K.Star -> [H.Tag 0]
  K.Arrow k1 k2 -> H.Tag 1 : (hashKindTokens k1 <> hashKindTokens k2)

hashLengthToken :: Foldable t => t a -> Token
hashLengthToken =
  H.Nat . fromIntegral @Int @Word64 . length

hashModifierTokens :: DD.Modifier -> [Token]
hashModifierTokens = \case
  DD.Structural -> [H.Tag 0]
  DD.Unique txt -> [H.Tag 1, H.Text txt]

hashPatternTokens :: PrettyPrintEnv -> Pattern.Pattern loc -> [Token]
hashPatternTokens ppe = \case
  Pattern.Unbound {} -> [H.Tag 0]
  Pattern.Var {} -> [H.Tag 1]
  Pattern.Boolean _ b -> [H.Tag 2, if b then H.Tag 0 else H.Tag 1]
  Pattern.Int _ n -> [H.Tag 3, H.Int n]
  Pattern.Nat _ n -> [H.Tag 4, H.Nat n]
  Pattern.Float _ f -> [H.Tag 5, H.Double f]
  Pattern.Text _ t -> [H.Tag 6, H.Text t]
  Pattern.Char _ c -> [H.Tag 7, H.Nat (fromIntegral (ord c))]
  Pattern.Constructor _ cr ps ->
    H.Tag 8
      : hashReferentToken ppe (Referent.Con cr CT.Data)
      : hashLengthToken ps
      : (ps >>= hashPatternTokens ppe)
  Pattern.As _ p -> H.Tag 9 : hashPatternTokens ppe p
  Pattern.EffectPure _ p -> H.Tag 10 : hashPatternTokens ppe p
  Pattern.EffectBind _ cr ps k ->
    H.Tag 11
      : hashReferentToken ppe (Referent.Con cr CT.Effect)
      : hashLengthToken ps
      : hashPatternTokens ppe k <> (ps >>= hashPatternTokens ppe)
  Pattern.SequenceLiteral _ ps -> H.Tag 12 : hashLengthToken ps : (ps >>= hashPatternTokens ppe)
  Pattern.SequenceOp _ p op q -> H.Tag 16 : top op : hashPatternTokens ppe p <> hashPatternTokens ppe q
    where
      top = \case
        Pattern.Concat -> H.Tag 0
        Pattern.Snoc -> H.Tag 1
        Pattern.Cons -> H.Tag 2

hashReferentToken :: PrettyPrintEnv -> Referent -> Token
hashReferentToken ppe =
  hashHQNameToken . PPE.termNameOrHashOnlyFq ppe

synhashTerm ::
  forall m v a.
  (Monad m, Var v) =>
  (TypeReferenceId -> m (Term v a)) ->
  PrettyPrintEnv ->
  V1.TermReference ->
  m Hash
synhashTerm loadTerm ppe = \case
  ReferenceBuiltin builtin -> pure (hashBuiltinTerm builtin)
  ReferenceDerived ref -> hashDerivedTerm ppe <$> loadTerm ref

hashTermFTokens :: Var v => PrettyPrintEnv -> Term.F v a a () -> [Token]
hashTermFTokens ppe = \case
  Term.Int n -> [H.Tag 0, H.Int n]
  Term.Nat n -> [H.Tag 1, H.Nat n]
  Term.Float n -> [H.Tag 2, H.Double n]
  Term.Boolean b -> [H.Tag 3, if b then H.Tag 0 else H.Tag 1]
  Term.Text t -> [H.Tag 4, H.Text t]
  Term.Char c -> [H.Tag 5, H.Nat (fromIntegral (ord c))]
  Term.Blank {} -> error "tried to hash a term with blanks, something's very wrong"
  -- note: these are all hashed the same, just based on the name
  Term.Ref r -> [H.Tag 7, hashReferentToken ppe (Referent.Ref r)]
  Term.Constructor cr -> [H.Tag 7, hashReferentToken ppe (Referent.Con cr CT.Data)]
  Term.Request cr -> [H.Tag 7, hashReferentToken ppe (Referent.Con cr CT.Effect)]
  Term.Handle {} -> [H.Tag 8]
  Term.App {} -> [H.Tag 9]
  Term.Ann _ ty -> H.Tag 10 : hashTypeTokens ppe [] ty
  Term.List xs -> [H.Tag 11, hashLengthToken xs]
  Term.If {} -> [H.Tag 12]
  Term.And {} -> [H.Tag 13]
  Term.Or {} -> [H.Tag 14]
  Term.Lam {} -> [H.Tag 15]
  Term.LetRec _ bs _ -> [H.Tag 16, hashLengthToken bs]
  Term.Let {} -> [H.Tag 17]
  Term.Match _scrute cases ->
    H.Tag 18 : hashLengthToken cases : (cases >>= hashCaseTokens ppe)
  Term.TermLink rf -> [H.Tag 19, hashReferentToken ppe rf]
  Term.TypeLink r -> [H.Tag 20, hashTypeReferenceToken ppe r]

-- | Syntactically hash a type, using reference names rather than hashes.
-- Two types will have the same syntactic hash if they would
-- print the the same way under the given pretty-print env.
synhashType :: Var v => PrettyPrintEnv -> Type v a -> Hash
synhashType ppe ty =
  H.accumulate $ hashTypeTokens ppe [] ty

hashTypeTokens :: forall v a. Var v => PrettyPrintEnv -> [v] -> Type v a -> [Token]
hashTypeTokens ppe = go
  where
    go :: [v] -> Type v a -> [Token]
    go bound t =
      H.Tag 254 : case ABT.out t of
        ABT.Var v -> [H.Tag 0, hashVarToken bound v]
        -- trick: encode the structure, followed the children as a flat list
        ABT.Tm f -> H.Tag 1 : (hashTypeFTokens ppe (void f) <> (toList f >>= go bound))
        ABT.Cycle c -> H.Tag 2 : go bound c
        ABT.Abs v body -> H.Tag 3 : go (v : bound) body

hashTypeFTokens :: PrettyPrintEnv -> Type.F () -> [Token]
hashTypeFTokens ppe = \case
  Type.Ref r -> [H.Tag 0, hashTypeReferenceToken ppe r]
  Type.Arrow {} -> [H.Tag 1]
  Type.Ann _ k -> H.Tag 2 : hashKindTokens k
  Type.App {} -> [H.Tag 3]
  Type.Effect {} -> [H.Tag 4]
  Type.Effects es -> [H.Tag 5, hashLengthToken es]
  Type.Forall {} -> [H.Tag 6]
  Type.IntroOuter {} -> [H.Tag 7]

hashTypeReferenceToken :: PrettyPrintEnv -> TypeReference -> Token
hashTypeReferenceToken ppe =
  hashHQNameToken . PPE.typeNameOrHashOnlyFq ppe

hashVarToken :: Var v => [v] -> v -> Token
hashVarToken bound v =
  case List.elemIndex v bound of
    Nothing -> error (reportBug "E633940" ("var " ++ show v ++ " not bound in " ++ show bound))
    Just index -> H.Nat (unsafeFrom @Int @Word64 index)
