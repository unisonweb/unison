module Unison.SyntacticHash
  ( hashType,
    hashTerm,
    hashBuiltinTerm,
    hashDecl,
    hashBuiltinDecl,
  )
where

import Data.Char (ord)
import Unison.ABT qualified as ABT
import Unison.ConstructorReference (GConstructorReference (ConstructorReference))
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration (Decl)
import Unison.DataDeclaration qualified as DD
import Unison.Hash (Hash)
import Unison.HashQualified as HQ
import Unison.Hashable qualified as H
import Unison.Kind qualified as K
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference (Reference' (..), TypeReferenceId)
import Unison.Referent qualified as Referent
import Unison.Syntax.Name qualified as Name (toText)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Var (Var)
import Unison.Var qualified as Var

type Token = H.Token Hash

-- A few tags for readability

isBuiltinTag, isNotBuiltinTag :: H.Token Hash
isBuiltinTag = H.Tag 0
isNotBuiltinTag = H.Tag 1

isDeclTag, isTermTag :: H.Token Hash
isDeclTag = H.Tag 0
isTermTag = H.Tag 1

-- | Syntactically hash a type, using reference names rather than hashes.
-- Two types will have the same syntactic hash if they would
-- print the the same way under the given pretty-print env.
hashType :: Var v => PPE.PrettyPrintEnv -> Type v a -> Hash
hashType ppe t = H.accumulate $ hashTypeTokens ppe t

-- | Syntactically hash a term, using reference names rather than hashes.
-- Two terms will have the same syntactic hash if they would
-- print the the same way under the given pretty-print env.
hashTerm :: Var v => PPE.PrettyPrintEnv -> Term v a -> Hash
hashTerm ppe t = H.accumulate $ isNotBuiltinTag : hashTermTokens ppe t

hashBuiltinTerm :: Text -> Hash
hashBuiltinTerm name =
  H.accumulate [isBuiltinTag, isTermTag, H.Text name]

-- | Syntactically hash a decl, using reference names rather than hashes.
-- Two decls will have the same syntactic hash if they they are
-- the same sort of decl (both are data decls or both are effect decls),
-- the unique type guid is the same, and the constructors appear in the
-- same order and their types have the same syntactic hash.
hashDecl :: Var v => PPE.PrettyPrintEnv -> TypeReferenceId -> Decl v a -> Hash
hashDecl ppe r t = H.accumulate $ isNotBuiltinTag : hashDeclTokens ppe r t

hashBuiltinDecl :: Text -> Hash
hashBuiltinDecl name =
  H.accumulate [isBuiltinTag, isDeclTag, H.Text name]

hashDeclTokens :: Var v => PPE.PrettyPrintEnv -> TypeReferenceId -> Decl v a -> [Token]
hashDeclTokens ppe r = \case
  Left (DD.EffectDeclaration dd) -> H.Tag 0 : go CT.Effect dd
  Right dd -> H.Tag 1 : go CT.Data dd
  where
    goMod = \case
      DD.Structural -> [H.Tag 0]
      DD.Unique txt -> [H.Tag 1, H.Text txt]
    goVs vs =
      H.Nat (fromIntegral (length vs)) : (H.Text . Var.name <$> vs)
    -- separating constructor types with tag of 99, which isn't used elsewhere
    goCtor ct ((_, _, ty), i) = H.Tag 99 : ctorName ct i : hashTypeTokens ppe ty
    ctorName ct i =
      let cr = ConstructorReference (ReferenceDerived r) i
       in H.Text (HQ.toTextWith Name.toText $ PPE.termNameOrHashOnlyFq ppe (Referent.Con cr ct))
    go ct (DD.DataDeclaration mod _ vs ctors) =
      goMod mod <> goVs vs <> ((zip ctors [0 ..]) >>= goCtor ct)

hashKindTokens :: K.Kind -> [Token]
hashKindTokens k = case k of
  K.Star -> [H.Tag 0]
  K.Arrow k1 k2 -> H.Tag 1 : (hashKindTokens k1 <> hashKindTokens k2)

hashTypeTokens :: forall v a. Var v => PPE.PrettyPrintEnv -> Type v a -> [Token]
hashTypeTokens ppe = go
  where
    ft :: Type.F () -> [Token]
    ft = \case
      Type.Ref r -> [H.Tag 0, H.Text (HQ.toTextWith Name.toText $ PPE.typeNameOrHashOnlyFq ppe r)]
      Type.Arrow {} -> [H.Tag 1]
      Type.Ann _ k -> H.Tag 2 : hashKindTokens k
      Type.App {} -> [H.Tag 3]
      Type.Effect {} -> [H.Tag 4]
      Type.Effects es -> [H.Tag 5, H.Nat (fromIntegral (length es))]
      Type.Forall {} -> [H.Tag 6]
      Type.IntroOuter {} -> [H.Tag 7]
    go :: Type v a -> [Token]
    go t =
      H.Tag 254 : case ABT.out t of
        ABT.Var v -> [H.Tag 0, H.Text (Var.name v)]
        -- trick: encode the structure, followed the children as a flat list
        ABT.Tm f -> H.Tag 1 : (ft (void f) <> (toList f >>= go))
        ABT.Cycle c -> H.Tag 2 : go c
        ABT.Abs v body -> H.Tag 3 : H.Text (Var.name v) : go body

hashCaseTokens :: PPE.PrettyPrintEnv -> Term.MatchCase loc a -> [Token]
hashCaseTokens ppe (Term.MatchCase pat Nothing _) = H.Tag 0 : hashPatternTokens ppe pat
hashCaseTokens ppe (Term.MatchCase pat (Just _) _) = H.Tag 1 : hashPatternTokens ppe pat

hashPatternTokens :: PPE.PrettyPrintEnv -> Pattern.Pattern loc -> [Token]
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
    H.Tag 8 : tn : len ps : (ps >>= hashPatternTokens ppe)
    where
      tn = H.Text (HQ.toTextWith Name.toText $ PPE.termNameOrHashOnlyFq ppe (Referent.Con cr CT.Data))
  Pattern.As _ p -> H.Tag 9 : hashPatternTokens ppe p
  Pattern.EffectPure _ p -> H.Tag 10 : hashPatternTokens ppe p
  Pattern.EffectBind _ cr ps k -> H.Tag 11 : tn : len ps : (hashPatternTokens ppe k <> (ps >>= hashPatternTokens ppe))
    where
      tn = H.Text (HQ.toTextWith Name.toText $ PPE.termNameOrHashOnlyFq ppe (Referent.Con cr CT.Effect))
  Pattern.SequenceLiteral _ ps -> H.Tag 12 : len ps : (ps >>= hashPatternTokens ppe)
  Pattern.SequenceOp _ p op q -> H.Tag 16 : top op : (hashPatternTokens ppe p <> hashPatternTokens ppe q)
    where
      top Pattern.Concat = H.Tag 0
      top Pattern.Snoc = H.Tag 1
      top Pattern.Cons = H.Tag 2
  where
    len ps = H.Nat (fromIntegral (length ps))

hashTermTokens :: forall v a. Var v => PPE.PrettyPrintEnv -> Term v a -> [Token]
hashTermTokens ppe = go
  where
    ft :: Term.F v a a () -> [Token]
    ft = \case
      Term.Int n -> [H.Tag 0, H.Int n]
      Term.Nat n -> [H.Tag 1, H.Nat n]
      Term.Float n -> [H.Tag 2, H.Double n]
      Term.Boolean b -> [H.Tag 3, if b then H.Tag 0 else H.Tag 1]
      Term.Text t -> [H.Tag 4, H.Text t]
      Term.Char c -> [H.Tag 5, H.Nat (fromIntegral (ord c))]
      Term.Blank {} -> error "tried to hash a term with blanks, something's very wrong"
      -- note: these are all hashed the same, just based on the name
      Term.Ref r -> [H.Tag 7, H.Text (HQ.toTextWith Name.toText $ PPE.termNameOrHashOnlyFq ppe (Referent.Ref r))]
      Term.Constructor cr -> [H.Tag 7, H.Text (HQ.toTextWith Name.toText $ PPE.termNameOrHashOnlyFq ppe (Referent.Con cr CT.Data))]
      Term.Request cr -> [H.Tag 7, H.Text (HQ.toTextWith Name.toText $ PPE.termNameOrHashOnlyFq ppe (Referent.Con cr CT.Effect))]
      Term.Handle {} -> [H.Tag 8]
      Term.App {} -> [H.Tag 9]
      Term.Ann _ ty -> H.Tag 10 : hashTypeTokens ppe ty
      Term.List xs -> [H.Tag 11, H.Nat (fromIntegral (length xs))]
      Term.If {} -> [H.Tag 12]
      Term.And {} -> [H.Tag 13]
      Term.Or {} -> [H.Tag 14]
      Term.Lam {} -> [H.Tag 15]
      Term.LetRec _ bs _ -> [H.Tag 16, H.Nat (fromIntegral (length bs))]
      Term.Let {} -> [H.Tag 17]
      Term.Match _scrute cases ->
        H.Tag 18 : H.Nat (fromIntegral (length cases)) : (cases >>= hashCaseTokens ppe)
      Term.TermLink rf -> [H.Tag 19, H.Text (HQ.toTextWith Name.toText $ PPE.termNameOrHashOnlyFq ppe rf)]
      Term.TypeLink r -> [H.Tag 20, H.Text (HQ.toTextWith Name.toText $ PPE.typeNameOrHashOnlyFq ppe r)]
    go :: Term v a -> [Token]
    go t =
      H.Tag 255 : case ABT.out t of
        ABT.Var v -> [H.Tag 0, H.Text (Var.name v)]
        -- trick: encode the structure, followed the children as a flat list
        ABT.Tm f -> H.Tag 1 : (ft (void f) <> (toList f >>= go))
        ABT.Cycle c -> H.Tag 2 : go c
        ABT.Abs v body -> H.Tag 3 : H.Text (Var.name v) : go body
