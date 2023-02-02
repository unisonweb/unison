{-# LANGUAGE DataKinds #-}

-- | Rewrites of some codebase queries, but which check the scratch file for info first.
module Unison.LSP.Queries
  ( getTypeOfReferent,
    getTypeDeclaration,
    refAtPosition,
    nodeAtPosition,
    refInTerm,
    refInType,
    findSmallestEnclosingNode,
    findSmallestEnclosingType,
    refInDecl,
    getTypeOfReferent,
    getTypeDeclaration,
    SourceNode (..),
  )
where

import Control.Lens
import qualified Control.Lens as Lens
import Control.Monad.Reader
import Data.Generics.Product (field)
import Language.LSP.Types
import qualified Unison.ABT as ABT
import qualified Unison.Codebase as Codebase
import Unison.ConstructorReference (GConstructorReference (..))
import qualified Unison.ConstructorType as CT
import Unison.DataDeclaration (Decl)
import qualified Unison.DataDeclaration as DD
import Unison.LSP.Conversions (lspToUPos)
import Unison.LSP.FileAnalysis (getFileSummary)
import Unison.LSP.Orphans ()
import Unison.LSP.Types
import Unison.LabeledDependency
import qualified Unison.LabeledDependency as LD
import Unison.Lexer.Pos (Pos (..))
import Unison.Parser.Ann (Ann)
import qualified Unison.Parser.Ann as Ann
import qualified Unison.Pattern as Pattern
import Unison.Prelude
import Unison.Reference (TypeReference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Symbol (Symbol)
import Unison.Term (MatchCase (MatchCase), Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type

-- | Returns a reference to whatever the symbol at the given position refers to.
refAtPosition :: Uri -> Position -> MaybeT Lsp LabeledDependency
refAtPosition uri pos = do
  findInTermOrType <|> findInDecl
  where
    findInTermOrType :: MaybeT Lsp LabeledDependency
    findInTermOrType =
      nodeAtPosition uri pos >>= \case
        Left term -> hoistMaybe $ refInTerm term
        Right typ -> hoistMaybe $ fmap TypeReference (refInType typ)
    findInDecl :: MaybeT Lsp LabeledDependency
    findInDecl =
      LD.TypeReference <$> do
        let uPos = lspToUPos pos
        (FileSummary {dataDeclsBySymbol, effectDeclsBySymbol}) <- getFileSummary uri
        ( altMap (hoistMaybe . refInDecl uPos . Right . snd) dataDeclsBySymbol
            <|> altMap (hoistMaybe . refInDecl uPos . Left . snd) effectDeclsBySymbol
          )
    hoistMaybe :: Maybe a -> MaybeT Lsp a
    hoistMaybe = MaybeT . pure

-- | Gets the type of a reference from either the parsed file or the codebase.
getTypeOfReferent :: Uri -> Referent -> MaybeT Lsp (Type Symbol Ann)
getTypeOfReferent fileUri ref = do
  getFromFile <|> getFromCodebase
  where
    getFromFile = do
      FileSummary {termsByReference} <- getFileSummary fileUri
      case ref of
        Referent.Ref (Reference.Builtin {}) -> empty
        Referent.Ref (Reference.DerivedId termRefId) -> do
          MaybeT . pure $ (termsByReference ^? ix (Just termRefId) . folded . _2 . _Just)
        Referent.Con (ConstructorReference r0 cid) _type -> do
          case r0 of
            Reference.DerivedId r -> do
              decl <- getTypeDeclaration fileUri r
              MaybeT . pure $ DD.typeOfConstructor (either DD.toDataDecl id decl) cid
            Reference.Builtin _ -> empty
    getFromCodebase = do
      Env {codebase} <- ask
      MaybeT . liftIO $ Codebase.runTransaction codebase $ Codebase.getTypeOfReferent codebase ref

-- | Gets a decl from either the parsed file or the codebase.
getTypeDeclaration :: Uri -> Reference.Id -> MaybeT Lsp (Decl Symbol Ann)
getTypeDeclaration fileUri refId = do
  getFromFile <|> getFromCodebase
  where
    getFromFile :: MaybeT Lsp (Decl Symbol Ann)
    getFromFile = do
      FileSummary {dataDeclsByReference, effectDeclsByReference} <- getFileSummary fileUri
      let datas = dataDeclsByReference ^.. ix refId . folded
      let effects = effectDeclsByReference ^.. ix refId . folded
      MaybeT . pure . listToMaybe $ fmap Right datas <> fmap Left effects

    getFromCodebase = do
      Env {codebase} <- ask
      MaybeT . liftIO $ Codebase.runTransaction codebase $ Codebase.getTypeDeclaration codebase refId

-- | Returns the reference a given term node refers to, if any.
refInTerm :: (Term v a -> Maybe LabeledDependency)
refInTerm term =
  case ABT.out term of
    ABT.Tm f -> case f of
      Term.Int {} -> Nothing
      Term.Nat {} -> Nothing
      Term.Float {} -> Nothing
      Term.Boolean {} -> Nothing
      Term.Text {} -> Nothing
      Term.Char {} -> Nothing
      Term.Blank {} -> Nothing
      Term.Ref ref -> Just (LD.TermReference ref)
      Term.Constructor conRef -> Just (LD.ConReference conRef CT.Data)
      Term.Request conRef -> Just (LD.ConReference conRef CT.Effect)
      Term.Handle _a _b -> Nothing
      Term.App _a _b -> Nothing
      Term.Ann _a _typ -> Nothing
      Term.List _xs -> Nothing
      Term.If _cond _a _b -> Nothing
      Term.And _l _r -> Nothing
      Term.Or _l _r -> Nothing
      Term.Lam _a -> Nothing
      Term.LetRec _isTop _xs _y -> Nothing
      Term.Let _isTop _a _b -> Nothing
      Term.Match _a _cases -> Nothing
      Term.TermLink ref -> Just (LD.TermReferent ref)
      Term.TypeLink ref -> Just (LD.TypeReference ref)
    ABT.Var _v -> Nothing
    ABT.Cycle _r -> Nothing
    ABT.Abs _v _r -> Nothing

-- Returns the reference a given type node refers to, if any.
refInType :: Type v a -> Maybe TypeReference
refInType typ = case ABT.out typ of
  ABT.Tm f -> case f of
    Type.Ref ref -> Just ref
    Type.Arrow _a _b -> Nothing
    Type.Effect _a _b -> Nothing
    Type.App _a _b -> Nothing
    Type.Forall _r -> Nothing
    Type.Ann _a _kind -> Nothing
    Type.Effects _es -> Nothing
    Type.IntroOuter _a -> Nothing
  ABT.Var _v -> Nothing
  ABT.Cycle _r -> Nothing
  ABT.Abs _v _r -> Nothing

data SourceNode a
  = TermNode (Term Symbol a)
  | TypeNode (Type Symbol a)
  | PatternNode (Pattern.Pattern a)
  deriving stock (Eq, Show)

instance Functor SourceNode where
  fmap f (TermNode t) = TermNode (Term.amap f t)
  fmap f (TypeNode t) = TypeNode (fmap f t)
  fmap f (PatternNode t) = PatternNode (fmap f t)

-- | Find the the node in a term which contains the specified position, but none of its
-- children contain that position.
findSmallestEnclosingNode :: Pos -> Term Symbol Ann -> Maybe (SourceNode Ann)
findSmallestEnclosingNode pos term
  | annIsFilePosition (ABT.annotation term) && not (ABT.annotation term `Ann.contains` pos) = Nothing
  | otherwise = do
      -- For leaf nodes we require that they be an in-file position, not Intrinsic or
      -- external.
      -- In some rare cases it's possible for an External/Intrinsic node to have children that
      -- ARE in the file, so we need to make sure we still crawl their children.
      let guardInFile = guard (annIsFilePosition (ABT.annotation term))
      let bestChild = case ABT.out term of
            ABT.Tm f -> case f of
              Term.Int {} -> guardInFile *> Just (TermNode term)
              Term.Nat {} -> guardInFile *> Just (TermNode term)
              Term.Float {} -> guardInFile *> Just (TermNode term)
              Term.Boolean {} -> guardInFile *> Just (TermNode term)
              Term.Text {} -> guardInFile *> Just (TermNode term)
              Term.Char {} -> guardInFile *> Just (TermNode term)
              Term.Blank {} -> guardInFile *> Just (TermNode term)
              Term.Ref {} -> guardInFile *> Just (TermNode term)
              Term.Constructor {} -> guardInFile *> Just (TermNode term)
              Term.Request {} -> guardInFile *> Just (TermNode term)
              Term.Handle a b -> findSmallestEnclosingNode pos a <|> findSmallestEnclosingNode pos b
              Term.App a b ->
                -- We crawl the body of the App first because the annotations for certain
                -- lambda syntaxes get a bit squirrelly.
                -- Specifically Tuple constructor apps will have an annotation which spans the
                -- whole tuple, e.g. the annotation of the tuple constructor for `(1, 2)` will
                -- cover ALL of `(1, 2)`, so we check the body of the tuple app first to see
                -- if the cursor is on 1 or 2 before falling back on the annotation of the
                -- 'function' of the app.
                findSmallestEnclosingNode pos b <|> findSmallestEnclosingNode pos a
              Term.Ann a typ -> findSmallestEnclosingNode pos a <|> (TypeNode <$> findSmallestEnclosingType pos typ)
              Term.List xs -> altSum (findSmallestEnclosingNode pos <$> xs)
              Term.If cond a b -> findSmallestEnclosingNode pos cond <|> findSmallestEnclosingNode pos a <|> findSmallestEnclosingNode pos b
              Term.And l r -> findSmallestEnclosingNode pos l <|> findSmallestEnclosingNode pos r
              Term.Or l r -> findSmallestEnclosingNode pos l <|> findSmallestEnclosingNode pos r
              Term.Lam a -> findSmallestEnclosingNode pos a
              Term.LetRec _isTop xs y -> altSum (findSmallestEnclosingNode pos <$> xs) <|> findSmallestEnclosingNode pos y
              Term.Let _isTop a b -> findSmallestEnclosingNode pos a <|> findSmallestEnclosingNode pos b
              Term.Match a cases ->
                findSmallestEnclosingNode pos a
                  <|> altSum (cases <&> \(MatchCase pat grd body) -> ((PatternNode <$> findSmallestEnclosingPattern pos pat) <|> (grd >>= findSmallestEnclosingNode pos) <|> findSmallestEnclosingNode pos body))
              Term.TermLink {} -> guardInFile *> Just (TermNode term)
              Term.TypeLink {} -> guardInFile *> Just (TermNode term)
            ABT.Var _v -> guardInFile *> Just (TermNode term)
            ABT.Cycle r -> findSmallestEnclosingNode pos r
            ABT.Abs _v r -> findSmallestEnclosingNode pos r
      let fallback = if annIsFilePosition (ABT.annotation term) then Just (TermNode term) else Nothing
      bestChild <|> fallback

findSmallestEnclosingPattern :: Pos -> Pattern.Pattern Ann -> Maybe (Pattern.Pattern Ann)
findSmallestEnclosingPattern pos pat
  | annIsFilePosition (Pattern.annotation pat) && not (Pattern.annotation pat `Ann.contains` pos) = Nothing
  | otherwise = do
      -- For leaf nodes we require that they be an in-file position, not Intrinsic or
      -- external.
      -- In some rare cases it's possible for an External/Intrinsic node to have children that
      -- ARE in the file, so we need to make sure we still crawl their children.
      let guardInFile = guard (annIsFilePosition (Pattern.annotation pat))
      let bestChild = case pat of
            Pattern.Unbound {} -> guardInFile *> Just pat
            Pattern.Var {} -> guardInFile *> Just pat
            Pattern.Boolean {} -> guardInFile *> Just pat
            Pattern.Int {} -> guardInFile *> Just pat
            Pattern.Nat {} -> guardInFile *> Just pat
            Pattern.Float {} -> guardInFile *> Just pat
            Pattern.Text {} -> guardInFile *> Just pat
            Pattern.Char {} -> guardInFile *> Just pat
            Pattern.Constructor _loc _conRef pats -> altSum (findSmallestEnclosingPattern pos <$> pats)
            Pattern.As _loc p -> findSmallestEnclosingPattern pos p
            Pattern.EffectPure _loc p -> findSmallestEnclosingPattern pos p
            Pattern.EffectBind _loc _conRef pats p -> altSum (findSmallestEnclosingPattern pos <$> pats) <|> findSmallestEnclosingPattern pos p
            Pattern.SequenceLiteral _loc pats -> altSum (findSmallestEnclosingPattern pos <$> pats)
            Pattern.SequenceOp _loc p1 _op p2 -> findSmallestEnclosingPattern pos p1 <|> findSmallestEnclosingPattern pos p2
      let fallback = if annIsFilePosition (Pattern.annotation pat) then Just pat else Nothing
      bestChild <|> fallback

-- | Find the the node in a type which contains the specified position, but none of its
-- children contain that position.
-- This is helpful for finding the specific type reference of a given argument within a type arrow
-- that a position references.
findSmallestEnclosingType :: Pos -> Type Symbol Ann -> Maybe (Type Symbol Ann)
findSmallestEnclosingType pos typ
  | annIsFilePosition (ABT.annotation typ) && not (ABT.annotation typ `Ann.contains` pos) = Nothing
  | otherwise = do
      -- For leaf nodes we require that they be an in-file position, not Intrinsic or
      -- external.
      -- In some rare cases it's possible for an External/Intrinsic node to have children that
      -- ARE in the file, so we need to make sure we still crawl their children.
      let guardInFile = guard (annIsFilePosition (ABT.annotation typ))
      let bestChild = case ABT.out typ of
            ABT.Tm f -> case f of
              Type.Ref {} -> guardInFile *> Just typ
              Type.Arrow a b -> findSmallestEnclosingType pos a <|> findSmallestEnclosingType pos b
              Type.Effect effs rhs ->
                -- There's currently a bug in the annotations for effects which cause them to
                -- span larger than they should. As  a workaround for now we just make sure to
                -- search the RHS before the effects.
                findSmallestEnclosingType pos rhs <|> findSmallestEnclosingType pos effs
              Type.App a b -> findSmallestEnclosingType pos a <|> findSmallestEnclosingType pos b
              Type.Forall r -> findSmallestEnclosingType pos r
              Type.Ann a _kind -> findSmallestEnclosingType pos a
              Type.Effects es -> altSum (findSmallestEnclosingType pos <$> es)
              Type.IntroOuter a -> findSmallestEnclosingType pos a
            ABT.Var _v -> guardInFile *> Just typ
            ABT.Cycle r -> findSmallestEnclosingType pos r
            ABT.Abs _v r -> findSmallestEnclosingType pos r
      let fallback = if annIsFilePosition (ABT.annotation typ) then Just typ else Nothing
      bestChild <|> fallback

-- | Returns the type reference the given position applies to within a Decl, if any.
--
-- I.e. if the cursor is over a type reference within a constructor signature or ability
-- request signature, that type reference will be returned.
refInDecl :: Pos -> DD.Decl Symbol Ann -> Maybe TypeReference
refInDecl p (DD.asDataDecl -> dd) =
  DD.constructors' dd
    & altMap \(_conNameAnn, _v, typ) -> do
      typeNode <- findSmallestEnclosingType p typ
      ref <- refInType typeNode
      pure ref

-- | Returns the ABT node at the provided position.
-- Does not return Decl nodes.
nodeAtPosition :: Uri -> Position -> MaybeT Lsp (Either (Term Symbol Ann) (Type Symbol Ann))
nodeAtPosition uri (lspToUPos -> pos) = do
  (FileSummary {termsBySymbol, testWatchSummary, exprWatchSummary}) <- getFileSummary uri

  let (trms, typs) = termsBySymbol & foldMap \(_ref, trm, mayTyp) -> ([trm], toList mayTyp)
  ( altMap (hoistMaybe . findSmallestEnclosingNode pos . removeInferredTypeAnnotations) trms
      <|> altMap (hoistMaybe . findSmallestEnclosingNode pos . removeInferredTypeAnnotations) (testWatchSummary ^.. folded . _3)
      <|> altMap (hoistMaybe . findSmallestEnclosingNode pos . removeInferredTypeAnnotations) (exprWatchSummary ^.. folded . _3)
      <|> altMap (fmap Right . hoistMaybe . findSmallestEnclosingType pos) typs
    )
  where
    hoistMaybe :: Maybe a -> MaybeT Lsp a
    hoistMaybe = MaybeT . pure

annIsFilePosition :: Ann -> Bool
annIsFilePosition = \case
  Ann.Intrinsic -> False
  Ann.External -> False
  Ann.Ann {} -> True

-- | Okay, so currently during synthesis in typechecking the typechecker adds `Ann` nodes
-- to the term specifying types of subterms. This is a problem because we the types in these
-- Ann nodes are just tagged with the full `Ann` from the term it was inferred for, even
-- though none of these types exist in the file, and at a glance we can't tell whether a type
-- is inferred or user-specified.
--
-- So for now we crawl the term and remove any Ann nodes from within. The downside being you
-- can no longer hover on Type signatures within a term, but the benefit is that hover
-- actually works.
removeInferredTypeAnnotations :: Ord v => Term.Term v Ann -> Term.Term v Ann
removeInferredTypeAnnotations =
  Lens.transformOf (field @"out" . traversed) \case
    ABT.Term {out = ABT.Tm (Term.Ann trm typ)}
      -- If the type's annotation is identical to the term's annotation, then this must be an inferred type
      | ABT.annotation typ == ABT.annotation trm -> trm
    t -> t
