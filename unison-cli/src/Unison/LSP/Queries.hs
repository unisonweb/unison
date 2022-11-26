-- | Rewrites of some codebase queries, but which check the scratch file for info first.
module Unison.LSP.Queries
  ( getTypeOfReferent,
    getTypeDeclaration,
    refAtPosition,
  )
where

import Control.Lens
import Control.Monad.Reader
import qualified Data.Set as Set
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
import Unison.Prelude
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Symbol (Symbol)
import Unison.Term (MatchCase (MatchCase), Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Util.Relation as R
import qualified Unison.Util.Relation3 as R3
import qualified Unison.Util.Relation4 as R4

-- | Gets the type of a reference from either the parsed file or the codebase.
getTypeOfReferent :: Uri -> Referent -> MaybeT Lsp (Type Symbol Ann)
getTypeOfReferent fileUri ref = do
  getFromFile <|> getFromCodebase
  where
    getFromFile = do
      FileSummary {termSummary} <- getFileSummary fileUri
      case ref of
        Referent.Ref (Reference.Builtin {}) -> empty
        Referent.Ref (Reference.DerivedId termRefId) ->
          do
            termSummary
            & R4.lookupD2 (Just termRefId)
            & R3.d3s
            & Set.toList
            & fmap (MaybeT . pure)
            & altSum
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
      FileSummary {dataDeclSummary, effectDeclSummary} <- getFileSummary fileUri
      let datas = Set.toList . R.ran $ R3.lookupD2 refId dataDeclSummary
      let effects = Set.toList . R.ran $ R3.lookupD2 refId effectDeclSummary
      MaybeT . pure . listToMaybe $ fmap Right datas <> fmap Left effects

    getFromCodebase = do
      Env {codebase} <- ask
      MaybeT . liftIO $ Codebase.runTransaction codebase $ Codebase.getTypeDeclaration codebase refId

-- | Returns a reference to whatever the symbol at the given position refers to.
refAtPosition :: Uri -> Position -> MaybeT Lsp LabeledDependency
refAtPosition uri (lspToUPos -> pos) = do
  (FileSummary {dataDeclSummary, effectDeclSummary, termSummary, testWatchSummary, exprWatchSummary}) <- getFileSummary uri
  ( altMap (hoistMaybe . refInDecl pos . Right) (R3.d3s dataDeclSummary)
      <|> altMap (hoistMaybe . refInDecl pos . Left) (R3.d3s effectDeclSummary)
      <|> altMap findRefInTerm (R4.d3s termSummary)
      <|> altMap findRefInTerm (testWatchSummary ^.. folded . _3)
      <|> altMap findRefInTerm (exprWatchSummary ^.. folded . _3)
    )
  where
    hoistMaybe :: Maybe a -> MaybeT Lsp a
    hoistMaybe = MaybeT . pure
    findRefInTerm :: Term Symbol Ann -> MaybeT Lsp LabeledDependency
    findRefInTerm term = do
      hoistMaybe (findSmallestEnclosingNode pos term) >>= \case
        Left term -> hoistMaybe $ refInTerm term
        Right typ -> hoistMaybe $ refInType typ

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

refInType :: Type v a -> Maybe LabeledDependency
refInType typ = case ABT.out typ of
  ABT.Tm f -> case f of
    Type.Ref ref -> Just (LD.TypeReference ref)
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

-- | Find the the node in a term which contains the specified position, but none of its
-- children contain that position.
findSmallestEnclosingNode :: Pos -> Term v Ann -> Maybe (Either (Term v Ann) (Type v Ann))
findSmallestEnclosingNode pos term
  | not (ABT.annotation term `Ann.contains` pos) = Nothing
  | otherwise = (<|> Just (Left term)) $ do
      case ABT.out term of
        ABT.Tm f -> case f of
          Term.Int {} -> Just (Left term)
          Term.Nat {} -> Just (Left term)
          Term.Float {} -> Just (Left term)
          Term.Boolean {} -> Just (Left term)
          Term.Text {} -> Just (Left term)
          Term.Char {} -> Just (Left term)
          Term.Blank {} -> Just (Left term)
          Term.Ref {} -> Just (Left term)
          Term.Constructor {} -> Just (Left term)
          Term.Request {} -> Just (Left term)
          Term.Handle a b -> findSmallestEnclosingNode pos a <|> findSmallestEnclosingNode pos b
          Term.App a b -> findSmallestEnclosingNode pos a <|> findSmallestEnclosingNode pos b
          Term.Ann a typ -> findSmallestEnclosingNode pos a <|> (Right <$> findSmallestEnclosingType pos typ)
          Term.List xs -> altSum (findSmallestEnclosingNode pos <$> xs)
          Term.If cond a b -> findSmallestEnclosingNode pos cond <|> findSmallestEnclosingNode pos a <|> findSmallestEnclosingNode pos b
          Term.And l r -> findSmallestEnclosingNode pos l <|> findSmallestEnclosingNode pos r
          Term.Or l r -> findSmallestEnclosingNode pos l <|> findSmallestEnclosingNode pos r
          Term.Lam a -> findSmallestEnclosingNode pos a
          Term.LetRec _isTop xs y -> altSum (findSmallestEnclosingNode pos <$> xs) <|> findSmallestEnclosingNode pos y
          Term.Let _isTop a b -> findSmallestEnclosingNode pos a <|> findSmallestEnclosingNode pos b
          Term.Match a cases ->
            findSmallestEnclosingNode pos a
              <|> altSum (cases <&> \(MatchCase _pat grd body) -> altSum (findSmallestEnclosingNode pos <$> grd) <|> findSmallestEnclosingNode pos body)
          Term.TermLink {} -> Just (Left term)
          Term.TypeLink {} -> Just (Left term)
        ABT.Var _v -> Just (Left term)
        ABT.Cycle r -> findSmallestEnclosingNode pos r
        ABT.Abs _v r -> findSmallestEnclosingNode pos r

findSmallestEnclosingType :: Pos -> Type v Ann -> Maybe (Type v Ann)
findSmallestEnclosingType pos typ
  | not (ABT.annotation typ `Ann.contains` pos) = Nothing
  | otherwise = (<|> Just typ) $ do
      case ABT.out typ of
        ABT.Tm f -> case f of
          Type.Ref {} -> Just typ
          Type.Arrow a b -> findSmallestEnclosingType pos a <|> findSmallestEnclosingType pos b
          Type.Effect a b -> findSmallestEnclosingType pos a <|> findSmallestEnclosingType pos b
          Type.App a b -> findSmallestEnclosingType pos a <|> findSmallestEnclosingType pos b
          Type.Forall r -> findSmallestEnclosingType pos r
          Type.Ann a _kind -> findSmallestEnclosingType pos a
          Type.Effects es -> altSum (findSmallestEnclosingType pos <$> es)
          Type.IntroOuter a -> findSmallestEnclosingType pos a
        ABT.Var _v -> Just typ
        ABT.Cycle r -> findSmallestEnclosingType pos r
        ABT.Abs _v r -> findSmallestEnclosingType pos r

refInDecl :: Pos -> DD.Decl v Ann -> Maybe LabeledDependency
refInDecl p (DD.asDataDecl -> dd)
  | not (DD.annotation dd `Ann.contains` p) =
      Nothing
  | otherwise =
      DD.constructors' dd
        & altMap \(ann, _v, typ) -> do
          guard (ann `Ann.contains` p)
          typeNode <- findSmallestEnclosingType p typ
          refInType typeNode
