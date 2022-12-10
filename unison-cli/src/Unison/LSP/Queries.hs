-- | Rewrites of some codebase queries, but which check the scratch file for info first.
module Unison.LSP.Queries
  ( refInTerm,
    refInType,
    findSmallestEnclosingNode,
    findSmallestEnclosingType,
    refInDecl,
  )
where

import Control.Lens
import qualified Unison.ABT as ABT
import qualified Unison.ConstructorType as CT
import qualified Unison.DataDeclaration as DD
import Unison.LSP.Orphans ()
import Unison.LabeledDependency
import qualified Unison.LabeledDependency as LD
import Unison.Lexer.Pos (Pos (..))
import Unison.Parser.Ann (Ann)
import qualified Unison.Parser.Ann as Ann
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Symbol (Symbol)
import Unison.Term (MatchCase (MatchCase), Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type

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

-- | Find the the node in a term which contains the specified position, but none of its
-- children contain that position.
findSmallestEnclosingNode :: Pos -> Term Symbol Ann -> Maybe (Either (Term Symbol Ann) (Type Symbol Ann))
findSmallestEnclosingNode pos term
  | annIsFilePosition (ABT.annotation term) && not (ABT.annotation term `Ann.contains` pos) = Nothing
  | otherwise = do
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

-- | Find the the node in a type which contains the specified position, but none of its
-- children contain that position.
-- This is helpful for finding the specific type reference of a given argument within a type arrow
-- that a position references.
findSmallestEnclosingType :: Pos -> Type Symbol Ann -> Maybe (Type Symbol Ann)
findSmallestEnclosingType pos typ
  | annIsFilePosition (ABT.annotation typ) && not (ABT.annotation typ `Ann.contains` pos) = Nothing
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

annIsFilePosition :: Ann -> Bool
annIsFilePosition = \case
  Ann.Intrinsic -> False
  Ann.External -> False
  Ann.Ann {} -> True
