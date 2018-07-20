{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.TypeParser2 where

import           Control.Applicative
import           Control.Monad
import           Data.Char (isUpper, isLower)
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Megaparsec as P
import           Unison.ABT (annotation)
import qualified Unison.Lexer as L
import           Unison.Parser2
import           Unison.Type (Type, AnnotatedType)
import qualified Unison.Type as Type
import           Unison.Var (Var)

type Ann = (L.Pos, L.Pos)

-- A parsed type is annotated with its starting and ending position in the
-- source text.
type P = P.ParsecT Text Input ((->) PEnv)
type TypeP v = P (AnnotatedType v Ann)

-- Value types cannot have effects, unless those effects appear to
-- the right of a function arrow:
--   valueType ::= Int | Text | App valueType valueType | Arrow valueType computationType
valueType :: Var v => TypeP v
valueType = forall type1 <|> type1

-- Computation
-- computationType ::= [{effect*}] valueType
computationType :: Var v => TypeP v
computationType = effect <|> valueType

valueTypeLeaf :: Var v => TypeP v
valueTypeLeaf =
  tupleOrParenthesized valueType <|> typeVar

typeVar :: Var v => TypeP v
typeVar = fmap (Type.v' . Text.pack) (P.try wordyId)

type1 :: Var v => TypeP v
type1 = arrow type2

type2 :: Var v => TypeP v
type2 = app valueTypeLeaf

-- ex : {State Text, IO} (Sequence Int64)
effect :: Var v => TypeP v
effect = do
  P.try $ reserved "{"
  es <- sepBy (P.try (reserved ",")) valueType
  P.try $ reserved "}"
  t <- valueTypeLeaf
  pure (Type.effect es t)

tupleOrParenthesized :: Ord v => TypeP v -> TypeP v
tupleOrParenthesized rec =
  parenthesized $ go <$> sepBy (P.try $ reserved ",") rec where
    go [t] = t
    go types = foldr pair unit types
    pair t1 t2 = Type.builtin "Pair" `Type.app` t1 `Type.app` t2
    unit = Type.builtin "()"

-- "TypeA TypeB TypeC"
app :: Ord v => TypeP v -> TypeP v
app rec = do
  (hd:tl) <- some rec
  pos <- P.getPosition
  pure $ foldl' (\a b -> getPosition >>= \pos -> Type.app pos a b) hd tl

--  valueType ::= ... | Arrow valueType computationType
arrow :: Var v => TypeP v -> TypeP v
arrow rec = do
  let p = sepBy1 (P.try (reserved "->")) (effect <|> rec)
      mkArrow a b =
        Type.arrow (Pos (fst $ annotation a) (snd $ annotation b)) a b
  t <- foldr1 mkArrow <$> p
  case t of
    Type.Arrow' (Type.Effect' _ _) _ -> fail "effect to the left of an ->"
    _ -> pure t

-- "forall a b . List a -> List b -> Maybe Text"
forall :: Var v => TypeP v -> TypeP v
forall rec = do
    kw <- P.try (reserved "forall") <|> P.try (reserved "∀")
    vars <- fmap (fmap L.payload) . some $ P.try varName
    _ <- P.try . matchToken $ L.SymbolyId "."
    t <- rec
    let pos = ((L.start kw), snd $ annotation t)
    pure $ Type.forall' pos (fmap Text.pack vars) t

varName :: UnisonParser String
varName = do
  name <- wordyId
  guard (isLower . head $ L.payload name)
  pure name

typeName :: UnisonParser String
typeName = do
  name <- wordyId
  guard (isUpper . head $ L.payload name)
  pure name

-- qualifiedTypeName :: Parser String
-- qualifiedTypeName = f <$> typeName <*> optional more
--   where
--     f :: String -> (Maybe String) -> String
--     f first more = maybe first (first++) more
--     more = (:) <$> char '.' <*> qualifiedTypeName

posMap :: (Ann -> a -> b) -> UnisonParser a -> P b
posMap f = fmap $ \case L.Token a start end -> f (start, end) a

literal :: Var v => TypeP v
literal =
  P.label "literal" . P.try .
    posMap (\pos -> Type.av' pos . Text.pack) $ typeName

