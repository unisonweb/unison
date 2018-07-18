{-# LANGUAGE OverloadedStrings #-}

module Unison.TypeParser2 where

import           Control.Applicative
import           Control.Monad
import           Data.Char (isUpper, isLower)
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Megaparsec as P
import qualified Unison.Lexer as L
import           Unison.Parser2
import           Unison.Type (Type)
import qualified Unison.Type as Type
import           Unison.Var (Var)

type TypeP v = P.ParsecT Text Input ((->) PEnv) (Type v)

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
  pure $ foldl' Type.app hd tl

--  valueType ::= ... | Arrow valueType computationType
arrow :: Var v => TypeP v -> TypeP v
arrow rec = do
  t <- foldr1 Type.arrow <$> sepBy1 (P.try (reserved "->")) (effect <|> rec)
  case t of
    Type.Arrow' (Type.Effect' _ _) _ -> fail "effect to the left of an ->"
    _ -> pure t

-- "forall a b . List a -> List b -> Maybe Text"
forall :: Var v => TypeP v -> TypeP v
forall rec = do
    (P.try $ reserved "forall") <|> void (P.try $ reserved "∀")
    vars <- some $ P.try varName
    _ <- P.try . matchToken $ L.SymbolyId "."
    t <- rec
    pure $ Type.forall' (fmap Text.pack vars) t

varName :: UnisonParser String
varName = do
  name <- wordyId
  guard (isLower . head $ name)
  pure name

typeName :: UnisonParser String
typeName = do
  name <- wordyId
  guard (isUpper . head $ name)
  pure name

-- qualifiedTypeName :: Parser String
-- qualifiedTypeName = f <$> typeName <*> optional more
--   where
--     f :: String -> (Maybe String) -> String
--     f first more = maybe first (first++) more
--     more = (:) <$> char '.' <*> qualifiedTypeName

literal :: Var v => TypeP v
literal = P.label "literal" . P.try $ (Type.v' . Text.pack) <$> typeName

