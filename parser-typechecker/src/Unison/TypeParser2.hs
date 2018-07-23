{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.TypeParser2 where

import           Control.Applicative
import           Control.Monad
import           Data.Char (isUpper, isLower)
import           Data.List
import qualified Data.Text as Text
import qualified Text.Megaparsec as P
import           Unison.ABT (annotate)
import qualified Unison.Lexer as L
import           Unison.Parser2
import           Unison.Type (AnnotatedType)
import qualified Unison.Type as Type
import           Unison.Var (Var)

-- A parsed type is annotated with its starting and ending position in the
-- source text.
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
typeVar = posMap (\pos -> Type.av' pos . Text.pack) wordyId

type1 :: Var v => TypeP v
type1 = arrow type2

type2 :: Var v => TypeP v
type2 = app valueTypeLeaf

-- ex : {State Text, IO} (Sequence Int64)
effect :: Var v => TypeP v
effect = do
  open <- reserved "{"
  es <- sepBy (reserved ",") valueType
  _ <- reserved "}"
  t <- valueTypeLeaf
  pure (Type.effect (Ann (L.start open) (end $ ann t)) es t)

tupleOrParenthesized :: Ord v => TypeP v -> TypeP v
tupleOrParenthesized rec = do
    open <- reserved "("
    es <- sepBy (reserved ",") rec
    close <- reserved ")"
    pure $ go es open close
  where
    go [t] _ _ = t
    go types s e = annotate (ann s <> ann e) $ foldr pair (unit e e) types
    pair t1 t2 =
      let a = ann t1 <> ann t2
      in Type.app a (Type.app (ann t1) (Type.builtin a "Pair") t1) t2
    unit s e = Type.builtin (ann s <> ann e) "()"

-- "TypeA TypeB TypeC"
app :: Ord v => TypeP v -> TypeP v
app rec = do
  (hd:tl) <- some rec
  pure $ foldl' (\a b -> Type.app (ann a <> ann b) a b) hd tl

--  valueType ::= ... | Arrow valueType computationType
arrow :: Var v => TypeP v -> TypeP v
arrow rec =
  let p = sepBy1 (reserved "->") (effect <|> rec)
  in foldr1 (\a b -> Type.arrow (ann a <> ann b) a b) <$> p

-- "forall a b . List a -> List b -> Maybe Text"
forall :: Var v => TypeP v -> TypeP v
forall rec = do
    kw <- reserved "forall" <|> reserved "∀"
    vars <- fmap (fmap L.payload) . some $ varName
    _ <- matchToken $ L.SymbolyId "."
    t <- rec
    pure $ Type.forall' (ann kw <> ann t) (fmap Text.pack vars) t

varName :: Parser String
varName = do
  name <- wordyId
  guard (isLower . head $ L.payload name)
  pure name

typeName :: Parser String
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

posMap :: (Ann -> a -> b) -> Parser a -> P b
posMap f = fmap $ \case L.Token a start end -> f (Ann start end) a

literal :: Var v => TypeP v
literal =
  P.label "literal" . posMap (\pos -> Type.av' pos . Text.pack) $ typeName

