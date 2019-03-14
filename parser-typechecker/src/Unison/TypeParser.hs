{-# LANGUAGE OverloadedStrings #-}

module Unison.TypeParser where

import           Control.Applicative
import           Control.Monad
import           Data.Char (isUpper, isLower)
import           Data.List
import qualified Data.Text as Text
import qualified Text.Megaparsec as P
import qualified Unison.Lexer as L
import           Unison.Parser
import           Unison.Type (AnnotatedType)
import qualified Unison.Type as Type
import           Unison.Var (Var)
import qualified Unison.DataDeclaration as DD

-- A parsed type is annotated with its starting and ending position in the
-- source text.
type TypeP v = P v (AnnotatedType v Ann)

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
  tupleOrParenthesizedType valueType <|> typeVar <|> sequenceTyp

typeVar :: Var v => TypeP v
typeVar = posMap (\pos -> Type.av' pos . Text.pack) wordyId

type1 :: Var v => TypeP v
type1 = arrow type2a

type2a :: Var v => TypeP v
type2a = delayed <|> type2

delayed :: Var v => TypeP v
delayed = do
  q <- reserved "'"
  t <- effect <|> type2
  pure $ Type.arrow (Ann (L.start q) (end $ ann t))
                    (DD.unitType (ann q))
                    t

type2 :: Var v => TypeP v
type2 = do
  hd <- valueTypeLeaf
  tl <- many (effectList <|> valueTypeLeaf)
  pure $ foldl' (\a b -> Type.app (ann a <> ann b) a b) hd tl

-- ex : {State Text, IO} (Sequence Int)
effect :: Var v => TypeP v
effect = do
 es <- effectList
 t <- valueTypeLeaf
 pure (Type.effect1 (ann es <> ann t) es t)

effectList :: Var v => TypeP v
effectList = do
  open <- openBlockWith "{"
  es <- sepBy (reserved ",") valueType
  _ <- closeBlock
  close <- reserved "}"
  pure $ Type.effects (ann open <> ann close) es

sequenceTyp :: Var v => TypeP v
sequenceTyp = do
  open <- reserved "["
  t <- valueType
  close <- reserved "]"
  let a = ann open <> ann close
  pure $ Type.app a (Type.vector a) t

tupleOrParenthesizedType :: Var v => TypeP v -> TypeP v
tupleOrParenthesizedType rec = tupleOrParenthesized rec DD.unitType pair
  where
    pair t1 t2 =
      let a = ann t1 <> ann t2
      in Type.app a (Type.app (ann t1) (DD.pairType a) t1) t2

--  valueType ::= ... | Arrow valueType computationType
arrow :: Var v => TypeP v -> TypeP v
arrow rec =
  let eff = mkArr <$> optional effectList
      mkArr Nothing a b = Type.arrow (ann a <> ann b) a b
      mkArr (Just es) a b = Type.arrow (ann a <> ann b) a (Type.effect1 (ann es <> ann b) es b)
  in chainr1 (effect <|> rec) (reserved "->" *> eff)

-- "forall a b . List a -> List b -> Maybe Text"
forall :: Var v => TypeP v -> TypeP v
forall rec = do
    kw <- reserved "forall" <|> reserved "∀"
    vars <- fmap (fmap L.payload) . some $ varName
    _ <- matchToken $ L.SymbolyId "."
    t <- rec
    pure $ Type.forall' (ann kw <> ann t) (fmap Text.pack vars) t

varName :: Var v => P v (L.Token String)
varName = do
  name <- wordyId
  guard (isLower . head $ L.payload name)
  pure name

typeName :: Var v => P v (L.Token String)
typeName = do
  name <- wordyId
  guard (isUpper . head $ L.payload name)
  pure name

-- qualifiedTypeName :: P v (L.Token String
-- qualifiedTypeName = f <$> typeName <*> optional more
--   where
--     f :: String -> (Maybe String) -> String
--     f first more = maybe first (first++) more
--     more = (:) <$> char '.' <*> qualifiedTypeName

posMap :: (Ann -> a -> b) -> P v (L.Token a) -> P v b
posMap f = fmap $ \case L.Token a start end -> f (Ann start end) a

literal :: Var v => TypeP v
literal =
  P.label "literal" . posMap (\pos -> Type.av' pos . Text.pack) $ typeName
