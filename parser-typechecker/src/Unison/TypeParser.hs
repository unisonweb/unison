{-# LANGUAGE OverloadedStrings #-}

module Unison.TypeParser
  ( valueType
  , computationType
  , valueTypeLeaf
  ) where

import Control.Monad.Reader (asks)
import qualified Data.Set as Set
import qualified Text.Megaparsec as P
import qualified Unison.Builtin.Decls as DD
import qualified Unison.HashQualified as HQ
import qualified Unison.Lexer as L
import qualified Unison.Name as Name
import qualified Unison.NamesWithHistory as Names
import Unison.Parser
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.Type (Type)
import qualified Unison.Type as Type
import Unison.Var (Var)

-- A parsed type is annotated with its starting and ending position in the
-- source text.
type TypeP v = P v (Type v Ann)

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
  tupleOrParenthesizedType valueType <|> typeAtom <|> sequenceTyp

-- Examples: Optional, Optional#abc, woot, #abc
typeAtom :: Var v => TypeP v
typeAtom =
  hqPrefixId >>= \tok -> case L.payload tok of
    HQ.NameOnly n -> pure $ Type.var (ann tok) (Name.toVar n)
    hq -> do
      names <- asks names
      let matches = Names.lookupHQType hq names
      if Set.size matches /= 1
        then P.customFailure (UnknownType tok matches)
        else pure $ Type.ref (ann tok) (Set.findMin matches)

type1 :: Var v => TypeP v
type1 = arrow type2a

type2a :: Var v => TypeP v
type2a = delayed <|> type2

delayed :: Var v => TypeP v
delayed = do
  q <- reserved "'"
  t <- effect <|> type2a
  pure $
    Type.arrow
      (Ann (L.start q) (end $ ann t))
      (DD.unitType (ann q))
      t

type2 :: Var v => TypeP v
type2 = do
  hd <- valueTypeLeaf
  tl <- many (effectList <|> valueTypeLeaf)
  pure $ foldl' (\a b -> Type.app (ann a <> ann b) a b) hd tl

-- ex : {State Text, IO} (List Int)
effect :: Var v => TypeP v
effect = do
  es <- effectList
  t <- type2
  pure (Type.effect1 (ann es <> ann t) es t)

effectList :: Var v => TypeP v
effectList = do
  open <- openBlockWith "{"
  es <- sepBy (reserved ",") valueType
  close <- closeBlock
  pure $ Type.effects (ann open <> ann close) es

sequenceTyp :: Var v => TypeP v
sequenceTyp = do
  open <- openBlockWith "["
  t <- valueType
  close <- closeBlock
  let a = ann open <> ann close
  pure $ Type.app a (Type.list a) t

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
  vars <- fmap (fmap L.payload) . some $ prefixDefinitionName
  _ <- matchToken $ L.SymbolyId "." Nothing
  t <- rec
  pure $ Type.foralls (ann kw <> ann t) vars t
