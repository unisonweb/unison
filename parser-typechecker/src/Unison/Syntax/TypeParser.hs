{-# LANGUAGE OverloadedStrings #-}

module Unison.Syntax.TypeParser
  ( computationType,
    valueType,
    valueTypeLeaf,
  )
where

import Control.Monad.Reader (asks)
import Data.Set qualified as Set
import Text.Megaparsec qualified as P
import Unison.ABT qualified as ABT
import Unison.Builtin.Decls qualified as DD
import Unison.HashQualified qualified as HQ
import Unison.NamesWithHistory qualified as Names
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.Syntax.Lexer qualified as L
import Unison.Syntax.Name qualified as Name (toVar)
import Unison.Syntax.Parser
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Var (Var)

-- A parsed type is annotated with its starting and ending position in the
-- source text.
type TypeP v m = P v m (Type v Ann)

-- Value types cannot have effects, unless those effects appear to
-- the right of a function arrow:
--   valueType ::= Int | Text | App valueType valueType | Arrow valueType computationType
--              | Constraint+ '=>' valueType
valueType :: (Monad m, Var v) => TypeP v m
valueType = constrainedType <|> forAll (constrainedType <|> type1) <|> type1

-- | Parse the implicit-parameter constraint-arrow form:
--
-- > Show a => a -> Text
-- > (Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)
-- > Functor f => (a -> b) -> f a -> f b
--
-- Multi-constraint signatures @(C1 a, C2 b) => T@ desugar to a
-- right-folded chain of 'ImplicitArrow's:
-- @ImplicitArrow C1a (ImplicitArrow C2b T)@. This distinguishes the
-- constraint arrow @=>@ from the explicit function arrow @->@ in the
-- type AST; the printer renders @ImplicitArrow@ chains back as @=>@
-- tuples so signatures round-trip.
constrainedType :: (Monad m, Var v) => TypeP v m
constrainedType = do
  cs <- P.try constraintContext
  body <- forAll (constrainedType <|> type1) <|> type1
  -- Right-fold: (C1, C2, ..., Cn) => T  ===>
  --   ImplicitArrow C1 (ImplicitArrow C2 (... (ImplicitArrow Cn T))).
  -- Each step preserves the source range of the constraint by combining
  -- annotations.
  pure $ foldr (\c t -> Type.implicitArrow (ann c <> ann t) c t) body cs

-- | Parses one of:
--
-- > C        -- a single constraint, e.g. @Show a@
-- > (C1, ..., Cn)  -- a parenthesised list of constraints, n >= 1
--
-- followed by the @=>@ arrow. Wrapped in 'P.try' at the call site so
-- non-constrained types fall back to the regular grammar without
-- consuming input.
constraintContext :: (Monad m, Var v) => P v m [Type v Ann]
constraintContext = do
  cs <- parenthesisedConstraints <|> ((: []) <$> type2)
  _ <- reserved "=>"
  pure cs
  where
    parenthesisedConstraints = do
      _ <- openBlockWith "("
      -- '(' opens a layout block, so a multi-line constraint context can
      -- have layout-inserted semicolons around the commas; tolerate them
      -- (and any leading/trailing one) the way the tuple parser does.
      _ <- optional semi
      -- A constraint context cannot be empty; @() => T@ is rejected.
      cs <- sepBy1 (P.try (optional semi *> reserved "," <* optional semi)) type2
      _ <- optional semi
      _ <- closeBlock
      pure cs

-- Computation
-- computationType ::= [{effect*}] valueType
computationType :: (Monad m, Var v) => TypeP v m
computationType = effect <|> valueType

valueTypeLeaf :: (Monad m, Var v) => TypeP v m
valueTypeLeaf =
  tupleOrParenthesizedType valueType <|> typeAtom <|> sequenceTyp

-- Examples: Optional, Optional#abc, woot, #abc
typeAtom :: (Monad m, Var v) => TypeP v m
typeAtom =
  hqPrefixId >>= \tok -> case L.payload tok of
    HQ.NameOnly n -> pure $ Type.var (ann tok) (Name.toVar n)
    hq -> do
      names <- asks names
      let matches = Names.lookupHQType Names.IncludeSuffixes hq names
      if Set.size matches /= 1
        then P.customFailure (UnknownType tok matches)
        else pure $ Type.ref (ann tok) (Set.findMin matches)

type1 :: (Monad m, Var v) => TypeP v m
type1 = arrow type2a

type2a :: (Monad m, Var v) => TypeP v m
type2a = delayed <|> type2

delayed :: (Monad m, Var v) => TypeP v m
delayed = do
  q <- reserved "'"
  t <- effect <|> (pt <$> type2a)
  pure $
    Type.arrow
      (Ann (L.start q) (end $ ann t))
      (DD.thunkArgType (ann q))
      t
  where
    -- if no abilities listed on 't, assume '{} t
    pt t = Type.effect (ann t) [] t

type2 :: (Monad m, Var v) => TypeP v m
type2 = do
  hd <- valueTypeLeaf
  tl <- many (effectList <|> valueTypeLeaf)
  pure $ foldl' (\a b -> Type.app (ann a <> ann b) a b) hd tl

-- ex : {State Text, IO} (List Int)
effect :: (Monad m, Var v) => TypeP v m
effect = do
  es <- effectList
  t <- type2
  pure (Type.effect1 (ann es <> ann t) es t)

effectList :: (Monad m, Var v) => TypeP v m
effectList = do
  open <- openBlockWith "{"
  es <- sepBy (reserved ",") valueType
  close <- closeBlock
  pure $ Type.effects (ann open <> ann close) es

sequenceTyp :: (Monad m, Var v) => TypeP v m
sequenceTyp = do
  open <- openBlockWith "["
  t <- valueType
  close <- closeBlock
  let a = ann open <> ann close
  pure $ Type.app a (Type.list a) t

tupleOrParenthesizedType :: (Var v) => TypeP v m -> TypeP v m
tupleOrParenthesizedType rec = do
  (spanAnn, ty) <- tupleOrParenthesized rec DD.unitType pair
  pure (ty {ABT.annotation = ABT.annotation ty <> spanAnn})
  where
    pair t1 t2 =
      let a = ann t1 <> ann t2
       in Type.app a (Type.app (ann t1) (DD.pairType a) t1) t2

--  valueType ::= ... | Arrow valueType computationType
arrow :: (Monad m, Var v) => TypeP v m -> TypeP v m
arrow rec =
  let eff = mkArr <$> optional effectList
      mkArr Nothing a b = Type.arrow (ann a <> ann b) a b
      mkArr (Just es) a b = Type.arrow (ann a <> ann b) a (Type.effect1 (ann es <> ann b) es b)
   in chainr1 (effect <|> rec) (reserved "->" *> eff)

-- "forall a b . List a -> List b -> Maybe Text"
forAll :: (Var v) => TypeP v m -> TypeP v m
forAll rec = do
  kw <- reserved "forall" <|> reserved "∀"
  vars <- fmap (fmap L.payload) . some $ prefixDefinitionName
  _ <- reserved "."
  t <- rec
  pure $ Type.foralls (ann kw <> ann t) vars t
