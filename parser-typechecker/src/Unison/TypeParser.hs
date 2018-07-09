{-# Language OverloadedStrings #-}
{-# Language BangPatterns #-}

module Unison.TypeParser where

import Control.Monad
import Control.Applicative
import Data.Char (isUpper, isLower)
import Data.List
import Unison.Parser
import Unison.Type (Type)
import Unison.Var (Var)
import qualified Data.Text as Text
import qualified Unison.Type as Type

--import Debug.Trace
--
--pTrace s = pt <|> return ()
--    where pt = attempt $
--               do
--                 x <- attempt $ many anyChar
--                 trace (s++": " ++x) $ attempt $ char 'z'
--                 fail x
--
---- traced s p = p
--traced s p = do
--  pTrace s
--  a <- p <|> trace (s ++ " backtracked") (fail s)
--  let !x = trace (s ++ " succeeded") ()
--  pure a

newtype S v = Aliases [(v, [Type v] -> Type v)]
s0 :: S v
s0 = Aliases []

type TypeP v = Parser (S v) (Type v)

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
typeVar = fmap (Type.v' . Text.pack) (token $ wordyId keywords)

type1 :: Var v => TypeP v
type1 = arrow type2

type2 :: Var v => TypeP v
type2 = app valueTypeLeaf

-- ex : {State Text, IO} (Sequence Int64)
effect :: Var v => TypeP v
effect = do
  token_ $ string "{"
  es <- sepBy (token (string ",")) valueType
  token_ $ string "}"
  t <- valueTypeLeaf
  pure (Type.effect() es t)

tupleOrParenthesized :: Ord v => TypeP v -> TypeP v
tupleOrParenthesized rec =
  parenthesized $ go <$> sepBy (token $ string ",") rec where
    go [t] = t
    go types = foldr pair unit types
    pair t1 t2 = Type.app() (Type.app() (Type.builtin() "Pair") t1) t2
    unit = Type.builtin() "()"

-- "TypeA TypeB TypeC"
app :: Ord v => TypeP v -> TypeP v
app rec = get >>= \(Aliases aliases) -> do
  (hd:tl) <- some rec
  pure $ case hd of
    Type.Var' v -> case lookup v aliases of
      Nothing -> foldl' (Type.app()) hd tl
      Just apply -> apply tl
    _ -> foldl' (Type.app()) hd tl

--  valueType ::= ... | Arrow valueType computationType
arrow :: Var v => TypeP v -> TypeP v
arrow rec = do
  t <- foldr1 (Type.arrow()) <$> sepBy1 (token (string "->")) (effect <|> rec)
  case t of
    Type.Arrow' (Type.Effect' _ _) _ -> fail "effect to the left of an ->"
    _ -> pure t

-- "forall a b . List a -> List b -> Maybe Text"
forall :: Var v => TypeP v -> TypeP v
forall rec = do
    (void . token $ string "forall") <|> void (token (char '∀'))
    vars <- some $ token varName
    _ <- token (char '.')
    t <- rec
    pure $ Type.forall'() (fmap Text.pack vars) t

varName :: Parser s String
varName = do
  name <- wordyId keywords
  guard (isLower . head $ name)
  pure name

typeName :: Parser s String
typeName = do
  name <- wordyId keywords
  guard (isUpper . head $ name)
  pure name

keywords :: [String]
keywords = ["forall", "∀"]

-- qualifiedTypeName :: Parser String
-- qualifiedTypeName = f <$> typeName <*> optional more
--   where
--     f :: String -> (Maybe String) -> String
--     f first more = maybe first (first++) more
--     more = (:) <$> char '.' <*> qualifiedTypeName

literal :: Var v => TypeP v
literal = label "literal" . token $ (Type.v' . Text.pack) <$> typeName
