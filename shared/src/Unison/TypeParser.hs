{-# Language OverloadedStrings #-}
{-# Language BangPatterns #-}

module Unison.TypeParser where

import Control.Monad
import Control.Applicative
import Data.Char (isUpper, isLower)
import Data.Foldable (asum)
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

type_ :: Var v => Parser (S v) (Type v)
type_ = forall type1 <|> type1

typeLeaf :: Var v => Parser (S v) (Type v)
typeLeaf =
  asum [ literal
       , tupleOrParenthesized type_
       , fmap (Type.v' . Text.pack) (token varName)
       ]

tupleOrParenthesized :: Ord v => Parser (S v) (Type v) -> Parser (S v) (Type v)
tupleOrParenthesized rec =
  parenthesized $ go <$> sepBy1 (token $ string ",") rec where
    go [t] = t
    go types = foldr pair unit types
    pair t1 t2 = Type.builtin "Pair" `Type.app` t1 `Type.app` t2
    unit = Type.builtin "Unit"

type1 :: Var v => Parser (S v) (Type v)
type1 = arrow type2

type2 :: Var v => Parser (S v) (Type v)
type2 = app typeLeaf

-- "TypeA TypeB TypeC"
app :: Ord v => Parser (S v) (Type v) -> Parser (S v) (Type v)
app rec = get >>= \(Aliases aliases) -> do
  (hd:tl) <- some rec
  pure $ case hd of
    Type.Var' v -> case lookup v aliases of
      Nothing -> foldl' Type.app hd tl
      Just apply -> apply tl
    _ -> foldl' Type.app hd tl

arrow :: Ord v => Parser (S v) (Type v) -> Parser (S v) (Type v)
arrow rec = foldr1 Type.arrow <$> sepBy1 (token (string "->")) rec

-- "forall a b . List a -> List b -> Maybe Text"
forall :: Var v => Parser (S v) (Type v) -> Parser (S v) (Type v)
forall rec = do
    (void . token $ string "forall") <|> void (token (char '∀'))
    vars <- some $ token varName
    _ <- token (char '.')
    t <- rec
    pure $ Type.forall' (fmap Text.pack vars) t

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

literal :: Var v => Parser (S v) (Type v)
literal = label "literal" . token $
  asum [ Type.lit Type.Number <$ token (string "Number")
       , Type.lit Type.Text <$ token (string "Text")
       , Type.lit Type.Vector <$ token (string "Vector")
       , (Type.v' . Text.pack) <$> typeName
       ]
