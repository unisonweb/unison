{-# Language OverloadedStrings #-}

module Unison.TypeParser where


import Control.Applicative ((<|>), some)
import Data.Char (isUpper, isLower, isAlpha)
import Data.Foldable (asum)
import Data.Functor
import Data.List (foldl1')
import Unison.Parser
import Unison.Type (Type)
import Unison.Var (Var)
import qualified Data.Text as Text
import qualified Unison.Type as Type

type_ :: Var v => Parser (Type v)
type_ = forall type1 <|> type1

typeLeaf :: Var v => Parser (Type v)
typeLeaf =
  asum [ literal
       , tupleOrParenthesized type_
       , fmap (Type.v' . Text.pack) (token varName)
       ]

tupleOrParenthesized :: Ord v => Parser (Type v) -> Parser (Type v)
tupleOrParenthesized rec =
  parenthesized $ go <$> sepBy1 (token $ string ",") rec where
    go [t] = t
    go types = foldr pair unit types
    pair t1 t2 = Type.builtin "Pair" `Type.app` t1 `Type.app` t2
    unit = Type.builtin "Unit"

type1 :: Var v => Parser (Type v)
type1 = arrow type2

type2 :: Var v => Parser (Type v)
type2 = app typeLeaf

-- "TypeA TypeB TypeC"
app :: Ord v => Parser (Type v) -> Parser (Type v)
app rec = fmap (foldl1' Type.app) (some rec)

arrow :: Ord v => Parser (Type v) -> Parser (Type v)
arrow rec = foldr1 Type.arrow <$> sepBy1 (token $ string "->") rec

-- "forall a b . List a -> List b -> Maybe Text"
forall :: Var v => Parser (Type v) -> Parser (Type v)
forall rec = do
    (void . token $ string "forall") <|> void (token (char '∀'))
    vars <- some $ token varName
    _ <- token (char '.')
    t <- rec
    pure $ Type.forall' (fmap Text.pack vars) t

varName :: Parser String
varName = identifier [isLower.head, all isAlpha]

typeName :: Parser String
typeName = identifier [isUpper.head]

-- qualifiedTypeName :: Parser String
-- qualifiedTypeName = f <$> typeName <*> optional more
--   where
--     f :: String -> (Maybe String) -> String
--     f first more = maybe first (first++) more
--     more = (:) <$> char '.' <*> qualifiedTypeName

literal :: Var v => Parser (Type v)
literal =
  token $ asum [ Type.lit Type.Number <$ string "Number"
               , Type.lit Type.Text <$ string "Text"
               , Type.lit Type.Vector <$ string "Vector"
               , (Type.v' . Text.pack) <$> typeName
               ]
