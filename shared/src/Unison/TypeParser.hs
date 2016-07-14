module Unison.TypeParser where

import Control.Applicative ((<|>), some)
import Data.Char (isUpper, isLower, isAlpha)
import Data.List (foldl1')
import Data.Foldable (asum)
import qualified Data.Text as Text

import Unison.Parser
import Unison.Type (Type)
import Unison.Var (Var)
import qualified Unison.Type as Type

-- type V = Symbol DFO

type_ :: Var v => Parser (Type v)
type_ = forall type1 <|> type1

typeLeaf :: Var v => Parser (Type v)
typeLeaf =
  asum [ literal
       , parenthesized type_
       , fmap (Type.v' . Text.pack) (token varName)
       ]

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
    _ <- token $ string "forall"
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
