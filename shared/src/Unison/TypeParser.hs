module Unison.TypeParser where

import Control.Applicative ((<|>), some, optional)
import Data.Char (isUpper, isLower, isAlpha)
import Data.List (foldl1')
import Data.Foldable (asum)
import qualified Data.Text as Text

import Unison.Parser
import Unison.Symbol (Symbol)
import Unison.Type (Type)
import Unison.View (DFO)
import qualified Unison.Type as Type

type V = Symbol DFO

type_ :: Parser (Type V)
type_ = forall type1 <|> type1

typeLeaf :: Parser (Type V)
typeLeaf =
  asum [ literal
       , parenthesized type_
       , fmap (Type.v' . Text.pack) (token varName)
       ]

type1 :: Parser (Type V)
type1 = arrow type2

type2 :: Parser (Type V)
type2 = app typeLeaf

-- "TypeA TypeB TypeC"
app :: Parser (Type V) -> Parser (Type V)
app rec = fmap (foldl1' Type.app) (some rec)

arrow :: Parser (Type V) -> Parser (Type V)
arrow rec = foldr1 Type.arrow <$> sepBy1 (token $ string "->") rec

-- "forall a b . List a -> List b -> Maybe Text"
forall :: Parser (Type V) -> Parser (Type V)
forall rec = do
    _ <- token $ string "forall"
    vars <- some $ token varName
    _ <- token (char '.')
    t <- rec
    pure $ Type.forall' (fmap Text.pack vars) t

varName :: Parser String
varName =
  constrainedIdentifier [ isLower . head
                        , all isAlpha
                        ]

typeName :: Parser String
typeName =
  constrainedIdentifier [ isUpper . head
                        , all isAlpha
                        ]

-- qualifiedTypeName :: Parser String
-- qualifiedTypeName = f <$> typeName <*> optional more
--   where
--     f :: String -> (Maybe String) -> String
--     f first more = maybe first (first++) more
--     more = (:) <$> char '.' <*> qualifiedTypeName

literal :: Parser (Type V)
literal =
  token $ asum [ Type.lit Type.Number <$ string "Number"
               , Type.lit Type.Text <$ string "Text"
               , Type.lit Type.Vector <$ string "Vector"
               , (Type.v' . Text.pack) <$> typeName
               ]
