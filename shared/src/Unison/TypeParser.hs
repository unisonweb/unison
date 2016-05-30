module Unison.TypeParser where

import Control.Applicative (empty, (<|>), some)
import Control.Monad (guard)
import Data.Char (isUpper, isLower, isAlpha)
import Data.List (foldl1')
import Data.Foldable (asum)
import qualified Data.Text as Text

import Unison.Parser
import Unison.Reference (Reference)
import Unison.Symbol (Symbol)
import Unison.Type (Type)
import Unison.Var (Var)
import Unison.View (DFO)
import qualified Unison.Reference as Reference
import qualified Unison.Type as Type

type RefLookup = (String -> Maybe Reference)
type MakeParser v = RefLookup -> Parser (Type v)

type_ :: Var v => RefLookup -> Parser (Type v)
type_ l = forall type1 l <|> type1 l

typeLeaf :: Var v => MakeParser v
typeLeaf l =
  asum [ literal l
       , parenthesized (type_ l)
       , fmap (Type.v' . Text.pack) (token varName)
       ]

type1 :: Var v => MakeParser v
type1 l = arrow type2 l

type2 :: Var v => MakeParser v
type2 l = app typeLeaf l

-- "TypeA TypeB TypeC"
app :: Var v => MakeParser v -> MakeParser v
app rec l = fmap (foldl1' Type.app) (some (rec l))

arrow :: Var v => MakeParser v -> MakeParser v
arrow rec l = foldr1 Type.arrow <$> sepBy1 (token $ string "->") (rec l)

-- "forall a b . List a -> List b -> Maybe Text"
forall :: Var v => MakeParser v -> MakeParser v
forall rec l = do
    _ <- token $ string "forall"
    vars <- some $ token varName
    _ <- token (char '.')
    t <- rec l
    pure $ Type.forall' (fmap Text.pack vars) t

varName :: Parser String
varName =
  constrainedIdentifier [ (isLower . head)
                        , all isAlpha
                        ]

typeName :: Parser String
typeName =
  constrainedIdentifier [ (isUpper . head)
                        , all isAlpha
                        ]

literal :: Var v => RefLookup -> Parser (Type v)
literal lookup =
  token $ asum [ Type.lit Type.Number <$ string "Number"
               , Type.lit Type.Text <$ string "Text"
               , Type.lit Type.Vector <$ string "Vector"
               , lookupReference
               ]
  where
    lookupReference = do -- identifier starting with upper-case letter
      id <- token typeName
      case lookup id of
        Nothing -> empty
        Just ref -> pure (Type.lit (Type.Ref ref))

l :: RefLookup
l s = Just (Reference.Builtin $ Text.pack s)

type V = Symbol DFO
foo :: String -> Result (Type V)
foo s = run (type_ l) s
