module Unison.TypeParser2 where

import Unison.Parser

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

