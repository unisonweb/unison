module Unison.Syntax.ReservedWords
  ( keywords,
    typeModifiers,
    typeOrAbility,
    reservedOperators,
    delimiters,
  )
where

import Data.Set qualified as Set
import Unison.Prelude

keywords :: Set Text
keywords =
  Set.fromList
    [ "@rewrite",
      "alias",
      "cases",
      "do",
      "else",
      "false",
      "forall",
      "handle",
      "if",
      "let",
      "match",
      "namespace",
      "termLink",
      "then",
      "true",
      "typeLink",
      "use",
      "where",
      "with",
      "∀"
    ]
    <> typeModifiers
    <> typeOrAbility

typeModifiers :: Set Text
typeModifiers =
  Set.fromList ["structural", "unique"]

typeOrAbility :: Set Text
typeOrAbility =
  Set.fromList ["type", "ability"]

reservedOperators :: Set Text
reservedOperators =
  Set.fromList
    [ "=",
      "->",
      ":",
      "&&",
      "||",
      "|",
      "!",
      "'",
      "==>"
    ]

delimiters :: Set Char
delimiters =
  Set.fromList "()[]{},?;"
