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
      "∀",
      -- Implicit-parameter feature. `given` is a top-level
      -- declaration prefix and a let-block statement opener. User
      -- code that names a definition or local `given` must rename or
      -- use backtick escaping (`` `given` ``). (`summon` is /not/
      -- reserved; it is a regular builtin term reference whose type
      -- @forall a. a => a@ already gets the right behaviour through
      -- the normal implicit-resolution machinery.)
      "given",
      -- `give f` is a prefix syntactic transformation that demotes
      -- f's leading `=>` arrows to `->`. Has to be a keyword (not a
      -- builtin) because the type transformation cannot be expressed
      -- as a regular polymorphic signature without the typechecker's
      -- implicit-resolution rule firing first.
      "give"
    ]
    <> typeModifiers
    <> typeOrAbility

typeModifiers :: Set Text
typeModifiers =
  Set.fromList ["structural", "unique"]

typeOrAbility :: Set Text
typeOrAbility =
  Set.fromList ["type", "ability", "class"]

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
      "==>",
      -- Implicit-parameter constraint arrow. Listed after "==>" so
      -- the longer match wins via the lexer's alternation order (see
      -- 'symbolyKw' in Lexer/Unison.hs).
      "=>"
    ]

delimiters :: Set Char
delimiters =
  Set.fromList "()[]{},?;"
