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
      -- Implicit-parameter feature (chunk A2; ADRs 006, 010, 022).
      -- `given` is a top-level declaration prefix and a let-block
      -- statement opener. Per ADR-022 the production rollout uses
      -- a one-release deprecation cycle; for the prototype we
      -- hard-break, so user code that names a definition or local
      -- `given` must rename or use backtick escaping (`` `given` ``).
      -- ('summon' was previously reserved too; ADR-006 now treats it
      -- as a regular builtin term reference, since its type
      -- @forall a. a => a@ already gets the right behaviour through
      -- the normal implicit-resolution machinery.)
      "given",
      -- ADR-007: `give f` is a prefix syntactic transformation that
      -- demotes f's leading `=>` arrows to `->`. Has to be a keyword
      -- (not a builtin) because the type transformation cannot be
      -- expressed as a regular polymorphic signature without the
      -- typechecker's implicit-resolution rule firing first.
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
      -- Implicit-parameter constraint arrow (per ADR-001).
      -- Listed after "==>" so the longer match wins via the lexer's
      -- alternation order (see 'symbolyKw' in Lexer/Unison.hs).
      "=>"
    ]

delimiters :: Set Char
delimiters =
  Set.fromList "()[]{},?;"
