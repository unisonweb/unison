{-# LANGUAGE OverloadedStrings #-}

-- | Snapshot tests for the type-signature parser, exercising the
-- implicit-parameter constraint-arrow @=>@ syntax introduced by
-- chunk A1 of the implicits feature (see
-- @docs/implicits-phase-2-chunks.md@ and ADR-001).
--
-- Round-trip pretty-printing is a Phase 4 deliverable; for now these
-- tests ensure that:
--
--   * positive examples from @docs/implicits-plan.md@ §1.2 parse;
--   * malformed @=>@ usages produce parse errors (no spurious
--     successes that downstream phases would have to clean up).
module Unison.Test.Syntax.TypeParser where

import Control.Monad (join)
import Data.Functor.Identity (Identity (..))
import Data.Text qualified as Text
import EasyTest
import Unison.Parsers qualified as Ps
import Unison.PrintError (renderParseErrorAsANSI)
import Unison.Symbol (Symbol)
import Unison.Syntax.TypeParser qualified as TP
import Unison.Test.Common qualified as Common

test :: Test ()
test =
  scope "typeparser" . tests $
    [ scope "constraint-arrow" $ tests (parses <$> positives),
      scope "constraint-arrow.malformed" $ tests (failsToParse <$> negatives),
      scope "lexer-distinguishes-arrows" $ tests (parses <$> arrowDistinctions)
    ]

-- | Examples taken from @docs/implicits-plan.md@ §1.2 (consuming givens).
positives :: [String]
positives =
  [ -- single constraint
    "Show a => a -> Text",
    "Ord a => [a] -> [a]",
    -- HKT
    "Functor f => (a -> b) -> f a -> f b",
    "Monad m => (a -> m b) -> m a -> m b",
    -- multiple constraints in a tuple
    "(Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)",
    "(Eq a, Show a) => a -> a -> Text",
    -- constraint followed by a forall'd body
    -- (note: forall on the LHS would be unusual; on the RHS is the typical form)
    "Show a => forall b . b -> a -> Text",
    -- nested parens in body
    "Show a => (a -> Text) -> Text",
    -- multiple constraints, single-paren grouping
    "(Show a) => a -> Text",
    -- constraint on a higher-arity type
    "Functor f => f a -> f a",
    -- A1 carry-over: given-conclusion form, plan §1.2.
    -- A constraint followed directly by a constraint-shaped conclusion
    -- (no arrow, just `Show (List a)`) must parse: this is the
    -- "given Show (List a) requires Show a" reading of `=>`.
    "Show a => Show (List a)",
    -- A1 carry-over: constraint combined with an effect-arrow body.
    -- Plan §1.2 explicitly lists the `(Monad m) => (a ->{e} m b) -> ...`
    -- form; the body uses the existing `->{eff}` arrow syntax.
    "(Monad m) => (a ->{e} m b) -> m a -> m b"
  ]

-- | The lexer must continue to distinguish @=>@ from @==>@ and @=@.
-- These don't use @=>@ themselves but live in the same neighborhood
-- and would regress if the lexer were edited carelessly.
arrowDistinctions :: [String]
arrowDistinctions =
  [ "Nat -> Nat",
    "a -> b -> c",
    "{State s} a -> b"
  ]

-- | Negative cases: malformed constraint arrows. Each of these should
-- fail to parse as a value type.
--
-- We don't assert a specific error class because the cheapest A1
-- behaviour is "generic parse failure"; A4's snapshot suite will
-- pin precise diagnostics.
negatives :: [(String, String)]
negatives =
  [ ("=> Foo", "missing LHS"),
    ("Foo =>", "missing RHS"),
    ("Show a =>", "missing RHS after multi-token LHS"),
    ("=> => Foo", "double constraint arrow"),
    ("Show => => Foo", "constraint arrow with no body between two LHSes"),
    -- A1 carry-over: ADR-001 forbids nested `=>` chains. The user
    -- must group constraints with parens: `(Show a, Eq a) => T`.
    ("Show a => Eq a => T", "nested =>: ADR-001 requires (..., ...) => grouping")
  ]

parses :: String -> Test ()
parses s = scope (label s) $
  case runIdentity (Ps.parse @_ @Symbol TP.valueType s Common.parsingEnv) of
    Left e -> do
      note . Text.unpack $ renderParseErrorAsANSI 60 s e
      crash . Text.unpack $ renderParseErrorAsANSI 60 s e
    Right _ -> ok

failsToParse :: (String, String) -> Test ()
failsToParse (s, why) = scope (label (s ++ "  -- " ++ why)) $
  case runIdentity (Ps.parse @_ @Symbol TP.valueType s Common.parsingEnv) of
    Left _ -> ok
    Right t -> do
      note $ "expected parse failure (" ++ why ++ "); got: " ++ show t
      crash "parser unexpectedly succeeded"

-- | Pick the first non-empty line for the test scope name.
label :: String -> String
label = join . take 1 . lines
