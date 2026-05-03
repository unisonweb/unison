{-# LANGUAGE OverloadedStrings #-}

-- | Round-trip property tests for the @=>@ constraint-arrow syntax
-- (chunk P1). For each fixture we:
--
--   1. parse the source as a value type;
--   2. pretty-print the parsed type via 'Unison.Syntax.TypePrinter';
--   3. re-parse the printed text;
--   4. assert the two ABTs are structurally equal (modulo annotations).
--
-- This pins the contract that @view@\/@edit@\/@update@ of declarations
-- containing @=>@ preserve the constraint-arrow syntax: without the
-- printer arm added in chunk P1, step 2 would emit @->@ for implicit
-- positions and the second parse would yield a different AST.
--
-- We use 'Unison.Test.Common.t', which calls 'TypeParser.valueType'
-- under the builtins-only parsing env and applies
-- 'Type.generalizeLowercase' so lowercase free names become @forall@-bound.
-- The round-trip therefore exercises the @forall ... =>@ interaction in
-- addition to the @=>@ rendering itself.
module Unison.Test.Syntax.ImplicitPrinter where

import Data.Map qualified as Map
import Data.Text qualified as Text
import EasyTest
import Unison.ABT qualified as ABT
import Unison.Builtin qualified
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.Syntax.TypePrinter qualified as TypePrinter
import Unison.Test.Common qualified as Common
import Unison.Util.ColorText (toPlain)
import Unison.Util.Pretty qualified as PP

-- | Round-trip a single source fixture: parse -> print -> parse,
-- assert structural equality (modulo annotations).
roundTrip :: String -> Test ()
roundTrip s = scope (label s) $ do
  let original = Common.t s
      ppe =
        PPE.makePPE
          (PPE.hqNamer Common.hqLength Unison.Builtin.names)
          PPE.dontSuffixify
      printed =
        Text.unpack
          . toPlain
          . PP.render 80
          . PP.syntaxToColor
          . TypePrinter.runPretty ppe
          $ TypePrinter.prettyRaw Map.empty (-1) original
      reparsed = Common.t printed
  -- ABT structural equality drops annotations, so this is a faithful
  -- round-trip property: any cosmetic difference (whitespace, paren
  -- redundancy) is allowed; structural difference is not.
  if ABT.amap (const ()) original == ABT.amap (const ()) reparsed
    then ok
    else do
      note $ "input    : " <> s
      note $ "printed  : " <> printed
      note $ "original : " <> show original
      note $ "reparsed : " <> show reparsed
      crash "round-trip mismatch"

-- | Fixtures cover the corpus the printer must reproduce, drawn from
-- @docs/implicits-plan.md@ §1.2 and the A4 parser snapshot suite:
--
--   * single-constraint signatures (@Show a => a -> Text@);
--   * tuple-of-constraints signatures (@(Show a, Eq a) => [a] -> [a]@);
--   * higher-kinded constraints (@Functor f => ...@);
--   * constraints with effect arrows in the conclusion;
--   * the given-conclusion form (@Show a => Show (List a)@).
fixtures :: [String]
fixtures =
  [ "Show a => a -> Text",
    "(Show a, Eq a) => [a] -> [a]",
    "Functor f => (a -> b) -> f a -> f b",
    "(Monad m, Traversable t) => (a ->{e} m b) -> t a ->{e} m (t b)",
    "Show a => Show (List a)"
  ]

test :: Test ()
test =
  scope "implicit-printer" . tests $
    [ scope "round-trip" $ tests (roundTrip <$> fixtures)
    ]

-- | Pick the first non-empty line for the test scope name.
label :: String -> String
label s = case filter (not . null) (lines s) of
  [] -> ""
  (l : _) -> l
