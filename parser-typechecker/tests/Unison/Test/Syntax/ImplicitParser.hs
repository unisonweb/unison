{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Snapshot tests for the implicit-parameters surface syntax.
--
--   * '=>' constraint arrow: see also @TypeParser.hs@; this module
--     exercises the constraint arrow in *term* contexts (inside
--     @summon@ and inside @given@ signatures).
--   * Top-level @given@ declarations, @summon T@ expressions, and
--     @let given@ block statements.
--   * @\@@-positional explicit override at call sites — @f \@ d x@
--     fills @f@'s next implicit slot with @d@, then applies to @x@.
--     The same @\@@ token is also used in pattern position
--     (as-patterns, e.g. @Foo\@Bar@) and in doc-block position
--     (@\@rewrite@); all three uses are confirmed below to coexist.
--
-- These are parser-only tests: @summon T@ desugars to a typed hole
-- and a @given@ declaration to a regular type-annotated binding;
-- @f \@ d@ desugars to ordinary positional application (with the
-- argument's source range widened to include the leading @\@@).
module Unison.Test.Syntax.ImplicitParser where

import Data.Functor.Identity (Identity (..))
import Data.List (intercalate)
import Data.Text qualified as Text
import EasyTest
import Text.RawString.QQ
import Unison.ABT qualified as ABT
import Unison.Parser.Ann (Ann (..))
import Unison.Parser.Ann qualified as Ann
import Unison.Parsers qualified as Ps
import Unison.PrintError (renderParseErrorAsANSI)
import Unison.Symbol (Symbol)
import Unison.Syntax.FileParser (file)
import Unison.Syntax.Parser qualified as P
import Unison.Syntax.TermParser qualified as TP
import Unison.Term qualified as Term
import Unison.Test.Common qualified as Common
import Unison.UnisonFile (UnisonFile)

test :: Test ()
test =
  scope "implicit-parser" . tests $
    [ scope "summon-expr" $ tests (parsesTerm <$> summonExprs),
      scope "let-given" $ tests (parsesTerm <$> letGivens),
      scope "top-level-given" $ tests (parsesFile <$> topLevelGivens),
      scope "override-app" $ tests (parsesTerm <$> overrideApps),
      scope "as-pattern-still-works" $ tests (parsesTerm <$> asPatterns),
      scope "doc-rewrite-still-works" $ tests (parsesTerm <$> docRewrites),
      scope "constraint-in-given-signature" $ tests (parsesTerm <$> constraintInGivenSig),
      scope "combination" $ tests (parsesTerm <$> combinations),
      scope "combination-file" $ tests (parsesFile <$> combinationFiles),
      scope "summon.malformed" $ tests (failsToParseTerm <$> summonNegatives),
      scope "given.malformed" $ tests (failsToParseFile <$> givenFileNegatives),
      scope "let-given.malformed" $ tests (failsToParseTerm <$> letGivenNegatives),
      scope "override.malformed" $ tests (failsToParseTerm <$> overrideNegatives),
      scope "override.structural" overrideArgWidensAnnotation
    ]

-- | @summon@ in expression position.
--
-- @summon@ is a regular term reference whose declared type
-- @forall a. a => a@ drives implicit resolution via the normal
-- machinery. The user pins the type either with an explicit
-- annotation — @(summon : T)@ — or implicitly via the surrounding
-- context (e.g. a function argument whose parameter type fixes it).
--
-- These exercise the parser only; the typechecker later wires the
-- @=>@ on @summon@'s declared type to a 'ConstraintGoal'.
summonExprs :: [String]
summonExprs =
  [ -- bare reference (no annotation; typechecker will need context)
    "summon",
    -- explicit annotation
    "(summon : Nat)",
    -- parenthesized list type
    "(summon : [Nat])",
    -- nested: summon used as a function argument
    "id (summon : Nat)",
    -- annotation that is a parenthesized arrow type
    "(summon : (Nat -> Nat))"
  ]

-- | @let given name : T = body@ inside a let block. The block syntax
-- relies on @binding@ accepting the @given@ prefix; see the call
-- site in @Unison.Syntax.TermParser.statement@.
--
-- These run through @TP.term@ with the builtins-only parsing env,
-- so we stick to types that resolve there.
letGivens :: [String]
letGivens =
  [ unlines
      [ "let",
        "  given x : Nat = 42",
        "  x"
      ],
    unlines
      [ "let",
        "  given xs : [Nat] = [1,2,3]",
        "  xs"
      ]
  ]

-- | Top-level @given@ definitions in a file. The file parser reaches
-- @binding@ from its top-level statement parser, so this exercises
-- the same keyword path as the let-block tests but at file scope.
--
-- We restrict to types that resolve in the builtins-only parsing
-- environment (@Common.parsingEnv@); standard library types like
-- @Show@/@Ord@ are not in scope here, so we use @Nat@ and @[Nat]@
-- as stand-ins. The point is to exercise the @given@-keyword
-- grammar, not to typecheck.
topLevelGivens :: [String]
topLevelGivens =
  [ unlines
      [ "given fortyTwo : Nat = 42",
        "main = fortyTwo"
      ],
    [r|given ones : [Nat] = [1,1,1]
main = ones
|]
  ]

-- | A @given@ whose declared type uses the constraint arrow @=>@.
-- Exercises the wiring between @givenBindingBody@'s type-annotation
-- slot and 'TypeParser.valueType'.
--
-- We exercise this through @let given@ (term position) rather than
-- file scope so we don't depend on type-name resolution: the file
-- parser would reject unknown type names like @Foo@/@Bar@; @TP.term@
-- doesn't run that pass.
constraintInGivenSig :: [String]
constraintInGivenSig =
  [ unlines
      [ "let",
        "  given showList : Show a => Show (List a) = listShow",
        "  showList"
      ],
    unlines
      [ "let",
        "  given fooBar : Foo a => Bar a = bar",
        "  fooBar"
      ],
    -- multi-constraint variant of the same form
    unlines
      [ "let",
        "  given d : (Eq a, Show a) => ShowEq a = mkShowEq",
        "  d"
      ]
  ]

-- | @give@-prefix explicit dictionary at call sites. @give f@
-- syntactically demotes one leading @=>@ in @f@'s declared type to
-- @->@, so the next argument fills the implicit slot positionally.
--
-- We use bare identifiers (not in the builtins-only parsing env) on
-- purpose: at parse time these become free variables, which is
-- enough to exercise the application grammar without depending on
-- the standard library being loaded.
overrideApps :: [String]
overrideApps =
  [ -- the canonical example from the design doc
    "give sort ordDescending xs",
    -- override at the last position
    "give f d",
    -- chained overrides: fills two implicit slots (nest @give@)
    "give (give g) d1 d2 x",
    -- override sandwiched between regular args (parenthesized so it
    -- binds tightly to its function head)
    "h x (give id d) y",
    -- parenthesized dictionary expression as override argument
    "give f (mkOrd compare) xs",
    -- override-only call of a single-implicit function
    "give showD mockShow"
  ]

-- | Realistic combinations that mix multiple implicit-syntax features.
combinations :: [String]
combinations =
  [ -- The @give@-supplied dictionary is itself a @summon@ expression
    -- with a @=>@ arrow inside its annotation.
    "give sort (summon : Ord a => Ord [a]) xs",
    -- @summon@ inside the body of a @let given@.
    unlines
      [ "let",
        "  given d : Nat = (summon : Nat)",
        "  d"
      ],
    -- Nested @give@ — supply an explicit dict to a function whose
    -- result is itself given an explicit dict.
    "give build d (give resolve d2 x)",
    -- @(summon : Nat) + 1@: arithmetic against a summoned value.
    "(summon : Nat) + 1",
    -- @summon@ whose annotation uses a constraint arrow followed by
    -- an effect arrow.
    "(summon : Monad m => (a ->{e} m b) -> m a -> m b)"
  ]

-- | Combinations that have to live at file scope because they
-- contain top-level @given@ declarations.
combinationFiles :: [String]
combinationFiles =
  [ -- Top-level given whose body is a `summon` call.
    unlines
      [ "given fortyTwo : Nat = (summon : Nat)",
        "main = fortyTwo"
      ],
    -- Top-level given referenced through a @give@-prefix in @main@.
    unlines
      [ "given d : Nat = 42",
        "main = give sort d xs"
      ]
  ]

-- | The same @\@@ token still parses as an as-pattern in pattern
-- position. Pattern-context @\@@ is handled by 'pHqNamey' in
-- 'TermParser.hs' (~line 410), a different parser entry-point from
-- the term-application path, so the two grammars are syntactically
-- disjoint.
asPatterns :: [String]
asPatterns =
  [ unlines
      [ "match xs with",
        "  whole@(_ +: _) -> whole",
        "  _ -> []"
      ],
    -- nested as-pattern with constructor sub-pattern
    unlines
      [ "match opt with",
        "  pair@(Some _) -> pair",
        "  None -> None"
      ]
  ]

-- | The @\@rewrite@ syntax — the third use of the @\@@ token —
-- is parsed in 'rewriteBlock' (line ~104) using
-- 'openBlockWith \"\@rewrite\"'. It lives entirely outside
-- 'term4'\'s application path, so the application-path changes do
-- not affect it. The form is @\@rewrite term LHS ==> RHS@ and friends.
docRewrites :: [String]
docRewrites =
  [ unlines
      [ "@rewrite",
        "  term x ==> x"
      ],
    -- two rules in one block
    unlines
      [ "@rewrite",
        "  term x ==> x",
        "  term y ==> y"
      ]
  ]

-- | Negative cases for @summon@. Each pair is @(source, why)@; @why@
-- shows up in the test scope name.
--
-- @summon@ is a regular term reference rather than a keyword, so
-- bare @summon@ is a syntactically valid term. (The typechecker
-- still rejects it without surrounding type context — but that is
-- a /typecheck/ failure, not a parse one.)
summonNegatives :: [(String, String)]
summonNegatives =
  [ -- `summon` followed by a stray closing token in expression
    -- position: still a parse failure because @)@ cannot follow an
    -- identifier here.
    ("summon )", "summon followed by stray closing paren")
  ]

-- | Negative cases for @given@ at top level / in files.
givenFileNegatives :: [(String, String)]
givenFileNegatives =
  [ -- `given` declaration with no body.
    ( unlines
        [ "given x : Nat",
          "main = x"
        ],
      "given with no body"
    ),
    -- Top-level `given x = …` with no `:` annotation. The type
    -- annotation is required; it drives signature-checking and
    -- constraint-goal emission downstream.
    ( unlines
        [ "given x = 42",
          "main = x"
        ],
      "top-level given missing : annotation"
    ),
    -- `given` with no name at all.
    ( unlines
        [ "given : Nat = 42",
          "main = 1"
        ],
      "given with no name"
    )
  ]

-- | Negative cases for @let given@ inside a let-block.
letGivenNegatives :: [(String, String)]
letGivenNegatives =
  [ -- `let given x = …` with no annotation.
    ( unlines
        [ "let",
          "  given x = 42",
          "  x"
        ],
      "let-given missing : annotation"
    ),
    -- `let given` with neither annotation nor body.
    ( unlines
        [ "let",
          "  given x",
          "  x"
        ],
      "let-given with no body or annotation"
    )
  ]

-- | Negative cases for @give@-prefix: malformed @give@ syntax must
-- be rejected, not silently accepted.
overrideNegatives :: [(String, String)]
overrideNegatives =
  [ -- @give@ with no following term.
    ("give", "give with no following term")
  ]

-- | Structural test: parsing @give f d@ must produce an application
-- whose /function/ head has its annotation wrapped with
-- 'Ann.Lowered'. That's the discriminator downstream (typechecker,
-- GivenApply) relies on to skip the implicit-resolution machinery
-- at @f@'s use-site.
overrideArgWidensAnnotation :: Test ()
overrideArgWidensAnnotation = scope "give wraps head annotation with Lowered" $
  case runIdentity (Ps.parse @_ @Symbol TP.term "give f d" Common.parsingEnv) of
    Left e ->
      crash . Text.unpack $ renderParseErrorAsANSI 60 "give f d" e
    Right t -> case Term.unApps t of
      Just (f, [_arg]) ->
        case ABT.annotation f of
          Ann.Lowered _ -> ok
          a -> do
            note $ "expected Lowered annotation on function head; got: " ++ show a
            crash "give-prefix did not wrap head with Ann.Lowered"
      _ -> do
        note $ "expected `give f d` to parse as one application with one arg; got: " ++ show t
        crash "structural mismatch"

parsesTerm :: String -> Test ()
parsesTerm s = scope (label s) $
  case runIdentity (Ps.parse @_ @Symbol TP.term s Common.parsingEnv) of
    Left e -> do
      note . Text.unpack $ renderParseErrorAsANSI 60 s e
      crash . Text.unpack $ renderParseErrorAsANSI 60 s e
    Right _ -> ok

parsesFile :: String -> Test ()
parsesFile s = scope (label s) $
  case runIdentity (P.run (P.rootFile file) s Common.parsingEnv) ::
         Either (P.Err Symbol) (UnisonFile Symbol Ann) of
    Left e -> do
      note . Text.unpack $ renderParseErrorAsANSI 60 s e
      crash . Text.unpack $ renderParseErrorAsANSI 60 s e
    Right _ -> ok

-- | Negative-test helper for term-parser inputs. The parser is
-- expected to fail; on failure we record the error message so a
-- reviewer can confirm it is helpful.
failsToParseTerm :: (String, String) -> Test ()
failsToParseTerm (s, why) = scope (label (s ++ "  -- " ++ why)) $
  case runIdentity (Ps.parse @_ @Symbol TP.term s Common.parsingEnv) of
    Left e -> do
      -- Record the error message so the snapshot suite makes the
      -- diagnostic visible to the next reviewer; if the message is
      -- confusing for a given case, that is a signal to the parser
      -- author to add a more targeted failure.
      note . Text.unpack $ renderParseErrorAsANSI 60 s e
      ok
    Right t -> do
      note $ "expected parse failure (" ++ why ++ "); got: " ++ show t
      crash "parser unexpectedly succeeded"

-- | Negative-test helper for file-parser inputs.
failsToParseFile :: (String, String) -> Test ()
failsToParseFile (s, why) = scope (label (s ++ "  -- " ++ why)) $
  case runIdentity (P.run (P.rootFile file) s Common.parsingEnv) ::
         Either (P.Err Symbol) (UnisonFile Symbol Ann) of
    Left e -> do
      note . Text.unpack $ renderParseErrorAsANSI 60 s e
      ok
    Right _ -> do
      note $ "expected parse failure (" ++ why ++ ")"
      crash "parser unexpectedly succeeded"

-- | Build a distinct test-scope name from a fixture.
--
-- A naïve "first line" picker collides for multi-line fixtures that
-- begin with a layout opener (e.g., two @let@-given fixtures both
-- displaying as @let-given.let@; two @\@rewrite@ fixtures sharing
-- the same first rule). The rules below restore distinctness:
--
--   * Drop a leading layout opener (@let@, @do@, @where@, @with@) —
--     the next non-empty line is the part the reader cares about.
--   * For @\@rewrite@ fixtures, join *all* non-empty content lines
--     with @\" / \"@ so two rule lists differ in their label.
--   * For all other multi-line fixtures, use the first non-empty
--     content line.
label :: String -> String
label s = case filter (not . null) . map trim $ lines s of
  [] -> ""
  [single] -> single
  contentLines@(first : second : _)
    -- Layout openers (@let@, @do@, etc.) carry no distinguishing
    -- info on their own; use the next non-empty content line.
    | first `elem` layoutOpeners -> second
    -- @\@rewrite@: join all content lines so two rewrite fixtures
    -- differ in their labels even when they share a first rule.
    | first == "@rewrite" -> intercalate " / " contentLines
    | otherwise -> first
  where
    trim = dropWhile (== ' ')
    layoutOpeners = ["let", "do", "where", "with"]
