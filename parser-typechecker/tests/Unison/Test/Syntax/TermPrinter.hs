{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Syntax.TermPrinter (test) where

import qualified Data.Text as Text
import EasyTest
import Unison.ABT (annotation)
import qualified Unison.Builtin
import Unison.Parser.Ann (Ann (..))
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.PrettyPrintEnv.Names as PPE
import Unison.Symbol (Symbol, symbol)
import qualified Unison.Syntax.HashQualified as HQ (unsafeFromVar)
import Unison.Syntax.TermPrinter
import Unison.Term (Term)
import qualified Unison.Term as Term
import Unison.Test.Common (t, tm)
import qualified Unison.Test.Common as Common
import qualified Unison.Type as Type
import qualified Unison.Util.ColorText as CT
import qualified Unison.Util.Pretty as PP

getNames :: PPE.PrettyPrintEnv
getNames = PPE.fromNames Common.hqLength Unison.Builtin.names

-- Test the result of the pretty-printer.  Expect the pretty-printer to
-- produce output that differs cosmetically from the original code we parsed.
-- Check also that re-parsing the pretty-printed code gives us the same ABT.
-- (Skip that latter check if rtt is false.)
-- Note that this does not verify the position of the PrettyPrint Break elements.
tcDiffRtt :: Bool -> String -> String -> PP.Width -> Test ()
tcDiffRtt rtt s expected width =
  let inputTerm = tm s :: Term Symbol Ann
      prettied = CT.toPlain <$> pretty getNames inputTerm
      actual =
        if width == 0
          then PP.renderUnbroken prettied
          else PP.render width prettied
      actualReparsed = tm actual
   in scope s $
        tests
          [ if actual == expected
              then ok
              else do
                note $ "expected:\n" ++ expected
                note $ "actual:\n" ++ actual
                note $ "show(input)  : " ++ show inputTerm
                -- note $ "prettyprint  : " ++ show prettied
                crash "actual != expected",
            if not rtt || (inputTerm == actualReparsed)
              then ok
              else do
                note "round trip test..."
                note $ "single parse: " ++ show inputTerm
                note $ "double parse: " ++ show actualReparsed
                note $ "prettyprint  : " ++ show prettied
                crash "single parse != double parse"
          ]

-- As above, but do the round-trip test unconditionally.
tcDiff :: String -> String -> Test ()
tcDiff s expected = tcDiffRtt True s expected 0

-- As above, but expect not even cosmetic differences between the input string
-- and the pretty-printed version.
tc :: String -> Test ()
tc s = tcDiff s s

-- Use renderBroken to render the output to some maximum width.
tcBreaksDiff :: PP.Width -> String -> String -> Test ()
tcBreaksDiff width s expected = tcDiffRtt True s expected width

tcBreaks :: PP.Width -> String -> Test ()
tcBreaks width s = tcDiffRtt True s s width

tcBinding :: PP.Width -> String -> Maybe String -> String -> String -> Test ()
tcBinding width v mtp tm expected =
  let baseTerm =
        Unison.Test.Common.tm tm :: Term Symbol Ann
      inputType = fmap Unison.Test.Common.t mtp :: Maybe (Type.Type Symbol Ann)
      inputTerm (Just tp) = Term.ann (annotation tp) baseTerm tp
      inputTerm Nothing = baseTerm
      varV = symbol $ Text.pack v
      prettied =
        fmap CT.toPlain $
          PP.syntaxToColor $
            prettyBinding
              getNames
              (HQ.unsafeFromVar varV)
              (inputTerm inputType)
      actual =
        if width == 0
          then PP.renderUnbroken prettied
          else PP.render width prettied
   in scope expected $
        tests
          [ if actual == expected
              then ok
              else do
                note $ "expected: " ++ show expected
                note $ "actual  : " ++ show actual
                note $ "show(input)  : " ++ show (inputTerm inputType)
                note $ "prettyprint  : " ++ show prettied
                crash "actual != expected"
          ]

test :: Test ()
test =
  scope "termprinter" $
    tests
      [ tc "if true then +2 else -2",
        tc "[2, 3, 4]",
        tc "[2]",
        tc "[]",
        tc "true && false",
        tc "false || false",
        tc "g ((true || false) && (f x y))",
        tc "if _something then _foo else _blah",
        tc "3.14159",
        tc "+0",
        tc "0xsabba1234",
        tcDiff "0x00000001" "1",
        tcDiff "+0x00001" "+1",
        tcDiff "-0x0001" "-1",
        tcDiff "0xff" "255",
        tcDiff "+0xff" "+255",
        tcDiff "0o77777777" "16777215", -- Each octal digit is 3 bits, 8 7s is 2^(8*3) - 1
        tc "\"some text\"",
        tc "\"they said \\\"hi\\\"\"",
        pending $ tc "\'they said \\\'hi\\\'\'", -- TODO lexer doesn't support strings with single quotes in
        tc "Rúnar",
        pending $ tc "῎Ανδρα μοι ἔννεπε, Μοῦσα, πολύτροπον", -- TODO lexer does not like classics!
        tc "古池や蛙飛びこむ水の音",
        tc "2 : Nat",
        tc "x -> x && false",
        tc "x y -> x && y",
        tc "x y z -> x && y",
        tc "x y y -> x && y",
        tc "()",
        tc "Cons",
        tc "foo",
        tc "List.empty",
        tc "None",
        tc "Optional.None",
        tc "handle foo with bar",
        tc "Cons 1 1",
        tc
          "let\n\
          \  x = 1\n\
          \  x",
        tcBreaks
          50
          "let\n\
          \  x = 1\n\
          \  x",
        tcBreaks
          50
          "let\n\
          \  x = 1\n\
          \  y = 2\n\
          \  f x y",
        tc
          "let\n\
          \  f = cases\n\
          \    0 -> 0\n\
          \    x -> x\n\
          \  f y",
        tc
          "let\n\
          \  f z = cases\n\
          \    0 -> z\n\
          \    y -> g y\n\
          \  f \"\" 1",
        tc
          "let\n\
          \  f _ = cases\n\
          \    0 -> 0\n\
          \    x -> x\n\
          \  !f 1",
        tc
          "let\n\
          \  f = cases\n\
          \    0, x -> 0\n\
          \    x, 0 -> x\n\
          \  f y",
        tc
          "let\n\
          \  interleave = cases\n\
          \    [], x            -> x\n\
          \    x, []            -> y\n\
          \    h +: t, h2 +: t2 -> [h, h2] ++ interleave t t2\n\
          \  f y",
        pending $ tc "match x with Pair t 0 -> foo t", -- TODO hitting UnknownDataConstructor when parsing pattern
        pending $ tc "match x with Pair t 0 | pred t -> foo t", -- ditto
        pending $ tc "match x with Pair t 0 | pred t -> foo t; Pair t 0 -> foo' t; Pair t u -> bar;", -- ditto
        tcDiffRtt False "match x with () -> foo" "let\n  () = x\n  foo" 0,
        tcDiffRtt False "match x with _ -> foo" "let\n  _ = x\n  foo" 0,
        tcDiffRtt False "match x with y -> y" "let\n  y = x\n  y" 0,
        tc "match x with 1 -> foo",
        tc "match x with +1 -> foo",
        tc "match x with -1 -> foo",
        tcDiffRtt
          False
          "match x with\n\
          \  true  -> foo\n\
          \  false -> bar"
          "match x with\n\
          \  true  -> foo\n\
          \  false -> bar"
          0,
        tcBreaks
          50
          "match x with\n\
          \  true  -> foo\n\
          \  false -> bar",
        tc "match x with false -> foo",
        tcDiff "match x with y@() -> y" "let\n  y@() = x\n  y",
        tcDiff "match x with a@(b@(c@())) -> c" "let\n  a@(b@(c@())) = x\n  c",
        tcDiff "match e with { a } -> z" "let\n  { a } = e\n  z",
        pending $ tc "match e with { () -> k } -> z", -- TODO doesn't parse since 'many leaf' expected before the "-> k"
        -- need an actual effect constructor to test this with
        tc "cases x -> x",
        tc
          "cases\n\
          \  []  -> 0\n\
          \  [x] -> 1\n\
          \  _   -> 2",
        tc "if a then if b then c else d else e",
        tc "handle handle foo with bar with baz",
        tcBreaks
          16
          "match (if a then\n\
          \  b\n\
          \else c) with\n\
          \  112 -> x", -- dodgy layout.  note #517
        tc "handle bar with Pair 1 1",
        tcDiff "handle bar with x -> foo" "handle bar with 'foo",
        tcDiffRtt
          True
          "let\n\
          \  x = (1 : Int)\n\
          \  (x : Int)"
          "let\n\
          \  x : Int\n\
          \  x = 1\n\
          \  (x : Int)"
          50,
        tc "match x with 12 -> (y : Int)",
        tc "if a then (b : Int) else (c : Int)",
        tc "match x with 12 -> if a then b else c",
        tc "match x with 12 -> x -> f x",
        tcDiff "match x with (12) -> x" "match x with 12 -> x",
        tcDiff "match (x) with 12 -> x" "match x with 12 -> x",
        tc "match x with 12 -> x",
        tcDiffRtt
          True
          "match x with\n\
          \  12 -> x"
          "match x with 12 -> x"
          50,
        tcBreaks
          15
          "match x with\n\
          \  12 -> x\n\
          \  13 -> y\n\
          \  14 -> z",
        -- These used to align, but alignment looked very bad when guards were long.
        -- -- R.Ó.B.
        tcBreaks
          21
          "match x with\n\
          \  12 | p x -> x\n\
          \  13 | q x -> y\n\
          \  14 | r x y -> z",
        tcBreaks
          9
          "match x with\n\
          \  112 ->\n\
          \    x\n\
          \  113 ->\n\
          \    y\n\
          \  114 ->\n\
          \    z",
        pending $
          tcBreaks
            19
            "match\n\
            \  myFunction\n\
            \    argument1\n\
            \    argument2\n\
            \with\n\
            \  112 -> x", -- TODO, 'unexpected semi' before 'of' - should the parser accept this?
        tc "if c then x -> f x else x -> g x",
        tc "(f x) : Int",
        tc "(f x) : Pair Int Int",
        tcBreaks
          50
          "let\n\
          \  x = if a then b else c\n\
          \  if x then y else z",
        tc "f x y",
        tc "f x y z",
        tc "f (g x) y",
        tcDiff "(f x) y" "f x y",
        pending $ tc "1.0e-19", -- TODO parser throws UnknownLexeme
        pending $ tc "-1.0e19", -- ditto
        tc "0.0",
        tc "-0.0",
        pending $ tcDiff "+0.0" "0.0", -- TODO parser throws "Prelude.read: no parse" - should it?  Note +0 works for UInt.
        tcBreaksDiff
          21
          "match x with 12 -> if a then b else c"
          "match x with\n\
          \  12 ->\n\
          \    if a then b\n\
          \    else c",
        tcDiffRtt
          True
          "if foo\n\
          \then\n\
          \  true && true\n\
          \  12\n\
          \else\n\
          \  baz.f : Int -> Int\n\
          \  baz.f x = x\n\
          \  13"
          "if foo then\n\
          \  true && true\n\
          \  12\n\
          \else\n\
          \  baz.f : Int -> Int\n\
          \  baz.f x = x\n\
          \  13"
          50,
        tcBreaks
          50
          "if foo then\n\
          \  true && true\n\
          \  12\n\
          \else\n\
          \  baz.f : Int -> Int\n\
          \  baz.f x = x\n\
          \  13",
        tcBreaks
          90
          "handle\n\
          \  a = 5\n\
          \  b =\n\
          \    c = 3\n\
          \    true\n\
          \  false\n\
          \with foo",
        tcBreaks
          50
          "match x with\n\
          \  true  ->\n\
          \    d = 1\n\
          \    false\n\
          \  false ->\n\
          \    f x = x + 1\n\
          \    true",
        pending $
          tcBreaks
            50
            "x -> e = 12\n\
            \     x + 1", -- TODO parser looks like lambda body should be a block, but we hit 'unexpected ='
        tc "x + y",
        tc "x ~ y",
        tc "x + (y + z)",
        tc "x + y + z",
        tc "x + y * z", -- i.e. (x + y) * z !
        tc "x \\ y == z ~ a",
        tc "foo x (y + z)",
        tc "foo (x + y) z",
        tc "foo x y + z",
        tc "foo p q + r + s",
        tc "foo (p + q) r + s",
        tc "foo (p + q + r) s",
        tc "p + q + r + s",
        tcDiffRtt False "(foo.+) x y" "x foo.+ y" 0,
        tc "x + y + f a b c",
        tc "x + y + foo a b",
        tc "foo x y p + z",
        tc "foo p q a + r + s",
        tc "foo (p + q) r a + s",
        tc "foo (x + y) (p - q)",
        tc "x -> x + y",
        tc "if p then x + y else a - b",
        tc "(x + y) : Int",
        tc "!foo",
        tc "!(foo a b)",
        tc "!f a",
        tcDiff "f () a ()" "!(!f a)",
        tcDiff "f a b ()" "!(f a b)",
        tcDiff "!f ()" "!!f",
        tcDiff "!(!foo)" "!!foo",
        tc "'bar",
        tc "'(bar a b)",
        tcDiff "'('bar)" "''bar",
        tcDiff "!('bar)" "!'bar",
        tcDiff "'(!foo)" "'!foo",
        tcDiff "x -> '(y -> 'z)" "''''z",
        tcDiff "'(x -> '(y -> z))" "''''z",
        tc "(\"a\", 2)",
        tc "(\"a\", 2, 2.0)",
        tcDiff "(2)" "2",
        pending $ tcDiff "Pair \"2\" (Pair 2 ())" "(\"2\", 2)", -- TODO parser produced
        --  Pair "2" (Pair 2 ()#0)
        -- instead of
        --  Pair#0 "2" (Pair#0 2 ()#0)
        -- Maybe because in this context the
        -- parser can't distinguish between a constructor
        -- called 'Pair' and a function called 'Pair'.
        pending $ tc "Pair 2 ()", -- unary tuple; fails for same reason as above
        tcDiff "match x with (a, b) -> a" "let\n  (a, b) = x\n  a",
        tcDiff "match x with () -> foo" "let\n  () = x\n  foo",
        pending $ tc "match x with [a, b] -> a", -- issue #266
        pending $ tc "match x with [a] -> a", -- ditto
        pending $ tc "match x with [] -> a", -- ditto
        tcDiff
          "match x with Optional.Some (Optional.Some _) -> ()"
          "let\n  (Optional.Some (Optional.Some _)) = x\n  ()",
        -- need an actual effect constructor to test the following
        pending $ tc "match x with { SomeRequest (Optional.Some _) -> k } -> ()",
        tcBinding
          50
          "foo"
          (Just "Int")
          "3"
          "foo : Int\n\
          \foo = 3",
        tcBinding 50 "foo" Nothing "3" "foo = 3",
        tcBinding
          50
          "foo"
          (Just "Int -> Int")
          "n -> 3"
          "foo : Int -> Int\n\
          \foo n = 3",
        tcBinding 50 "foo" Nothing "n -> 3" "foo n = 3",
        tcBinding 50 "foo" Nothing "n m -> 3" "foo n m = 3",
        tcBinding
          9
          "foo"
          Nothing
          "n m -> 3"
          "foo n m =\n\
          \  3",
        tcBinding
          50
          "+"
          (Just "Int -> Int -> Int")
          "a b -> foo a b"
          "(+) : Int -> Int -> Int\n\
          \a + b = foo a b",
        tcBinding
          50
          "+"
          (Just "Int -> Int -> Int -> Int")
          "a b c -> foo a b c"
          "(+) : Int -> Int -> Int -> Int\n\
          \(+) a b c = foo a b c",
        tcBinding 50 "+" Nothing "a b -> foo a b" "a + b = foo a b",
        tcBinding 50 "+" Nothing "a b c -> foo a b c" "(+) a b c = foo a b c",
        tcBinding 50 "." Nothing "f g x -> f (g x)" "(.) f g x = f (g x)",
        tcBreaks
          32
          "let\n\
          \  go acc a b =\n\
          \    match List.at 0 a with\n\
          \      Optional.None     -> 0\n\
          \      Optional.Some hd1 -> 0\n\
          \  go [] a b",
        tcDiff
          "match x with (Optional.None, _) -> foo"
          "let\n  (Optional.None, _) = x\n  foo",
        tcBreaks 50 "if true then match x with 12 -> x else x",
        tcBreaks 50 "if true then x else match x with 12 -> x",
        pending $ tcBreaks 80 "x -> (if c then t else f)", -- TODO 'unexpected )', surplus parens
        tcDiffRtt
          True
          "'let\n\
          \  foo = bar\n\
          \  baz foo"
          "do\n\
          \  foo = bar\n\
          \  baz foo"
          80,
        tcBreaks
          80
          "!let\n\
          \  foo = bar\n\
          \  baz foo",
        tcDiffRtt
          True
          "foo let\n\
          \      a = 1\n\
          \      b"
          "foo\n\
          \  let\n\
          \    a = 1\n\
          \    b"
          80,
        tcBreaks
          80
          "if\n\
          \  a = b\n\
          \  a\n\
          \then foo\n\
          \else bar",
        tcBreaks 80 "Stream.foldLeft 0 (+) t",
        tcBreaks
          80
          "let\n\
          \  delay = 'isEven\n\
          \  ()",
        tcBreaks
          80
          "let\n\
          \  a = ()\n\
          \  b = ()\n\
          \  c = (1, 2)\n\
          \  ()",
        tcBreaks
          80
          "let\n\
          \  a = [: escaped: \\@ :]\n\
          \  ()",
        -- FQN elision tests
        tcBreaks
          12
          "if foo then\n\
          \  use A x\n\
          \  f x x\n\
          \else\n\
          \  use B y\n\
          \  f y y",
        tcBreaks
          12
          "if foo then\n\
          \  use A x\n\
          \  f x x\n\
          \else\n\
          \  use B x\n\
          \  f x x",
        tcBreaks
          80
          "let\n\
          \  a =\n\
          \    use A x\n\
          \    if foo then f x x else g x x\n\
          \  bar",
        tcBreaks 80 "if foo then f A.x B.x else f A.x B.x",
        tcBreaks 80 "if foo then f A.x A.x B.x else y",
        tcBreaks 80 "if foo then A.f x else y",
        tcBreaks
          13
          "if foo then\n\
          \  use A +\n\
          \  x + y\n\
          \else y",
        tcBreaks
          20
          "if p then\n\
          \  use A x\n\
          \  use B y z\n\
          \  f z z y y x x\n\
          \else q",
        tcBreaks
          30
          "if foo then\n\
          \  use A.X c\n\
          \  use AA.PP.QQ e\n\
          \  f c c e e\n\
          \else\n\
          \  use A.B X.d Y.d\n\
          \  use A.B.X f\n\
          \  g X.d X.d Y.d Y.d f f",
        tcBreaks
          30
          "if foo then\n\
          \  use A.X c\n\
          \  f c c\n\
          \else\n\
          \  use A X.c YY.c\n\
          \  g X.c X.c YY.c YY.c",
        tcBreaks
          20
          "handle\n\
          \  if foo then\n\
          \    use A.X c\n\
          \    f c c\n\
          \  else\n\
          \    use A.Y c\n\
          \    g c c\n\
          \with bar",
        tcBreaks
          20
          "let\n\
          \  a = 2\n\
          \  handle baz\n\
          \  with\n\
          \    use A.X c\n\
          \    if foo then\n\
          \      f c c\n\
          \    else g c c",
        tcBreaks
          28
          "if foo then\n\
          \  f (x : (∀ t. Pair t t))\n\
          \else\n\
          \  f (x : (∀ t. Pair t t))",
        tcBreaks
          15
          "handle\n\
          \  use A x\n\
          \  if f x x then\n\
          \    x\n\
          \  else y\n\
          \with foo", -- missing break before 'then', issue #518
        tcDiff
          "match x with () ->\n  use A y\n  f y y"
          "let\n  () = x\n  f A.y A.y",
        tcBreaks
          12
          "let\n\
          \  use A x\n\
          \  f x x\n\
          \  c = g x x\n\
          \  h x x",
        tcBreaks
          15
          "handle\n\
          \  use A x\n\
          \  f x x\n\
          \with foo",
        tcBreaks
          15
          "let\n\
          \  c =\n\
          \    use A x\n\
          \    f x x\n\
          \  g c",
        tcBreaks
          20
          "if foo then\n\
          \  f x x A.x A.x\n\
          \else g",
        tcBreaks
          27
          "match t with\n\
          \  () ->\n\
          \    a =\n\
          \      use A B.x\n\
          \      f B.x B.x\n\
          \      handle\n\
          \        q =\n\
          \          use A.B.D x\n\
          \          h x x\n\
          \        foo\n\
          \      with foo\n\
          \    bar\n\
          \  _  ->\n\
          \    b =\n\
          \      use A.C x\n\
          \      g x x\n\
          \    bar",
        tcBreaks
          20
          "let\n\
          \  a =\n\
          \    handle\n\
          \      use A x\n\
          \      f x x\n\
          \    with foo\n\
          \  bar",
        tcBreaks
          16
          "let\n\
          \  a =\n\
          \    b =\n\
          \      use A x\n\
          \      f x x\n\
          \    foo\n\
          \  bar",
        tcBreaks
          20
          "let\n\
          \  a =\n\
          \    match x with\n\
          \      42 ->\n\
          \        use A x\n\
          \        f x x\n\
          \  bar",
        tcBreaks
          20
          "let\n\
          \  a =\n\
          \    use A x\n\
          \    b = f x x\n\
          \    c = g x x\n\
          \    foo\n\
          \  bar",
        tcBreaks
          13
          "let\n\
          \  a =\n\
          \    use A p q r\n\
          \    f p p\n\
          \    f q q\n\
          \    f r r\n\
          \  foo",
        tcBreaks
          13
          "let\n\
          \  (x, y) =\n\
          \    use A p\n\
          \    f p p\n\
          \  x",
        -- The following behaviour is possibly not ideal.  Note how the `use A B.x`
        -- would have the same effect if it was under the `c =`.  It doesn't actually
        -- need to be above the `b =`, because all the usages of A.B.X in that tree are
        -- covered by another use statement, the `use A.B x`.  Fixing this would
        -- probably require another annotation pass over the AST, to place 'candidate'
        -- use statements, to then push some of them down on the next pass.
        -- Not worth it!
        tcBreaks
          20
          "let\n\
          \  a =\n\
          \    use A B.x\n\
          \    b =\n\
          \      use A.B x\n\
          \      f x x\n\
          \    c =\n\
          \      g B.x B.x\n\
          \      h A.D.x\n\
          \    foo\n\
          \  bar",
        tcBreaks
          80
          "let\n\
          \  use A x\n\
          \  use A.T.A T1\n\
          \  g = T1 +3\n\
          \  h = T1 +4\n\
          \  i : T -> T -> Int\n\
          \  i p q =\n\
          \    g' = T1 +3\n\
          \    h' = T1 +4\n\
          \    +2\n\
          \  if true then x else x"
      ]
