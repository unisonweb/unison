module Unison.Test.TypePrinter where

import EasyTest
import qualified Data.Text as Text
import Unison.Type
import Unison.TypePrinter
import Unison.Symbol (Symbol)
import Unison.Builtin
import Unison.Parser (Ann(..))
import Unison.Reference
import qualified Unison.Util.PrettyPrint as PP

-- Test the result of the pretty-printer.  Expect the pretty-printer to
-- produce output that differs cosmetically from the original code we parsed.
-- Check also that re-parsing the pretty-printed code gives us the same ABT.
-- (Skip that latter check if rtt is false.)
-- Note that this does not verify the position of the PrettyPrint Break elements.
tc_diff_rtt :: Bool -> String -> String -> Int -> Test ()
tc_diff_rtt rtt s expected width =
   let input_term = Unison.Builtin.t s :: Unison.Type.AnnotatedType Symbol Ann
       get_names x = case x of
                       Builtin t -> t
                       Derived _ -> Text.empty
       actual = if width == 0
                then PP.renderUnbroken $ pretty get_names (-1) input_term
                else PP.renderBroken width True '\n' $ pretty get_names (-1) input_term
       actual_reparsed = Unison.Builtin.t actual
   in scope s $ tests [(
       if actual == expected then ok
       else do note $ "expected: " ++ show expected
               note $ "actual  : "   ++ show actual
               note $ "show(input)  : "   ++ show input_term
               crash "actual != expected"
       ), (
       if (not rtt) || (input_term == actual_reparsed) then ok
       else do note $ "round trip test..."
               note $ "single parse: " ++ show input_term
               note $ "double parse: " ++ show actual_reparsed
               crash "single parse != double parse"
       )]

-- As above, but do the round-trip test unconditionally.
tc_diff s expected = tc_diff_rtt True s expected 0

-- As above, but expect not even cosmetic differences between the input string
-- and the pretty-printed version.
tc s = tc_diff s s

-- Use renderBroken to render the output to some maximum width.
tc_breaks s width expected = tc_diff_rtt True s expected width

test :: Test ()
test = scope "typeprinter" . tests $
  [ tc "a -> b"
  , tc "()"
  , tc "Pair"
  , tc "Pair a b"
  , tc "Pair a a"
  , tc_diff "((a))" $ "a"
  , tc "Pair a ()" -- unary tuple
  , tc "(a, a)"
  , tc "(a, a, a)"
  , tc "(a, b, c, d)"
  , tc "Pair a (Pair a a)"
  , tc "Pair (Pair a a) a"
  , tc "{} (Pair a a)"
  , tc "a ->{} b"
  , tc "a ->{e1} b"
  , tc "a ->{e1, e2} b -> c ->{} d"
  , tc "a ->{e1, e2} b ->{} c -> d"
  , tc "a -> b -> c ->{} d"
  , tc "a -> b ->{} c -> d"
  , tc "{e1, e2} (Pair a a)"
  , tc "Pair (a -> b) (c -> d)"
  , tc "Pair a b ->{e1, e2} Pair a b ->{} Pair (a -> b) d -> Pair c d"
  , tc "[Pair a a]"
  , tc "'a"
  , tc "'Pair a a"
  , tc "a -> 'b"
  , tc "'(a -> b)"
  -- The next six tests will change after the pretty-printer can reverse
  -- generalizeEffects.  Also, they currently fail the round-trip test because
  -- generalizeEffects leaves the `Forall 𝛆` on the outside of the expression,
  -- whereas the parse of the current explicit post-pretty-print version does not.
  , tc_diff_rtt False "(a -> b) -> c" "(a ->{𝛆} b) -> c" 0
  , tc_diff_rtt False "'a -> b" "'{𝛆} a -> b" 0
  , tc_diff_rtt False "a -> 'b -> c" "a -> '{𝛆} b -> c" 0
  , tc_diff_rtt False "a -> (b -> c) -> d" "a -> (b ->{𝛆} c) -> d" 0
  , tc_diff_rtt False "(a -> b) -> c -> d" "(a ->{𝛆} b) -> c -> d" 0
  , tc_diff_rtt False "((a -> b) -> c) -> d" "((a ->{𝛆} b) ->{𝛆} c) -> d" 0
  -- This test is pending for a similar reason - need to faithfully reverse generalizeLowercase.
  , pending $ tc_diff "(∀ a . 'a) -> ()" $ "('{𝛆} a) -> ()"  -- note rank-2: pretty-printer needs to avoid suppressing the forall
  , tc "a -> '(b -> c)"
  , tc "a -> b -> c -> d"
  , tc "a -> 'Pair b c"
  , tc "a -> b -> 'c"
  , tc "a ->{e} 'b"
  , tc "a -> '{e} b"
  , tc "a -> '{e} b -> c"
  , tc "a -> '{e} b ->{f} c"
  , tc "a -> '{e} (b -> c)"
  , tc "a -> '{e} (b ->{f} c)"
  , tc "a -> 'b"
  , tc "a -> '('b)"
  , tc "a -> '('(b -> c))"
  , tc "a -> '('('(b -> c)))"
  , tc "a -> '{e} ('('(b -> c)))"
  , tc "a -> '('{e} ('(b -> c)))"
  , tc "a -> '('('{e} (b -> c)))"
  , tc "a -> 'b ->{f} c"
  , tc "a -> '(b -> c)"
  , tc "a -> '(b ->{f} c)"
  , tc "a -> '{e} ('b)"
  , pending $ tc "a -> '{e} 'b"      -- issue #249
  , pending $ tc "a -> '{e} '{f} b"  -- issue #249
  , tc "a -> '{e} ('b)"
  , tc_diff "a -> () ->{e} () -> b -> c" $ "a -> '{e} ('(b -> c))"
  , tc "a -> '{e} ('(b -> c))"
  , tc_diff "a ->{e} () ->{f} b" $ "a ->{e} '{f} b"
  , tc "a ->{e} '{f} b"
  , tc_diff "a -> () ->{e} () ->{f} b" $ "a -> '{e} ('{f} b)"
  , tc "a -> '{e} ('{f} b)"
  , tc "a -> '{e} () ->{f} b"
  , tc "a -> '{e} ('{f} (b -> c))"
  , tc "a ->{e} '(b -> c)"
  , tc "a -> '{e} (b -> c)"
  , tc_diff "a -> () ->{e} () -> b" $ "a -> '{e} ('b)"
  , tc "'{e} a"
  , tc "'{e} (a -> b)"
  , tc "'{e} (a ->{f} b)"
  , tc "'(a -> 'a)"
  , tc "'()"
  , tc "'('a)"
  , pending $ tc "''a"  -- issue #249
  , pending $ tc "'''a" -- issue #249
  , tc_diff "∀ a . a" $ "a"
  , tc_diff "∀ a. a" $ "a"
  , tc_diff "∀ a . 'a" $ "'a"
  , pending $ tc_diff "∀a . a" $ "a" -- lexer doesn't accept, treats ∀a as one lexeme - feels like it should work
  , pending $ tc_diff "∀ A . 'A" $ "'A"  -- 'unknown parse error' - should this be accepted?

  , pending $ tc_breaks "a -> b -> c -> d" 10 $  -- hitting 'unexpected Semi' in the reparse
              "a\n\
              \-> b\n\
              \-> c\n\
              \-> d"

  , pending $ tc_breaks "a -> Pair b c -> d" 14 $  -- ditto, and extra line breaks that seem superfluous in Pair
              "a\n\
              \-> Pair b c\n\
              \-> d"

  , pending $ tc_breaks "a -> Pair b c -> d" 10 $  -- as above, and missing indentation, pending fix to Nest rendering
              "a\n\
              \-> Pair\n\
              \b\n\
              \c\n\
              \-> d"

  , pending $ tc_breaks "Pair (forall a. a -> a -> a) b" 26 $   -- as above, and more indenting would be nice
              "Pair\n\
              \(∀ a . (a\n\
              \-> a\n\
              \-> a))\n\
              \b"

  , pending $ tc_breaks "Pair (forall a. a -> a -> a) b" 18 $   -- ditto
              "Pair (∀ a .\n\
              \ a\n\
              \ -> a\n\
              \ -> a) b"

  ]
