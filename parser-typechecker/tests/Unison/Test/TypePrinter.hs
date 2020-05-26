module Unison.Test.TypePrinter where

import EasyTest
import qualified Data.Map as Map
import Unison.TypePrinter
import qualified Unison.Builtin
import Unison.Util.ColorText (toPlain)
import qualified Unison.Util.Pretty as PP
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Test.Common as Common


-- Test the result of the pretty-printer.  Expect the pretty-printer to
-- produce output that differs cosmetically from the original code we parsed.
-- Check also that re-parsing the pretty-printed code gives us the same ABT.
-- (Skip that latter check if rtt is false.)
-- Note that this does not verify the position of the PrettyPrint Break elements.
tc_diff_rtt :: Bool -> String -> String -> Int -> Test ()
tc_diff_rtt rtt s expected width =
   let input_type = Common.t s
       get_names = PPE.fromNames Common.hqLength Unison.Builtin.names
       prettied = fmap toPlain $ PP.syntaxToColor $ prettyRaw get_names Map.empty (-1) input_type
       actual = if width == 0
                then PP.renderUnbroken $ prettied
                else PP.render width $ prettied
       actual_reparsed = Common.t actual
   in scope s $ tests [(
       if actual == expected then ok
       else do note $ "expected: " ++ show expected
               note $ "actual  : "   ++ show actual
               note $ "expectedS:\n"   ++ expected
               note $ "actualS:\n"   ++ actual
               note $ "show(input)  : "   ++ show input_type
               note $ "prettyprint  : "   ++ show prettied
               crash "actual != expected"
       ), (
       if (not rtt) || (input_type == actual_reparsed) then ok
       else do note $ "round trip test..."
               note $ "single parse: " ++ show input_type
               note $ "double parse: " ++ show actual_reparsed
               note $ "prettyprint  : "   ++ show prettied
               crash "single parse != double parse"
       )]

-- As above, but do the round-trip test unconditionally.
tc_diff :: String -> String -> Test ()
tc_diff s expected = tc_diff_rtt True s expected 0

-- As above, but expect not even cosmetic differences between the input string
-- and the pretty-printed version.
tc :: String -> Test ()
tc s = tc_diff s s

tc' :: String -> String -> Test ()
tc' s1 s2 = tc_diff_rtt False s1 s2 80

-- Use renderBroken to render the output to some maximum width.
tc_breaks :: String -> Int -> String -> Test ()
tc_breaks s width expected = tc_diff_rtt True s expected width

test :: Test ()
test = scope "typeprinter" . tests $
  [ tc' "a -> b" "'b"
  , tc "()"
  , tc "Pair"
  , tc "Pair a b"
  , tc "Pair a a"
  , tc_diff "((a))" "a"
  , tc "Pair a ()" -- unary tuple
  , tc "(a, a)"
  , tc "(a, a, a)"
  , tc "(a, b, c, d)"
  , tc "Pair a (Pair a a)"
  , tc "Pair (Pair a a) a"
  , tc "{} (Pair a a)"
  , tc' "a ->{} b" "'{} b"
  , tc' "a ->{e1} b" "'{e1} b"
  , tc "A ->{E1, E2} B -> C ->{} D"
  , tc' "a ->{e1, e2} b -> c ->{} d" "'''d"
  , tc' "a ->{e1, e2} b ->{} c -> d" "'''d"
  , tc "A ->{E1, E2} B ->{} C -> D"
  , tc "A -> B ->{} C -> D"
  , tc' "a -> b ->{} c -> d" "'''d"
  , tc "A -> B -> C ->{} D"
  , tc' "a -> b -> c ->{} d" "'''d"
  , tc "{e1, e2} (Pair a a)"
  , tc "Pair (A -> B) (C -> D)"
  , tc "Pair a b ->{e1, e2} Pair a b ->{} Pair (a -> b) d -> Pair c d"
  , tc "[Pair a a]"
  , tc "'a"
  , tc "'Pair a a"
  , tc' "a -> 'b" "''b"
  , tc' "'(a -> b)" "''b"
  , tc "(a -> b) -> c"
  , tc "'a -> b"
  , tc "∀ A. A -> A"
  , tc "∀ foo.A. foo.A -> foo.A"
  , tc "∀ A B. A -> B -> (A, B)"
  , tc "a -> 'b -> c"
  , tc "a -> (b -> c) -> d"
  , tc "(A -> B) -> C -> D"
  , tc' "(a -> b) -> c -> d" "(a -> b) -> 'd"
  , tc "((a -> b) -> c) -> d"
  , tc "(∀ a. 'a) -> ()"
  , tc "(∀ a. (∀ b. 'b) -> a) -> ()"
  , tc_diff "∀ a. 'a" "'a"
  , tc' "a -> '(b -> c)" "'''c"
  , tc "A -> '(B -> C)"
  , tc "A -> B -> C -> D"
  , tc' "a -> b -> c -> d" "'''d"
  , tc "A -> 'Pair B C"
  , tc' "a -> 'Pair b c" "''Pair b c"
  , tc "A -> B -> 'C"
  , tc' "a -> b -> 'c" "'''c"
  , tc' "a ->{e} 'b" "''b"
  , tc "A ->{E} 'B"
  , tc "A -> '{E} B"
  , tc' "a -> '{e} b" "''b"
  , tc "A -> '{E} B -> C"
  , tc' "a -> '{e} b -> c" "'''c"
  , tc "A -> '{E} B ->{F} C"
  , tc' "a -> '{e} b ->{f} c" "'''c"
  , tc "A -> '{E} (B -> C)"
  , tc' "a -> '{e} (b -> c)" "'''c"
  , tc "A -> '{E} (B ->{F} C)"
  , tc' "a -> '{e} (b ->{f} c)" "'''c"
  , tc "A -> 'B"
  , tc' "a -> 'b" "''b"
  , tc' "A -> '('B)" "A -> ''B"
  , tc' "a -> '('b)" "'''b"
  , tc' "A -> '('(B -> C))" "A -> ''(B -> C)"
  , tc' "a -> '('(b -> c))" "''''c"
  , tc' "A -> '('('(B -> C)))" "A -> '''(B -> C)"
  , tc' "a -> '('('(b -> c)))" "'''''c"
  , tc' "a -> '{e} ('('(b -> c)))" "'''''c"
  , tc' "A -> '{E} ('('(B -> C)))" "A -> '{E} ''(B -> C)"
  , tc' "a -> '('{e} ('(b -> c)))" "'''''c"
  , tc' "A -> '('{E} ('(B -> C)))" "A -> ''{E} '(B -> C)"
  , tc' "a -> '('('{e} (b -> c)))" "'''''c"
  , tc' "A -> '('('{E} (B -> C)))" "A -> '''{E} (B -> C)"
  , tc' "a -> 'b ->{f} c" "'''c"
  , tc "A -> 'B ->{F} C"
  , tc' "a -> '(b -> c)" "'''c"
  , tc "A -> '(B -> C)"
  , tc "A -> '(B ->{F} C)"
  , tc' "a -> '(b ->{f} c)" "'''c"
  , tc "a -> '{e} ('b)" "'''b"
  , tc "A -> '{E} ('B)"
  , pending $ tc "a -> '{e} 'b"      -- issue #249
  , pending $ tc "a -> '{e} '{f} b"  -- issue #249
  , tc "a -> '{e} ('b)"
  , tc_diff "a -> () ->{e} () -> b -> c" "a -> '{e} ('(b -> c))"
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
  , tc_diff_rtt False "'{Nat} (a -> b)" "'{Nat} 'b" 80
  , tc_diff_rtt False "'{Nat} (a ->{Int} b)" "'{Nat} '{Int} b" 80
  , pending $ tc "Pair a '{e} b"                           -- parser hits unexpected '
  , tc_diff_rtt False "Pair a ('{e} b)" "Pair a '{e} b" 80 -- no RTT due to the above
  , tc "'(a -> 'a)"
  , tc "'()"
  , tc' "'('a)" "''a"
  , tc "''a"
  , tc "'''a"
  , tc_diff "∀ a . a" "a"
  , tc_diff "∀ a. a" "a"
  , tc_diff "∀ a . 'a" "'a"
  , pending $ tc_diff "∀a . a" $ "a" -- lexer doesn't accept, treats ∀a as one lexeme - feels like it should work
  , pending $ tc_diff "∀ A . 'A" $ "'A"  -- 'unknown parse error' - should this be accepted?

  , tc_diff_rtt False "A -> B -> C -> D"   -- hitting 'unexpected Semi' in the reparse
              "A\n\
              \-> B\n\
              \-> C\n\
              \-> D" 10

  , tc_diff_rtt False "A -> Pair B C -> D"   -- ditto, and extra line breaks that seem superfluous in Pair
              "A\n\
              \-> Pair B C\n\
              \-> D" 14

  , tc_diff_rtt False "Pair (forall a. (a -> a -> a)) b"    -- as above, and TODO not nesting under Pair
              "Pair\n\
              \  (∀ a. a -> a -> a) b" 24

  , tc_diff_rtt False "Pair (forall a. (a -> a -> a)) b"    -- as above, and TODO not breaking under forall
              "Pair\n\
              \  (∀ a. a -> a -> a)\n\
              \  b" 21

  ]
