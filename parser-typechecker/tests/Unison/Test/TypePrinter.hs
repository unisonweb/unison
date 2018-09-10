module Unison.Test.TypePrinter where

import EasyTest
import qualified Data.Text as Text
import Unison.Type
import Unison.TypePrinter
import Unison.Symbol (Symbol)
import Unison.Builtin
import Unison.Parser (Ann(..))
import Unison.Reference

-- Test the result of the pretty-printer.  Expect the pretty-printer to
-- produce output that differs cosmetically from the original code we parsed.
-- Check also that re-parsing the pretty-printed code gives us the same ABT.
tc_diff :: String -> String -> Test ()
tc_diff s expected =
   let input_term = Unison.Builtin.t s :: Unison.Type.AnnotatedType Symbol Ann
       get_names x = case x of
                       Builtin t -> t
                       Derived _ -> Text.empty
       actual = pretty get_names 0 input_term
       actual_reparsed = Unison.Builtin.t actual
   in scope s $ tests [(
       if actual == expected then ok
       else do note $ "expected: " ++ show expected
               note $ "actual  : "   ++ show actual
               crash "actual != expected"
       ), (
       if input_term == actual_reparsed then ok
       else do note $ "round trip test..."
               note $ "single parse: " ++ show input_term
               note $ "double parse: " ++ show actual_reparsed
               crash "single parse != double parse"
       )]

-- As above, but expect not even cosmetic differences between the input string
-- and the pretty-printed version.
tc_same s = tc_diff s s

test :: Test ()
test = scope "typeprinter" . tests $
  [ tc_same "a -> b"
  , tc_diff "(a -> b)" $ "a -> b"
  , tc_same "Pair"
  , tc_same "Pair a a"
  , tc_same "Pair (Pair a a) a"
  , tc_same "{} (Pair a a)"
  , tc_same "a ->{} b"
  , tc_same "a ->{e1} b"
  , tc_same "a ->{e1, e2} b -> c ->{} d"
  , tc_same "a ->{e1, e2} b ->{} c -> d"
  , tc_same "{e1, e2} (Pair a a)"
  , tc_same "Pair (a -> b) (c -> d)"
  , tc_same "Pair a b ->{e1, e2} Pair a b ->{} Pair (a -> b) d -> Pair c d"
  , tc_same "[Pair a a]"
  , tc_same "()"
  ]
