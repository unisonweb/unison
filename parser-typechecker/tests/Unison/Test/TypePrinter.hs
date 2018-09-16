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
-- Note that this does not verify the position of the PrettyPrint Break elements.
tc_diff :: String -> String -> Test ()
tc_diff s expected =
   let input_term = Unison.Builtin.t s :: Unison.Type.AnnotatedType Symbol Ann
       get_names x = case x of
                       Builtin t -> t
                       Derived _ -> Text.empty
       actual = PP.renderUnbroken $ pretty get_names 0 input_term
       actual_reparsed = Unison.Builtin.t actual
   in scope s $ tests [(
       if actual == expected then ok
       else do note $ "expected: " ++ show expected
               note $ "actual  : "   ++ show actual
               note $ "show(input)  : "   ++ show input_term
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
tc s = tc_diff s s

test :: Test ()
test = scope "typeprinter" . tests $
  [ tc "a -> b"
  , tc "()"
  , tc "Pair"
  , tc "Pair a b"
  , tc "Pair a a"
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
--TODO, tc "(a -> b) -> c" -- I need to strip out the effect variable added into argument for effect polymorphism.
--TODO, tc "'a -> b" -- same as above; pretty = "'{\120518} a -> b"; show input = "(𝛆. (a. (b. (() -> (({[𝛆]} a))) -> b)))"
--TODO, tc "a -> 'b -> c" -- ditto
  , tc "a -> '(b -> c)"
  , tc "a -> 'Pair b c"
  , tc "a -> b -> 'c"
  , tc "a ->{e} 'b"
  , tc "a -> '{e} b"  -- Ideally this would output "a ->'{e} b" but the lexer can't yet handle that.
  , tc "a -> '{e} b -> c"
  , tc "a -> '{e} b ->{f} c"
  , tc "a -> '{e} (b -> c)"
  , tc "a -> '{e} (b ->{f} c)"
  , tc "a -> 'b"
  , tc "a -> 'b ->{f} c"
  , tc "a -> '(b -> c)"
  , tc "a -> '(b ->{f} c)"
  , tc "a -> '{e} (() -> b)"
  , tc_diff "a -> '{e} ('b)" $ "a -> '{e} (() -> b)"  -- parser can't handle "a -> '{e} 'b"
    -- TODO re above, I should remove the prevention of outputting chained delays
  , tc_diff "a -> () ->{e} () -> b -> c" $ "a -> '{e} (() -> b -> c)"
  , tc "a -> '{e} (() -> b -> c)"
  , tc_diff "a ->{e} () ->{f} b" $ "a ->{e} '{f} b"
  , tc "a ->{e} '{f} b"
  , tc_diff "a -> () ->{e} () ->{f} b" $ "a -> '{e} (() ->{f} b)"
  , tc "a -> '{e} () ->{f} b"
  , tc "a ->{e} '(b -> c)"
  , tc "a -> '{e} (b -> c)"
  , tc_diff "a -> () ->{e} () -> b" $ "a -> '{e} (() -> b)"
  , tc "'{e} a"
  , tc "'{e} (a -> b)"
  , tc "'{e} (a ->{f} b)"
  , tc "'(a -> 'a)"
  , tc "'()"
  , tc_diff "'('a)" $ "'(() -> a)"  -- lexer can't handle "''a"
  ]
