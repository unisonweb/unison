module Unison.Test.TypePrinter where

import EasyTest
import qualified Data.Text as Text
import Unison.Type
import Unison.TypePrinter
import Unison.Symbol (Symbol)
import Unison.Builtin
import Unison.Parser (Ann(..))
import Unison.Reference

tc :: String -> String -> Test ()
tc s expected =
   let input_term = Unison.Builtin.t s :: Unison.Type.AnnotatedType Symbol Ann
       get_names x = case x of
                       Builtin t -> t
                       Derived _ -> Text.empty
       actual = pretty get_names input_term
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

test :: Test ()
test = scope "typeprinter" . tests $
  [ tc "a -> b" $ "(a -> b)"
  , tc "Pair" $ "Pair"
  , tc "Pair a a" $ "((Pair a) a)"
  , tc "{} (Pair a a)" $ "({} ((Pair a) a))"
  , tc "a ->{} b" $ "(a ->{} b)"
  , tc "a ->{e1} b" $ "(a ->{e1} b)"
  , tc "a ->{e1, e2} b -> c ->{} d" $ "(a ->{e1, e2} b -> c ->{} d)"
  , tc "a ->{e1, e2} b ->{} c -> d" $ "(a ->{e1, e2} b ->{} c -> d)"
  , tc "{e1, e2} (Pair a a)" $ "({e1, e2} ((Pair a) a))"
  , tc "[Pair a a]" $ "(Sequence ((Pair a) a))"
  , tc "()" $ "()"
  ]
