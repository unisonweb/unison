module Unison.Test.Interpreter where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Unison.Parsers as P
import qualified Unison.Node as Node
import qualified Unison.Note as Note
import qualified Unison.Test.Common as Common

tests :: TestTree
tests = withResource Common.node (\_ -> pure ()) $ \node ->
  let
    tests =
      [ t "1 + 1" "2"
      , t "1 + 1 + 1" "3"
      , t "(x -> x) 42" "42"
      , t "let x = 2; y = 3 ; x + y;;" "5"
      , t "if False then 0 else 1" "1"
      , t "if True then 12 else 13" "12"
      , t "1 >_Number 0" "True"
      , t "1 ==_Number 1" "True"
      , t "2 ==_Number 0" "False"
      , t "1 <_Number 2" "True"
      , t "1 <=_Number 1" "True"
      , t "1 >=_Number 1" "True"
      , t "Comparison.fold 1 0 0 Less" "1"
      , t "Comparison.fold 0 1 0 Equal" "1"
      , t "Comparison.fold 0 0 1 Greater" "1"
      , t "Order.compare (Order.invert <| Order.tuple2 Number.Order Number.Order) (1,2) (1,3)" "Greater"
      , t "Order.compare (Order.invert <| Order.tuple2 Number.Order Number.Order) (2,1) (1,3)" "Less"
      , t "Order.compare (Order.tuple2 Number.Order Order.ignore) (1,2) (1,3)" "Equal"
      , t "Order.compare (Order.tuple2 Order.ignore Number.Order ) (2,2) (1,3)" "Less"
      , t "True `or` False" "True"
      , t "False `or` True" "True"
      , t "True `or` True" "True"
      , t "False `or` False" "False"
      , t "True `and` True" "True"
      , t "True `and` False" "False"
      , t "False `and` True" "False"
      , t "False `and` False" "False"
      , t "not False" "True"
      , t "not True" "False"
      , t "let rec fac n = if n ==_Number 0 then 1 else n * fac (n - 1); fac 5;;" "120"
      , t "let rec ping n = if n >=_Number 10 then n else pong (n + 1); pong n = ping (n + 1); ping 0;;"
          "10"
      , t "let id x = x; g = id 42; p = id \"hi\" ; g;;" "42"
      , t "let id : forall a . a -> a; id x = x; g = id 42; p = id \"hi\" ; g;;" "42"
      , t "(let id x = x; id;; : forall a . a -> a) 42" "42"
      , t "Optional.map ((+) 1) (Some 1)" "Some 2"
      , t "Optional.map ((+) 1) ((Some: ∀ a . a -> Optional a) 1)" "Some 2"
      , t "Either.fold ((+) 1) ((+) 2) (Left 1)" "2"
      , t "Either.fold ((+) 1) ((+) 2) (Right 1)" "3"
      , t "Either.swap (Left 1)" "Either.Right 1"
      , t "Pair.fold (x y -> x) (1, 2)" "1"
      , t "const 41 0" "41"
      , t "1st (1,2,3,4)" "1"
      , t "2nd (1,2 + 1,3,4)" "3"
      , t "identity <| (1 + 1)" "2"
      , t "(1 + 1) |> identity" "2"
      , t "if \"hi\" ==_Text \"hi\" then 1 else 2" "1"
      , t "if \"hi\" <_Text \"hiya\" then 1 else 2" "1"
      , t "if \"hi\" <=_Text \"hiya\" then 1 else 2" "1"
      , t "if \"hiya\" >_Text \"hi\" then 1 else 2" "1"
      , t "if \"hiya\" >=_Text \"hi\" then 1 else 2" "1"
      , t "if \"hi\" >=_Text \"hi\" then 1 else 2" "1"
      , t "if \"hi\" <=_Text \"hi\" then 1 else 2" "1"
      , t "Vector.reverse [1,2,3]" "[3,2,1]"
      , t "Vector.reverse Vector.empty" "[]"
      , t "Vector.fold-right Vector.prepend Vector.empty [1,2,3]" "[1,2,3]"
      , t "Vector.fold-balanced Vector.concatenate Vector.empty (Vector.map Vector.single [1,2,3,4,5])"
          "[1,2,3,4,5]"
      , t "Vector.fold-balanced Vector.concatenate Vector.empty [[1],[2],[3,4],[5]]"
          "[1,2,3,4,5]"
      , t "Vector.fold-balanced (+) 0 [1,2,3]" "6"
      , t "Vector.range 0 10" "[0,1,2,3,4,5,6,7,8,9]"
      , t "Vector.range 0 0" "[]"
      , t "Vector.fold-left (+) 0 (Vector.replicate 5 1)" "5"
      , t "Vector.sort Number.Order identity [5,2,1,3,4]" "[1,2,3,4,5]"
      , t "Vector.sort (Order.invert Number.Order) identity [5,2,1,3,4]" "[5,4,3,2,1]"
      , t "Vector.bind 2nd (Vector.zip [1,2,3] [[1],[2],[3]])" "[1,2,3]"
      , t "Vector.all? identity [True,True,True,True]" "True"
      , t "Vector.all? identity [True,False,True,True]" "False"
      , t "Optional.get-or 96 (Vector.at 1 [0,1,2,3,4])" "1"
      , t "Vector.take 0 [1,2,3]" "[]"
      , t "Vector.take 2 [1,2,3]" "[1,2]"
      , t "Vector.drop 2 [1,2,3]" "[3]"
      ]
    t uneval eval = testCase (uneval ++ " ⟹  " ++ eval) $ do
      (node, _, builtins) <- node
      -- putStrLn (show $ map fst builtins)
      let term = P.bindBuiltins builtins [] $ P.unsafeParseTerm uneval
      _ <- Note.run $ Node.typeAt node term []
      [(_,_,result)] <- Note.run $ Node.evaluateTerms node [([], term)]
      assertEqual "comparing results" (P.unsafeParseTerm eval) result
  in testGroup "Interpreter" tests

main = defaultMain tests
