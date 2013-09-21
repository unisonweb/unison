module Main where

import Unison.Syntax.Term
import Unison.Syntax.Term.Examples

expr :: Term () ()
expr = identity

main :: IO ()
main = putStrLn (show expr)
