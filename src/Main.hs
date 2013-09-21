module Main where

import Unison.Syntax.Term
import Unison.Syntax.Term.Examples

import Unison.Syntax.Type as T

expr :: Term () ()
expr = identity

main :: IO ()
main = putStrLn (show expr)
