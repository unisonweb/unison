{-# LANGUAGE OverloadedStrings #-}
module Main where

import Unison.Note
import Unison.Term as E
import Unison.Type as T
import Unison.Typechecker as Typechecker
import Unison.Reference as R

builtin s = E.ref (R.Builtin s)

expr = lam' ["a"] $ var' "a"
expr2 = builtin "Color.rgba"

showType :: Either Note T.Type -> String
showType (Left err) = show err
showType (Right a) = show a

main :: IO ()
-- main = putStrLn . showType $ Typechecker.synthesize' expr
main = putStrLn . show $ dependencies' expr2
