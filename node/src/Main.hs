{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Unison.Note
import Unison.Term as E
import Unison.Type as T
import Unison.Typechecker as Typechecker
import Unison.Reference as R

infixr 1 -->
(-->) = T.arrow
numT =  T.lit T.Number

expr :: Term
expr = e where
  e = builtin "View.view" `E.apps`
        [ blank
        , builtin "Color.rgba" `E.apps` [blank, blank, blank, blank] ]
  -- id = lam' ["a"] $ var' "a"
  builtin s = E.ref (R.Builtin s)

showType :: Either Note T.Type -> String
showType (Left err) = show err
showType (Right a) = show a

env :: Applicative f => T.Env f
env r =
  let
    view a = T.app (T.ref (R.Builtin "View")) a
  in pure $ case r of
    Builtin "Color.rgba" -> numT --> numT --> numT --> numT --> T.ref (R.Builtin "Color")
    Builtin "View.view" -> forall' ["a"] $ view (T.v' "a") --> T.v' "a" --> T.v' "a"

main :: IO ()
main = putStrLn . showType $ run $ Typechecker.typeAt env [Fn,Fn] expr
-- main = putStrLn . show $ expr
