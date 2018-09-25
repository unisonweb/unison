{-# LANGUAGE OverloadedStrings   #-}

module Unison.PrettyPrint where


import Unison.Util.PrettyPrint
import Data.String (IsString)
import Unison.Lexer (symbolyId)
import Data.Either (isRight)

parenthesize :: (Semigroup a, IsString a) => a -> a
parenthesize doc = "(" <> doc <> ")"

parenthesizeIf :: (Semigroup a, IsString a) => Bool -> a -> a
parenthesizeIf cond doc = if cond then parenthesize doc else doc

parenthesizeGroupIf :: (Semigroup a, IsString a) => Bool -> PrettyPrint a -> PrettyPrint a
parenthesizeGroupIf cond doc = parenthesizeIf cond (Group doc)

prettyVar :: (Semigroup a, IsString a) => String -> a -> a
prettyVar a = parenthesizeIf(isRight $ symbolyId a)
