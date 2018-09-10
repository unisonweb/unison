{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.TypePrinter where

import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text
import           Unison.Reference (Reference(..))
import           Unison.Type
import           Unison.Var (Var)
import qualified Unison.Var as Var

pretty :: Var v => (Reference -> Text) -> Int -> AnnotatedType v a -> String
-- p is the operator precedence of the enclosing context (a number from 0
-- to 11).  Function application has precedence 10.
pretty n p = \case
  Var' v       -> Text.unpack (Var.name v)
  Ref' r       -> Text.unpack (n r)
  Cycle' _ _   -> "error" -- TypeParser does not currently emit Cycle
  Abs' _       -> "error" -- TypeParser does not currently emit Abs
  Ann' _ _     -> "error" -- TypeParser does not currently emit Ann
  App' (Ref' (Builtin "Sequence")) x -> "[" ++ pretty n 0 x ++ "]"
  App' f x     -> paren (p >= 10) $ (pretty n 9 f) ++ " " ++ (pretty n 10 x)
  Effect1' e t -> paren (p >= 10) $ (pretty n 9 e) ++ " " ++ (pretty n 10 t)
  Effects' es  -> effects n p es
  ForallNamed' v body -> case p of
    0 -> pretty n p body
    _ -> paren True $ "∀ " ++ (Text.unpack (Var.name v)) ++ ". " ++ (pretty n 0 body)
  EffectfulArrows' fst rest -> paren (p > 0) $ (pretty n 0 fst) ++ (arrows 0 rest)
  _ -> "error"
  where effects n _ es = "{" ++ (intercalate ", " (map (pretty n 0) es)) ++ "}"
        arrows p ((Nothing, t) : rest) = " -> " ++ (pretty n p t) ++ (arrows p rest)
        arrows p ((Just es, t) : rest) = " ->" ++ (effects n p es) ++ " " ++ (pretty n p t) ++ (arrows p rest)
        arrows _ [] = ""
        paren True s = "(" ++ s ++ ")"
        paren False s = s

pretty' :: Var v => (Reference -> Text) -> AnnotatedType v a -> String
pretty' n t = pretty n 0 t
