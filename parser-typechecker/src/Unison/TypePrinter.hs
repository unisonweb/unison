{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Unison.TypePrinter where

import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text
import           Unison.Reference (Reference)
import           Unison.Type
import           Unison.Var (Var)
import qualified Unison.Var as Var

pretty :: Var v => (Reference -> Text) -> AnnotatedType v a -> String
pretty n = \case
  Var' v       -> Text.unpack (Var.name v)
  Ref' r       -> Text.unpack (n r)
  Cycle' _ _   -> "error" -- TypeParser does not currently emit Cycle
  Abs' _       -> "error" -- TypeParser does not currently emit Abs
  Ann' _ _     -> "error" -- TypeParser does not currently emit Ann
  App' f x     -> "(" ++ (pretty n f) ++ " " ++ (pretty n x) ++ ")"
  Effect1' e t -> "(" ++ (pretty n e) ++ " " ++ (pretty n t) ++ ")"
  Effects' es  -> effects n es
  ForallNamed' _ body -> pretty n body
  EffectfulArrows' fst rest -> "(" ++ (pretty n fst) ++ (arrows rest) ++ ")"
  _ -> "error"
  where effects n es = "{" ++ (intercalate ", " (map (pretty n) es)) ++ "}"
        arrows ((Nothing, t) : rest) = " -> " ++ (pretty n t) ++ (arrows rest)
        arrows ((Just es, t) : rest) = " ->" ++ (effects n es) ++ " " ++ (pretty n t) ++ (arrows rest)
        arrows [] = ""
