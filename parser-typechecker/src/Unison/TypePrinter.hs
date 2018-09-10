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
import qualified Unison.Util.PrettyPrint as PP
import           Unison.Util.PrettyPrint (PrettyPrint(..))

pretty :: Var v => (Reference -> Text) -> Int -> AnnotatedType v a -> PrettyPrint String
-- p is the operator precedence of the enclosing context (a number from 0
-- to 11).  Function application has precedence 10.
pretty n p = \case
  Var' v       -> l $ Text.unpack (Var.name v)
  Ref' r       -> l $ Text.unpack (n r)
  Cycle' _ _   -> l $ "error" -- TypeParser does not currently emit Cycle
  Abs' _       -> l $ "error" -- TypeParser does not currently emit Abs
  Ann' _ _     -> l $ "error" -- TypeParser does not currently emit Ann
  App' (Ref' (Builtin "Sequence")) x -> l"[" ++ pretty n 0 x ++ l"]"
  App' f x     -> paren (p >= 10) $ pretty n 9 f ++ b" " ++ pretty n 10 x
  Effect1' e t -> paren (p >= 10) $ pretty n 9 e ++ l" " ++ pretty n 10 t
  Effects' es  -> effects n p es
  ForallNamed' v body -> case p of
    0 -> pretty n p body
    _ -> paren True $ l"∀ " ++ l (Text.unpack (Var.name v)) ++ l". " ++ pretty n 0 body
  EffectfulArrows' fst rest -> paren (p > 0) $ pretty n 0 fst ++ arrows 0 rest
  _ -> l"error"
  where effects n _ es = l"{" ++ (foldl (++) Empty (intersperse (l"," ++ b" ") (map (pretty n 0) es))) ++ l"}"
        arrows p ((Nothing, t) : rest) = b" " ++ l"-> " ++ pretty n p t ++ arrows p rest
        arrows p ((Just es, t) : rest) = b" " ++ l"->" ++ effects n p es ++ l" " ++ pretty n p t ++ arrows p rest
        arrows _ [] = l""
        paren True s = l"(" ++ s ++ l")"
        paren False s = s
        l = Literal
        b = Breakable
        a ++ b = Append a b

pretty' :: Var v => (Reference -> Text) -> AnnotatedType v a -> String
pretty' n t = PP.renderUnbroken $ pretty n 0 t
