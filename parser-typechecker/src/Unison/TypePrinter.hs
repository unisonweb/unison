{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.TypePrinter where

import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Foldable (fold)
import           Data.Maybe (isJust)
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
  App' (Ref' (Builtin "Sequence")) x -> l"[" <> pretty n 0 x <> l"]"
  Tuple' xs    -> l"(" <> commaList xs <> l")"
  App' f x     -> paren (p >= 10) $ pretty n 9 f <> b" " <> pretty n 10 x
  Effect1' e t -> paren (p >= 10) $ pretty n 9 e <> l" " <> pretty n 10 t
  Effects' es  -> l"{" <> commaList es <> l"}"
  ForallNamed' v body -> case p of
    0 -> pretty n p body
    _ -> paren True $ l"∀ " <> l (Text.unpack (Var.name v)) <> l". " <> pretty n 0 body
  EffectfulArrows' (Ref' (Builtin "()")) rest -> arrows True True rest
  EffectfulArrows' fst rest -> PP.Group $ paren (p > 0) $ pretty n 0 fst <> arrows False False rest
  _ -> l"error"
  where commaList xs = fold $ intersperse (l"," <> b" ") (map (pretty n 0) xs)
        effects Nothing = Empty
        effects (Just es) = l"{" <> commaList es <> l"}"
        arrow delay first mes = (if first then Empty else b" " <> l"->" ) <>
                                (if delay
                                  then (if first then l"'" else (if isJust mes then l" '" else l" '"))
                                  -- really we want to drop the space in the second literal above,
                                  -- but the lexer won't let us.  e.g. we want "a ->'{e} b"
                                  -- rather than "a -> '{e} b"
                                  else Empty) <>
                                effects mes <>
                                if (isJust mes) || (not delay) && (not first) then l" " else Empty

        arrows delay first [(mes, Ref' (Builtin "()"))] = arrow delay first mes <> l"()"
        arrows False first ((mes, Ref' (Builtin "()")) : rest) =
          if (isJust mes)
          then arrow False first mes <> arrows True True rest
          else arrows True first rest
        arrows delay first ((mes, arg) : rest) = arrow delay first mes <>
                                                 (paren (delay && (not $ null rest)) $
                                                   pretty n 0 arg <> arrows False False rest)
        arrows False False [] = Empty
        arrows False True [] = Empty  -- not reachable
        arrows True _ [] = Empty      -- not reachable

        paren True s = l"(" <> s <> l")"
        paren False s = s
        l = Literal
        b = Breakable

-- TODO group pretty much everywhere parens are used
-- TODO `parse . pretty = id` test on all types in test suite
-- TODO some renderBroken testing
-- TODO PR for type pretty-printer
-- TODO terms etc, and more attention to line-breaking behaviour

pretty' :: Var v => (Reference -> Text) -> AnnotatedType v a -> String
pretty' n t = PP.renderUnbroken $ pretty n 0 t
