{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
-- p is the operator precedence of the enclosing context (a number from 0 to 11, or
-- -1 to avoid outer parentheses unconditionally).  Function application has precedence 10.
pretty n p tp = case tp of
  Var' v       -> l $ Text.unpack (Var.name v)
  Ref' r       -> l $ Text.unpack (n r)
  Cycle' _ _   -> l $ "error" -- TypeParser does not currently emit Cycle
  Abs' _       -> l $ "error" -- TypeParser does not currently emit Abs
  Ann' _ _     -> l $ "error" -- TypeParser does not currently emit Ann
  App' (Ref' (Builtin "Sequence")) x -> PP.Group $ l"[" <> pretty n 0 x <> l"]"
  Tuple' [x]   -> parenNest (p >= 10) $ l"Pair" <> b" " <> pretty n 10 x <> b" " <> l"()"
  Tuple' xs    -> parenNest True $ commaList xs
  Apps' f xs   -> parenNest (p >= 10) $ pretty n 9 f <> appArgs xs
  Effect1' e t -> parenNest (p >= 10) $ pretty n 9 e <> l" " <> pretty n 10 t
  Effects' es  -> effects (Just es)
  ForallNamed' v body ->
    if (p <= 0)
    then pretty n p body
    else paren True $ l"∀ " <> l (Text.unpack (Var.name v)) <> l". " <> PP.Nest "  " (PP.Group $ pretty n 0 body)
  --TODO undo generalizeEffects before printing - see Type.ungeneralizeEffects
  EffectfulArrows' (Ref' (Builtin "()")) rest -> arrows True True rest
  EffectfulArrows' fst rest -> parenNest (p >= 0) $ pretty n 0 fst <> arrows False False rest
  _ -> l"error"
  where commaList xs = fold $ intersperse (l"," <> b" ") (map (pretty n 0) xs)
        effects Nothing = Empty
        effects (Just es) = PP.Group $ l"{" <> commaList es <> l"}"
        arrow delay first mes = (if first then Empty else b" " <> l"->" ) <>
                                (if delay
                                  then (if first then l"'" else l" '")
                                  else Empty) <>
                                effects mes <>
                                if (isJust mes) || (not delay) && (not first) then l" " else Empty

        arrows delay first [(mes, Ref' (Builtin "()"))] =
                                                 arrow delay first mes <> l"()"
        arrows delay first ((mes, Ref' (Builtin "()")) : rest) =
                                                 arrow delay first mes <>
                                                 (paren delay $ arrows True True rest)
        arrows delay first ((mes, arg) : rest) = arrow delay first mes <>
                                                 (paren (delay && (not $ null rest)) $
                                                   pretty n 0 arg <> arrows False False rest)
        arrows False False [] = Empty
        arrows False True [] = Empty  -- not reachable
        arrows True _ [] = Empty      -- not reachable

        appArgs (x : xs) = b" " <> pretty n 10 x <> appArgs xs
        appArgs [] = Empty

        paren True s = PP.Group $ l"(" <> s <> l")"
        paren False s = PP.Group s
        -- TODO fix bug in Nest rendering
        parenNest useParen contents = {-PP.Nest " " $ -} paren useParen contents
        l = Literal
        b = Breakable

pretty' :: Var v => (Reference -> Text) -> AnnotatedType v a -> String
pretty' n t = PP.renderUnbroken $ pretty n (-1) t
