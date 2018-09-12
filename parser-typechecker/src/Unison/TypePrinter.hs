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
  Tuple' xs    -> l"(" <> commaList n xs <> l")"
  App' f x     -> paren (p >= 10) $ pretty n 9 f <> b" " <> pretty n 10 x
  Effect1' e t -> paren (p >= 10) $ pretty n 9 e <> l" " <> pretty n 10 t
  Effects' es  -> l"{" <> commaList n es <> l"}"
  ForallNamed' v body -> case p of
    0 -> pretty n p body
    _ -> paren True $ l"∀ " <> l (Text.unpack (Var.name v)) <> l". " <> pretty n 0 body
  Arrow' (Ref' (Builtin "()")) o -> l"'" <> pretty n 9 o  -- BUG fails to group with subsequent arrows -- BUG misrendering effects on the arrow?
  EffectfulArrows' fst rest -> PP.Group $ paren (p > 0) $ pretty n 0 fst <> arrows 0 rest
  _ -> l"error"
  where commaList n xs = fold $ intersperse (l"," <> b" ") (map (pretty n 0) xs)
        -- pure arrow to a delayed type
        arrows _ ((Nothing, Ref' (Builtin "()")) : (Nothing, t) : rest) =
          b" " <> l"-> '" <> arrowArg (rest /= []) n t rest
        -- effectful arrow to a delayed type
        arrows _ ((Just es, Ref' (Builtin "()")) : (Nothing, t) : rest) =
          b" " <> l"->{" <> commaList n es <> l"} '" <> arrowArg (rest /= []) n t rest
        -- pure arrow to non-delayed type
        arrows p ((Nothing, t) : rest) =
          b" " <> l"-> " <> pretty n p t <> arrows p rest
        -- effectful arrow to non-delayed type
        arrows p ((Just es, t) : rest) =
          b" " <> l"->{" <> commaList n es <> l"} " <> pretty n p t <> arrows p rest
        arrows _ [] = Empty
        arrowArg pred n t rest = let p = if pred then 0 else 9
                                 in paren pred $ pretty n p t <> arrows p rest
        paren True s = l"(" <> s <> l")"
        paren False s = s
        l = Literal
        b = Breakable

pretty' :: Var v => (Reference -> Text) -> AnnotatedType v a -> String
pretty' n t = PP.renderUnbroken $ pretty n 0 t
