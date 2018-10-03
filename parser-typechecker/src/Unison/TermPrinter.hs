{-# LANGUAGE PatternSynonyms #-}
--{-# LANGUAGE ViewPatterns #-}
--{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
--{-# LANGUAGE OverloadedStrings #-}

module Unison.TermPrinter where

import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Foldable (fold, toList)
import           Data.Maybe (fromMaybe)
import           Data.Vector()
import           Unison.ABT (pattern AbsN')
import qualified Unison.Blank as Blank
import           Unison.PatternP (Pattern)
import qualified Unison.PatternP as Pattern
import           Unison.Reference (Reference(..))
import           Unison.Term
import qualified Unison.TypePrinter as TypePrinter
import           Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.Util.PrettyPrint as PP
import           Unison.Util.PrettyPrint (PrettyPrint(..))

--TODO force, delay, tuples, sequence (also in patterns)
--TODO fix precedence, nesting and grouping, throughout
--TODO print let and case using layout even if renderBroken doesn't kick in?
--TODO more testing, throughout
--TODO use imports to trim fully qualified names

pretty :: Var v => (Reference -> Text) -> Int -> AnnotatedTerm v a -> PrettyPrint String
-- p is the operator precedence of the enclosing context (a number from 0 to 11, or
-- -1 to avoid outer parentheses unconditionally).  Function application has precedence 10.
pretty n p = \case
  Var' v       -> l $ Text.unpack (Var.name v)
  Ref' r       -> l $ Text.unpack (n r)
  Ann' tm t    -> parenNest (p >= 42)  $ PP.Group $
                    pretty n 42 tm <> b" " <> l": " <> TypePrinter.pretty n (-1) t
                    -- TODO do we actually always want to display these annotations?
  Int64' i     -> (if i >= 0 then l"+" else Empty) <> (l $ show i)
  UInt64' u    -> l $ show u
  Float' f     -> l $ show f   -- TODO check this will always contain a '.'
  Boolean' b   -> if b then l"true" else l"false"
  Text' s      -> l $ show s   -- TODO check show escapes ", in the same way as Unison
  Blank' id    -> l"_" <> (l $ fromMaybe "" (Blank.nameb id))
  RequestOrCtor' ref i -> l (Text.unpack (n ref)) <> l"#" <> (l $ show i)  -- TODO presumably I need to take
                                        -- an argument that allows me to determine constructor names.
  Handle' h body -> parenNest (p >= 42) $ PP.Group $
                      l"handle" <> b" " <> pretty n (-1) h <> b" " <> l"in" <> b" "
                      <> PP.Nest " " (PP.Group (pretty n (-1) body))
  Apps' f args -> parenNest (p >= 10) $ PP.Group $ pretty n 9 f <> appArgs args
  Vector' xs   -> PP.Nest " " $ PP.Group $ l"[" <> commaList (toList xs) <> l"]"
  If' cond t f -> parenNest (p >= 42) $ PP.Group $
                    (PP.Group (l"if" <> b" " <> (parenNest (p >= 42) $ pretty n 42 cond)) <> b" " <>
                     PP.Group (l"then" <> b" " <> (parenNest (p >= 42) $ pretty n 42 t)) <> b" " <>
                     PP.Group (l"else" <> b" " <> (parenNest (p >= 42) $ pretty n 42 f)))
  And' x y     -> parenNest (p >= 42) $ PP.Group $ l"and" <> b" " <> pretty n 42 x <> b" " <> pretty n 42 y
  Or' x y      -> parenNest (p >= 42) $ PP.Group $ l"or" <> b" " <> pretty n 42 x <> b" " <> pretty n 42 y
  LamsNamed' vs body -> parenNest (p >= 42) $ PP.Group $
                          (PP.Nest " " $ PP.Group $ varList vs) <>
                          l" ->" <> b" " <> pretty n 0 body
  LetRecNamed' bs e -> printLet bs e  -- TODO really same as Lets' case?
  Lets' bs e ->   printLet bs e
  Match' scrutinee branches -> l"case" <> b" " <> pretty n 42 scrutinee <> b" " <> l"of" <>
                               fold (intersperse (b";") (map printCase branches))
  t -> l"error: " <> l (show t)
  where sepList sep xs = sepList' (pretty n 0) sep xs
        sepList' f sep xs = fold $ intersperse sep (map f xs)
        varList vs = sepList' (\v -> l $ Text.unpack (Var.name v)) (b" ") vs
        commaList = sepList (l"," <> b" ")

        appArgs (x : xs) = b" " <> pretty n 10 x <> appArgs xs
        appArgs [] = Empty

        printLet bs e = parenNest (p >= 42) $ PP.Group $
                        l"let" <> b" " <> lets bs <> pretty n 42 e

        lets ((v, binding) : rest) = (l $ Text.unpack (Var.name v)) <> b" " <> l"=" <> b" " <>
                                     pretty n 42 binding <> b"\n" <> lets rest
        lets [] = Empty

        printCase (MatchCase pat guard (AbsN' vs body)) =
          b" " <> (fst $ prettyPattern n (-1) vs pat) <> b" " <> printGuard guard <> l"->" <> b" " <>
          pretty n (-1) body
        printCase _ = l"error"

        printGuard (Just g) = l"|" <> b" " <> pretty n 42 g <> b" "
        printGuard Nothing = Empty

pretty' :: Var v => (Reference -> Text) -> AnnotatedTerm v a -> String
pretty' n t = PP.renderUnbroken $ pretty n (-1) t

prettyPattern :: Var v => (Reference -> Text) -> Int -> [v] -> Pattern loc -> (PrettyPrint String, [v])
-- vs is the list of pattern variables used by the pattern, plus possibly a tail of variables it doesn't use.
-- This tail is the second component of the return value.
prettyPattern n p vs = \case
  Pattern.Unbound _  -> (l"_", vs)
  Pattern.Var _      -> let (v : tail_vs) = vs
                        in (l $ Text.unpack (Var.name v), tail_vs)
  Pattern.Boolean _ b -> (if b then l"true" else l"false", vs)
  Pattern.Int64 _ i   -> ((if i >= 0 then l"+" else Empty) <> (l $ show i), vs)
  Pattern.UInt64 _ u  -> (l $ show u, vs)
  Pattern.Float _ f   -> (l $ show f, vs)   -- TODO check this will always contain a '.'
  Pattern.Constructor _ ref _ pats -> let (pats_printed, tail_vs) = patterns vs pats
                                      in (parenNest (p >= 42) $ PP.Group $ l (Text.unpack (n ref)) <> pats_printed, tail_vs)
                                      -- TODO use the _ argument to get actual constructor name
  Pattern.As _ pat    -> let (v : tail_vs) = vs
                             (printed, eventual_tail) = prettyPattern n (-1) tail_vs pat  -- TODO check -1
                         in ((l $ Text.unpack (Var.name v)) <> l"@" <> printed, eventual_tail)
  Pattern.EffectPure _ pat -> let (printed, eventual_tail) = prettyPattern n (-1) vs pat
                              in (l"{" <> b" " <> printed <> b" " <> l"}", eventual_tail)
                              -- TODO use constructor ID below
  Pattern.EffectBind _ ref _ pats k_pat -> let (pats_printed, tail_vs) = patterns vs pats
                                               (k_pat_printed, eventual_tail) = prettyPattern n 42 tail_vs k_pat
                                           in (PP.Nest " " $ PP.Group $ l"{" <> b" " <>
                                               l (Text.unpack (n ref)) <> pats_printed <> b" " <> l"->" <> b" " <>
                                               k_pat_printed <> b" " <> l"}", eventual_tail)
  t                   -> (l"error: " <> l (show t), vs)
  where l = Literal
        patterns vs (pat : pats) = let (printed, tail_vs) = prettyPattern n 42 vs pat
                                       (rest_printed, eventual_tail) = patterns tail_vs pats
                                   in (b" " <> printed <> rest_printed, eventual_tail)
        patterns vs [] = (Empty, vs)

paren :: Bool -> PrettyPrint String -> PrettyPrint String
paren True s = PP.Group $ l"(" <> s <> l")"
paren False s = PP.Group s

parenNest :: Bool -> PrettyPrint String -> PrettyPrint String
parenNest useParen contents = PP.Nest " " $ paren useParen contents

l :: String -> PrettyPrint String
l = Literal

b :: String -> PrettyPrint String
b = Breakable
