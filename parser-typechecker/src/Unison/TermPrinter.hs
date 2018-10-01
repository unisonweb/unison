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
import           Unison.Reference (Reference(..))
import           Unison.Term
import qualified Unison.TypePrinter as TypePrinter
import           Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.Util.PrettyPrint as PP
import           Unison.Util.PrettyPrint (PrettyPrint(..))

--TODO force, delay
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
                                        -- Also a bit confused since I can't see a place in the parser that
                                        -- actually produces Constructor terms except for builtins.
  Handle' h body -> parenNest (p >= 42) $ PP.Group $
                      (PP.Nest " " $ PP.Group $ l"handle" <> b" " <> pretty n 42 h <> b" " <> l"in" <> b" ")
                      <> pretty n 42 body
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
                               printCases branches
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

        printCases ((MatchCase pat guard (AbsN' _ body)) : rest) =
          b" " <> prettyPattern n 42 pat <> b" " <> printGuard guard <> l"->" <> b" " <>
          pretty n 42 body <> b";" <> printCases rest
        printCases [] = Empty
        printCases _ = l"error"

        printGuard (Just g) = l"|" <> b" " <> pretty n 42 g <> b" "
        printGuard Nothing = Empty

        paren True s = PP.Group $ l"(" <> s <> l")"
        paren False s = PP.Group s

        parenNest useParen contents = PP.Nest " " $ paren useParen contents
        l = Literal
        b = Breakable

pretty' :: Var v => (Reference -> Text) -> AnnotatedTerm v a -> String
pretty' n t = PP.renderUnbroken $ pretty n (-1) t

-- TODO rename and move to new file
prettyPattern :: (Reference -> Text) -> Int -> Pattern loc -> PrettyPrint String
prettyPattern _ _ p = Literal $ show p -- todo
