--{-# LANGUAGE PatternSynonyms #-}
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
import qualified Unison.Blank as Blank
import           Unison.Reference (Reference(..))
import           Unison.Term
import qualified Unison.TypePrinter as TypePrinter
import           Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.Util.PrettyPrint as PP
import           Unison.Util.PrettyPrint (PrettyPrint(..))

--TODO force, delay
--TODO fix precedence, nesting and grouping, throughout
--TODO more testing, throughout

pretty :: Var v => (Reference -> Text) -> Int -> AnnotatedTerm v a -> PrettyPrint String
-- p is the operator precedence of the enclosing context (a number from 0 to 11, or
-- -1 to avoid outer parentheses unconditionally).  Function application has precedence 10.
pretty n p = \case
  Var' v       -> l $ Text.unpack (Var.name v)
  Ref' r       -> l $ Text.unpack (n r)
  Ann' tm t    -> parenNest (p >= 42)  $ PP.Group $
                    pretty n 42 tm <> b" " <> l": " <> TypePrinter.pretty n 42 t
                    -- TODO do we actually always want to display these annotations?
  Int64' i     -> (if i >= 0 then l"+" else Empty) <> (l $ show i)
  UInt64' u    -> l $ show u
  Float' f     -> l $ show f   -- TODO check this will always contain a '.'
  Boolean' b   -> if b then l"true" else l"false"
  Text' s      -> l $ show s   -- TODO check show escapes ", in the same way as Unison
  Blank' id    -> l"_" <> (l $ fromMaybe "" (Blank.nameb id))
  Constructor' ref n -> l $ (show ref <> show n)   -- todo  -- RequestOrCtor'
  Request' ref n -> l $ (show ref <> show n)   -- todo
  Handle' h body -> l $ (show h <> show body)   -- todo
  Apps' f args -> l $ (show f <> show args)   -- todo  -- parenNest (p >= 10) $ PP.Group $ pretty n 9 f <> appArgs xs
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
  LetRecNamed' bs e -> l $ (show bs <> show e)   -- todo
  Lets' bs e -> l $ (show bs <> show e)   -- todo
  Match' scrutinee branches -> l $ (show scrutinee <> show branches)   -- todo
  _ -> l"error"
  where sepList sep xs = sepList' (pretty n 0) sep xs
        sepList' f sep xs = fold $ intersperse sep (map f xs)
        varList vs = sepList' (\v -> l $ Text.unpack (Var.name v)) (b" ") vs
        commaList = sepList (l"," <> b" ")

        {-appArgs (x : xs) = b" " <> pretty n 10 x <> appArgs xs
        appArgs [] = Empty -}

        paren True s = PP.Group $ l"(" <> s <> l")"
        paren False s = PP.Group s

        parenNest useParen contents = PP.Nest " " $ paren useParen contents
        l = Literal
        b = Breakable

pretty' :: Var v => (Reference -> Text) -> AnnotatedTerm v a -> String
pretty' n t = PP.renderUnbroken $ pretty n (-1) t
