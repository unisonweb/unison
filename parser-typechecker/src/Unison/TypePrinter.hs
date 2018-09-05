{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.TypePrinter where

import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Unison.ABT as ABT
import           Unison.Reference (Reference)
import           Unison.Type
import           Unison.Var (Var)
import qualified Unison.Var as Var

pretty :: Var v => (Reference -> Text) -> AnnotatedType v a -> String
pretty _ (Var' v) = Text.unpack (Var.name v)
pretty _ (Cycle' _ _) = "error" -- TypeParser does not currently emit Cycle
pretty _ (Abs' _) = "error" -- TypeParser does not currently emit Abs
pretty n (Ref' r) = Text.unpack (n r)
pretty n (Arrow' i o) = "(" ++ (pretty n i) ++ " -> " ++ (pretty n o) ++ ")"
pretty _ (Ann' _ _) = "error" -- TypeParser does not currently emit Ann
pretty n (App' f x) = "(" ++ (pretty n f) ++ " " ++ (pretty n x) ++ ")"
pretty n (Effect1' e t) = "(" ++ (pretty n e) ++ " " ++ (pretty n t) ++ ")"
pretty n (Effects' es) = "{" ++ (intercalate ", " (map (pretty n) es)) ++ "}"
pretty n (Forall' (ABT.Subst _ _ body v)) = pretty n (body (ABT.var v))
pretty _ _ = "error"
