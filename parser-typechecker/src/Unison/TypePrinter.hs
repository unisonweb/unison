{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Unison.TypePrinter where

import qualified Data.ListLike                 as LL
import qualified Data.Map              as Map
import           Data.Maybe            (isJust)
import           Data.String           (IsString, fromString)
import           Unison.HashQualified  (HashQualified)
import           Unison.NamePrinter    (prettyHashQualified, prettyHashQualified')
import           Unison.PrettyPrintEnv (PrettyPrintEnv, Imports, elideFQN)
import qualified Unison.PrettyPrintEnv as PrettyPrintEnv
import           Unison.Reference      (pattern Builtin)
import           Unison.Type
import           Unison.Util.Pretty    (ColorText, Pretty)
import qualified Unison.Util.Pretty    as PP
import           Unison.Var            (Var)
import qualified Unison.Var            as Var
import qualified Unison.DataDeclaration as DD

{- Explanation of precedence handling

   We illustrate precedence rules as follows.

     >=10
       10f 10x

   This example shows that a type application f x is enclosed in parentheses
   whenever the ambient precedence around it is >= 10, and that when printing
   its two components, an ambient precedence of 10 is used in both places.

   The pretty-printer uses the following rules for printing types.

     >=10
       10f 10x
       { 0e } 10t

     >=0
       0a -> 0b

-}

pretty
  :: forall s v a . (IsString s, LL.ListLike s Char, Var v)
  => PrettyPrintEnv
  -> Imports
  -> Int
  -> AnnotatedType v a
  -> Pretty s
pretty n im p tp = pretty0 n im p (cleanup (removePureEffects tp))

pretty0
  :: forall s v a . (IsString s, LL.ListLike s Char, Var v)
  => PrettyPrintEnv
  -> Imports
  -> Int
  -> AnnotatedType v a
  -> Pretty s
-- p is the operator precedence of the enclosing context (a number from 0 to
-- 11, or -1 to avoid outer parentheses unconditionally).  Function
-- application has precedence 10.
pretty0 n im p tp = go n im p tp
  where
  go :: PrettyPrintEnv -> Imports -> Int -> AnnotatedType v a -> Pretty s
  go n im p tp = case stripIntroOuters tp of
    Var' v     -> PP.text (Var.name v)
    Ref' r     -> prettyHashQualified' $ elideFQN im (PrettyPrintEnv.typeName n r)
    Cycle' _ _ -> fromString "error: TypeParser does not currently emit Cycle"
    Abs' _     -> fromString "error: TypeParser does not currently emit Abs"
    Ann' _ _   -> fromString "error: TypeParser does not currently emit Ann"
    App' (Ref' (Builtin "Sequence")) x ->
      PP.group $ "[" <> go n im 0 x <> "]"
    DD.TupleType' [x] -> PP.parenthesizeIf (p >= 10) $ "Pair" `PP.hang` PP.spaced
      [go n im 10 x, "()"]
    DD.TupleType' xs  -> PP.parenthesizeCommas $ map (go n im 0) xs
    Apps' f xs -> PP.parenthesizeIf (p >= 10) $ go n im 9 f `PP.hang` PP.spaced
      (go n im 10 <$> xs)
    Effect1' e t ->
      PP.parenthesizeIf (p >= 10) $ go n im 9 e <> " " <> go n im 10 t
    Effects' es         -> effects (Just es)
    ForallNamed' v body -> if (p < 0)
      then go n im p body
      else
        paren True
        $         ("∀ " <> PP.text (Var.name v) <> ".")
        `PP.hang` go n im (-1) body
    t@(Arrow' _ _) -> case t of
      EffectfulArrows' (Ref' DD.UnitRef) rest -> arrows True True rest
      EffectfulArrows' fst rest ->
        PP.parenthesizeIf (p >= 0) $ go n im 0 fst <> arrows False False rest
      _ -> "error"
    _ -> "error"
  effects Nothing   = mempty
  effects (Just es) = PP.group $ "{" <> PP.commas (go n im 0 <$> es) <> "}"
  arrow delay first mes =
    (if first then mempty else PP.softbreak <> "->")
      <> (if delay then (if first then "'" else " '") else mempty)
      <> effects mes
      <> if (isJust mes) || (not delay) && (not first) then " " else mempty

  arrows delay first [(mes, Ref' DD.UnitRef)] = arrow delay first mes <> "()"
  arrows delay first ((mes, Ref' DD.UnitRef) : rest) =
    arrow delay first mes <> (parenNoGroup delay $ arrows True True rest)
  arrows delay first ((mes, arg) : rest) =
    arrow delay first mes
      <> (  parenNoGroup (delay && (not $ null rest))
         $  go n im 0 arg
         <> arrows False False rest
         )
  arrows False False [] = mempty
  arrows False True  [] = mempty  -- not reachable
  arrows True  _     [] = mempty  -- not reachable

  paren True  s = PP.group $ "(" <> s <> ")"
  paren False s = PP.group s

  parenNoGroup True  s = "(" <> s <> ")"
  parenNoGroup False s = s

  -- parenNest useParen contents = PP.Nest "  " $ paren useParen contents
  -- b = Breakable

pretty' :: Var v => Maybe Int -> PrettyPrintEnv -> AnnotatedType v a -> String
pretty' (Just width) n t = PP.render width $ pretty n Map.empty (-1) t
pretty' Nothing      n t = PP.render maxBound $ pretty n Map.empty (-1) t

-- todo: provide sample output in comment
prettySignatures'
  :: Var v => PrettyPrintEnv
  -> [(HashQualified, AnnotatedType v a)]
  -> [Pretty ColorText]
prettySignatures' env ts = PP.align
  [ (prettyHashQualified name, (": " <> PP.map fromString (pretty env Map.empty (-1) typ)) `PP.orElse`
                   (": " <> PP.indentNAfterNewline 2 (PP.map fromString (pretty env Map.empty (-1) typ))))
  | (name, typ) <- ts
  ]

-- todo: provide sample output in comment; different from prettySignatures'
prettySignaturesAlt'
  :: Var v => PrettyPrintEnv
  -> [([HashQualified], AnnotatedType v a)]
  -> [Pretty ColorText]
prettySignaturesAlt' env ts = PP.align
  [ (PP.commas . fmap prettyHashQualified $ names, (": " <> PP.map fromString (pretty env Map.empty (-1) typ)) `PP.orElse`
     (": " <> PP.indentNAfterNewline 2 (PP.map fromString (pretty env Map.empty (-1) typ))))
  | (names, typ) <- ts
  ]

-- prettySignatures'' :: Var v => PrettyPrintEnv -> [(Name, AnnotatedType v a)] -> [Pretty ColorText]
-- prettySignatures'' env ts = prettySignatures' env (first HQ.fromName <$> ts)

prettySignatures
  :: Var v
  => PrettyPrintEnv
  -> [(HashQualified, AnnotatedType v a)]
  -> Pretty ColorText
prettySignatures env ts = PP.lines $
  PP.group <$> prettySignatures' env ts

prettySignaturesAlt
  :: Var v
  => PrettyPrintEnv
  -> [([HashQualified], AnnotatedType v a)]
  -> Pretty ColorText
prettySignaturesAlt env ts = PP.lines $
  PP.group <$> prettySignaturesAlt' env ts


