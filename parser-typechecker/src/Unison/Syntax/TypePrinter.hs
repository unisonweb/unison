{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Syntax.TypePrinter
  ( pretty,
    pretty0,
    prettyRaw,
    prettyStr,
    prettySyntax,
    prettySignaturesST,
    prettySignaturesCT,
    prettySignaturesCTCollapsed,
    prettySignaturesAlt,
    prettySignaturesAlt',
  )
where

import qualified Data.Map as Map
import qualified Unison.Builtin.Decls as DD
import Unison.HashQualified (HashQualified)
import Unison.Name (Name)
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import qualified Unison.PrettyPrintEnv as PrettyPrintEnv
import Unison.PrettyPrintEnv.FQN (Imports, elideFQN)
import Unison.Reference (Reference, pattern Builtin)
import Unison.Referent (Referent)
import Unison.Syntax.NamePrinter (styleHashQualified'')
import Unison.Type
import Unison.Util.ColorText (toPlain)
import Unison.Util.Pretty (ColorText, Pretty, Width)
import qualified Unison.Util.Pretty as PP
import qualified Unison.Util.SyntaxText as S
import Unison.Var (Var)
import qualified Unison.Var as Var

type SyntaxText = S.SyntaxText' Reference

pretty :: forall v a. (Var v) => PrettyPrintEnv -> Type v a -> Pretty ColorText
pretty ppe = PP.syntaxToColor . prettySyntax ppe

prettySyntax :: forall v a. (Var v) => PrettyPrintEnv -> Type v a -> Pretty SyntaxText
prettySyntax ppe = pretty0 ppe mempty (-1)

prettyStr :: Var v => Maybe Width -> PrettyPrintEnv -> Type v a -> String
prettyStr (Just width) n t =
  toPlain $ PP.render width $ PP.syntaxToColor $ pretty0 n Map.empty (-1) t
prettyStr Nothing n t =
  toPlain $ PP.render maxBound $ PP.syntaxToColor $ pretty0 n Map.empty (-1) t

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

pretty0 ::
  forall v a.
  (Var v) =>
  PrettyPrintEnv ->
  Imports ->
  Int ->
  Type v a ->
  Pretty SyntaxText
pretty0 n im p tp = prettyRaw n im p (cleanup (removePureEffects tp))

prettyRaw ::
  forall v a.
  (Var v) =>
  PrettyPrintEnv ->
  Imports ->
  Int ->
  Type v a ->
  Pretty SyntaxText
-- p is the operator precedence of the enclosing context (a number from 0 to
-- 11, or -1 to avoid outer parentheses unconditionally).  Function
-- application has precedence 10.
prettyRaw n im p tp = go n im p tp
  where
    go :: PrettyPrintEnv -> Imports -> Int -> Type v a -> Pretty SyntaxText
    go n im p tp = case stripIntroOuters tp of
      Var' v -> fmt S.Var $ PP.text (Var.name v)
      DD.TupleType' xs | length xs /= 1 -> PP.parenthesizeCommas $ map (go n im 0) xs
      -- Would be nice to use a different SyntaxHighlights color if the reference is an ability.
      Ref' r -> styleHashQualified'' (fmt $ S.TypeReference r) $ elideFQN im (PrettyPrintEnv.typeName n r)
      Cycle' _ _ -> fromString "error: TypeParser does not currently emit Cycle"
      Abs' _ -> fromString "error: TypeParser does not currently emit Abs"
      Ann' _ _ -> fromString "error: TypeParser does not currently emit Ann"
      App' (Ref' (Builtin "Sequence")) x ->
        PP.group $ (fmt S.DelimiterChar "[") <> go n im 0 x <> (fmt S.DelimiterChar "]")
      Apps' f xs ->
        PP.parenthesizeIf (p >= 10) $
          go n im 9 f
            `PP.hang` PP.spaced
              (go n im 10 <$> xs)
      Effect1' e t ->
        PP.parenthesizeIf (p >= 10) $ go n im 9 e <> " " <> go n im 10 t
      Effects' es -> effects (Just es)
      ForallsNamed' vs' body ->
        let vs = filter (\v -> Var.name v /= "()") vs'
         in if p < 0 && all Var.universallyQuantifyIfFree vs
              then go n im p body
              else
                paren (p >= 0) $
                  let vformatted = PP.sep " " (fmt S.Var . PP.text . Var.name <$> vs)
                   in (fmt S.TypeOperator "∀ " <> vformatted <> fmt S.TypeOperator ".")
                        `PP.hang` go n im (-1) body
      t@(Arrow' _ _) -> case t of
        EffectfulArrows' (Ref' DD.UnitRef) rest ->
          PP.parenthesizeIf (p >= 10) $ arrows True True rest
        EffectfulArrows' fst rest ->
          case fst of
            Var' v
              | Var.name v == "()" ->
                  PP.parenthesizeIf (p >= 10) $ arrows True True rest
            _ ->
              PP.parenthesizeIf (p >= 0) $
                go n im 0 fst <> arrows False False rest
        _ -> "error"
      _ -> "error"
    effects Nothing = mempty
    effects (Just es) = PP.group $ fmt S.AbilityBraces "{" <> PP.commas (go n im 0 <$> es) <> (fmt S.AbilityBraces "}")
    -- `first`: is this the first argument?
    -- `mes`: list of effects
    arrow delay first mes =
      (if first then mempty else PP.softbreak <> fmt S.TypeOperator "->")
        <> (if delay then (if first then fmt S.DelayForceChar "'" else fmt S.DelayForceChar " '") else mempty)
        <> effects mes
        <> if isJust mes || not delay && not first then " " else mempty

    arrows delay first [(mes, Ref' DD.UnitRef)] = arrow delay first mes <> fmt S.Unit "()"
    arrows delay first ((mes, Ref' DD.UnitRef) : rest) =
      arrow delay first mes <> parenNoGroup delay (arrows True True rest)
    arrows delay first ((mes, arg) : rest) =
      arrow delay first mes
        <> parenNoGroup
          (delay && not (null rest))
          (go n im 0 arg <> arrows False False rest)
    arrows False False [] = mempty
    arrows False True [] = mempty -- not reachable
    arrows True _ [] = mempty -- not reachable
    paren True s = PP.group $ fmt S.Parenthesis "(" <> s <> fmt S.Parenthesis ")"
    paren False s = PP.group s

    parenNoGroup True s = fmt S.Parenthesis "(" <> s <> fmt S.Parenthesis ")"
    parenNoGroup False s = s

fmt :: S.Element r -> Pretty (S.SyntaxText' r) -> Pretty (S.SyntaxText' r)
fmt = PP.withSyntax

-- todo: provide sample output in comment
prettySignaturesCT ::
  Var v =>
  PrettyPrintEnv ->
  [(Referent, HashQualified Name, Type v a)] ->
  [Pretty ColorText]
prettySignaturesCT env ts = map PP.syntaxToColor $ prettySignaturesST env ts

prettySignaturesCTCollapsed ::
  Var v =>
  PrettyPrintEnv ->
  [(Referent, HashQualified Name, Type v a)] ->
  Pretty ColorText
prettySignaturesCTCollapsed env ts =
  PP.lines $
    PP.group <$> prettySignaturesCT env ts

prettySignaturesST ::
  Var v =>
  PrettyPrintEnv ->
  [(Referent, HashQualified Name, Type v a)] ->
  [Pretty SyntaxText]
prettySignaturesST env ts =
  PP.align [(name r hq, sig typ) | (r, hq, typ) <- ts]
  where
    name r hq =
      styleHashQualified'' (fmt $ S.TermReference r) hq
    sig typ =
      (fmt S.TypeAscriptionColon ": " <> pretty0 env Map.empty (-1) typ)
        `PP.orElse` (fmt S.TypeAscriptionColon ": " <> PP.indentNAfterNewline 2 (pretty0 env Map.empty (-1) typ))

-- todo: provide sample output in comment; different from prettySignatures'
prettySignaturesAlt' ::
  Var v =>
  PrettyPrintEnv ->
  [([HashQualified Name], Type v a)] ->
  [Pretty ColorText]
prettySignaturesAlt' env ts =
  map PP.syntaxToColor $
    PP.align
      [ ( PP.commas . fmap (\name -> styleHashQualified'' (fmt $ S.HashQualifier name) name) $ names,
          (fmt S.TypeAscriptionColon ": " <> pretty0 env Map.empty (-1) typ)
            `PP.orElse` ( fmt S.TypeAscriptionColon ": "
                            <> PP.indentNAfterNewline 2 (pretty0 env Map.empty (-1) typ)
                        )
        )
        | (names, typ) <- ts
      ]

-- prettySignatures'' :: Var v => PrettyPrintEnv -> [(Name, Type v a)] -> [Pretty ColorText]
-- prettySignatures'' env ts = prettySignatures' env (first HQ.fromName <$> ts)

prettySignaturesAlt ::
  Var v =>
  PrettyPrintEnv ->
  [([HashQualified Name], Type v a)] ->
  Pretty ColorText
prettySignaturesAlt env ts =
  PP.lines $
    PP.group <$> prettySignaturesAlt' env ts
