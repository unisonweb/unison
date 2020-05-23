{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Unison.TypePrinter where

import Unison.Prelude

import qualified Data.Map              as Map
import qualified Data.Set              as Set
import           Unison.HashQualified  (HashQualified)
import           Unison.NamePrinter    (styleHashQualified'')
import           Unison.PrettyPrintEnv (PrettyPrintEnv, Imports, elideFQN)
import qualified Unison.PrettyPrintEnv as PrettyPrintEnv
import           Unison.Reference      (pattern Builtin)
import           Unison.Type
import           Unison.Util.Pretty    (ColorText, Pretty)
import           Unison.Util.ColorText (toPlain)
import qualified Unison.Util.SyntaxText as S
import           Unison.Util.SyntaxText (SyntaxText)
import qualified Unison.Util.Pretty    as PP
import           Unison.Var            (Var)
import qualified Unison.Var            as Var
import qualified Unison.Builtin.Decls as DD

pretty :: forall v a . (Var v) => PrettyPrintEnv -> Type v a -> Pretty ColorText
pretty ppe = PP.syntaxToColor . pretty0 ppe mempty (-1)

pretty' :: Var v => Maybe Int -> PrettyPrintEnv -> Type v a -> String
pretty' (Just width) n t = toPlain $ PP.render width $ PP.syntaxToColor $ pretty0 n Map.empty (-1) t
pretty' Nothing      n t = toPlain $ PP.render maxBound $ PP.syntaxToColor $ pretty0 n Map.empty (-1) t

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

pretty0
  :: forall v a . (Var v)
  => PrettyPrintEnv
  -> Imports
  -> Int
  -> Type v a
  -> Pretty SyntaxText
pretty0 n im p tp = prettyRaw n im p (cleanup (removePureEffects tp))

prettyRaw
  :: forall v a . (Var v)
  => PrettyPrintEnv
  -> Imports
  -> Int
  -> Type v a
  -> Pretty SyntaxText
-- p is the operator precedence of the enclosing context (a number from 0 to
-- 11, or -1 to avoid outer parentheses unconditionally).  Function
-- application has precedence 10.
prettyRaw n im p tp = go n im p tp
  where
  shouldDelay :: v -> [v] -> [(Maybe [Type v a], Type v a)] -> Bool
  shouldDelay v vs rest =
    elem v vs
      && all
           ( Set.notMember v
           . Set.unions
           . fmap freeVars
           . (\(m, t) -> t : concat m)
           )
           rest
  go :: PrettyPrintEnv -> Imports -> Int -> Type v a -> Pretty SyntaxText
  go n im p tp = case stripIntroOuters tp of
    Var' v     -> fmt S.Var $ PP.text (Var.name v)
    DD.TupleType' xs | length xs /= 1 -> PP.parenthesizeCommas $ map (go n im 0) xs
    -- Would be nice to use a different SyntaxHighlights color if the reference is an ability.
    Ref' r     -> styleHashQualified'' (fmt S.DataType) $ elideFQN im (PrettyPrintEnv.typeName n r)
    Cycle' _ _ -> fromString "error: TypeParser does not currently emit Cycle"
    Abs' _     -> fromString "error: TypeParser does not currently emit Abs"
    Ann' _ _   -> fromString "error: TypeParser does not currently emit Ann"
    App' (Ref' (Builtin "Sequence")) x ->
      PP.group $ (fmt S.DelimiterChar "[") <> go n im 0 x <> (fmt S.DelimiterChar "]")
    Apps' f xs -> PP.parenthesizeIf (p >= 10) $ go n im 9 f `PP.hang` PP.spaced
      (go n im 10 <$> xs)
    Effect1' e t ->
      PP.parenthesizeIf (p >= 10) $ go n im 9 e <> " " <> go n im 10 t
    Effects' es         -> effects (Just es)
    -- `forall a. a -> x` where `x` does not involve `a` at all should be
    -- rendered as `'x`.
    -- Note: rest has type `[(Maybe [Type v a], Type v a)]`
    ForallsNamed' vs (EffectfulArrows' (Var' v) rest)
      | shouldDelay v vs rest -> arrows vs True True rest
    ForallsNamed' vs body -> if p < 0 && all Var.universallyQuantifyIfFree vs
      then go n im p body
      else paren (p >= 0) $
        let vformatted = PP.sep " " (fmt S.Var . PP.text . Var.name <$> vs)
        in (fmt S.TypeOperator "∀ " <> vformatted <> fmt S.TypeOperator ".")
           `PP.hang` go n im (-1) body
    t@(Arrow' _ _) -> case t of
      EffectfulArrows' (Ref' DD.UnitRef) rest -> arrows mempty True True rest
      EffectfulArrows' fst rest ->
        PP.parenthesizeIf (p >= 0) $
          go n im 0 fst <> arrows mempty False False rest
      _ -> "error"
    _ -> "error"
  effects Nothing   = mempty
  effects (Just es) = PP.group $ (fmt S.AbilityBraces "{") <> PP.commas (go n im 0 <$> es) <> (fmt S.AbilityBraces "}")
  arrow delay first mes =
    (if first then mempty else PP.softbreak <> (fmt S.TypeOperator "->"))
      <> (if delay then (if first then (fmt S.DelayForceChar "'") else (fmt S.DelayForceChar " '")) else mempty)
      <> effects mes
      <> if (isJust mes) || (not delay) && (not first) then " " else mempty

  -- `delay` is whether this arrow is a "delayed" computation and should start
  -- with `'`.
  -- `first` is True if we're about to emit the first arrow in a function type
  arrows
    :: [v] -> Bool -> Bool -> [(Maybe [Type v a], Type v a)] -> Pretty SyntaxText
  arrows _ delay first [(mes, Ref' DD.UnitRef)] =
    arrow delay first mes <> fmt S.DataType "()"
  arrows vs delay first ((mes, Ref' DD.UnitRef) : rest) =
    arrow delay first mes <> parenNoGroup delay (arrows vs True True rest)
  arrows vs delay first ((mes, arg) : rest) =
    -- Whether to delay the next arrow
    let delay' = case arg of
          Var' v -> shouldDelay v vs rest && not (null rest)
          _      -> False
    in  arrow delay first mes <> parenNoGroup
          (delay && not (null rest) && not delay')
          (  (if delay' then mempty else go n im 0 arg)
          <> arrows vs delay' delay' rest
          )
  arrows _ _ _ [] = mempty

  paren True  s = PP.group $ ( fmt S.Parenthesis "(" ) <> s <> ( fmt S.Parenthesis ")" )
  paren False s = PP.group s

  parenNoGroup True  s = ( fmt S.Parenthesis "(" ) <> s <> ( fmt S.Parenthesis ")" )
  parenNoGroup False s = s

fmt :: S.Element -> Pretty S.SyntaxText -> Pretty S.SyntaxText
fmt = PP.withSyntax

-- todo: provide sample output in comment
prettySignatures'
  :: Var v => PrettyPrintEnv
  -> [(HashQualified, Type v a)]
  -> [Pretty ColorText]
prettySignatures' env ts = map PP.syntaxToColor $ PP.align
  [ ( styleHashQualified'' (fmt S.DataType) name
    , (fmt S.TypeAscriptionColon ": " <> pretty0 env Map.empty (-1) typ)
      `PP.orElse` (  fmt S.TypeAscriptionColon ": "
                  <> PP.indentNAfterNewline 2 (pretty0 env Map.empty (-1) typ)
                  )
    )
  | (name, typ) <- ts
  ]

-- todo: provide sample output in comment; different from prettySignatures'
prettySignaturesAlt'
  :: Var v => PrettyPrintEnv
  -> [([HashQualified], Type v a)]
  -> [Pretty ColorText]
prettySignaturesAlt' env ts = map PP.syntaxToColor $ PP.align
  [ ( PP.commas . fmap (styleHashQualified'' (fmt S.DataType)) $ names
    , (fmt S.TypeAscriptionColon ": " <> pretty0 env Map.empty (-1) typ)
      `PP.orElse` (  fmt S.TypeAscriptionColon ": "
                  <> PP.indentNAfterNewline 2 (pretty0 env Map.empty (-1) typ)
                  )
    )
  | (names, typ) <- ts
  ]

-- prettySignatures'' :: Var v => PrettyPrintEnv -> [(Name, Type v a)] -> [Pretty ColorText]
-- prettySignatures'' env ts = prettySignatures' env (first HQ.fromName <$> ts)

prettySignatures
  :: Var v
  => PrettyPrintEnv
  -> [(HashQualified, Type v a)]
  -> Pretty ColorText
prettySignatures env ts = PP.lines $
  PP.group <$> prettySignatures' env ts

prettySignaturesAlt
  :: Var v
  => PrettyPrintEnv
  -> [([HashQualified], Type v a)]
  -> Pretty ColorText
prettySignaturesAlt env ts = PP.lines $
  PP.group <$> prettySignaturesAlt' env ts
