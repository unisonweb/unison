{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.TermPrinter where

import           Control.Monad                  (join)
import           Data.List
import           Data.Foldable                  ( fold
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           Data.String                    ( IsString, fromString )
import           Data.Vector                    ( )
import           Text.Read                      ( readMaybe )
import           Unison.ABT                     ( pattern AbsN' )
import qualified Unison.Blank                  as Blank
import qualified Unison.HashQualified          as HQ
import           Unison.Lexer                   ( symbolyId )
import           Unison.Name                    ( Name )
import qualified Unison.Name                   as Name
import           Unison.NamePrinter             ( prettyHashQualified, prettyHashQualified' )
import           Unison.PatternP                ( Pattern )
import qualified Unison.PatternP               as Pattern
import qualified Unison.Referent               as Referent
import           Unison.Term
import qualified Unison.TypePrinter            as TypePrinter
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var
import           Unison.Util.Monoid             ( intercalateMap )
import qualified Unison.Util.Pretty             as PP
import           Unison.Util.Pretty             ( Pretty, ColorText )
import           Unison.PrettyPrintEnv          ( PrettyPrintEnv )
import qualified Unison.PrettyPrintEnv         as PrettyPrintEnv
import qualified Unison.DataDeclaration        as DD
import Unison.DataDeclaration (pattern TuplePattern, pattern TupleTerm')

--TODO use imports to trim fully qualified names

-- Information about the context in which a term appears, which affects how the
-- term should be rendered.
data AmbientContext = AmbientContext
  {
    -- The operator precedence of the enclosing context (a number from 0 to 11,
    -- or -1 to render without outer parentheses unconditionally).
    -- Function application has precedence 10.
    precedence :: Int
  , blockContext :: BlockContext
  , infixContext :: InfixContext
  }

-- Description of the position of this ABT node, when viewed in the
-- surface syntax.
data BlockContext
  -- This ABT node is at the top level of a TermParser.block.
  = Block
  | Normal
  deriving (Eq)

data InfixContext
  -- This ABT node is an infix operator being used in infix position.
  = Infix
  | NonInfix
  deriving (Eq)

{- Explanation of precedence handling

   We illustrate precedence rules as follows.

     >=10
       10f 10x

   This example shows that a function application f x is enclosed in
   parentheses whenever the ambient precedence around it is >= 10, and that
   when printing its two components, an ambient precedence of 10 is used in
   both places.

   The pretty-printer uses the following rules for printing terms.

     >=12
       let x = (-1)y
         1z

     >=11
       ! 11x
       ' 11x
       11x ?

     >=10
       10f 10x 10y ...

     >=3
       x -> 2y
       3x + 3y + ... 3z

     >=2
       if 2a then 2b else 2c
       handle 2h in 2b
       case 2x of
         a | 2g -> 0b

     >=0
       10a : 0Int


   And the following for patterns.

     >=11
       x@11p

     >=10
       Con 10p 10q ...

     -- never any external parens added around the following
       { p }
       { Eff 10p 10q ... -> 0k }

-}

prettyTop :: Var v => PrettyPrintEnv -> AnnotatedTerm v a -> Pretty ColorText
prettyTop env = pretty env (ac (-1) Normal)

pretty
  :: Var v
  => PrettyPrintEnv
  -> AmbientContext
  -> AnnotatedTerm v a
  -> Pretty ColorText
pretty n AmbientContext { precedence = p, blockContext = bc, infixContext = ic } term
  = specialCases term $ \case
    Var' v -> parenIfInfix name ic . prettyHashQualified $ name
      where name = HQ.fromVar v
    Ref' r -> parenIfInfix name ic . prettyHashQualified' $ name
      where name = PrettyPrintEnv.termName n (Referent.Ref r)
    Ann' tm t ->
      paren (p >= 0)
        $  pretty n (ac 10 Normal) tm
        <> PP.hang " :" (TypePrinter.pretty n 0 t)
    Int'     i  -> (if i >= 0 then l "+" else mempty) <> (l $ show i)
    Nat'     u  -> l $ show u
    Float'   f  -> l $ show f
    -- TODO How to handle Infinity, -Infinity and NaN?  Parser cannot parse
    --      them.  Haskell doesn't have literals for them either.  Is this
    --      function only required to operate on terms produced by the parser?
    --      In which case the code is fine as it stands.  If it can somehow run
    --      on values produced by execution (or, one day, on terms produced by
    --      metaprograms), then it needs to be able to print them (and then the
    --       parser ought to be able to parse them, to maintain symmetry.)
    Boolean' b  -> if b then l "true" else l "false"
    Text'    s  -> l $ show s
    Blank'   id -> l "_" <> (l $ fromMaybe "" (Blank.nameb id))
    Constructor' ref i ->
      prettyHashQualified $ PrettyPrintEnv.termName n (Referent.Con ref i)
    Request' ref i ->
      prettyHashQualified $ PrettyPrintEnv.termName n (Referent.Con ref i)
    Handle' h body ->
      paren (p >= 2)
        $ ("handle" `PP.hang` pretty n (ac 2 Normal) h)
        <> PP.softbreak
        <> ("in" `PP.hang` pretty n (ac 2 Block) body)
    App' x (Constructor' DD.UnitRef 0) ->
      paren (p >= 11) $ l "!" <> pretty n (ac 11 Normal) x
    AskInfo' x -> paren (p >= 11) $ pretty n (ac 11 Normal) x <> l "?"
    LamNamed' v x | (Var.name v) == "()" ->
      paren (p >= 11) $ l "'" <> pretty n (ac 11 Normal) x
    Sequence' xs -> PP.group $
      "[" <> optSpace
          <> intercalateMap ("," <> PP.softbreak <> optSpace <> optSpace)
                            (pretty n (ac 0 Normal))
                            xs
        <> optSpace <> "]"
      where optSpace = PP.orElse "" " "
    If' cond t f -> paren (p >= 2) $
      if height > 0 then PP.lines [
        "if " <> pcond <> (" then") `PP.hang` pt,
        "else" `PP.hang` pf
       ]
      else PP.spaced [
        "if" `PP.hang` pcond <> (" then" `PP.hang` pt),
        "else" `PP.hang` pf
       ]
     where
       height = PP.preferredHeight pt `max` PP.preferredHeight pf
       pcond  = pretty n (ac 2 Block) cond
       pt     = pretty n (ac 2 Block) t
       pf     = pretty n (ac 2 Block) f
    And' x y ->
      paren (p >= 10) $ PP.spaced [
        "and", pretty n (ac 10 Normal) x,
               pretty n (ac 10 Normal) y
      ]
    Or' x y ->
      paren (p >= 10) $ PP.spaced [
        "or", pretty n (ac 10 Normal) x,
              pretty n (ac 10 Normal) y
      ]
    LetRecNamed' bs e -> printLet bc bs e
    Lets' bs e -> printLet bc (map (\(_, v, binding) -> (v, binding)) bs) e
    Match' scrutinee branches -> paren (p >= 2) $
      ("case " <> pretty n (ac 2 Normal) scrutinee <> " of") `PP.hang` bs
      where bs = PP.lines (map printCase branches)
    t -> l "error: " <> l (show t)
 where
  specialCases term go = case (term, binaryOpsPred) of
    (TupleTerm' [x], _) ->
      paren (p >= 10) $ "Pair" `PP.hang`
        PP.spaced [pretty n (ac 10 Normal) x, "()" ]
    (TupleTerm' xs, _) -> paren True $ commaList xs
    BinaryAppsPred' apps lastArg -> paren (p >= 3) $
      binaryApps apps (pretty n (ac 3 Normal) lastArg)
    _ -> case (term, nonForcePred) of
      AppsPred' f args | not $ isVarKindInfo f ->
        paren (p >= 10) $ pretty n (ac 10 Normal) f `PP.hang`
          PP.spacedMap (pretty n (ac 10 Normal)) args
      _ -> case (term, nonUnitArgPred) of
        LamsNamedPred' vs body ->
          paren (p >= 3) $
            PP.group (varList vs <> " ->") `PP.hang` pretty n (ac 2 Block) body
        _ -> go term

  sepList sep xs = sepList' (pretty n (ac 0 Normal)) sep xs
  sepList' f sep xs = fold $ intersperse sep (map f xs)
  varList vs = sepList' (PP.text . Var.name) PP.softbreak vs
  commaList = sepList ("," <> PP.softbreak)

  printLet :: Var v => BlockContext -> [(v, AnnotatedTerm v a)] -> AnnotatedTerm v a -> Pretty ColorText
  printLet sc bs e =
    paren ((sc /= Block) && p >= 12)
      $  letIntro
      $  PP.lines (map printBinding bs ++
                   [PP.group $ pretty n (ac 0 Normal) e])
   where
    printBinding (v, binding) = if isBlank $ Var.nameStr v
      then pretty n (ac (-1) Normal) binding
      else prettyBinding n (HQ.fromVar v) binding
    letIntro = case sc of
      Block  -> id
      Normal -> \x -> "let" `PP.hang` x
    isBlank ('_' : rest) | (isJust ((readMaybe rest) :: Maybe Int)) = True
    isBlank _ = False

  printCase :: Var v => MatchCase a (AnnotatedTerm v a) -> Pretty ColorText
  printCase (MatchCase pat guard (AbsN' vs body)) =
    PP.group $ lhs `PP.hang` pretty n (ac 0 Block) body
    where
    lhs = PP.group (fst (prettyPattern n (-1) vs pat) <> " ")
       <> printGuard guard
       <> "->"
    printGuard (Just g) = PP.group $ PP.spaced ["|", pretty n (ac 2 Normal) g, ""]
    printGuard Nothing  = mempty
  printCase _ = l "error"

  -- This predicate controls which binary functions we render as infix
  -- operators.  At the moment the policy is just to render symbolic
  -- operators as infix - not 'wordy' function names.  So we produce
  -- "x + y" and "foo x y" but not "x `foo` y".
  binaryOpsPred :: Var v => AnnotatedTerm v a -> Bool
  binaryOpsPred = \case
    Ref' r | isSymbolic (PrettyPrintEnv.termName n (Referent.Ref r)) -> True
    Var' v | isSymbolic (HQ.fromVar v) -> True
    _ -> False

  nonForcePred :: AnnotatedTerm v a -> Bool
  nonForcePred = \case
    Constructor' DD.UnitRef 0 -> False
    _                           -> True

  nonUnitArgPred :: Var v => v -> Bool
  nonUnitArgPred v = (Var.name v) /= "()"

  -- Render a binary infix operator sequence, like [(a2, f2), (a1, f1)],
  -- meaning (a1 `f1` a2) `f2` (a3 rendered by the caller), producing
  -- "a1 `f1` a2 `f2`".  Except the operators are all symbolic, so we won't
  -- produce any backticks.  We build the result out from the right,
  -- starting at `f2`.
  binaryApps
    :: Var v => [(AnnotatedTerm v a, AnnotatedTerm v a)]
             -> Pretty ColorText
             -> Pretty ColorText
  binaryApps xs last = unbroken `PP.orElse` broken
   -- todo: use `PP.column2` in the case where we need to break
   where
    unbroken = PP.spaced (ps ++ [last])
    broken = PP.column2 (psCols $ [""] ++ ps ++ [last])
    psCols ps = case take 2 ps of
      [x,y] -> (x,y) : psCols (drop 2 ps)
      [] -> []
      _ -> error "??"
    ps = join $ [r a f | (a, f) <- reverse xs ]
    r a f = [pretty n (ac 3 Normal) a,
             pretty n (AmbientContext 10 Normal Infix) f]

pretty' ::
  Var v => Maybe Int -> PrettyPrintEnv -> AnnotatedTerm v a -> ColorText
pretty' (Just width) n t = PP.render width $ pretty n (ac (-1) Normal) t
pretty' Nothing      n t = PP.renderUnbroken $ pretty n (ac (-1) Normal) t

prettyPattern
  :: Var v
  => PrettyPrintEnv
  -> Int
  -> [v]
  -> Pattern loc
  -> (Pretty ColorText, [v])
-- vs is the list of pattern variables used by the pattern, plus possibly a
-- tail of variables it doesn't use.  This tail is the second component of
-- the return value.
prettyPattern n p vs patt = case patt of
  Pattern.Unbound _   -> (l "_", vs)
  Pattern.Var     _   -> let (v : tail_vs) = vs in (l $ Var.nameStr v, tail_vs)
  Pattern.Boolean _ b -> (if b then l "true" else l "false", vs)
  Pattern.Int     _ i -> ((if i >= 0 then l "+" else mempty) <> (l $ show i), vs)
  Pattern.Nat     _ u -> (l $ show u, vs)
  Pattern.Float   _ f -> (l $ show f, vs)
  Pattern.Text    _ t -> (l $ show t, vs)
  TuplePattern [pp] ->
    let (printed, tail_vs) = prettyPattern n 10 vs pp
    in  ( paren (p >= 10) $ PP.sep " " ["Pair", printed, "()"]
        , tail_vs )
  TuplePattern pats ->
    let (pats_printed, tail_vs) = patterns vs pats
    in  (PP.parenthesizeCommas pats_printed, tail_vs)
  Pattern.Constructor _ ref i [] ->
    (prettyHashQualified (PrettyPrintEnv.patternName n ref i), vs)
  Pattern.Constructor _ ref i pats ->
    let (pats_printed, tail_vs) = patternsSep PP.softbreak vs pats
    in  ( paren (p >= 10)
          $ prettyHashQualified (PrettyPrintEnv.patternName n ref i)
            `PP.hang` pats_printed
        , tail_vs)
  Pattern.As _ pat ->
    let (v : tail_vs)            = vs
        (printed, eventual_tail) = prettyPattern n 11 tail_vs pat
    in  (paren (p >= 11) $ ((l $ Var.nameStr v) <> l "@" <> printed), eventual_tail)
  Pattern.EffectPure _ pat ->
    let (printed, eventual_tail) = prettyPattern n (-1) vs pat
    in  (PP.sep " " ["{", printed, "}"], eventual_tail)
  Pattern.EffectBind _ ref i pats k_pat ->
    let (pats_printed , tail_vs      ) = patternsSep PP.softbreak vs pats
        (k_pat_printed, eventual_tail) = prettyPattern n 0 tail_vs k_pat
    in  ("{" <>
          (PP.sep " " . PP.nonEmpty $ [
            prettyHashQualified (PrettyPrintEnv.patternName n ref i),
            pats_printed,
            "->",
            k_pat_printed]) <>
         "}"
        , eventual_tail)
  t -> (l "error: " <> l (show t), vs)
 where
  l :: IsString s => String -> s
  l = fromString
  patterns vs (pat : pats) =
    let (printed     , tail_vs      ) = prettyPattern n (-1) vs pat
        (rest_printed, eventual_tail) = patterns tail_vs pats
    in  (printed : rest_printed, eventual_tail)
  patterns vs [] = ([], vs)
  patternsSep sep vs pats = case patterns vs pats of
    (printed, tail_vs) -> (PP.sep sep printed, tail_vs)

{- Render a binding, producing output of the form

foo : t -> u
foo a = ...

The first line is only output if the term has a type annotation as the
outermost constructor.

Binary functions with symbolic names are output infix, as follows:

(+) : t -> t -> t
a + b = ...

-}
prettyBinding ::
  Var v => PrettyPrintEnv -> HQ.HashQualified -> AnnotatedTerm v a -> Pretty ColorText
prettyBinding env v term = go (symbolic && isBinary term) term where
  go infix' = \case
    Ann' tm tp -> PP.lines [
      PP.group (renderName v <> PP.hang " :" (TypePrinter.pretty env (-1) tp)),
      PP.group (prettyBinding env v tm) ]
    LamsNamedOpt' vs body -> PP.group $
      PP.group (defnLhs v vs <> " =") `PP.hang`
      pretty env (ac (-1) Block) body
     where
    t -> l "error: " <> l (show t)
   where
    defnLhs v vs = if infix'
      then case vs of
        x : y : _ ->
          PP.sep " " [PP.text (Var.name x),
                      prettyHashQualified v,
                      PP.text (Var.name y)]
        _ -> l "error"
      else if null vs then renderName v
      else renderName v `PP.hang` args vs
    args vs = PP.spacedMap (PP.text . Var.name) vs
    renderName n = parenIfInfix n NonInfix $ prettyHashQualified n
  symbolic = isSymbolic v
  isBinary = \case
    Ann'          tm _ -> isBinary tm
    LamsNamedOpt' vs _ -> length vs == 2
    _                  -> False -- unhittable

prettyBinding'
  :: Var v => Int -> PrettyPrintEnv -> HQ.HashQualified -> AnnotatedTerm v a -> ColorText
prettyBinding' width n v t = PP.render width $ prettyBinding n v t

paren :: IsString s => Bool -> Pretty s -> Pretty s
paren True  s = PP.group $ "(" <> s <> ")"
paren False s = PP.group s

parenIfInfix
  :: IsString s => HQ.HashQualified -> InfixContext -> (Pretty s -> Pretty s)
parenIfInfix name ic =
  if isSymbolic name && ic == NonInfix then paren True else id

l :: IsString s => String -> Pretty s
l = fromString

-- b :: String -> Pretty String
-- b = Breakable

-- When we use imports in rendering, this will need revisiting, so that we can
-- render say 'foo.+ x y' as 'import foo ... x + y'.  symbolyId doesn't match
-- 'foo.+', only '+'.
isSymbolic :: HQ.HashQualified -> Bool
isSymbolic (HQ.NameOnly name) = isSymbolic' name
isSymbolic (HQ.HashQualified name _) = isSymbolic' name
isSymbolic (HQ.HashOnly _) = False

isSymbolic' :: Name -> Bool
isSymbolic' name = case symbolyId . Name.toString $ name of
  Right _ -> True
  _       -> False

ac :: Int -> BlockContext -> AmbientContext
ac prec bc = AmbientContext prec bc NonInfix
