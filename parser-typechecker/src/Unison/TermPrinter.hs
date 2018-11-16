{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.TermPrinter where

import           Data.List
import qualified Data.Text as Text
import           Data.Foldable (fold, toList)
import           Data.Maybe (fromMaybe)
import           Data.Vector()
import           Unison.ABT (pattern AbsN')
import qualified Unison.Blank as Blank
import           Unison.Lexer (symbolyId)
import qualified Unison.Names as Names
import           Unison.PatternP (Pattern)
import qualified Unison.PatternP as Pattern
import           Unison.Term
import qualified Unison.Type as Type
import qualified Unison.TypePrinter as TypePrinter
import           Unison.Var (Var)
import qualified Unison.Var as Var
import           Unison.Util.Monoid (intercalateMap)
import qualified Unison.Util.PrettyPrint as PP
import           Unison.Util.PrettyPrint (PrettyPrint(..))
import           Unison.PrettyPrintEnv (PrettyPrintEnv)
import qualified Unison.PrettyPrintEnv as PrettyPrintEnv

--TODO let suppression (eg console.u `simulate`, delay blocks (eg ability-keyword.u)
--TODO "in cases where let is needed, let has higher precedence than fn application"
--TODO surplus parens around a case statement as else body, see tests line 334; ditto surplus parens around if/then/else in lambda body
--TODO `sum = Stream.fold-left 0 (+) t` being rendered as `sum = Stream.fold-left 0 + t`

--TODO precedence comment and double check in type printer
--TODO ? askInfo suffix; > watches
--TODO (improve code layout below)
--TODO use imports to trim fully qualified names

-- Description of the position of this ABT node, when viewed in the surface syntax.
data SyntaxContext
  -- This ABT node is at the top level of a TermParser.block.  
  = Block
  | Normal

{- Explanation of precedence handling

   We illustrate precedence rules as follows.

     >=10
       10f 10x

   This example shows that a function application f x is enclosed in parentheses
   whenever the ambient precedence around it is >= 10, and that when printing its
   two components, an ambient precedence of 10 is used in both places.

   The pretty-printer uses the following rules for printing terms.

     >=11
       ! 11x
       ' 11x

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
       let x = (-1)y
           1z

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

pretty :: Var v => PrettyPrintEnv -> Int -> SyntaxContext -> AnnotatedTerm v a -> PrettyPrint String
-- p is the operator precedence of the enclosing context (a number from 0 to 11, or
-- -1 to avoid outer parentheses unconditionally).  Function application has precedence 10.
-- n resolves references to text names.  When getting the name of one of the constructors of a type, the
-- `Maybe Int` identifies which constructor.
pretty n p _ term = specialCases term $ \case
  Var' v       -> l $ varName v
  Ref' r       -> l $ Text.unpack (PrettyPrintEnv.termName n (Names.Ref r))
  Ann' tm t    -> paren (p >= 0) $
                    pretty n 10 Normal tm <> b" " <> (PP.Nest "  " $ PP.Group (l": " <> TypePrinter.pretty n 0 t))
  Int' i       -> (if i >= 0 then l"+" else Empty) <> (l $ show i)
  Nat' u       -> l $ show u
  Float' f     -> l $ show f
  -- TODO How to handle Infinity, -Infinity and NaN?  Parser cannot parse them.  Haskell
  --      doesn't have literals for them either.  Is this function only required to
  --      operate on terms produced by the parser?  In which case the code is fine as
  --      it stands.  If it can somehow run on values produced by execution (or, one day, on
  --      terms produced by metaprograms), then it needs to be able to print them (and
  --      then the parser ought to be able to parse them, to maintain symmetry.)
  Boolean' b   -> if b then l"true" else l"false"
  Text' s      -> l $ show s
  Blank' id    -> l"_" <> (l $ fromMaybe "" (Blank.nameb id))
  Constructor' ref i -> l (Text.unpack (PrettyPrintEnv.constructorName n ref i))
  Request' ref i -> l (Text.unpack (PrettyPrintEnv.requestName n ref i))
  Handle' h body -> paren (p >= 2) $
                      l"handle" <> b" " <> pretty n 2 Normal h <> b" " <> l"in" <> b" "
                      <> PP.Nest "  " (PP.Group (pretty n 2 Block body))
  App' x (Constructor' Type.UnitRef 0) -> paren (p >= 11) $ l"!" <> pretty n 11 Normal x
  LamNamed' v x | (Var.name v) == "()"   -> paren (p >= 11) $ l"'" <> pretty n 11 Normal x
  Vector' xs   -> PP.Group $ l"[" <> intercalateMap ("," <> b" ") (PP.Nest " " . pretty n 0 Normal) (toList xs) <> l"]"
  If' cond t f -> paren (p >= 2) $
                    (PP.Group (l"if" <> b" " <> (PP.Nest "  " $ PP.Group $ pretty n 2 Block cond)) <> b" " <>
                     PP.Group (l"then" <> b" " <> (PP.Nest "  " $ PP.Group $ pretty n 2 Block t)) <> b" " <>
                     PP.Group (l"else" <> b" " <> (PP.Nest "  " $ PP.Group $ pretty n 2 Block f)))
  And' x y     -> paren (p >= 10) $ l"and" <> b" " <> pretty n 10 Normal x <> b" " <> pretty n 10 Normal y
  Or' x y      -> paren (p >= 10) $ l"or" <> b" " <> pretty n 10 Normal x <> b" " <> pretty n 10 Normal y
  LetRecNamed' bs e -> printLet bs e
  Lets' bs e ->   printLet (map (\(_, v, binding) -> (v, binding)) bs) e
  Match' scrutinee branches -> paren (p >= 2) $ PP.BrokenGroup $ 
                               PP.Group (l"case" <> b" " <> pretty n 2 Normal scrutinee <> b" " <> l"of") <> b" " <>
                               (PP.Nest "  " $ fold (intersperse (b"; ") (map printCase branches)))
  t -> l"error: " <> l (show t)
  where specialCases term go =
          case (term, binaryOpsPred) of
            (Tuple' [x], _) -> paren (p >= 10) $ l"Pair" <> b" " <> pretty n 10 Normal x <> b" " <> l"()"
            (Tuple' xs, _)  -> paren True $ commaList xs
            BinaryAppsPred' apps lastArg -> paren (p >= 3) $ binaryApps apps <> pretty n 3 Normal lastArg
            _ -> case (term, nonForcePred) of
              AppsPred' f args -> paren (p >= 10) $
                pretty n 10 Normal f <> b" " <> PP.Nest "  " (PP.Group (intercalateMap (b" ") (pretty n 10 Normal) args))
              _ -> case (term, nonUnitArgPred) of
                LamsNamedPred' vs body -> paren (p >= 3) $
                                            varList vs <> l" ->" <> b" " <>
                                            (PP.Nest "  " $ PP.Group $ pretty n 2 Block body)
                _ -> go term

        sepList sep xs = sepList' (pretty n 0 Normal) sep xs
        sepList' f sep xs = fold $ intersperse sep (map f xs)
        varList vs = sepList' (\v -> l $ varName v) (b" ") vs
        commaList = sepList (l"," <> b" ")

        -- The parser requires lets to use layout, so use BrokenGroup to get some unconditional line-breaks.
        -- These will replace the occurrences of b"; ".
        printLet bs e = paren (p >= 2) $
                        PP.BrokenGroup $ l"let" <> b"; " <> (PP.Nest "  " $
                          (mconcat (map printBinding bs)) <>
                          PP.Group (pretty n 0 Block e))
                        where
                          printBinding (v, binding) = prettyBinding n v binding <> b"; "

        printCase (MatchCase pat guard (AbsN' vs body)) = PP.Group $
          PP.Group ((fst $ prettyPattern n (-1) vs pat) <> b" " <> printGuard guard <> l"->") <> b" " <>
          (PP.Nest "  " $ PP.Group $ pretty n 0 Block body) where
            printGuard (Just g) = l"|" <> b" " <> pretty n 2 Normal g <> b" "
            printGuard Nothing = Empty
        printCase _ = l"error"

        -- This predicate controls which binary functions we render as infix operators.
        -- At the moment the policy is just to render symbolic operators as infix - not 'wordy'
        -- function names.  So we produce "x + y" and "foo x y" but not "x `foo` y".
        binaryOpsPred :: Var v => AnnotatedTerm v a -> Bool
        binaryOpsPred = \case
          Ref' r | isSymbolic (PrettyPrintEnv.termName n (Names.Ref r)) -> True
          Var' v | isSymbolic (Var.name v)  -> True
          _                                 -> False

        nonForcePred :: AnnotatedTerm v a -> Bool
        nonForcePred = \case
          Constructor' Type.UnitRef 0 -> False
          _                           -> True

        nonUnitArgPred :: Var v => v -> Bool
        nonUnitArgPred v = (Var.name v) /= "()"

        -- Render a binary infix operator sequence, like [(a2, f2), (a1, f1)],
        -- meaning (a1 `f1` a2) `f2` (a3 rendered by the caller), producing "a1 `f1` a2 `f2`".  Except
        -- the operators are all symbolic, so we won't produce any backticks.
        -- We build the result out from the right, starting at `f2`.
        binaryApps :: Var v => [(AnnotatedTerm v a, AnnotatedTerm v a)] -> PrettyPrint String
        binaryApps xs = foldr (flip (<>)) mempty (map r xs)
                        where r (a, f) = pretty n 3 Normal a <> b" " <> pretty n 10 Normal f <> b" "

pretty' :: Var v => Maybe Int -> PrettyPrintEnv -> AnnotatedTerm v a -> String
pretty' (Just width) n t = PP.render width   $ pretty n (-1) Normal t
pretty' Nothing      n t = PP.renderUnbroken $ pretty n (-1) Normal t

prettyPattern :: Var v => PrettyPrintEnv -> Int -> [v] -> Pattern loc -> (PrettyPrint String, [v])
-- vs is the list of pattern variables used by the pattern, plus possibly a tail of variables it doesn't use.
-- This tail is the second component of the return value.
prettyPattern n p vs patt = case patt of
  Pattern.Unbound _  -> (l"_", vs)
  Pattern.Var _      -> let (v : tail_vs) = vs
                        in (l $ varName v, tail_vs)
  Pattern.Boolean _ b -> (if b then l"true" else l"false", vs)
  Pattern.Int _ i     -> ((if i >= 0 then l"+" else Empty) <> (l $ show i), vs)
  Pattern.Nat _ u     -> (l $ show u, vs)
  Pattern.Float _ f   -> (l $ show f, vs)
  Pattern.Tuple [pp]   -> let
    (printed, tail_vs) = prettyPattern n 10 vs pp
    in (paren (p >= 10) $ l"Pair" <> b" " <> printed <> b" " <> l"()", tail_vs)
  Pattern.Tuple pats  -> let
    (pats_printed, tail_vs) = patterns vs pats
    in (paren True $ intercalateMap (l"," <> b" ") id pats_printed, tail_vs)
  Pattern.Constructor _ ref i pats -> let
    (pats_printed, tail_vs) = patternsSep (b" ") vs pats
    in (paren (p >= 10) $ l (Text.unpack (PrettyPrintEnv.patternName n ref i)) <> pats_printed, tail_vs)
  Pattern.As _ pat    -> let (v : tail_vs) = vs
                             (printed, eventual_tail) = prettyPattern n 11 tail_vs pat
                         in (paren (p >= 11) $ ((l $ varName v) <> l"@" <> printed), eventual_tail)
  Pattern.EffectPure _ pat -> let (printed, eventual_tail) = prettyPattern n (-1) vs pat
                              in (l"{" <> b" " <> printed <> b" " <> l"}", eventual_tail)
  Pattern.EffectBind _ ref i pats k_pat -> let
    (pats_printed, tail_vs) = patternsSep (b" ") vs pats
    (k_pat_printed, eventual_tail) = prettyPattern n 0 tail_vs k_pat
    in (l"{" <> b"" <> (PP.Nest "  " $ PP.Group $ b" " <>
       l (Text.unpack (PrettyPrintEnv.patternName n ref i)) <> pats_printed <> b" " <> l"->" <> b" " <>
                                               k_pat_printed <> b" ") <> l"}", eventual_tail)
  t -> (l"error: " <> l (show t), vs)
  where l = Literal
        patterns vs (pat : pats) = let (printed, tail_vs) = prettyPattern n (-1) vs pat
                                       (rest_printed, eventual_tail) = patterns tail_vs pats
                                   in (printed : rest_printed, eventual_tail)
        patterns vs [] = ([], vs)
        patternsSep sep vs pats = case patterns vs pats of
          (printed, tail_vs) -> (foldMap (\x -> sep <> x) printed, tail_vs)

{- Render a binding, producing output of the form

foo : t -> u
foo a = ...

The first line is only output if the term has a type annotation as the outermost constructor.

Binary functions with symbolic names are output infix, as follows:

(+) : t -> t -> t
a + b = ...

-}
prettyBinding :: Var v => PrettyPrintEnv -> v -> AnnotatedTerm v a -> PrettyPrint String
prettyBinding n v term = go (symbolic && isBinary term) term where
  go infix' = \case
    Ann' tm tp -> PP.BrokenGroup $
      PP.Group (renderName v <> l" : " <> TypePrinter.pretty n (-1) tp) <> b";" <>
      PP.Group (prettyBinding n v tm)
    LamsNamedOpt' vs body ->
      PP.Group (PP.Group (defnLhs v vs <> b" " <> l"=") <> b" " <>
                (PP.Nest "  " $ PP.Group (pretty n (-1) Block body)))
      where
    t -> l"error: " <> l (show t)
    where
      renderName v = (if symbolic
                      then paren True
                      else id) $ l (varName v)
      defnLhs v vs = if infix'
                     then case vs of
                            x : y : _ -> l (Text.unpack (Var.name x)) <> b" " <>
                                         l (varName v) <> b" " <>
                                         l (Text.unpack (Var.name y))
                            _ -> l"error"
                     else renderName v <> (args vs)
      args vs = foldMap (\x -> b" " <> l (Text.unpack (Var.name x))) vs
  isBinary = \case
    Ann' tm _ -> isBinary tm
    LamsNamedOpt' vs _ -> length vs == 2
    _ -> False -- unhittable
  symbolic = isSymbolic (Var.name v)

prettyBinding' :: Var v => Int -> PrettyPrintEnv -> v -> AnnotatedTerm v a -> String
prettyBinding' width n v t = PP.render width $ prettyBinding n v t

paren :: Bool -> PrettyPrint String -> PrettyPrint String
paren True s = PP.Group $ l"(" <> s <> l")"
paren False s = PP.Group s

varName :: Var v => v -> String
varName v = (Text.unpack (Var.name v))

l :: String -> PrettyPrint String
l = Literal

b :: String -> PrettyPrint String
b = Breakable

-- When we use imports in rendering, this will need revisiting, so that we can render
-- say 'foo.+ x y' as 'import foo ... x + y'.  symbolyId0 doesn't match 'foo.+', only '+'.
isSymbolic :: Text.Text -> Bool
isSymbolic name = case symbolyId $ Text.unpack $ name of Right _ -> True; _ -> False
