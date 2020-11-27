{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.TermPrinter where

import Control.Monad.State (evalState)
import qualified Control.Monad.State as State
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (unpack)
import qualified Data.Text as Text
import Data.Vector ()
import qualified Text.Show.Unicode as U
import Unison.ABT (annotation, reannotateUp, pattern AbsN')
import qualified Unison.ABT as ABT
import qualified Unison.Blank as Blank
import Unison.Builtin.Decls (pattern TuplePattern, pattern TupleTerm')
import qualified Unison.Builtin.Decls as DD
import qualified Unison.ConstructorType as CT
import qualified Unison.HashQualified as HQ
import Unison.Lexer (showEscapeChar, symbolyId)
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NamePrinter (styleHashQualified'')
import qualified Unison.NameSegment as NameSegment
import Unison.Pattern (Pattern)
import qualified Unison.Pattern as Pattern
import Unison.Prelude
import Unison.PrettyPrintEnv (Imports, Prefix, PrettyPrintEnv, Suffix, elideFQN)
import qualified Unison.PrettyPrintEnv as PrettyPrintEnv
import Unison.Reference (Reference)
import qualified Unison.Referent as Referent
import Unison.Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.TypePrinter as TypePrinter
import qualified Unison.Util.Bytes as Bytes
import Unison.Util.Monoid (intercalateMap)
import Unison.Util.Pretty (ColorText, Pretty)
import qualified Unison.Util.Pretty as PP
import Unison.Util.SyntaxText (SyntaxText)
import qualified Unison.Util.SyntaxText as S
import Unison.Var (Var)
import qualified Unison.Var as Var

pretty :: Var v => PrettyPrintEnv -> Term v a -> Pretty ColorText
pretty env tm = PP.syntaxToColor $ pretty0 env (ac (-1) Normal Map.empty MaybeDoc) (printAnnotate env tm)

pretty' :: Var v => Maybe Int -> PrettyPrintEnv -> Term v a -> ColorText
pretty' (Just width) n t = PP.render width $ PP.syntaxToColor $ pretty0 n (ac (-1) Normal Map.empty MaybeDoc) (printAnnotate n t)
pretty' Nothing n t = PP.renderUnbroken $ PP.syntaxToColor $ pretty0 n (ac (-1) Normal Map.empty MaybeDoc) (printAnnotate n t)

-- Information about the context in which a term appears, which affects how the
-- term should be rendered.
data AmbientContext = AmbientContext
  { -- The operator precedence of the enclosing context (a number from 0 to 11,
    -- or -1 to render without outer parentheses unconditionally).
    -- Function application has precedence 10.
    precedence :: Int,
    blockContext :: BlockContext,
    infixContext :: InfixContext,
    imports :: Imports,
    docContext :: DocLiteralContext
  }

-- Description of the position of this ABT node, when viewed in the
-- surface syntax.
data BlockContext
  = -- This ABT node is at the top level of a TermParser.block.
    Block
  | Normal
  deriving (Eq)

data InfixContext
  = -- This ABT node is an infix operator being used in infix position.
    Infix
  | NonInfix
  deriving (Eq)

data DocLiteralContext
  = -- We won't try and render this ABT node or anything under it as a [: @Doc literal :]
    NoDoc
  | -- We'll keep checking as we recurse down
    MaybeDoc
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
       if 0a then 0b else 0c
       handle 0b with 0h
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

pretty0 ::
  Var v =>
  PrettyPrintEnv ->
  AmbientContext ->
  Term3 v PrintAnnotation ->
  Pretty SyntaxText
pretty0
  n
  a@AmbientContext
    { precedence = p,
      blockContext = bc,
      infixContext = ic,
      imports = im,
      docContext = doc
    }
  term =
    -- Note: the set of places in this function that call calcImports has to be kept in sync
    -- with the definition of immediateChildBlockTerms, otherwise `use` statements get
    -- inserted at the wrong scope.
    specialCases term $ \case
      Var' v -> parenIfInfix name ic $ styleHashQualified'' (fmt S.Var) name
        where
          -- OK since all term vars are user specified, any freshening was just added during typechecking
          name = elideFQN im $ HQ.unsafeFromVar (Var.reset v)
      Ref' r -> parenIfInfix name ic $ styleHashQualified'' (fmt $ S.Reference r) name
        where
          name = elideFQN im $ PrettyPrintEnv.termName n (Referent.Ref r)
      TermLink' r ->
        parenIfInfix name ic $
          fmt S.LinkKeyword "termLink " <> styleHashQualified'' (fmt $ S.Referent r) name
        where
          name = elideFQN im $ PrettyPrintEnv.termName n r
      TypeLink' r ->
        parenIfInfix name ic $
          fmt S.LinkKeyword "typeLink " <> styleHashQualified'' (fmt $ S.Reference r) name
        where
          name = elideFQN im $ PrettyPrintEnv.typeName n r
      Ann' tm t ->
        paren (p >= 0) $
          pretty0 n (ac 10 Normal im doc) tm
            <> PP.hang (fmt S.TypeAscriptionColon " :") (TypePrinter.pretty0 n im 0 t)
      Int' i -> fmt S.NumericLiteral $ (if i >= 0 then l "+" else mempty) <> l (show i)
      Nat' u -> fmt S.NumericLiteral $ l $ show u
      Float' f -> fmt S.NumericLiteral $ l $ show f
      -- TODO How to handle Infinity, -Infinity and NaN?  Parser cannot parse
      --      them.  Haskell doesn't have literals for them either.  Is this
      --      function only required to operate on terms produced by the parser?
      --      In which case the code is fine as it stands.  If it can somehow run
      --      on values produced by execution (or, one day, on terms produced by
      --      metaprograms), then it needs to be able to print them (and then the
      --      parser ought to be able to parse them, to maintain symmetry.)
      Boolean' b -> fmt S.BooleanLiteral $ if b then l "true" else l "false"
      Text' s -> fmt S.TextLiteral $ l $ U.ushow s
      Char' c -> fmt S.CharLiteral $
        l $ case showEscapeChar c of
          Just c -> "?\\" ++ [c]
          Nothing -> '?' : [c]
      Blank' id -> fmt S.Blank $ l "_" <> l (fromMaybe "" (Blank.nameb id))
      Constructor' ref i ->
        styleHashQualified'' (fmt S.Constructor) $
          elideFQN im $ PrettyPrintEnv.termName n (Referent.Con ref i CT.Data)
      Request' ref i ->
        styleHashQualified'' (fmt S.Request) $
          elideFQN im $ PrettyPrintEnv.termName n (Referent.Con ref i CT.Effect)
      Handle' h body ->
        paren (p >= 2) $
          if PP.isMultiLine pb || PP.isMultiLine ph
            then
              PP.lines
                [ (fmt S.ControlKeyword "handle") `PP.hang` pb,
                  (fmt S.ControlKeyword "with") `PP.hang` ph
                ]
            else
              PP.spaced
                [ (fmt S.ControlKeyword "handle") `PP.hang` pb
                    <> PP.softbreak
                    <> (fmt S.ControlKeyword "with") `PP.hang` ph
                ]
        where
          pb = pblock body
          ph = pblock h
          pblock tm =
            let (im', uses) = calcImports im tm
             in uses $ [pretty0 n (ac 0 Block im' doc) tm]
      App' x (Constructor' DD.UnitRef 0) ->
        paren (p >= 11) $ (fmt S.DelayForceChar $ l "!") <> pretty0 n (ac 11 Normal im doc) x
      LamNamed' v x
        | (Var.name v) == "()" ->
          paren (p >= 11) $ (fmt S.DelayForceChar $ l "'") <> pretty0 n (ac 11 Normal im doc) x
      Sequence' xs ->
        PP.group $
          (fmt S.DelimiterChar $ l "[") <> optSpace
            <> intercalateMap
              ((fmt S.DelimiterChar $ l ",") <> PP.softbreak <> optSpace <> optSpace)
              (pretty0 n (ac 0 Normal im doc))
              xs
            <> optSpace
            <> (fmt S.DelimiterChar $ l "]")
        where
          optSpace = PP.orElse "" " "
      If' cond t f ->
        paren (p >= 2) $
          if PP.isMultiLine pt || PP.isMultiLine pf
            then
              PP.lines
                [ (fmt S.ControlKeyword "if ") <> pcond <> (fmt S.ControlKeyword " then") `PP.hang` pt,
                  (fmt S.ControlKeyword "else") `PP.hang` pf
                ]
            else
              PP.spaced
                [ ((fmt S.ControlKeyword "if") `PP.hang` pcond) <> ((fmt S.ControlKeyword " then") `PP.hang` pt),
                  (fmt S.ControlKeyword "else") `PP.hang` pf
                ]
        where
          pcond = pretty0 n (ac 2 Block im doc) cond
          pt = branch t
          pf = branch f
          branch tm =
            let (im', uses) = calcImports im tm
             in uses $ [pretty0 n (ac 0 Block im' doc) tm]
      And' x y ->
        paren (p >= 10) $
          PP.spaced
            [ pretty0 n (ac 10 Normal im doc) x,
              fmt S.ControlKeyword "&&",
              pretty0 n (ac 10 Normal im doc) y
            ]
      Or' x y ->
        paren (p >= 10) $
          PP.spaced
            [ pretty0 n (ac 10 Normal im doc) x,
              fmt S.ControlKeyword "||",
              pretty0 n (ac 10 Normal im doc) y
            ]
      LetBlock bs e ->
        let (im', uses) = calcImports im term
         in printLet bc bs e im' uses
      -- Some matches are rendered as a destructuring bind, like
      --   match foo with (a,b) -> blah
      -- becomes
      --   (a,b) = foo
      --   blah
      -- See `isDestructuringBind` definition.
      Match' scrutinee cs@[MatchCase pat guard (AbsN' vs body)]
        | p < 1 && isDestructuringBind scrutinee cs ->
          letIntro $
            PP.lines
              [ (lhs <> eq) `PP.hang` rhs,
                pretty0 n (ac (-1) Block im doc) body
              ]
        where
          letIntro = case bc of
            Block -> id
            Normal -> \x ->
              -- We don't call calcImports here, because we can't easily do the
              -- corequisite step in immediateChildBlockTerms (because it doesn't
              -- know bc.)  So we'll fail to take advantage of any opportunity
              -- this let block provides to add a use statement.  Not so bad.
              (fmt S.ControlKeyword "let") `PP.hang` x
          lhs =
            PP.group (fst (prettyPattern n (ac 0 Block im doc) (-1) vs pat))
              <> printGuard guard
          printGuard Nothing = mempty
          printGuard (Just g') =
            let (_, g) = ABT.unabs g'
             in PP.group $ PP.spaced [(fmt S.DelimiterChar " |"), pretty0 n (ac 2 Normal im doc) g]
          eq = fmt S.BindingEquals " ="
          rhs =
            let (im', uses) = calcImports im scrutinee
             in uses $ [pretty0 n (ac (-1) Block im' doc) scrutinee]
      Match' scrutinee branches ->
        paren (p >= 2) $
          if PP.isMultiLine ps
            then
              PP.lines
                [ (fmt S.ControlKeyword "match ") `PP.hang` ps,
                  (fmt S.ControlKeyword " with") `PP.hang` pbs
                ]
            else ((fmt S.ControlKeyword "match ") <> ps <> (fmt S.ControlKeyword " with")) `PP.hang` pbs
        where
          ps = pretty0 n (ac 2 Normal im doc) scrutinee
          pbs = printCase n im doc (arity1Branches branches) -- don't print with `cases` syntax
      t -> l "error: " <> l (show t)
    where
      specialCases term go = case (term, binaryOpsPred) of
        (DD.Doc, _)
          | doc == MaybeDoc ->
            if isDocLiteral term
              then prettyDoc n im term
              else pretty0 n (a {docContext = NoDoc}) term
        (TupleTerm' [x], _) ->
          let pair = parenIfInfix name ic $ styleHashQualified'' (fmt S.Constructor) name
                where
                  name = elideFQN im $ PrettyPrintEnv.termName n (DD.pairCtorRef)
           in paren (p >= 10) $
                pair
                  `PP.hang` PP.spaced [pretty0 n (ac 10 Normal im doc) x, fmt S.Constructor "()"]
        (TupleTerm' xs, _) -> paren True $ commaList xs
        (Bytes' bs, _) ->
          fmt S.BytesLiteral "0xs" <> (PP.shown $ Bytes.fromWord8s (map fromIntegral bs))
        BinaryAppsPred' apps lastArg ->
          paren (p >= 3) $
            binaryApps apps (pretty0 n (ac 3 Normal im doc) lastArg)
        _ -> case (term, nonForcePred) of
          AppsPred' f args ->
            paren (p >= 10) $
              pretty0 n (ac 10 Normal im doc) f
                `PP.hang` PP.spacedMap (pretty0 n (ac 10 Normal im doc)) args
          _ -> case (term, nonUnitArgPred) of
            (LamsNamedMatch' [] branches, _) ->
              paren (p >= 3) $
                PP.group (fmt S.ControlKeyword "cases") `PP.hang` printCase n im doc branches
            LamsNamedPred' vs body ->
              paren (p >= 3) $
                PP.group (varList vs <> fmt S.ControlKeyword " ->") `PP.hang` pretty0 n (ac 2 Block im doc) body
            _ -> go term

      sepList = sepList' (pretty0 n (ac 0 Normal im doc))
      sepList' f sep xs = fold $ intersperse sep (map f xs)
      varList = sepList' (PP.text . Var.name) PP.softbreak
      commaList = sepList (fmt S.DelimiterChar (l ",") <> PP.softbreak)

      printLet ::
        Var v =>
        BlockContext ->
        [(v, Term3 v PrintAnnotation)] ->
        Term3 v PrintAnnotation ->
        Imports ->
        ([Pretty SyntaxText] -> Pretty SyntaxText) ->
        Pretty SyntaxText
      printLet sc bs e im uses =
        paren ((sc /= Block) && p >= 12) $
          letIntro $
            ( uses
                [ ( PP.lines
                      ( map printBinding bs
                          ++ [PP.group $ pretty0 n (ac 0 Normal im doc) e]
                      )
                  )
                ]
            )
        where
          printBinding (v, binding) =
            if isBlank $ Var.nameStr v
              then pretty0 n (ac (-1) Normal im doc) binding
              else prettyBinding0 n (ac (-1) Normal im doc) (HQ.unsafeFromVar v) binding
          letIntro = case sc of
            Block -> id
            Normal -> \x -> (fmt S.ControlKeyword "let") `PP.hang` x

      -- This predicate controls which binary functions we render as infix
      -- operators.  At the moment the policy is just to render symbolic
      -- operators as infix - not 'wordy' function names.  So we produce
      -- "x + y" and "foo x y" but not "x `foo` y".
      binaryOpsPred :: Var v => Term3 v PrintAnnotation -> Bool
      binaryOpsPred = \case
        Ref' r | isSymbolic (PrettyPrintEnv.termName n (Referent.Ref r)) -> True
        Var' v | isSymbolic (HQ.unsafeFromVar v) -> True
        _ -> False

      nonForcePred :: Term3 v PrintAnnotation -> Bool
      nonForcePred = \case
        Constructor' DD.UnitRef 0 -> False
        Constructor' DD.DocRef _ -> False
        _ -> True

      nonUnitArgPred :: Var v => v -> Bool
      nonUnitArgPred v = (Var.name v) /= "()"

      -- Render a binary infix operator sequence, like [(a2, f2), (a1, f1)],
      -- meaning (a1 `f1` a2) `f2` (a3 rendered by the caller), producing
      -- "a1 `f1` a2 `f2`".  Except the operators are all symbolic, so we won't
      -- produce any backticks.  We build the result out from the right,
      -- starting at `f2`.
      binaryApps ::
        Var v =>
        [(Term3 v PrintAnnotation, Term3 v PrintAnnotation)] ->
        Pretty SyntaxText ->
        Pretty SyntaxText
      binaryApps xs last = unbroken `PP.orElse` broken
        where
          -- todo: use `PP.column2` in the case where we need to break

          unbroken = PP.spaced (ps ++ [last])
          broken = PP.column2 (psCols $ [""] ++ ps ++ [last])
          psCols ps = case take 2 ps of
            [x, y] -> (x, y) : psCols (drop 2 ps)
            [] -> []
            _ -> error "??"
          ps = join $ [r a f | (a, f) <- reverse xs]
          r a f =
            [ pretty0 n (ac 3 Normal im doc) a,
              pretty0 n (AmbientContext 10 Normal Infix im doc) f
            ]

prettyPattern ::
  forall v loc.
  Var v =>
  PrettyPrintEnv ->
  AmbientContext ->
  Int ->
  [v] ->
  Pattern loc ->
  (Pretty SyntaxText, [v])
-- vs is the list of pattern variables used by the pattern, plus possibly a
-- tail of variables it doesn't use.  This tail is the second component of
-- the return value.
prettyPattern n c@(AmbientContext {imports = im}) p vs patt = case patt of
  Pattern.Char _ c ->
    ( fmt S.CharLiteral $
        l $ case showEscapeChar c of
          Just c -> "?\\" ++ [c]
          Nothing -> '?' : [c],
      vs
    )
  Pattern.Unbound _ -> (fmt S.DelimiterChar $ l "_", vs)
  Pattern.Var _ -> let (v : tail_vs) = vs in (fmt S.Var $ l $ Var.nameStr v, tail_vs)
  Pattern.Boolean _ b -> (fmt S.BooleanLiteral $ if b then l "true" else l "false", vs)
  Pattern.Int _ i -> (fmt S.NumericLiteral $ (if i >= 0 then l "+" else mempty) <> (l $ show i), vs)
  Pattern.Nat _ u -> (fmt S.NumericLiteral $ l $ show u, vs)
  Pattern.Float _ f -> (fmt S.NumericLiteral $ l $ show f, vs)
  Pattern.Text _ t -> (fmt S.TextLiteral $ l $ show t, vs)
  TuplePattern pats
    | length pats /= 1 ->
      let (pats_printed, tail_vs) = patterns (-1) vs pats
       in (PP.parenthesizeCommas pats_printed, tail_vs)
  Pattern.Constructor _ ref i [] ->
    (styleHashQualified'' (fmt S.Constructor) $ elideFQN im (PrettyPrintEnv.patternName n ref i), vs)
  Pattern.Constructor _ ref i pats ->
    let (pats_printed, tail_vs) = patternsSep 10 PP.softbreak vs pats
     in ( paren (p >= 10) $
            styleHashQualified'' (fmt S.Constructor) (elideFQN im (PrettyPrintEnv.patternName n ref i))
              `PP.hang` pats_printed,
          tail_vs
        )
  Pattern.As _ pat ->
    let (v : tail_vs) = vs
        (printed, eventual_tail) = prettyPattern n c 11 tail_vs pat
     in (paren (p >= 11) $ ((fmt S.Var $ l $ Var.nameStr v) <> (fmt S.DelimiterChar $ l "@") <> printed), eventual_tail)
  Pattern.EffectPure _ pat ->
    let (printed, eventual_tail) = prettyPattern n c (-1) vs pat
     in (PP.sep " " [fmt S.DelimiterChar "{", printed, fmt S.DelimiterChar "}"], eventual_tail)
  Pattern.EffectBind _ ref i pats k_pat ->
    let (pats_printed, tail_vs) = patternsSep 10 PP.softbreak vs pats
        (k_pat_printed, eventual_tail) = prettyPattern n c 0 tail_vs k_pat
     in ( (fmt S.DelimiterChar "{")
            <> ( PP.sep " " . PP.nonEmpty $
                   [ styleHashQualified'' (fmt S.Request) $ elideFQN im (PrettyPrintEnv.patternName n ref i),
                     pats_printed,
                     fmt S.ControlKeyword "->",
                     k_pat_printed
                   ]
               )
            <> (fmt S.DelimiterChar "}"),
          eventual_tail
        )
  Pattern.SequenceLiteral _ pats ->
    let (pats_printed, tail_vs) = patternsSep (-1) (fmt S.DelimiterChar ", ") vs pats
     in ((fmt S.DelimiterChar "[") <> pats_printed <> (fmt S.DelimiterChar "]"), tail_vs)
  Pattern.SequenceOp _ l op r ->
    let (pl, lvs) = prettyPattern n c p vs l
        (pr, rvs) = prettyPattern n c (p + 1) lvs r
        f i s = (paren (p >= i) (pl <> " " <> (fmt (S.Op op) s) <> " " <> pr), rvs)
     in case op of
          Pattern.Cons -> f 9 "+:"
          Pattern.Snoc -> f 9 ":+"
          Pattern.Concat -> f 9 "++"
  where
    l :: IsString s => String -> s
    l = fromString
    patterns p vs (pat : pats) =
      let (printed, tail_vs) =
            prettyPattern n c p vs pat
          (rest_printed, eventual_tail) = patterns p tail_vs pats
       in (printed : rest_printed, eventual_tail)
    patterns _ vs [] = ([], vs)
    patternsSep p sep vs pats = case patterns p vs pats of
      (printed, tail_vs) -> (PP.sep sep printed, tail_vs)

type MatchCase' ann tm = ([Pattern ann], Maybe tm, tm)

arity1Branches :: [MatchCase ann tm] -> [MatchCase' ann tm]
arity1Branches bs = [([pat], guard, body) | MatchCase pat guard body <- bs]

printCase ::
  Var v =>
  PrettyPrintEnv ->
  Imports ->
  DocLiteralContext ->
  [MatchCase' () (Term3 v PrintAnnotation)] ->
  Pretty SyntaxText
printCase env im doc ms = PP.lines $ map each gridArrowsAligned
  where
    each (lhs, arrow, body) = PP.group $ (lhs <> arrow) `PP.hang` body
    grid = go <$> ms
    gridArrowsAligned = tidy <$> zip (PP.align' (f <$> grid)) grid
      where
        f (a, b, _) = (a, Just b)
        tidy ((a', b'), (_, _, c)) = (a', b', c)
    go (pats, guard, (AbsN' vs body)) =
      (lhs, arrow, (uses [pretty0 env (ac 0 Block im' doc) body]))
      where
        lhs =
          ( case pats of
              [pat] -> PP.group (fst (prettyPattern env (ac 0 Block im doc) (-1) vs pat))
              pats -> PP.group . PP.sep ("," <> PP.softbreak) . (`evalState` vs) . for pats $ \pat -> do
                vs <- State.get
                let (p, rem) = prettyPattern env (ac 0 Block im doc) (-1) vs pat
                State.put rem
                pure p
          )
            <> printGuard guard
        arrow = fmt S.ControlKeyword "->"
        printGuard (Just g') =
          let (_, g) = ABT.unabs g'
           in -- strip off any Abs-chain around the guard, guard variables are rendered
              -- like any other variable, ex: case Foo x y | x < y -> ...
              PP.group $ PP.spaced [(fmt S.DelimiterChar " |"), pretty0 env (ac 2 Normal im doc) g]
        printGuard Nothing = mempty
        (im', uses) = calcImports im body
    go _ = (l "error", mempty, mempty)

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
  Var v =>
  PrettyPrintEnv ->
  HQ.HashQualified ->
  Term2 v at ap v a ->
  Pretty SyntaxText
prettyBinding n = prettyBinding0 n $ ac (-1) Block Map.empty MaybeDoc

prettyBinding' ::
  Var v => Int -> PrettyPrintEnv -> HQ.HashQualified -> Term v a -> ColorText
prettyBinding' width n v t = PP.render width $ PP.syntaxToColor $ prettyBinding n v t

prettyBinding0 ::
  Var v =>
  PrettyPrintEnv ->
  AmbientContext ->
  HQ.HashQualified ->
  Term2 v at ap v a ->
  Pretty SyntaxText
prettyBinding0 env a@AmbientContext {imports = im, docContext = doc} v term =
  go
    (symbolic && isBinary term)
    term
  where
    go infix' = \case
      Ann' tm tp ->
        PP.lines
          [ PP.group
              ( renderName v
                  <> PP.hang
                    (fmt S.TypeAscriptionColon " :")
                    (TypePrinter.pretty0 env im (-1) tp)
              ),
            PP.group (prettyBinding0 env a v tm)
          ]
      (printAnnotate env -> LamsNamedMatch' vs branches) ->
        PP.group $
          PP.group (defnLhs v vs <> fmt S.BindingEquals " =" <> " " <> fmt S.ControlKeyword "cases")
            `PP.hang` printCase env im doc branches
      LamsNamedOrDelay' vs body ->
        let (im', uses) = calcImports im body'
            -- In the case where we're being called from inside `pretty0`, this
            -- call to printAnnotate is unfortunately repeating work we've already
            -- done.
            body' = printAnnotate env body
         in PP.group $
              PP.group (defnLhs v vs <> fmt S.BindingEquals " =")
                `PP.hang` uses [pretty0 env (ac (-1) Block im' doc) body']
      t -> l "error: " <> l (show t)
      where
        defnLhs v vs
          | infix' = case vs of
            x : y : _ ->
              PP.sep
                " "
                [ fmt S.Var $ PP.text (Var.name x),
                  styleHashQualified'' (fmt $ S.HashQualifier v) $ elideFQN im v,
                  fmt S.Var $ PP.text (Var.name y)
                ]
            _ -> l "error"
          | null vs = renderName v
          | otherwise = renderName v `PP.hang` args vs
        args = PP.spacedMap $ fmt S.Var . PP.text . Var.name
        renderName n =
          let n' = elideFQN im n
           in parenIfInfix n' NonInfix $ styleHashQualified'' (fmt $ S.HashQualifier n') n'
    symbolic = isSymbolic v
    isBinary = \case
      Ann' tm _ -> isBinary tm
      LamsNamedMatch' vs _ -> length vs == 1
      LamsNamedOrDelay' vs _ -> length vs == 2
      _ -> False -- unhittable

isDocLiteral :: Term3 v PrintAnnotation -> Bool
isDocLiteral term = case term of
  DD.DocJoin segs -> all isDocLiteral segs
  DD.DocBlob _ -> True
  DD.DocLink (DD.LinkTerm (TermLink' _)) -> True
  DD.DocLink (DD.LinkType (TypeLink' _)) -> True
  DD.DocSource (DD.LinkTerm (TermLink' _)) -> True
  DD.DocSource (DD.LinkType (TypeLink' _)) -> True
  DD.DocSignature (TermLink' _) -> True
  DD.DocEvaluate (TermLink' _) -> True
  Ref' _ -> True -- @[include]
  _ -> False

-- Similar to DisplayValues.displayDoc, but does not follow and expand references.
prettyDoc :: Var v => PrettyPrintEnv -> Imports -> Term3 v a -> Pretty SyntaxText
prettyDoc n im term =
  mconcat
    [ fmt S.DocDelimiter $ l "[: ",
      go term,
      spaceUnlessBroken,
      fmt S.DocDelimiter $ l ":]"
    ]
  where
    go (DD.DocJoin segs) = foldMap go segs
    go (DD.DocBlob txt) = PP.paragraphyText (escaped txt)
    go (DD.DocLink (DD.LinkTerm (TermLink' r))) =
      (fmt S.DocDelimiter $ l "@") <> ((fmt $ S.Referent r) $ fmtTerm r)
    go (DD.DocLink (DD.LinkType (TypeLink' r))) =
      (fmt S.DocDelimiter $ l "@") <> ((fmt $ S.Reference r) $ fmtType r)
    go (DD.DocSource (DD.LinkTerm (TermLink' r))) =
      atKeyword "source" <> fmtTerm r
    go (DD.DocSource (DD.LinkType (TypeLink' r))) =
      atKeyword "source" <> fmtType r
    go (DD.DocSignature (TermLink' r)) =
      atKeyword "signature" <> fmtTerm r
    go (DD.DocEvaluate (TermLink' r)) =
      atKeyword "evaluate" <> fmtTerm r
    go (Ref' r) = atKeyword "include" <> fmtTerm (Referent.Ref r)
    go _ = l $ "(invalid doc literal: " ++ show term ++ ")"
    fmtName s = styleHashQualified'' (fmt $ S.HashQualifier s) $ elideFQN im s
    fmtTerm r = fmtName $ PrettyPrintEnv.termName n r
    fmtType r = fmtName $ PrettyPrintEnv.typeName n r
    atKeyword w =
      (fmt S.DocDelimiter $ l "@[")
        <> (fmt S.DocKeyword $ l w)
        <> (fmt S.DocDelimiter $ l "] ")
    escaped = Text.replace "@" "\\@" . Text.replace ":]" "\\:]"
    spaceUnlessBroken = PP.orElse " " ""

paren :: Bool -> Pretty SyntaxText -> Pretty SyntaxText
paren True s = PP.group $ fmt S.Parenthesis "(" <> s <> fmt S.Parenthesis ")"
paren False s = PP.group s

parenIfInfix ::
  HQ.HashQualified -> InfixContext -> (Pretty SyntaxText -> Pretty SyntaxText)
parenIfInfix name ic =
  if isSymbolic name && ic == NonInfix then paren True else id

l :: IsString s => String -> Pretty s
l = fromString

isSymbolic :: HQ.HashQualified -> Bool
isSymbolic (HQ.NameOnly name) = isSymbolic' name
isSymbolic (HQ.HashQualified name _) = isSymbolic' name
isSymbolic (HQ.HashOnly _) = False

isSymbolic' :: Name -> Bool
isSymbolic' name = case symbolyId . Name.toString $ name of
  Right _ -> True
  _ -> False

isBlank :: String -> Bool
isBlank ('_' : rest) | (isJust ((readMaybe rest) :: Maybe Int)) = True
isBlank _ = False

ac :: Int -> BlockContext -> Imports -> DocLiteralContext -> AmbientContext
ac prec bc im doc = AmbientContext prec bc NonInfix im doc

fmt :: S.Element -> Pretty S.SyntaxText -> Pretty S.SyntaxText
fmt = PP.withSyntax

{-
   # FQN elision

   The term pretty-printer inserts `use` statements in some circumstances, to
   avoid the need for using fully-qualified names (FQNs) everywhere.  The
   following is an explanation and specification, as developed in issue #285.

   As an example, instead of

     foo p q r =
       if p then Util.bar q else Util.bar r

   we actually output the following.

     foo p q r =
       use Util bar
       if p then bar q else bar r

   Here, the `use` statement `use Util bar` has been inserted at the start of
   the block statement containing the `if`.  Within that scope, `Util.bar` can
   be referred to just with `bar`.  We say `Util` is the prefix, and `bar` is
   the suffix.

   When choosing where to place `use` statements, the pretty-printer tries to
   - float them down, deeper into the syntax tree, to keep them visually close
     to the use sites ('usages') of the names involved, but also tries to
   - minimize the number of repetitions of `use` statements for the same names
     by floating them up, towards the top of the syntax tree, so that one
     `use` statement takes effect over more name usages.

   It avoids producing output like the following.

     foo p q r =
       use My bar
       if p then bar q else Your.bar r

   Here `My.bar` is imported with a `use` statement, but `Your.bar` is not.
   We avoid this because it would be easy to misread `bar` as meaning
   `Your.bar`.  Instead both names are output fully qualified.

   This means that a `use` statement is only emitted for a name
   when the suffix is unique, across all the names referenced in the scope of
   the `use` statement.

   We don't emit a `use` statement for a name if it only occurs once within
   the scope (unless it's an infix operator, since they look nicer without
   a namespace qualifier.)

   The emitted code does not depend on Type-Driven Name Resolution (TDNR).
   For example, we emit
     foo =
       use Nat +
       1 + 2
   even though TDNR means that `foo = 1 + 2` would have had the same
   meaning.  That avoids the reader having to run typechecker logic in their
   head in order to know what functions are being called.

   Multi-level name qualification is allowed - like `Foo.Bar.baz`.  The
   pretty-printer tries to strip off as many sections of the prefix as
   possible, without causing a clash with other names.  If more sections
   can be stripped off, further down the tree, then it does this too.

   ## Specification

   We output a `use` statement for prefix P and suffix S at a given scope if
     - the scope is a block statement (so the `use` is syntactically valid)
     - the number of usages of the thing referred to by P.S within the scope
       - is > 1, or
       - is 1, and S is an infix operator
     - [uniqueness] there is no other Q with Q.S used in that scope
     - there is no longer prefix PP (and suffix s, with PP.s == P.S) which
       satisfies uniqueness
     - [narrowness] there is no block statement further down inside this one
       which contains all of the usages.

   Use statements in a block statement are sorted alphabetically by prefix.
   Suffixes covered by a single use statement are sorted alphabetically.
   Note that each `use` line cannot be line-broken.  Ideally they would
   fit the available space by splitting into multiple separate `use` lines.

   ## Algorithm

   Bubbling up from the leaves of the syntax tree, we calculate for each
   node, a `Map Suffix (Map Prefix Int)` (the 'usages map'), where the `Int`
   is the number of usages of Prefix.Suffix at/under that node.  (Note that
   a usage of `A.B.c` corresponds to two entries in the outer map.)  See
   `printAnnotate`.

   Once we have this decoration on all the terms, we start pretty-printing.
   As we recurse back down through the tree, we keep a `Map Name Suffix` (the
   'imports map'), to record the effect of all the `use` statements we've added
   in the nodes above.  When outputting names, we check this map to work out
   how to render them, using any suffix we find, or else falling back to the
   FQN.  At each block statement, each suffix in that term's usages map is a
   candidate to be imported with a use statement, subject to the various
   rules in the specification.

   # Debugging

   Start by enabling the tracing in elideFQN in PrettyPrintEnv.hs.

   There's also tracing in allInSubBlock to help when the narrowness check
   is playing up.

   # Semantics of imports

   Here is some background on how imports work.

   `use XYZ blah` brings `XYZ.blah` into scope, bound to the name `blah`. More
   generally, `use` is followed by a FQN prefix, then the local suffix.
   Concatenate the FQN prefix with the local suffix, with a dot between them,
   and you get the FQN, which is bound to the name equal to the local suffix.

   `use XYZ blah qux` is equivalent to the two statements (and this
   generalizes for any N symbols):
     use XYZ blah
     use XYZ qux

   This syntax works the same even if XYZ or blah have dots in them, so:
   `use Util.External My.Foo` brings `Util.External.My.Foo` into scope, bound
   to the name `My.Foo`.

   That's it. No wildcard imports, imports that do renaming, etc. We can
   consider adding some features like this later.
-}

data PrintAnnotation = PrintAnnotation
  { -- For each suffix that appears in/under this term, the set of prefixes
    -- used with that suffix, and how many times each occurs.
    usages :: Map Suffix (Map Prefix Int)
  }
  deriving (Show)

instance Semigroup PrintAnnotation where
  (PrintAnnotation {usages = a}) <> (PrintAnnotation {usages = b}) =
    PrintAnnotation {usages = Map.unionWith f a b}
    where
      f a' b' = Map.unionWith (+) a' b'

instance Monoid PrintAnnotation where
  mempty = PrintAnnotation {usages = Map.empty}

suffixCounterTerm :: Var v => PrettyPrintEnv -> Term2 v at ap v a -> PrintAnnotation
suffixCounterTerm n = \case
  Var' v -> countHQ $ HQ.unsafeFromVar v
  Ref' r -> countHQ $ PrettyPrintEnv.termName n (Referent.Ref r)
  Ref' r -> countHQ $ PrettyPrintEnv.termName n (Referent.Ref r)
  Constructor' r _ | noImportRefs r -> mempty
  Constructor' r i -> countHQ $ PrettyPrintEnv.termName n (Referent.Con r i CT.Data)
  Request' r i -> countHQ $ PrettyPrintEnv.termName n (Referent.Con r i CT.Effect)
  Ann' _ t -> countTypeUsages n t
  Match' _ bs ->
    let pat (MatchCase p _ _) = p
     in foldMap ((countPatternUsages n) . pat) bs
  _ -> mempty

suffixCounterType :: Var v => PrettyPrintEnv -> Type v a -> PrintAnnotation
suffixCounterType n = \case
  Type.Var' v -> countHQ $ HQ.unsafeFromVar v
  Type.Ref' r | noImportRefs r || r == Type.vectorRef -> mempty
  Type.Ref' r -> countHQ $ PrettyPrintEnv.typeName n r
  _ -> mempty

printAnnotate :: (Var v, Ord v) => PrettyPrintEnv -> Term2 v at ap v a -> Term3 v PrintAnnotation
printAnnotate n tm = fmap snd (go (reannotateUp (suffixCounterTerm n) tm))
  where
    go :: Ord v => Term2 v at ap v b -> Term2 v () () v b
    go = extraMap' id (const ()) (const ())

countTypeUsages :: (Var v, Ord v) => PrettyPrintEnv -> Type v a -> PrintAnnotation
countTypeUsages n t = snd $ annotation $ reannotateUp (suffixCounterType n) t

countPatternUsages :: PrettyPrintEnv -> Pattern loc -> PrintAnnotation
countPatternUsages n p = Pattern.foldMap' f p
  where
    f = \case
      Pattern.Unbound _ -> mempty
      Pattern.Var _ -> mempty
      Pattern.Boolean _ _ -> mempty
      Pattern.Int _ _ -> mempty
      Pattern.Nat _ _ -> mempty
      Pattern.Float _ _ -> mempty
      Pattern.Text _ _ -> mempty
      Pattern.Char _ _ -> mempty
      Pattern.As _ _ -> mempty
      Pattern.SequenceLiteral _ _ -> mempty
      Pattern.SequenceOp _ _ _ _ -> mempty
      Pattern.EffectPure _ _ -> mempty
      Pattern.EffectBind _ r i _ _ -> countHQ $ PrettyPrintEnv.patternName n r i
      Pattern.Constructor _ r i _ ->
        if noImportRefs r
          then mempty
          else countHQ $ PrettyPrintEnv.patternName n r i

countHQ :: HQ.HashQualified -> PrintAnnotation
countHQ hq = fold $ fmap countName (HQ.toName $ hq)

countName :: Name -> PrintAnnotation
countName n =
  let f = \(p, s) -> (s, Map.singleton p 1)
   in PrintAnnotation {usages = Map.fromList $ map f $ splitName n}

-- Generates all valid splits of a name into a prefix and suffix.
-- See examples in Unison.Test.TermPrinter
splitName :: Name -> [(Prefix, Suffix)]
splitName n =
  let ns = NameSegment.toText <$> Name.segments n
   in filter (not . Text.null . snd) $ inits ns `zip` map dotConcat (tails ns)

joinName :: Prefix -> Suffix -> Name
joinName p s = Name.unsafeFromText $ dotConcat $ p ++ [s]

dotConcat :: [Text] -> Text
dotConcat = Text.concat . (intersperse ".")

-- This predicate is used to keep certain refs out of the FQN elision annotations,
-- so that we don't get `use` statements for them.
--
-- Don't do `use () ()` or `use Pair Pair`.  Tuple syntax generates ().() and Pair.Pair
-- under the covers anyway.  This does mean that if someone is using Pair.Pair directly,
-- then they'll miss out on FQN elision for that.
--
-- Don't do `use builtin.Doc Blob`, `use builtin.Link Term`, or similar.  That avoids
-- unnecessary use statements above Doc literals and termLink/typeLink.
noImportRefs :: Reference -> Bool
noImportRefs r =
  elem
    r
    [ DD.pairRef,
      DD.unitRef,
      DD.docRef,
      DD.linkRef
    ]

infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x

-- This function gets used each time we start printing a new block statement.
-- It decides what extra imports to introduce (returning the full new set), and
-- determines some pretty-printed lines that looks like
--    use A x
--    use B y
-- providing a `[Pretty SyntaxText] -> Pretty SyntaxText` that prepends those
-- lines to the list of lines provided, and then concatenates them.
calcImports ::
  (Var v, Ord v) =>
  Imports ->
  Term3 v PrintAnnotation ->
  (Imports, [Pretty SyntaxText] -> Pretty SyntaxText)
calcImports im tm = (im', render $ getUses result)
  where
    -- The guts of this function is a pipeline of transformations and filters, starting from the
    -- PrintAnnotation we built up in printAnnotate.
    -- In `result`, the Name matches Prefix ++ Suffix; and the Int is the number of usages in this scope.
    -- `result` lists all the names we're going to import, and what Prefix we'll use for each.
    result :: Map Name (Prefix, Suffix, Int)
    result =
      usages'
        |> uniqueness
        |> enoughUsages
        |> groupAndCountLength
        |> longestPrefix
        |> avoidRepeatsAndClashes
        |> narrowestPossible
    usages' :: Map Suffix (Map Prefix Int)
    usages' = usages $ annotation tm
    -- Keep only names P.S where there is no other Q with Q.S also used in this scope.
    uniqueness :: Map Suffix (Map Prefix Int) -> Map Suffix (Prefix, Int)
    uniqueness m =
      m |> Map.filter (\ps -> (Map.size ps) == 1)
        |> Map.map (\ps -> head $ Map.toList ps)
    -- Keep only names where the number of usages in this scope
    --   - is > 1, or
    --   - is 1, and S is an infix operator.
    -- Also drop names with an empty prefix.
    lookupOrDie s m = fromMaybe msg (Map.lookup s m)
      where
        msg = error $ "TermPrinter.enoughUsages " <> show (s, m)

    enoughUsages :: Map Suffix (Prefix, Int) -> Map Suffix (Prefix, Int)
    enoughUsages m =
      (Map.keys m)
        |> filter
          ( \s ->
              let (p, i) = lookupOrDie s m
               in (i > 1 || isRight (symbolyId (unpack s)))
                    && (length p > 0)
          )
        |> map (\s -> (s, lookupOrDie s m))
        |> Map.fromList
    -- Group by `Prefix ++ Suffix`, and then by `length Prefix`
    groupAndCountLength :: Map Suffix (Prefix, Int) -> Map (Name, Int) (Prefix, Suffix, Int)
    groupAndCountLength m =
      Map.toList m
        |> map
          ( \(s, (p, i)) ->
              let n = joinName p s
                  l = length p
               in ((n, l), (p, s, i))
          )
        |> Map.fromList
    -- For each k1, choose the v with the largest k2.
    longestPrefix :: (Show k1, Show k2, Ord k1, Ord k2) => Map (k1, k2) v -> Map k1 v
    longestPrefix m =
      let k1s = Set.map fst $ Map.keysSet m
          k2s =
            k1s
              |> Map.fromSet
                ( \k1' ->
                    Map.keysSet m
                      |> Set.filter (\(k1, _) -> k1 == k1')
                      |> Set.map snd
                )
          maxk2s = Map.map maximum k2s
          err k1 k2 =
            error $
              "TermPrinter.longestPrefix not found "
                <> show (k1, k2)
                <> " in "
                <> show maxk2s
       in Map.mapWithKey (\k1 k2 -> fromMaybe (err k1 k2) $ Map.lookup (k1, k2) m) maxk2s
    -- Don't do another `use` for a name for which we've already done one, unless the
    -- new suffix is shorter.
    avoidRepeatsAndClashes :: Map Name (Prefix, Suffix, Int) -> Map Name (Prefix, Suffix, Int)
    avoidRepeatsAndClashes = Map.filterWithKey $
      \n (_, s', _) -> case Map.lookup n im of
        Just s -> (Text.length s') < (Text.length s)
        Nothing -> True
    -- Is there a strictly smaller block term underneath this one, containing all the usages
    -- of some of the names?  Skip emitting `use` statements for those, so we can do it
    -- further down, closer to the use sites.
    narrowestPossible :: Map Name (Prefix, Suffix, Int) -> Map Name (Prefix, Suffix, Int)
    narrowestPossible m = m |> Map.filter (\(p, s, i) -> not $ allInSubBlock tm p s i)
    -- `union` is left-biased, so this can replace existing imports.
    im' = getImportMapAdditions result `Map.union` im
    getImportMapAdditions :: Map Name (Prefix, Suffix, Int) -> Map Name Suffix
    getImportMapAdditions = Map.map (\(_, s, _) -> s)
    getUses :: Map Name (Prefix, Suffix, Int) -> Map Prefix (Set Suffix)
    getUses m =
      Map.elems m |> map (\(p, s, _) -> (p, Set.singleton s))
        |> Map.fromListWith Set.union
    render :: Map Prefix (Set Suffix) -> [Pretty SyntaxText] -> Pretty SyntaxText
    render m rest =
      let uses =
            Map.mapWithKey
              ( \p ss ->
                  (fmt S.UseKeyword $ l "use ")
                    <> (fmt S.UsePrefix (intercalateMap (l ".") (l . unpack) p))
                    <> l " "
                    <> (fmt S.UseSuffix (intercalateMap (l " ") (l . unpack) (Set.toList ss)))
              )
              m
              |> Map.toList
              |> map snd
       in PP.lines (uses ++ rest)

-- Given a block term and a name (Prefix, Suffix) of interest, is there a strictly smaller
-- blockterm within it, containing all usages of that name?  A blockterm is a place
-- where the syntax lets us put a use statement, like the branches of an if/then/else.
-- We traverse the block terms by traversing the whole subtree with ABT.find, and paying
-- attention to those subterms that look like a blockterm.  This is complicated
-- by the fact that you can't always tell if a term is a blockterm just
-- by looking at it: in some cases you can only tell when you can see it in the context of
-- the wider term that contains it.  So actually we traverse the tree, at each term
-- looking for child terms that are block terms, and see if any of those contain
-- all the usages of the name.
-- Cut out the occurrences of "const id $" to get tracing.
allInSubBlock :: (Var v, Ord v) => Term3 v PrintAnnotation -> Prefix -> Suffix -> Int -> Bool
allInSubBlock tm p s i =
  let found = concat $ ABT.find finder tm
      result = any (/= tm) $ found
      tr =
        const id $
          trace
            ( "\nallInSubBlock(" ++ show p ++ ", "
                ++ show s
                ++ ", "
                ++ show i
                ++ "): returns "
                ++ show result
                ++ "\nInput:\n"
                ++ show tm
                ++ "\nFound: \n"
                ++ show found
                ++ "\n\n"
            )
   in tr result
  where
    getUsages t =
      annotation t
        |> usages
        |> Map.lookup s
        |> fmap (Map.lookup p)
        |> join
        |> fromMaybe 0
    finder t =
      let result =
            let i' = getUsages t
             in if i' < i
                  then ABT.Prune
                  else
                    let found = filter hit $ immediateChildBlockTerms t
                     in if (i' == i) && (not $ null found)
                          then ABT.Found found
                          else ABT.Continue
          children = concat (map (\t -> "child: " ++ show t ++ "\n") $ immediateChildBlockTerms t)
          tr =
            const id $
              trace
                ( "\nfinder: returns " ++ show result
                    ++ "\n  children:"
                    ++ children
                    ++ "\n  input: \n"
                    ++ show t
                    ++ "\n\n"
                )
       in tr $ result
    hit t = (getUsages t) == i

-- Return any blockterms at or immediately under this term.  Has to match the places in the
-- syntax that get a call to `calcImports` in `pretty0`.  AST nodes that do a calcImports in
-- pretty0, in order to try and emit a `use` statement, need to be emitted also by this
-- function, otherwise the `use` statement may come out at an enclosing scope instead.
immediateChildBlockTerms :: (Var vt, Var v) => Term2 vt at ap v a -> [Term2 vt at ap v a]
immediateChildBlockTerms = \case
  Handle' handler body -> [handler, body]
  If' _ t f -> [t, f]
  LetBlock bs _ -> concat $ map doLet bs
  Match' scrute branches ->
    if isDestructuringBind scrute branches
      then [scrute]
      else concat $ map doCase branches
  _ -> []
  where
    doCase (MatchCase _ _ (AbsN' _ body)) = [body]
    doCase _ = error "bad match" []
    doLet (v, Ann' tm _) = doLet (v, tm)
    doLet (v, LamsNamedOpt' _ body) =
      if isBlank $ Var.nameStr v
        then []
        else [body]
    doLet t = error (show t) []

-- Matches with a single case, no variable shadowing, and where the pattern
-- has no literals are treated as destructuring bind, for instance:
--   match blah with (x,y) -> body
-- BECOMES
--   (x,y) = blah
--   body
-- BUT
--   match (y,x) with (x,y) -> body
-- Has shadowing, is rendered as a regular `match`.
--   match blah with 42 -> body
-- Pattern has (is) a literal, rendered as a regular match (rather than `42 = blah; body`)
isDestructuringBind :: Ord v => ABT.Term f v a -> [MatchCase loc (ABT.Term f v a)] -> Bool
isDestructuringBind scrutinee [MatchCase pat _ (ABT.AbsN' vs _)] =
  all (`Set.notMember` ABT.freeVars scrutinee) vs && not (hasLiteral pat)
  where
    hasLiteral p = case p of
      Pattern.Int _ _ -> True
      Pattern.Boolean _ _ -> True
      Pattern.Nat _ _ -> True
      Pattern.Float _ _ -> True
      Pattern.Text _ _ -> True
      Pattern.Char _ _ -> True
      Pattern.Constructor _ _ _ ps -> any hasLiteral ps
      Pattern.As _ p -> hasLiteral p
      Pattern.EffectPure _ p -> hasLiteral p
      Pattern.EffectBind _ _ _ ps pk -> any hasLiteral (pk : ps)
      Pattern.SequenceLiteral _ ps -> any hasLiteral ps
      Pattern.SequenceOp _ p _ p2 -> hasLiteral p || hasLiteral p2
      Pattern.Var _ -> False
      Pattern.Unbound _ -> False
isDestructuringBind _ _ = False

pattern LetBlock bindings body <- (unLetBlock -> Just (bindings, body))

-- Collects nested let/let rec blocks into one minimally nested block.
-- Handy because `let` and `let rec` blocks get rendered the same way.
-- We preserve nesting when the inner block shadows definitions in the
-- outer block.
unLetBlock ::
  Ord v =>
  Term2 vt at ap v a ->
  Maybe ([(v, Term2 vt at ap v a)], Term2 vt at ap v a)
unLetBlock t = rec t
  where
    dontIntersect v1s v2s =
      all (`Set.notMember` v2set) (fst <$> v1s)
      where
        v2set = Set.fromList (fst <$> v2s)
    rec t = case unLetRecNamed t of
      Nothing -> nonrec t
      Just (_isTop, bindings, body) -> case rec body of
        Just (innerBindings, innerBody)
          | dontIntersect bindings innerBindings ->
            Just (bindings ++ innerBindings, innerBody)
        _ -> Just (bindings, body)
    nonrec t = case unLet t of
      Nothing -> Nothing
      Just (bindings0, body) ->
        let bindings = [(v, b) | (_, v, b) <- bindings0]
         in case rec body of
              Just (innerBindings, innerBody)
                | dontIntersect bindings innerBindings ->
                  Just (bindings ++ innerBindings, innerBody)
              _ -> Just (bindings, body)

pattern LamsNamedMatch' vs branches <- (unLamsMatch' -> Just (vs, branches))

-- This function is used to detect places where lambda case syntax can be used.
-- When given lambdas of a form that corresponds to a lambda case, it returns
-- `Just (varsBeforeCases, branches)`. Leading vars are the vars that should be
-- shown to the left of the `-> cases`.
--
-- For instance, if given this term:
--
--   x y z -> match z with
--     [] -> "empty"
--     (h +: t) -> "nonempty"
--
-- this function will return Just ([x,y], [[] -> "empty", (h +: t) -> "nonempty"])
-- and it would be rendered as
--
--   x y -> cases []     -> "empty"
--                h +: t -> "nonempty"
--
-- Given this term
--
--   x y z -> match (y, z) with
--     ("a", "b") -> "abba"
--     (x, y) -> y ++ x
--
-- this function will return Just ([x], [ "a" "b" -> "abba", x y -> y ++ x])
-- and it would be rendered as `x -> cases "a", "b" -> "abba"
--                                         x,    y  -> y ++ x
--
-- This function returns `Nothing` in cases where the term it is given isn't
-- a lambda, or when the lambda isn't in the correct form for lambda cases.
-- (For instance, `x -> match (x, 42) with ...` can't be written using
-- lambda case)
unLamsMatch' ::
  Var v =>
  Term2 vt at ap v a ->
  Maybe ([v], [([Pattern ap], Maybe (Term2 vt at ap v a), Term2 vt at ap v a)])
unLamsMatch' t = case unLamsUntilDelay' t of
  -- x -> match x with pat -> ...
  --   becomes
  -- cases pat -> ...
  Just (reverse -> (v1 : vs), Match' (Var' v1') branches)
    | -- if `v1'` is referenced in any of the branches, we can't use lambda case
      -- syntax as we need to keep the `v1'` name that was introduced
      (v1 == v1') && Set.notMember v1' (Set.unions $ freeVars <$> branches) ->
      Just (reverse vs, [([p], guard, body) | MatchCase p guard body <- branches])
  -- x y z -> match (x,y,z) with (pat1, pat2, pat3) -> ...
  --   becomes
  -- cases pat1 pat2 pat3 -> ...`
  Just (reverse -> vs@(_ : _), Match' (TupleTerm' scrutes) branches)
    | multiway vs (reverse scrutes)
        &&
        -- (as above) if any of the vars are referenced in any of the branches,
        -- we need to keep the names introduced by the lambda and can't use
        -- lambda case syntax
        all notFree (take len vs)
        && all isRightArity branches
        && len /= 0 -> -- all patterns need to match arity of scrutes
      Just (reverse (drop len vs), branches')
    where
      isRightArity (MatchCase (TuplePattern ps) _ _) = length ps == len
      isRightArity (MatchCase {}) = False
      len = length scrutes
      fvs = Set.unions $ freeVars <$> branches
      notFree v = Set.notMember v fvs
      branches' = [(ps, guard, body) | MatchCase (TuplePattern ps) guard body <- branches]
  _ -> Nothing
  where
    -- multiway vs tms checks that length tms <= length vs, and their common prefix
    -- is all matching variables
    multiway _ [] = True
    multiway (h : t) (Var' h2 : t2) | h == h2 = multiway t t2
    multiway _ _ = False
    freeVars (MatchCase _ g rhs) =
      let guardVars = (fromMaybe Set.empty $ ABT.freeVars <$> g)
          rhsVars = (ABT.freeVars rhs)
       in Set.union guardVars rhsVars

pattern Bytes' bs <- (toBytes -> Just bs)

toBytes :: Term3 v PrintAnnotation -> Maybe [Word64]
toBytes (App' (Builtin' "Bytes.fromList") (Sequence' bs)) =
  toList <$> traverse go bs
  where
    go (Nat' n) = Just n
    go _ = Nothing
toBytes _ = Nothing
