{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.TermPrinter where

import           Control.Monad                  ( join )
import           Data.List
import           Data.List.Extra                ( dropEnd )
import           Data.Either                    ( isRight )
import           Data.Foldable                  ( fold
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                , fromJust
                                                )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.String                    ( IsString, fromString )
import           Data.Text                      ( Text, splitOn, unpack )
import qualified Data.Text                     as Text
import           Data.Vector                    ( )
import           Text.Read                      ( readMaybe )
import           Unison.ABT                     ( pattern AbsN', reannotateUp, annotation ) 
import qualified Unison.ABT                    as ABT
import qualified Unison.Blank                  as Blank
import qualified Unison.HashQualified          as HQ
import           Unison.Lexer                   ( symbolyId )
import           Unison.Name                    ( Name )
import qualified Unison.Name                   as Name
import           Unison.NamePrinter             ( prettyHashQualified, prettyHashQualified0 )
import qualified Unison.Pattern                as Pattern
import           Unison.PatternP                ( Pattern )
import qualified Unison.PatternP               as PatternP
import qualified Unison.Referent               as Referent
import qualified Unison.SyntaxHighlights       as S
import           Unison.SyntaxHighlights        ( fmt )
import           Unison.Term
import           Debug.Trace                    ( trace )
import           Unison.Type                    ( AnnotatedType )
import qualified Unison.Type                   as Type
import qualified Unison.TypePrinter            as TypePrinter
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var
import           Unison.Util.Monoid             ( intercalateMap )
import qualified Unison.Util.Pretty             as PP
import           Unison.Util.Pretty             ( Pretty, ColorText )
import           Unison.PrettyPrintEnv          ( PrettyPrintEnv, Suffix, Prefix, Imports, elideFQN )
import qualified Unison.PrettyPrintEnv         as PrettyPrintEnv
import qualified Unison.DataDeclaration        as DD
import Unison.DataDeclaration (pattern TuplePattern, pattern TupleTerm')

--TODO #287:
--  - (fix Pair/Unit tuple FQN elision)
--  - hash qualification

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
  , imports :: Imports
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
prettyTop env tm = pretty env (ac (-1) Normal Map.empty) (printAnnotate env tm)

pretty
  :: Var v
  => PrettyPrintEnv
  -> AmbientContext
  -> AnnotatedTerm3 v PrintAnnotation
  -> Pretty ColorText
pretty n AmbientContext { precedence = p, blockContext = bc, infixContext = ic, imports = im} term
  = specialCases term $ \case
    Var' v -> parenIfInfix name ic . fmt S.Var . prettyHashQualified $ name
      -- OK since all term vars are user specified, any freshening was just added during typechecking
      where name = elideFQN im $ HQ.fromVar (Var.reset v)
    Ref' r -> parenIfInfix name ic . fmt S.Reference . prettyHashQualified0 $ name
      where name = elideFQN im $ PrettyPrintEnv.termName n (Referent.Ref r)
    Ann' tm t ->
      paren (p >= 0)
        $  pretty n (ac 10 Normal im) tm
        <> PP.hang (fmt S.TypeAscriptionColon " :" ) (TypePrinter.pretty n im 0 t)
    Int'     i  -> fmt S.NumericLiteral $ (if i >= 0 then l "+" else mempty) <> (l $ show i)
    Nat'     u  -> fmt S.NumericLiteral $ l $ show u
    Float'   f  -> fmt S.NumericLiteral $ l $ show f
    -- TODO How to handle Infinity, -Infinity and NaN?  Parser cannot parse
    --      them.  Haskell doesn't have literals for them either.  Is this
    --      function only required to operate on terms produced by the parser?
    --      In which case the code is fine as it stands.  If it can somehow run
    --      on values produced by execution (or, one day, on terms produced by
    --      metaprograms), then it needs to be able to print them (and then the
    --      parser ought to be able to parse them, to maintain symmetry.)
    Boolean' b  -> fmt S.BooleanLiteral $ if b then l "true" else l "false"
    Text'    s  -> fmt S.TextLiteral $ l $ show s
    Blank'   id -> fmt S.Blank $ l "_" <> (l $ fromMaybe "" (Blank.nameb id))
    Constructor' ref i -> fmt S.Constructor $ prettyHashQualified $ 
      elideFQN im $ PrettyPrintEnv.termName n (Referent.Con ref i)
    Request' ref i -> fmt S.Request $ prettyHashQualified $ 
      elideFQN im $ PrettyPrintEnv.termName n (Referent.Con ref i)
    Handle' h body -> let (im', uses) = calcImports im body in
      paren (p >= 2)
        $ ((fmt S.ControlKeyword "handle") `PP.hang` pretty n (ac 2 Normal im) h)
        <> PP.softbreak
        <> ((fmt S.ControlKeyword "in") `PP.hang` (uses $ [pretty n (ac 2 Block im') body]))
    App' x (Constructor' DD.UnitRef 0) ->
      paren (p >= 11) $ (fmt S.DelayForceChar $ l "!") <> pretty n (ac 11 Normal im) x
    AskInfo' x -> paren (p >= 11) $ pretty n (ac 11 Normal im) x <> (fmt S.DelimiterChar $ l "?")
    LamNamed' v x | (Var.name v) == "()" ->
      paren (p >= 11) $ (fmt S.DelayForceChar $ l "'") <> pretty n (ac 11 Normal im) x
    Sequence' xs -> PP.group $
      (fmt S.DelimiterChar $ l "[") <> optSpace
          <> intercalateMap ((fmt S.DelimiterChar $ l ",") <> PP.softbreak <> optSpace <> optSpace)
                            (pretty n (ac 0 Normal im))
                            xs
          <> optSpace <> (fmt S.DelimiterChar $ l "]")
      where optSpace = PP.orElse "" " "
    If' cond t f -> paren (p >= 2) $
      if height > 0 then PP.lines [
        (fmt S.ControlKeyword "if ") <> pcond <> (fmt S.ControlKeyword " then") `PP.hang` pt,
        (fmt S.ControlKeyword "else") `PP.hang` pf
       ]
      else PP.spaced [
        (fmt S.ControlKeyword "if") `PP.hang` pcond <> ((fmt S.ControlKeyword " then") `PP.hang` pt),
        (fmt S.ControlKeyword "else") `PP.hang` pf
       ]
     where
       height = PP.preferredHeight pt `max` PP.preferredHeight pf
       pcond  = branch cond
       pt     = branch t
       pf     = branch f
       branch tm = let (im', uses) = calcImports im tm
                   in uses $ [pretty n (ac 2 Block im') tm]
    And' x y ->
      paren (p >= 10) $ PP.spaced [
        fmt S.ControlKeyword "and", 
        pretty n (ac 10 Normal im) x,
        pretty n (ac 10 Normal im) y
      ]
    Or' x y ->
      paren (p >= 10) $ PP.spaced [
        fmt S.ControlKeyword "or", 
        pretty n (ac 10 Normal im) x,
        pretty n (ac 10 Normal im) y
      ]
    LetRecNamed' bs e -> printLet bc bs e im' uses
    Lets' bs e -> printLet bc (map (\(_, v, binding) -> (v, binding)) bs) e im' uses
    Match' scrutinee branches -> paren (p >= 2) $
      ((fmt S.ControlKeyword "case ") <> pretty n (ac 2 Normal im) scrutinee <> (fmt S.ControlKeyword " of")) `PP.hang` bs
      where bs = PP.lines (map printCase branches)
    t -> l "error: " <> l (show t)
 where
  specialCases term go = case (term, binaryOpsPred) of
    (TupleTerm' [x], _) ->
      paren (p >= 10) $ (fmt S.Constructor "Pair") `PP.hang`
        PP.spaced [pretty n (ac 10 Normal im) x, (fmt S.Constructor "()") ]
    (TupleTerm' xs, _) -> paren True $ commaList xs
    BinaryAppsPred' apps lastArg -> paren (p >= 3) $
      binaryApps apps (pretty n (ac 3 Normal im) lastArg)
    _ -> case (term, nonForcePred) of
      AppsPred' f args | not $ isVarKindInfo f ->
        paren (p >= 10) $ pretty n (ac 10 Normal im) f `PP.hang`
          PP.spacedMap (pretty n (ac 10 Normal im)) args
      _ -> case (term, nonUnitArgPred) of
        LamsNamedPred' vs body ->
          paren (p >= 3) $
            PP.group (varList vs <> (fmt S.ControlKeyword " ->")) `PP.hang` pretty n (ac 2 Block im) body
        _ -> go term

  sepList sep xs = sepList' (pretty n (ac 0 Normal im)) sep xs
  sepList' f sep xs = fold $ intersperse sep (map f xs)
  varList vs = sepList' (PP.text . Var.name) PP.softbreak vs
  commaList = sepList ((fmt S.DelimiterChar $ l ",") <> PP.softbreak)

  printLet :: Var v 
           => BlockContext 
           -> [(v, AnnotatedTerm3 v PrintAnnotation)] 
           -> AnnotatedTerm3 v PrintAnnotation 
           -> Imports 
           -> ([Pretty ColorText] -> Pretty ColorText)
           -> Pretty ColorText
  printLet sc bs e im' uses =
    paren ((sc /= Block) && p >= 12)
      $  letIntro
      $  (uses [(PP.lines (map printBinding bs ++
                            [PP.group $ pretty n (ac 0 Normal im') e]))])
   where
    printBinding (v, binding) = if isBlank $ Var.nameStr v
      then pretty n (ac (-1) Normal im') binding
      else prettyBinding2 n (ac (-1) Normal im') (HQ.fromVar v) binding
    letIntro = case sc of
      Block  -> id
      Normal -> \x -> (fmt S.ControlKeyword "let") `PP.hang` x
    
  printCase :: Var v => MatchCase () (AnnotatedTerm3 v PrintAnnotation) -> Pretty ColorText
  printCase (MatchCase pat guard (AbsN' vs body)) =
    PP.group $ lhs `PP.hang` (uses [pretty n (ac 0 Block im') body])
    where
    lhs = PP.group (fst (prettyPattern n (ac 0 Block im) (-1) vs pat) <> " ")
       <> printGuard guard
       <> (fmt S.ControlKeyword "->")
    printGuard (Just g) = PP.group $ PP.spaced [(fmt S.DelimiterChar "|"), pretty n (ac 2 Normal im) g, ""]
    printGuard Nothing  = mempty
    (im', uses) = calcImports im body
  printCase _ = l "error"

  -- This predicate controls which binary functions we render as infix
  -- operators.  At the moment the policy is just to render symbolic
  -- operators as infix - not 'wordy' function names.  So we produce
  -- "x + y" and "foo x y" but not "x `foo` y".
  binaryOpsPred :: Var v => AnnotatedTerm3 v PrintAnnotation -> Bool
  binaryOpsPred = \case
    Ref' r | isSymbolic (PrettyPrintEnv.termName n (Referent.Ref r)) -> True
    Var' v | isSymbolic (HQ.fromVar v) -> True
    _ -> False

  nonForcePred :: AnnotatedTerm3 v PrintAnnotation -> Bool
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
    :: Var v => [(AnnotatedTerm3 v PrintAnnotation, AnnotatedTerm3 v PrintAnnotation)]
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
    r a f = [pretty n (ac 3 Normal im) a,
             pretty n (AmbientContext 10 Normal Infix im) f]

  (im', uses) = calcImports im term

pretty' ::
  Var v => Maybe Int -> PrettyPrintEnv -> AnnotatedTerm v a -> ColorText
pretty' (Just width) n t = PP.render width $ pretty n (ac (-1) Normal Map.empty) (printAnnotate n t)
pretty' Nothing      n t = PP.renderUnbroken $ pretty n (ac (-1) Normal Map.empty) (printAnnotate n t)

prettyPattern
  :: forall v loc . Var v
  => PrettyPrintEnv
  -> AmbientContext
  -> Int
  -> [v]
  -> Pattern loc
  -> (Pretty ColorText, [v])
-- vs is the list of pattern variables used by the pattern, plus possibly a
-- tail of variables it doesn't use.  This tail is the second component of
-- the return value.
prettyPattern n c@(AmbientContext { imports = im }) p vs patt = case patt of
  PatternP.Unbound _   -> (fmt S.DelimiterChar $ l "_", vs)
  PatternP.Var     _   -> let (v : tail_vs) = vs in (fmt S.Var $ l $ Var.nameStr v, tail_vs)
  PatternP.Boolean _ b -> (fmt S.BooleanLiteral $ if b then l "true" else l "false", vs)
  PatternP.Int     _ i -> (fmt S.NumericLiteral $ (if i >= 0 then l "+" else mempty) <> (l $ show i), vs)
  PatternP.Nat     _ u -> (fmt S.NumericLiteral $ l $ show u, vs)
  PatternP.Float   _ f -> (fmt S.NumericLiteral $ l $ show f, vs)
  PatternP.Text    _ t -> (fmt S.TextLiteral $ l $ show t, vs)
  TuplePattern [pp] ->
    let (printed, tail_vs) = prettyPattern n c 10 vs pp
    in  ( paren (p >= 10) $ PP.sep " " [fmt S.Constructor "Pair", printed, fmt S.Constructor "()"]
        , tail_vs )
  TuplePattern pats ->
    let (pats_printed, tail_vs) = patterns vs pats
    in  (PP.parenthesizeCommas pats_printed, tail_vs)
  PatternP.Constructor _ ref i [] ->
    (fmt S.Constructor $ prettyHashQualified $ elideFQN im (PrettyPrintEnv.patternName n ref i), vs)
  PatternP.Constructor _ ref i pats ->
    let (pats_printed, tail_vs) = patternsSep PP.softbreak vs pats
    in  ( paren (p >= 10)
          $ prettyHashQualified (elideFQN im (PrettyPrintEnv.patternName n ref i))
            `PP.hang` pats_printed
        , tail_vs)
  PatternP.As _ pat ->
    let (v : tail_vs)            = vs
        (printed, eventual_tail) = prettyPattern n c 11 tail_vs pat
    in  (paren (p >= 11) $ ((fmt S.Var $ l $ Var.nameStr v) <> (fmt S.DelimiterChar $ l "@") <> printed), eventual_tail)
  PatternP.EffectPure _ pat ->
    let (printed, eventual_tail) = prettyPattern n c (-1) vs pat
    in  (PP.sep " " [fmt S.DelimiterChar "{", printed, fmt S.DelimiterChar "}"], eventual_tail)
  PatternP.EffectBind _ ref i pats k_pat ->
    let (pats_printed , tail_vs      ) = patternsSep PP.softbreak vs pats
        (k_pat_printed, eventual_tail) = prettyPattern n c 0 tail_vs k_pat
    in  ((fmt S.DelimiterChar "{" ) <>
          (PP.sep " " . PP.nonEmpty $ [
            prettyHashQualified $ elideFQN im (PrettyPrintEnv.patternName n ref i),
            pats_printed,
            fmt S.ControlKeyword "->",
            k_pat_printed]) <>
         (fmt S.DelimiterChar "}")
        , eventual_tail)
  PatternP.SequenceLiteral _ pats ->
    let (pats_printed, tail_vs) = patternsSep (fmt S.DelimiterChar ", ") vs pats
    in  ((fmt S.DelimiterChar "[") <> pats_printed <> (fmt S.DelimiterChar "]"), tail_vs)
  PatternP.SequenceOp _ l op r ->
    let (pl, lvs) = prettyPattern n c p vs l
        (pr, rvs) = prettyPattern n c (p + 1) lvs r
        f i s = (paren (p >= i) (pl <> " " <> (fmt S.Reference s) <> " " <> pr), rvs)
    in case op of
      Pattern.Cons -> f 9 "+:"
      Pattern.Snoc -> f 9 ":+"
      Pattern.Concat -> f 9 "++"
  t -> (l "error: " <> l (show t), vs)
 where
  l :: IsString s => String -> s
  l = fromString
  patterns vs (pat : pats) =
    let (printed     , tail_vs      ) = prettyPattern n c (-1) vs pat
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
  Var v => PrettyPrintEnv -> HQ.HashQualified -> AnnotatedTerm2 v at ap v a -> Pretty ColorText
prettyBinding n = prettyBinding2 n (ac (-1) Block Map.empty)

prettyBinding2 ::
  Var v => PrettyPrintEnv -> AmbientContext -> HQ.HashQualified -> AnnotatedTerm2 v at ap v a -> Pretty ColorText
prettyBinding2 env a@(AmbientContext { imports = im }) v term = go (symbolic && isBinary term) term where
  go infix' = \case
    Ann' tm tp -> PP.lines [
      PP.group (renderName v <> PP.hang (fmt S.TypeAscriptionColon " :") (TypePrinter.pretty env im (-1) tp)),
      PP.group (prettyBinding2 env a v tm) ]
    LamsNamedOpt' vs body -> 
      let 
        (im', uses) = calcImports im body'
        -- In the case where we're being called from inside `pretty`, this call to 
        -- printAnnotate is unfortunately repeating work we've already done.
        body' = printAnnotate env body 
      in PP.group $
        PP.group (defnLhs v vs <> (fmt S.BindingEquals " =")) `PP.hang`
        (uses [pretty env (ac (-1) Block im') body'])
    t -> l "error: " <> l (show t)
   where
    defnLhs v vs = if infix'
      then case vs of
        x : y : _ ->
          PP.sep " " [fmt S.Var $ PP.text (Var.name x),
                      fmt S.Reference $ prettyHashQualified $ elideFQN im v,
                      fmt S.Var $ PP.text (Var.name y)]
        _ -> l "error"
      else if null vs then renderName v
      else renderName v `PP.hang` args vs
    args vs = PP.spacedMap ((fmt S.Var) . PP.text . Var.name) vs
    renderName n = let n' = elideFQN im n 
                   in parenIfInfix n' NonInfix $ fmt S.Reference $ prettyHashQualified n'
                   
  symbolic = isSymbolic v
  isBinary = \case
    Ann'          tm _ -> isBinary tm
    LamsNamedOpt' vs _ -> length vs == 2
    _                  -> False -- unhittable

prettyBinding'
  :: Var v => Int -> PrettyPrintEnv -> HQ.HashQualified -> AnnotatedTerm v a -> ColorText
prettyBinding' width n v t = PP.render width $ prettyBinding n v t

paren :: Bool -> Pretty ColorText -> Pretty ColorText
paren True  s = PP.group $ ( fmt S.Parenthesis "(" ) <> s <> ( fmt S.Parenthesis ")" )
paren False s = PP.group s

parenIfInfix
  :: HQ.HashQualified -> InfixContext -> (Pretty ColorText -> Pretty ColorText)
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
  _       -> False

isBlank :: String -> Bool
isBlank ('_' : rest) | (isJust ((readMaybe rest) :: Maybe Int)) = True
isBlank _ = False

ac :: Int -> BlockContext -> Imports -> AmbientContext
ac prec bc im = AmbientContext prec bc NonInfix im

{- # FQN elision

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
  {
    -- For each suffix that appears in/under this term, the set of prefixes 
    -- used with that suffix, and how many times each occurs.  
    usages :: Map Suffix (Map Prefix Int)
  } deriving (Show)

instance Semigroup PrintAnnotation where
  (PrintAnnotation { usages = a } ) <> (PrintAnnotation { usages = b } ) = 
    PrintAnnotation { usages = Map.unionWith f a b } where
      f a' b' = Map.unionWith (+) a' b'

instance Monoid PrintAnnotation where
  mempty = PrintAnnotation { usages = Map.empty }

suffixCounterTerm :: Var v => PrettyPrintEnv -> AnnotatedTerm2 v at ap v a -> PrintAnnotation
suffixCounterTerm n = \case
    Var' v -> countHQ $ HQ.fromVar v
    Ref' r -> countHQ $ PrettyPrintEnv.termName n (Referent.Ref r)
    Constructor' r i -> countHQ $ PrettyPrintEnv.termName n (Referent.Con r i)
    Request' r i -> countHQ $ PrettyPrintEnv.termName n (Referent.Con r i)
    Ann' _ t -> countTypeUsages n t
    Match' _ bs -> let pat (MatchCase p _ _) = p
                   in foldMap ((countPatternUsages n) . pat) bs
    _ -> mempty

suffixCounterType :: Var v => PrettyPrintEnv -> AnnotatedType v a -> PrintAnnotation
suffixCounterType n = \case
    Type.Var' v -> countHQ $ HQ.fromVar v
    Type.Ref' r -> countHQ $ PrettyPrintEnv.typeName n r
    _ -> mempty

printAnnotate :: (Var v, Ord v) => PrettyPrintEnv -> AnnotatedTerm2 v at ap v a -> AnnotatedTerm3 v PrintAnnotation
printAnnotate n tm = fmap snd (go (reannotateUp (suffixCounterTerm n) tm)) where
  go :: Ord v => AnnotatedTerm2 v at ap v b -> AnnotatedTerm2 v () () v b
  go = extraMap' id (const ()) (const ())

countTypeUsages :: (Var v, Ord v) => PrettyPrintEnv -> AnnotatedType v a -> PrintAnnotation
countTypeUsages n t = snd $ annotation $ reannotateUp (suffixCounterType n) t 
                      
countPatternUsages :: PrettyPrintEnv -> Pattern loc -> PrintAnnotation
countPatternUsages n p = Pattern.foldMap' f p where
  f = \case 
    Pattern.UnboundP _            -> mempty
    Pattern.VarP _                -> mempty
    Pattern.BooleanP _ _          -> mempty
    Pattern.IntP _ _              -> mempty
    Pattern.NatP _ _              -> mempty
    Pattern.FloatP _ _            -> mempty
    Pattern.TextP _ _             -> mempty
    Pattern.AsP _ _               -> mempty
    Pattern.SequenceLiteralP _ _  -> mempty
    Pattern.SequenceOpP _ _ _ _   -> mempty
    Pattern.EffectPureP _ _       -> mempty
    Pattern.EffectBindP _ r i _ _ -> countHQ $ PrettyPrintEnv.patternName n r i
    Pattern.ConstructorP _ r i _  -> countHQ $ PrettyPrintEnv.patternName n r i

countHQ :: HQ.HashQualified -> PrintAnnotation
countHQ hq = fold $ fmap countName (HQ.toName $ hq)

countName :: Name -> PrintAnnotation
countName n = let f = \(p, s) -> (s, Map.singleton p 1)
              in PrintAnnotation { usages = Map.fromList $ map f $ splitName n}

splitName :: Name -> [(Prefix, Suffix)]
splitName n = let ns = splitOn "." (Name.toText n)
              in dropEnd 1 ((inits ns) `zip` (map dotConcat $ tails ns))
-- > splitName "x" == [([], "x")]
-- > splitName "A.x" == [(["A"], "x")]
-- > splitName "A.B.x" == [(["A"], "B.x"), (["A.B"], "x")]

joinName :: Prefix -> Suffix -> Name
joinName p s = Name.unsafeFromText $ dotConcat $ p ++ [s]

dotConcat :: [Text] -> Text
dotConcat = Text.concat . (intersperse ".")

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- This function gets used each time we start printing a new block statement.
-- It decides what extra imports to introduce (returning the full new set), and 
-- determines some pretty-printed lines that looks like
--    use A x
--    use B y
-- providing a `[Pretty ColorText] -> Pretty ColorText` that prepends those
-- lines to the list of lines provided, and then concatenates them.
calcImports 
  :: (Var v, Ord v)
  => Imports 
  -> AnnotatedTerm3 v PrintAnnotation 
  -> (Imports, [Pretty ColorText] -> Pretty ColorText)
calcImports im tm = (im', render $ getUses result)
  where 
    -- The guts of this function is a pipeline of transformations and filters, starting from the
    -- PrintAnnotation we built up in printAnnotate.  
    -- In `result`, the Name matches Prefix ++ Suffix; and the Int is the number of usages in this scope. 
    -- `result` lists all the names we're going to import, and what Prefix we'll use for each.
    result :: Map Name (Prefix, Suffix, Int)
    result =    usages' 
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
    uniqueness m = m |> Map.filter (\ps -> (Map.size ps) == 1)
                     |> Map.map (\ps -> head $ Map.toList ps)
    -- Keep only names where the number of usages in this scope 
    --   - is > 1, or
    --   - is 1, and S is an infix operator.
    -- Also drop names with an empty prefix.
    enoughUsages :: Map Suffix (Prefix, Int) -> Map Suffix (Prefix, Int)
    enoughUsages m = (Map.keys m) |> filter (\s -> let (p, i) = fromJust $ Map.lookup s m
                                                   in (i > 1 || isRight (symbolyId (unpack s))) &&
                                                      (length p > 0))
                                  |> map (\s -> (s, fromJust $ Map.lookup s m))
                                  |> Map.fromList
    -- Group by `Prefix ++ Suffix`, and then by `length Prefix`
    groupAndCountLength :: Map Suffix (Prefix, Int) -> Map (Name, Int) (Prefix, Suffix, Int)
    groupAndCountLength m = Map.toList m |> map (\(s, (p, i)) -> let n = joinName p s
                                                                     l = length p 
                                                                 in ((n, l), (p, s, i)))
                                         |> Map.fromList
    -- For each k1, choose the v with the largest k2.    
    longestPrefix :: (Ord k1, Ord k2) => Map (k1, k2) v -> Map k1 v
    longestPrefix m = let k1s = Set.map fst $ Map.keysSet m
                          k2s = k1s |> Map.fromSet (\k1' -> Map.keysSet m 
                                                              |> Set.filter (\(k1, _) -> k1 == k1')
                                                              |> Set.map snd)
                          maxk2s = Map.map maximum k2s
                      in Map.mapWithKey (\k1 k2 -> fromJust $ Map.lookup (k1, k2) m) maxk2s
    -- Don't do another `use` for a name for which we've already done one, unless the
    -- new suffix is shorter.
    avoidRepeatsAndClashes :: Map Name (Prefix, Suffix, Int) -> Map Name (Prefix, Suffix, Int)
    avoidRepeatsAndClashes = Map.filterWithKey $ 
                               \n (_, s', _) -> case Map.lookup n im of
                                 Just s  -> (Text.length s') < (Text.length s)
                                 Nothing -> True
    -- Is there a strictly smaller block term underneath this one, containing all the usages
    -- of some of the names?  Skip omitting `use` statements for those, so we can do it 
    -- further down, closer to the use sites.
    narrowestPossible :: Map Name (Prefix, Suffix, Int) -> Map Name (Prefix, Suffix, Int)
    narrowestPossible m = m |> Map.filter (\(p, s, i) -> not $ allInSubBlock tm p s i)
    -- `union` is left-biased, so this can replace existing imports.                      
    im' = getImportMapAdditions result `Map.union` im
    getImportMapAdditions :: Map Name (Prefix, Suffix, Int) -> Map Name Suffix
    getImportMapAdditions = Map.map (\(_, s, _) -> s)
    getUses :: Map Name (Prefix, Suffix, Int) -> Map Prefix (Set Suffix)
    getUses m = Map.elems m |> map (\(p, s, _) -> (p, Set.singleton s))
                            |> Map.fromListWith Set.union
    render :: Map Prefix (Set Suffix) -> [Pretty ColorText] -> Pretty ColorText
    render m rest = let uses = Map.mapWithKey (\p ss -> (fmt S.UseKeyword $ l"use ") <> 
                                   (fmt S.UsePrefix (intercalateMap (l".") (l . unpack) p)) <> l" " <>
                                   (fmt S.UseSuffix (intercalateMap (l" ") (l . unpack) (Set.toList ss)))) m
                                 |> Map.toList
                                 |> map snd
                    in PP.lines (uses ++ rest)

-- Given a block term and a name (Prefix, Suffix) of interest, is there a strictly smaller
-- block term within it, containing all usages of that name?  ABT.find does the heavy lifting.
-- Things are complicated by the fact that you can't always tell if a term is a blockterm just
-- by looking at it: in some cases you can only tell when you can see it in the context of
-- the wider term that contains it.  Hence `immediateChildBlockTerms`.  
-- Cut out the occurrences of "const id $" to get tracing.
allInSubBlock :: (Var v, Ord v) => AnnotatedTerm3 v PrintAnnotation -> Prefix -> Suffix -> Int -> Bool
allInSubBlock tm p s i = let found = concat $ ABT.find finder tm 
                             result = any (/= tm) $ found
                             tr = const id $ trace ("\nallInSubBlock(" ++ show p ++ ", " ++ 
                                                    show s ++ ", " ++ show i ++ "): returns " ++ 
                                                    show result ++ "\nInput:\n" ++ show tm ++ 
                                                    "\nFound: \n" ++ show found ++ "\n\n")
                         in tr result where 
  getUsages t =    annotation t
                |> usages
                |> Map.lookup s
                |> fmap (Map.lookup p)
                |> join
                |> fromMaybe 0
  finder t = let result = let i' = getUsages t
                          in if i' < i 
                             then ABT.Prune 
                             else 
                               let found = filter hit $ immediateChildBlockTerms t
                               in if (i' == i) && (not $ null found)
                                  then ABT.Found found
                                  else ABT.Continue
                 children = concat (map (\t -> "child: " ++ show t ++ "\n") $ immediateChildBlockTerms t)
                 tr = const id $ trace ("\nfinder: returns " ++ show result ++ 
                                        "\n  children:" ++ children ++ 
                                        "\n  input: \n" ++ show t ++ "\n\n")
             in tr $ result
  hit t = (getUsages t) == i

-- Return any blockterms at or immediately under this term.  Has to match the places in the
-- syntax that get a call to `calcImports` in `pretty`.
immediateChildBlockTerms :: (Var vt, Var v) => AnnotatedTerm2 vt at ap v a -> [AnnotatedTerm2 vt at ap v a]
immediateChildBlockTerms = \case
    Handle' _ body -> [body]
    If' cond t f -> [cond, t, f]
    tm@(LetRecNamed' bs _) -> [tm] ++ (concat $ map doLet bs)
    tm@(Lets' bs _)        -> [tm] ++ (concat $ map doLet ((map (\(_, v, binding) -> (v, binding)) bs)))
    Match' _ branches -> concat $ map doCase branches
    _ -> []
  where
    doCase (MatchCase _ _ (AbsN' _ body)) = [body]
    doCase _ = error "bad match" []
    doLet (v, Ann' tm _) = doLet (v, tm)
    doLet (v, LamsNamedOpt' _ body) = if isBlank $ Var.nameStr v
                                      then []
                                      else [body]
    doLet t = error (show t) []
