{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.TermPrinter where

import           Control.Monad                  (join)
import           Data.List
import           Data.Foldable                  ( fold
                                                )
import           Data.Map                       ( Map )
import qualified Data.Map                       as Map
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           Data.String                    ( IsString, fromString )
import           Data.Text                      ( Text )
import           Data.Vector                    ( )
import           Text.Read                      ( readMaybe )
import           Unison.ABT                     ( pattern AbsN', annotation, reannotateUp )
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
prettyTop env tm = pretty env (ac (-1) Normal Map.empty) (printAnnotate tm)

pretty
  :: Var v
  => PrettyPrintEnv
  -> AmbientContext
  -> AnnotatedTerm v (a, PrintAnnotation)
  -> Pretty ColorText
pretty n AmbientContext { precedence = p, blockContext = bc, infixContext = ic, imports = im} term
  = specialCases term $ \case
    -- TODO use im when rendering var/ref/constructor/request, see elideFQN
    -- TODO at each block statement, decide what extra imports to introduce, emit `use` statements, and adapt the AmbientContext
    -- TODO check the im argument of all places I construct AmbientContext
    Var' v -> parenIfInfix name ic . prettyHashQualified $ name
      where name = HQ.fromVar v
    Ref' r -> parenIfInfix name ic . prettyHashQualified' $ name
      where name = PrettyPrintEnv.termName n (Referent.Ref r)
    Ann' tm t ->
      paren (p >= 0)
        $  pretty n (ac 10 Normal im) tm
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
        $ ("handle" `PP.hang` pretty n (ac 2 Normal im) h)
        <> PP.softbreak
        <> ("in" `PP.hang` pretty n (ac 2 Block im) body)
    App' x (Constructor' DD.UnitRef 0) ->
      paren (p >= 11) $ l "!" <> pretty n (ac 11 Normal im) x
    AskInfo' x -> paren (p >= 11) $ pretty n (ac 11 Normal im) x <> l "?"
    LamNamed' v x | (Var.name v) == "()" ->
      paren (p >= 11) $ l "'" <> pretty n (ac 11 Normal im) x
    Sequence' xs -> PP.group $
      "[" <> optSpace
          <> intercalateMap ("," <> PP.softbreak <> optSpace <> optSpace)
                            (pretty n (ac 0 Normal im))
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
       pcond  = pretty n (ac 2 Block im) cond
       pt     = pretty n (ac 2 Block im) t
       pf     = pretty n (ac 2 Block im) f
    And' x y ->
      paren (p >= 10) $ PP.spaced [
        "and", pretty n (ac 10 Normal im) x,
               pretty n (ac 10 Normal im) y
      ]
    Or' x y ->
      paren (p >= 10) $ PP.spaced [
        "or", pretty n (ac 10 Normal im) x,
              pretty n (ac 10 Normal im) y
      ]
    LetRecNamed' bs e -> printLet bc bs e
    Lets' bs e -> printLet bc (map (\(_, v, binding) -> (v, binding)) bs) e
    Match' scrutinee branches -> paren (p >= 2) $
      ("case " <> pretty n (ac 2 Normal im) scrutinee <> " of") `PP.hang` bs
      where bs = PP.lines (map printCase branches)
    t -> l "error: " <> l (show t)
 where
  specialCases term go = case (term, binaryOpsPred) of
    (TupleTerm' [x], _) ->
      paren (p >= 10) $ "Pair" `PP.hang`
        PP.spaced [pretty n (ac 10 Normal im) x, "()" ]
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
            PP.group (varList vs <> " ->") `PP.hang` pretty n (ac 2 Block im) body
        _ -> go term

  sepList sep xs = sepList' (pretty n (ac 0 Normal im)) sep xs
  sepList' f sep xs = fold $ intersperse sep (map f xs)
  varList vs = sepList' (PP.text . Var.name) PP.softbreak vs
  commaList = sepList ("," <> PP.softbreak)

  printLet :: Var v => BlockContext -> [(v, AnnotatedTerm v (a, PrintAnnotation))] -> AnnotatedTerm v (a, PrintAnnotation) -> Pretty ColorText
  printLet sc bs e =
    paren ((sc /= Block) && p >= 12)
      $  letIntro
      $  PP.lines (map printBinding bs ++
                   [PP.group $ pretty n (ac 0 Normal im) e])
   where
    printBinding (v, binding) = if isBlank $ Var.nameStr v
      then pretty n (ac (-1) Normal im) binding
      else prettyBinding n (HQ.fromVar v) binding
    letIntro = case sc of
      Block  -> id
      Normal -> \x -> "let" `PP.hang` x
    isBlank ('_' : rest) | (isJust ((readMaybe rest) :: Maybe Int)) = True
    isBlank _ = False

  printCase :: Var v => MatchCase (a, PrintAnnotation) (AnnotatedTerm v (a, PrintAnnotation)) -> Pretty ColorText
  printCase (MatchCase pat guard (AbsN' vs body)) =
    PP.group $ lhs `PP.hang` pretty n (ac 0 Block im) body
    where
    lhs = PP.group (fst (prettyPattern n (-1) vs pat) <> " ")
       <> printGuard guard
       <> "->"
    printGuard (Just g) = PP.group $ PP.spaced ["|", pretty n (ac 2 Normal im) g, ""]
    printGuard Nothing  = mempty
  printCase _ = l "error"

  -- This predicate controls which binary functions we render as infix
  -- operators.  At the moment the policy is just to render symbolic
  -- operators as infix - not 'wordy' function names.  So we produce
  -- "x + y" and "foo x y" but not "x `foo` y".
  binaryOpsPred :: Var v => AnnotatedTerm v (a, PrintAnnotation) -> Bool
  binaryOpsPred = \case
    Ref' r | isSymbolic (PrettyPrintEnv.termName n (Referent.Ref r)) -> True
    Var' v | isSymbolic (HQ.fromVar v) -> True
    _ -> False

  nonForcePred :: AnnotatedTerm v (a, PrintAnnotation) -> Bool
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
    :: Var v => [(AnnotatedTerm v (a, PrintAnnotation), AnnotatedTerm v (a, PrintAnnotation))]
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

pretty' ::
  Var v => Maybe Int -> PrettyPrintEnv -> AnnotatedTerm v a -> ColorText
pretty' (Just width) n t = PP.render width $ pretty n (ac (-1) Normal Map.empty) (printAnnotate t)
pretty' Nothing      n t = PP.renderUnbroken $ pretty n (ac (-1) Normal Map.empty) (printAnnotate t)

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
      pretty env (ac (-1) Block Map.empty) (printAnnotate body)
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

-- When we use imports in rendering (TODO), this will need revisiting, so that we can
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

ac :: Int -> BlockContext -> Imports -> AmbientContext
ac prec bc im = AmbientContext prec bc NonInfix im

-- # FQN elision
--
-- TODO actually implement this
--
-- The term pretty-printer inserts `use` statements in some circumstances, to
-- avoid the need for using fully-qualified names (FQNs) everywhere.  The 
-- following is an explanation and specification, as developed in issue #285.  
--
-- As an example, instead of 
--
--   foo p q r = if p 
--               then Util.bar q
--               else Util.bar r
-- 
-- we actually output the following.
-- 
--   foo p q r = use Util bar
--               if p 
--               then bar q
--               else bar r
--
-- Here, the `use` statement `use Util bar` has been inserted at the start of 
-- the block statement containing the `if`.  Within that scope, `Util.bar` can
-- be referred to just with `bar`.  We say `Util` is the prefix, and `bar` is 
-- the suffix.  
--
-- When choosing where to place `use` statements, the pretty-printer tries to
-- - float them down, deeper into the syntax tree, to keep them visually close
--   to the use sites ('usages') of the names involved, but also tries to
-- - minimize the number of repetitions of `use` statements for the same names
--   by floating them up, towards the top of the syntax tree, so that one 
--   `use` statement takes effect over more name usages.
--
-- It avoids 'shadowing', so for example won't produce output like the 
-- following.
-- 
--   foo p q r = use My bar
--               if p 
--               then bar q
--               else Your.bar r
--   
-- Here `My.bar` is imported with a `use` statement, but `Your.bar` is not - 
-- `Your.bar` is shadowing `My.bar`.  We avoid this because it would be easy
-- to misread `bar` as meaning `Your.bar`.  
--
-- Avoiding shadowing means that a `use` statement is only emitted for a name
-- when the suffix is unique, across all the names referenced in the scope of
-- the `use` statement.
--
-- We don't emit a `use` statement for a name if it only occurs once within
-- the scope (unless it's an infix operator, since they look nicer without
-- a namespace qualifier.)
--
-- The emitted code does not depend on Type-Driven Name Resolution (TDNR).
-- For example, we emit
--   foo = use Nat (+)
--         1 + 2
-- even though TDNR means that `foo = 1 + 2` would have had the same
-- meaning.  That avoids the reader having to run typechecker logic in their
-- head in order to know what functions are being called.  
--
-- Multi-level name qualification is allowed - like `Foo.Bar.baz`.  The
-- pretty-printer tries to strip off as many sections of the prefix as 
-- possible, without causing a clash with other names.  If more sections
-- can be stripped off, further down the tree, then it does this too, taking
-- care to avoid emitting any `use` statements that never actually affect 
-- anything.  
--
-- ## Specification
--
-- We output a `use` statement for prefix P and suffix S at a given scope if
--   - the scope is a block statement (so the `use` is syntactically valid)
--   - the number of usages of the thing referred to by P.S within the scope
--     - is > 1, or
--     - is 1, and S is an infix operator
--   - [uniqueness] there is no other Q with Q.S used in that scope
--   - there is no longer prefix PP (and suffix s, with PP.s == P.S) which
--     satisfies uniqueness
--   - [narrowness] there is no block statement further down inside this one
--     which contains all of the usages, and
--   - [usefulness] the name will actually be used in this scope, in the form
--     enabled by this `use` statement.
--
-- The last clause can fail, for example, if we `use A X.c` (avoiding a clash
-- with `Y.c`, but there is a deeper block statement that contains all the
-- usages of `X.c` and none of `Y.c`, at which we insert a `use A.X c`. In 
-- this case, we want to avoid inserting the superfluous `use A X.c`, and just
-- make it `use A.X c` further down - hence the last clause in the spec.
--
-- Use statements in a block statement are sorted alphabetically by prefix.
-- Suffixes covered by a single use statement are sorted alphabetically.
-- Rather than letting a single use statement overflow a printed line, we
-- instead (TODO).
--
-- ## Algorithm
--
-- Bubbling up from the leaves of the syntax tree, we calculate for each
-- node, a `Map Suffix (Map Prefix Int)` (the 'usages map'), where the `Int`
-- is the number of usages of Prefix.Suffix at/under that node.  (Note that 
-- a usage of `A.B.c` corresponds to two entries in the outer map.)  See
-- `printAnnotate`.
-- 
-- Once we have this decoration on all the terms, we start pretty-printing.
-- As we recurse back down through the tree, we keep a `Map Name Suffix` (the
-- 'imports map'), to record the effect of all the `use` statements we've added 
-- in the nodes above.  When outputting names, we check this map to work out 
-- how to render them, using any suffix we find, or else falling back to the 
-- FQN.  At each block statement, each suffix in that term's usages map is a
-- candidate to be imported with a use statement, subject to the various 
-- rules in the specification.
--
-- # Semantics of imports
--
-- Here is some background on how imports work.  TODO is this all true today?
--
-- `use XYZ blah` brings `XYZ.blah` into scope, bound to the name `blah`. More 
-- generally, `use` is followed by a FQN prefix, then the local suffix. 
-- Concatenate the FQN prefix with the local suffix, with a dot between them,
-- and you get the FQN, which is bound to the name equal to the local suffix.
--
-- `use XYZ blah qux` is equivalent to the two statements (and this 
-- generalizes for any N symbols):
--   use XYZ blah
--   use XYZ qux
--
-- This syntax works the same even if XYZ or blah have dots in them, so:
-- `use Util.External My.Foo` brings `Util.External.My.Foo` into scope, bound 
-- to the name `My.Foo`.
--
-- That's it. No wildcard imports, imports that do renaming, etc. We can 
-- consider adding some features like this later.

-- Note that a Suffix can include dots.
type Suffix = Text
-- Each member of a Prefix list is dot-free.
type Prefix = [Text]
-- Keys are FQNs, values are shorter names which are equivalent, thanks to use
-- statements that are in scope.  
type Imports = Map Name Suffix

data PrintAnnotation = PrintAnnotation
  {
    -- For each suffix that appears in/under this term, the set of prefixes 
    -- used with that suffix, and how many times each occurs.  
    usages :: Map Suffix (Map Prefix Int)
  }

instance Semigroup PrintAnnotation where
  (PrintAnnotation { usages = _ } ) <> (PrintAnnotation { usages = _ } ) = error "todo"

instance Monoid PrintAnnotation where
  mempty = PrintAnnotation { usages = Map.empty }

suffixCounter :: AnnotatedTerm v a -> PrintAnnotation
suffixCounter = \case
  -- TODO is it right to do this for Var?
  Var' _ -> error "todo" -- v
  Ref' _ -> error "todo" -- r
  Constructor' _ _ -> error "todo"  -- ref i
  Request' _ _ -> error "todo" -- ref i
  tm -> mempty
  
printAnnotate :: AnnotatedTerm v a -> AnnotatedTerm v (a, PrintAnnotation)
printAnnotate = reannotateUp suffixCounter

splitName :: Name -> (Prefix, Suffix)
splitName = error "todo"

-- Give the shortened version of an FQN, if there's been a `use` statement for that FQN.
elideFQN :: Imports -> HQ.HashQualified -> HQ.HashQualified
elideFQN = error "todo"
