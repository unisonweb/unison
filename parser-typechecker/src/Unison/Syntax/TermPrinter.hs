module Unison.Syntax.TermPrinter
  ( emptyAc,
    pretty,
    prettyBlock,
    prettyBlock',
    pretty',
    prettyBinding,
    prettyBinding',
    prettyBindingWithoutTypeSignature,
    pretty0,
    runPretty,
  )
where

import Control.Lens (unsnoc, (^.))
import Control.Monad.State (evalState)
import qualified Control.Monad.State as State
import Data.Char (isPrint)
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
import Unison.ConstructorReference (GConstructorReference (..))
import qualified Unison.ConstructorReference as ConstructorReference
import qualified Unison.ConstructorType as CT
import qualified Unison.HashQualified as HQ
import Unison.Name (Name)
import qualified Unison.Name as Name
import qualified Unison.NameSegment as NameSegment
import Unison.Pattern (Pattern)
import qualified Unison.Pattern as Pattern
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import qualified Unison.PrettyPrintEnv as PrettyPrintEnv
import Unison.PrettyPrintEnv.FQN (Imports, Prefix, Suffix, elideFQN)
import Unison.PrettyPrintEnv.MonadPretty
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import qualified Unison.Syntax.HashQualified as HQ (unsafeFromVar)
import Unison.Syntax.Lexer (showEscapeChar, symbolyId)
import qualified Unison.Syntax.Name as Name (toString, toText, unsafeFromText)
import Unison.Syntax.NamePrinter (styleHashQualified'')
import qualified Unison.Syntax.TypePrinter as TypePrinter
import Unison.Term
import Unison.Type (Type, pattern ForallsNamed')
import qualified Unison.Type as Type
import qualified Unison.Util.Bytes as Bytes
import Unison.Util.Monoid (foldMapM, intercalateMap, intercalateMapM)
import Unison.Util.Pretty (ColorText, Pretty, Width)
import qualified Unison.Util.Pretty as PP
import qualified Unison.Util.SyntaxText as S
import Unison.Var (Var)
import qualified Unison.Var as Var

type SyntaxText = S.SyntaxText' Reference

pretty :: (Var v) => PrettyPrintEnv -> Term v a -> Pretty ColorText
pretty ppe tm =
  PP.syntaxToColor . runPretty ppe $ pretty0 emptyAc $ printAnnotate ppe tm

prettyBlock :: (Var v) => Bool -> PrettyPrintEnv -> Term v a -> Pretty ColorText
prettyBlock elideUnit ppe = PP.syntaxToColor . prettyBlock' elideUnit ppe

prettyBlock' :: (Var v) => Bool -> PrettyPrintEnv -> Term v a -> Pretty SyntaxText
prettyBlock' elideUnit ppe tm =
  runPretty ppe . pretty0 (emptyBlockAc {elideUnit = elideUnit}) $ printAnnotate ppe tm

pretty' :: (Var v) => Maybe Width -> PrettyPrintEnv -> Term v a -> ColorText
pretty' (Just width) n t =
  PP.render width . PP.syntaxToColor . runPretty n $ pretty0 emptyAc (printAnnotate n t)
pretty' Nothing n t =
  PP.renderUnbroken . PP.syntaxToColor . runPretty n $ pretty0 emptyAc (printAnnotate n t)

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
    docContext :: DocLiteralContext,
    elideUnit :: Bool -- `True` if a `()` at the end of a block should be elided
  }

-- Description of the position of this ABT node, when viewed in the
-- surface syntax.
data BlockContext
  = -- This ABT node is at the top level of a TermParser.block.
    Block
  | Normal
  deriving (Eq, Show)

data InfixContext
  = -- This ABT node is an infix operator being used in infix position.
    Infix
  | NonInfix
  deriving (Eq, Show)

data DocLiteralContext
  = -- We won't try and render this ABT node or anything under it as a [: @Doc literal :]
    NoDoc
  | -- We'll keep checking as we recurse down
    MaybeDoc
  deriving (Eq, Show)

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
       termLink t
       typeLink t

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
  forall v m.
  (MonadPretty v m) =>
  AmbientContext ->
  Term3 v PrintAnnotation ->
  m (Pretty SyntaxText)
pretty0
  a@AmbientContext
    { precedence = p,
      blockContext = bc,
      infixContext = ic,
      imports = im,
      docContext = doc,
      elideUnit = elideUnit
    }
  term =
    specialCases term $ \case
      Var' v -> pure . parenIfInfix name ic $ styleHashQualified'' (fmt S.Var) name
        where
          -- OK since all term vars are user specified, any freshening was just added during typechecking
          name = elideFQN im $ HQ.unsafeFromVar (Var.reset v)
      Ref' r -> do
        n <- getPPE
        let name = elideFQN im $ PrettyPrintEnv.termName n (Referent.Ref r)
        pure . parenIfInfix name ic $ styleHashQualified'' (fmt $ S.TermReference (Referent.Ref r)) name
      TermLink' r -> do
        n <- getPPE
        let name = elideFQN im $ PrettyPrintEnv.termName n r
        pure . paren (p >= 10) $
          fmt S.LinkKeyword "termLink "
            <> parenIfInfix name ic (styleHashQualified'' (fmt $ S.TermReference r) name)
        where

      TypeLink' r -> do
        n <- getPPE
        let name = elideFQN im $ PrettyPrintEnv.typeName n r
        pure . paren (p >= 10) $
          fmt S.LinkKeyword "typeLink "
            <> parenIfInfix name ic (styleHashQualified'' (fmt $ S.TypeReference r) name)
        where

      Ann' tm t -> do
        tm' <- pretty0 (ac 10 Normal im doc) tm
        tp' <- TypePrinter.pretty0 im 0 t
        pure . paren (p >= 0) $ tm' <> PP.hang (fmt S.TypeAscriptionColon " :") tp'
      Int' i -> pure . fmt S.NumericLiteral $ (if i >= 0 then l "+" else mempty) <> l (show i)
      Nat' u -> pure . fmt S.NumericLiteral . l $ show u
      Float' f -> pure . fmt S.NumericLiteral . l $ show f
      -- TODO How to handle Infinity, -Infinity and NaN?  Parser cannot parse
      --      them.  Haskell doesn't have literals for them either.  Is this
      --      function only required to operate on terms produced by the parser?
      --      In which case the code is fine as it stands.  If it can somehow run
      --      on values produced by execution (or, one day, on terms produced by
      --      metaprograms), then it needs to be able to print them (and then the
      --      parser ought to be able to parse them, to maintain symmetry.)
      Boolean' b -> pure . fmt S.BooleanLiteral $ if b then l "true" else l "false"
      Text' s
        | Just quotes <- useRaw s ->
            pure . fmt S.TextLiteral $ PP.text quotes <> "\n" <> PP.text s <> PP.text quotes
        where
          -- we only use this syntax if we're not wrapped in something else,
          -- to avoid possible round trip issues if the text ends at an odd column
          useRaw _ | p > 0 = Nothing
          useRaw s | Text.find (== '\n') s == Just '\n' && Text.all ok s = n 3
          useRaw _ = Nothing
          ok ch = isPrint ch || ch == '\n' || ch == '\r'
          -- Picks smallest number of surrounding """ to be unique
          n 10 = Nothing -- bail at 10, avoiding quadratic behavior in weird cases
          n cur =
            if null (Text.breakOnAll quotes s)
              then Just quotes
              else n (cur + 1)
            where
              quotes = Text.pack (replicate cur '"')
      Text' s -> pure . fmt S.TextLiteral $ l $ U.ushow s
      Char' c -> pure
        . fmt S.CharLiteral
        . l
        $ case showEscapeChar c of
          Just c -> "?\\" ++ [c]
          Nothing -> '?' : [c]
      Blank' id -> pure $ fmt S.Blank $ l "_" <> l (fromMaybe "" (Blank.nameb id))
      Constructor' ref -> do
        n <- getPPE
        let name = elideFQN im $ PrettyPrintEnv.termName n conRef
            conRef = Referent.Con ref CT.Data
        pure $ styleHashQualified'' (fmt $ S.TermReference conRef) name
      Request' ref -> do
        n <- getPPE
        let name = elideFQN im $ PrettyPrintEnv.termName n conRef
            conRef = Referent.Con ref CT.Effect
        pure $ styleHashQualified'' (fmt $ S.TermReference conRef) name
      Handle' h body -> do
        pb <- pblock body
        ph <- pblock h
        pure . paren (p >= 2) $
          if PP.isMultiLine pb || PP.isMultiLine ph
            then
              PP.lines
                [ fmt S.ControlKeyword "handle" `PP.hang` pb,
                  fmt S.ControlKeyword "with" `PP.hang` ph
                ]
            else
              PP.spaced
                [ fmt S.ControlKeyword "handle" `PP.hang` pb
                    <> PP.softbreak
                    <> fmt S.ControlKeyword "with" `PP.hang` ph
                ]
        where
          pblock tm =
            let (im', uses) = calcImports im tm
             in uses <$> sequence [pretty0 (ac 0 Block im' doc) tm]
      App' x (Constructor' (ConstructorReference DD.UnitRef 0)) -> do
        px <- pretty0 (ac (if isBlock x then 0 else 10) Normal im doc) x
        pure . paren (p >= 11 || isBlock x && p >= 3) $
          fmt S.DelayForceChar (l "!") <> px
      Delay' x
        | Lets' _ _ <- x -> do
            px <- pretty0 (ac 0 Block im doc) x
            pure . paren (p >= 3) $
              fmt S.ControlKeyword "do" `PP.hang` px
        | otherwise -> do
            px <- pretty0 (ac 10 Normal im doc) x
            pure . paren (p >= 11 || isBlock x && p >= 3) $
              fmt S.DelayForceChar (l "'")
                -- Add indentation below since we're opening parens with '(
                -- This is in case the contents are a long function application
                -- in which case the arguments should be indented.
                <> PP.indentAfterNewline "  " px
      List' xs ->
        PP.group <$> do
          xs' <- traverse (pretty0 (ac 0 Normal im doc)) xs
          pure $
            fmt S.DelimiterChar (l "[")
              <> optSpace
              <> intercalateMap
                (fmt S.DelimiterChar (l ",") <> PP.softbreak <> optSpace <> optSpace)
                (PP.indentNAfterNewline 2)
                xs'
              <> optSpace
              <> fmt S.DelimiterChar (l "]")
        where
          optSpace = PP.orElse "" " "
      If' cond t f ->
        do
          pcond <- pretty0 (ac 2 Block im doc) cond
          pt <- branch t
          pf <- branch f
          pure . paren (p >= 2) $
            if PP.isMultiLine pcond
              then
                PP.lines
                  [ fmt S.ControlKeyword "if" `PP.hang` pcond,
                    fmt S.ControlKeyword "then" `PP.hang` pt,
                    fmt S.ControlKeyword "else" `PP.hang` pf
                  ]
              else
                if PP.isMultiLine pt || PP.isMultiLine pf
                  then
                    PP.lines
                      [ fmt S.ControlKeyword "if " <> pcond <> fmt S.ControlKeyword " then" `PP.hang` pt,
                        fmt S.ControlKeyword "else" `PP.hang` pf
                      ]
                  else
                    PP.spaced
                      [ (fmt S.ControlKeyword "if" `PP.hang` pcond) <> (fmt S.ControlKeyword " then" `PP.hang` pt),
                        fmt S.ControlKeyword "else" `PP.hang` pf
                      ]
        where
          branch tm =
            let (im', uses) = calcImports im tm
             in uses <$> sequence [pretty0 (ac 0 Block im' doc) tm]
      LetBlock bs e ->
        let (im', uses) = calcImports im term
         in printLet elideUnit bc bs e im' uses
      -- Some matches are rendered as a destructuring bind, like
      --   match foo with (a,b) -> blah
      -- becomes
      --   (a,b) = foo
      --   blah
      -- See `isDestructuringBind` definition.
      Match' scrutinee cs@[MatchCase pat guard (AbsN' vs body)]
        | p < 1 && isDestructuringBind scrutinee cs -> do
            n <- getPPE
            let letIntro = case bc of
                  Block -> id
                  Normal -> \x ->
                    -- We don't call calcImports here, because we can't easily do the
                    -- corequisite step in immediateChildBlockTerms (because it doesn't
                    -- know bc.)  So we'll fail to take advantage of any opportunity
                    -- this let block provides to add a use statement.  Not so bad.
                    fmt S.ControlKeyword "let" `PP.hang` x
            lhs <- do
              let (lhs, _) = prettyPattern n (ac 0 Block im doc) 10 vs pat
              guard' <- printGuard guard
              pure $ PP.group lhs `PP.hang` guard'
            let eq = fmt S.BindingEquals "="
            rhs <- do
              let (im', uses) = calcImports im scrutinee
              uses <$> sequence [pretty0 (ac (-1) Block im' doc) scrutinee]
            letIntro <$> do
              prettyBody <- pretty0 (ac (-1) Block im doc) body
              pure $
                PP.lines
                  [ (lhs <> eq) `PP.hang` rhs,
                    prettyBody
                  ]
        where
          printGuard Nothing = pure mempty
          printGuard (Just g') = do
            let (_, g) = ABT.unabs g'
            prettyg <- pretty0 (ac 2 Normal im doc) g
            pure $ fmt S.DelimiterChar "| " <> prettyg
      Match' scrutinee branches ->
        do
          ps <- pretty0 (ac 2 Normal im doc) scrutinee
          pbs <- printCase im doc (arity1Branches branches) -- don't print with `cases` syntax
          pure . paren (p >= 2) $
            if PP.isMultiLine ps
              then
                PP.lines
                  [ fmt S.ControlKeyword "match " `PP.hang` ps,
                    fmt S.ControlKeyword " with" `PP.hang` pbs
                  ]
              else (fmt S.ControlKeyword "match " <> ps <> fmt S.ControlKeyword " with") `PP.hang` pbs
      t -> pure $ l "error: " <> l (show t)
    where
      goNormal prec tm = pretty0 (ac prec Normal im doc) tm
      specialCases term go = do
        doc <- prettyDoc2 a term
        case doc of
          Just d -> pure d
          Nothing -> notDoc go
        where
          notDoc go = do
            n <- getPPE
            let -- This predicate controls which binary functions we render as infix
                -- operators. At the moment the policy is just to render symbolic
                -- operators as infix.
                binaryOpsPred :: Term3 v PrintAnnotation -> Bool
                binaryOpsPred = \case
                  Ref' r -> isSymbolic $ PrettyPrintEnv.termName n (Referent.Ref r)
                  Var' v -> isSymbolic $ HQ.unsafeFromVar v
                  _ -> False
            case (term, binaryOpsPred) of
              (DD.Doc, _)
                | doc == MaybeDoc ->
                    if isDocLiteral term
                      then applyPPE3 prettyDoc im term
                      else pretty0 (a {docContext = NoDoc}) term
              (TupleTerm' [x], _) -> do
                let conRef = DD.pairCtorRef
                name <- elideFQN im <$> applyPPE2 PrettyPrintEnv.termName conRef
                let pair = parenIfInfix name ic $ styleHashQualified'' (fmt (S.TermReference conRef)) name
                x' <- pretty0 (ac 10 Normal im doc) x
                pure . paren (p >= 10) $
                  pair
                    `PP.hang` PP.spaced [x', fmt (S.TermReference DD.unitCtorRef) "()"]
              (TupleTerm' xs, _) -> do
                clist <- commaList xs
                let tupleLink p = fmt (S.TypeReference DD.unitRef) p
                pure $ PP.group (tupleLink "(" <> clist <> tupleLink ")")
              (App' f@(Builtin' "Any.Any") arg, _) ->
                paren (p >= 10) <$> (PP.hang <$> goNormal 9 f <*> goNormal 10 arg)
              (Apps' f@(Constructor' _) args, _) ->
                paren (p >= 10) <$> (PP.hang <$> goNormal 9 f <*> PP.spacedTraverse (goNormal 10) args)
              {-
              When a delayed computation block is passed to a function as the last argument
              in a context where the ambient precedence is low enough, we can elide parentheses
              around it and use a "soft hang" to put the `'let` on the same line as the function call.
              This looks nice.

                forkAt usEast 'let
                  x = thing1
                  y = thing2
                  ...

              instead of the ugly but effective

                forkAt
                  usEast
                  ('let
                    x = thing1
                    y = thing2
                    ...)
              -}
              (Apps' f (unsnoc -> Just (args, lastArg@(Delay' (Lets' _ _)))), _) -> do
                fun <- goNormal 9 f
                args' <- traverse (goNormal 10) args
                lastArg' <- goNormal 0 lastArg
                pure . paren (p >= 3) $ PP.softHang fun (PP.spaced (args' <> [lastArg']))
              (Bytes' bs, _) ->
                pure $ fmt S.BytesLiteral "0xs" <> PP.shown (Bytes.fromWord8s (map fromIntegral bs))
              BinaryAppsPred' apps lastArg -> do
                prettyLast <- pretty0 (ac 3 Normal im doc) lastArg
                prettyApps <- binaryApps apps prettyLast
                pure $ paren (p >= 3) prettyApps
              -- Note that && and || are at the same precedence, which can cause
              -- confusion, so for clarity we do not want to elide the parentheses in a
              -- case like `(x || y) && z`.
              (Ands' xs lastArg, _) ->
                -- Old code, without monadic booleanOps:
                -- paren (p >= 10)
                --   . booleanOps (fmt S.ControlKeyword "&&") xs
                --   <$> pretty0 (ac 10 Normal im doc) lastArg
                -- New code, where booleanOps is monadic like pretty0:
                paren (p >= 10) <$> do
                  lastArg' <- pretty0 (ac 10 Normal im doc) lastArg
                  booleanOps (fmt S.ControlKeyword "&&") xs lastArg'
              (Ors' xs lastArg, _) ->
                -- Old code:
                -- paren (p >= 10)
                --   . booleanOps (fmt S.ControlKeyword "||") xs
                --   <$> pretty0 (ac 10 Normal im doc) lastArg
                -- New code:
                paren (p >= 10) <$> do
                  lastArg' <- pretty0 (ac 10 Normal im doc) lastArg
                  booleanOps (fmt S.ControlKeyword "||") xs lastArg'
              _ -> case (term, nonForcePred) of
                OverappliedBinaryAppPred' f a b r
                  | binaryOpsPred f ->
                      -- Special case for overapplied binary op
                      do
                        prettyB <- pretty0 (ac 3 Normal im doc) b
                        prettyR <- PP.spacedTraverse (pretty0 (ac 10 Normal im doc)) r
                        prettyA <- binaryApps [(f, a)] prettyB
                        pure $ paren True $ PP.hang prettyA prettyR
                AppsPred' f args ->
                  paren (p >= 10) <$> do
                    f' <- pretty0 (ac 10 Normal im doc) f
                    args' <- PP.spacedTraverse (pretty0 (ac 10 Normal im doc)) args
                    pure $ f' `PP.hang` args'
                _ -> case (term, \v -> nonUnitArgPred v && not (isDelay term)) of
                  (LamsNamedMatch' [] branches, _) -> do
                    pbs <- printCase im doc branches
                    pure . paren (p >= 3) $
                      PP.group (fmt S.ControlKeyword "cases") `PP.hang` pbs
                  LamsNamedPred' vs body -> do
                    prettyBody <- pretty0 (ac 2 Block im doc) body
                    pure . paren (p >= 3) $
                      PP.group (varList vs <> fmt S.ControlKeyword " ->") `PP.hang` prettyBody
                  _ -> go term

      isDelay (Delay' _) = True
      isDelay _ = False
      sepList = sepList' (pretty0 (ac 0 Normal im doc))
      sepList' f sep xs = fold . intersperse sep <$> traverse f xs
      varList = runIdentity . sepList' (Identity . PP.text . Var.name) PP.softbreak
      commaList = sepList (fmt S.DelimiterChar (l ",") <> PP.softbreak)

      printLet ::
        Bool -> -- elideUnit
        BlockContext ->
        [(v, Term3 v PrintAnnotation)] ->
        Term3 v PrintAnnotation ->
        Imports ->
        ([Pretty SyntaxText] -> Pretty SyntaxText) ->
        m (Pretty SyntaxText)
      printLet elideUnit sc bs e im uses =
        paren (sc /= Block && p >= 12)
          . letIntro
          . uses
          <$> ((++) <$> traverse printBinding bs <*> body e)
        where
          body (Constructor' (ConstructorReference DD.UnitRef 0)) | elideUnit = pure []
          body e = (: []) <$> pretty0 (ac 0 Normal im doc) e
          printBinding (v, binding) =
            if Var.isAction v
              then pretty0 (ac (-1) Normal im doc) binding
              else renderPrettyBinding <$> prettyBinding0 (ac (-1) Normal im doc) (HQ.unsafeFromVar v) binding
          letIntro = case sc of
            Block -> id
            Normal -> \x -> fmt S.ControlKeyword "let" `PP.hang` x

      nonForcePred :: Term3 v PrintAnnotation -> Bool
      nonForcePred = \case
        Constructor' (ConstructorReference DD.UnitRef 0) -> False
        Constructor' (ConstructorReference DD.DocRef _) -> False
        _ -> True

      nonUnitArgPred :: (Var v) => v -> Bool
      nonUnitArgPred v = Var.name v /= "()"

      -- Render a binary infix operator sequence, like [(a2, f2), (a1, f1)],
      -- meaning (a1 `f1` a2) `f2` (a3 rendered by the caller), producing
      -- "a1 `f1` a2 `f2`".  Except the operators are all symbolic, so we won't
      -- produce any backticks.  We build the result out from the right,
      -- starting at `f2`.
      binaryApps ::
        [(Term3 v PrintAnnotation, Term3 v PrintAnnotation)] ->
        Pretty SyntaxText ->
        m (Pretty SyntaxText)
      binaryApps xs last =
        do
          ps <- join <$> traverse (uncurry r) (reverse xs)
          let unbroken = PP.spaced (ps <> [last])
              broken = PP.hang (head ps) . PP.column2 . psCols $ tail ps <> [last]
          pure (unbroken `PP.orElse` broken)
        where
          psCols ps = case take 2 ps of
            [x, y] -> (x, y) : psCols (drop 2 ps)
            [x] -> [(x, "")]
            [] -> []
            _ -> undefined
          r a f =
            sequenceA
              [ pretty0 (ac (if isBlock a then 12 else 3) Normal im doc) a,
                pretty0 (AmbientContext 10 Normal Infix im doc False) f
              ]

      -- Render sequence of infix &&s or ||s, like [x2, x1],
      -- meaning (x1 && x2) && (x3 rendered by the caller), producing
      -- "x1 && x2 &&". The result is built from the right.
      booleanOps ::
        Pretty SyntaxText ->
        [Term3 v PrintAnnotation] ->
        Pretty SyntaxText ->
        m (Pretty SyntaxText)
      booleanOps op xs last = do
        ps <- join <$> traverse r (reverse xs)
        let unbroken = PP.spaced (ps <> [last])
            broken = PP.hang (head ps) . PP.column2 . psCols $ tail ps <> [last]
        pure (unbroken `PP.orElse` broken)
        where
          psCols ps = case take 2 ps of
            [x, y] -> (x, y) : psCols (drop 2 ps)
            [x] -> [(x, "")]
            [] -> []
            _ -> undefined
          r a =
            sequence
              [ pretty0 (ac (if isBlock a then 12 else 10) Normal im doc) a,
                pure op
              ]

prettyPattern ::
  forall v loc.
  (Var v) =>
  PrettyPrintEnv ->
  AmbientContext ->
  Int ->
  [v] ->
  Pattern loc ->
  (Pretty SyntaxText, [v])
-- vs is the list of pattern variables used by the pattern, plus possibly a
-- tail of variables it doesn't use.  This tail is the second component of
-- the return value.
prettyPattern n c@AmbientContext {imports = im} p vs patt = case patt of
  Pattern.Char _ c ->
    ( fmt S.CharLiteral $
        l $ case showEscapeChar c of
          Just c -> "?\\" ++ [c]
          Nothing -> '?' : [c],
      vs
    )
  Pattern.Unbound _ -> (fmt S.DelimiterChar $ l "_", vs)
  Pattern.Var _ ->
    case vs of
      (v : tail_vs) -> (fmt S.Var $ l $ Var.nameStr v, tail_vs)
      _ -> error "prettyPattern: Expected at least one var"
  Pattern.Boolean _ b -> (fmt S.BooleanLiteral $ if b then l "true" else l "false", vs)
  Pattern.Int _ i -> (fmt S.NumericLiteral $ (if i >= 0 then l "+" else mempty) <> l (show i), vs)
  Pattern.Nat _ u -> (fmt S.NumericLiteral $ l $ show u, vs)
  Pattern.Float _ f -> (fmt S.NumericLiteral $ l $ show f, vs)
  Pattern.Text _ t -> (fmt S.TextLiteral $ l $ show t, vs)
  TuplePattern pats
    | length pats /= 1 ->
        let (pats_printed, tail_vs) = patterns (-1) vs pats
         in (PP.parenthesizeCommas pats_printed, tail_vs)
  Pattern.Constructor _ ref [] ->
    (styleHashQualified'' (fmt $ S.TermReference conRef) name, vs)
    where
      name = elideFQN im $ PrettyPrintEnv.termName n conRef
      conRef = Referent.Con ref CT.Data
  Pattern.Constructor _ ref pats ->
    let (pats_printed, tail_vs) = patternsSep 10 PP.softbreak vs pats
        name = elideFQN im $ PrettyPrintEnv.termName n conRef
        conRef = Referent.Con ref CT.Data
     in ( paren (p >= 10) $
            styleHashQualified'' (fmt $ S.TermReference conRef) name
              `PP.hang` pats_printed,
          tail_vs
        )
  Pattern.As _ pat ->
    case vs of
      (v : tail_vs) ->
        let (printed, eventual_tail) = prettyPattern n c 11 tail_vs pat
         in (paren (p >= 11) (fmt S.Var (l $ Var.nameStr v) <> fmt S.DelimiterChar (l "@") <> printed), eventual_tail)
      _ -> error "prettyPattern: Expected at least one var"
  Pattern.EffectPure _ pat ->
    let (printed, eventual_tail) = prettyPattern n c (-1) vs pat
     in (PP.sep " " [fmt S.DelimiterChar "{", printed, fmt S.DelimiterChar "}"], eventual_tail)
  Pattern.EffectBind _ ref pats k_pat ->
    let (pats_printed, tail_vs) = patternsSep 10 PP.softbreak vs pats
        (k_pat_printed, eventual_tail) = prettyPattern n c 0 tail_vs k_pat
        name = elideFQN im $ PrettyPrintEnv.termName n conRef
        conRef = Referent.Con ref CT.Effect
     in ( PP.group
            ( fmt S.DelimiterChar "{"
                <> ( PP.sep " " . PP.nonEmpty $
                       [ styleHashQualified'' (fmt (S.TermReference conRef)) name,
                         pats_printed,
                         fmt S.ControlKeyword "->",
                         k_pat_printed
                       ]
                   )
                <> fmt S.DelimiterChar "}"
            ),
          eventual_tail
        )
  Pattern.SequenceLiteral _ pats ->
    let (pats_printed, tail_vs) = patternsSep (-1) (fmt S.DelimiterChar ", ") vs pats
     in (fmt S.DelimiterChar "[" <> pats_printed <> fmt S.DelimiterChar "]", tail_vs)
  Pattern.SequenceOp _ l op r ->
    let (pl, lvs) = prettyPattern n c p vs l
        (pr, rvs) = prettyPattern n c (p + 1) lvs r
        f i s = (paren (p >= i) (pl <> " " <> fmt (S.Op op) s <> " " <> pr), rvs)
     in case op of
          Pattern.Cons -> f 0 "+:"
          Pattern.Snoc -> f 0 ":+"
          Pattern.Concat -> f 0 "++"
  where
    l :: (IsString s) => String -> s
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

-- Groups adjacent cases with the same pattern together,
-- for easier pretty-printing, for instance:
--
--   Foo x y | blah1 x -> body1
--   Foo x y | blah2 y -> body2
--
-- becomes
--
--   Foo x y, [x,y], [(blah1 x, body1), (blah2 y, body2)]
groupCases ::
  (Ord v) =>
  [MatchCase' () (Term3 v ann)] ->
  [([Pattern ()], [v], [(Maybe (Term3 v ann), Term3 v ann)])]
groupCases ms = go0 ms
  where
    go0 [] = []
    go0 ms@((p1, _, AbsN' vs1 _) : _) = go2 (p1, vs1) [] ms
    go2 (p0, vs0) acc [] = [(p0, vs0, reverse acc)]
    go2 (p0, vs0) acc ms@((p1, g1, AbsN' vs body) : tl)
      | p0 == p1 && vs == vs0 = go2 (p0, vs0) ((g1, body) : acc) tl
      | otherwise = (p0, vs0, reverse acc) : go0 ms

printCase ::
  (MonadPretty v m) =>
  Imports ->
  DocLiteralContext ->
  [MatchCase' () (Term3 v PrintAnnotation)] ->
  m (Pretty SyntaxText)
printCase im doc ms0 =
  PP.orElse
    <$> (PP.lines . alignGrid True <$> grid)
    <*> (PP.lines . alignGrid False <$> grid)
  where
    ms = groupCases ms0
    justify rows =
      zip (fmap fst . PP.align' $ fmap alignPatterns rows) $ fmap gbs rows
      where
        alignPatterns (p, _, _) = (p, Just "")
        gbs (_, gs, bs) = zip gs bs
    nojustify = map f
      where
        f (p, gs, bs) = (p, zip gs bs)
    alignGrid alignArrows grid =
      fmap alignCase $ if alignArrows then justify grid else nojustify grid
      where
        alignCase (p, gbs) =
          if not (null (drop 1 gbs))
            then PP.hang p guardBlock
            else p <> guardBlock
          where
            guardBlock =
              PP.lines $
                fmap
                  ( \(g, (a, b)) ->
                      PP.hang
                        ( PP.group
                            (g <> (if alignArrows then "" else " ") <> a)
                        )
                        b
                  )
                  justified
            justified = PP.leftJustify $ fmap (\(g, b) -> (g, (arrow, b))) gbs
    grid = traverse go ms
    patLhs env vs pats =
      case pats of
        [pat] -> PP.group (fst (prettyPattern env (ac 0 Block im doc) (-1) vs pat))
        pats -> PP.group
          . PP.sep (PP.indentAfterNewline "  " $ "," <> PP.softbreak)
          . (`evalState` vs)
          . for pats
          $ \pat -> do
            vs <- State.get
            let (p, rem) = prettyPattern env (ac 0 Block im doc) (-1) vs pat
            State.put rem
            pure p
    arrow = fmt S.ControlKeyword "->"
    goBody im' uses body = uses <$> sequence [pretty0 (ac 0 Block im' doc) body]
    -- If there's multiple guarded cases for this pattern, prints as:
    -- MyPattern x y
    --   | guard 1        -> 1
    --   | otherguard x y -> 2
    --   | otherwise      -> 3
    go (pats, vs, unzip -> (guards, bodies)) = do
      guards' <- traverse printGuard guards
      bodies' <- traverse printBody bodies
      ppe <- getPPE
      pure (patLhs ppe vs pats, guards', bodies')
      where
        noGuards = all (== Nothing) guards
        printGuard Nothing | noGuards = pure mempty
        printGuard Nothing =
          pure $ fmt S.DelimiterChar "|" <> " " <> fmt S.ControlKeyword "otherwise"
        printGuard (Just (ABT.AbsN' _ g)) =
          -- strip off any Abs-chain around the guard, guard variables are rendered
          -- like any other variable, ex: case Foo x y | x < y -> ...
          PP.spaceIfNeeded (fmt S.DelimiterChar "|")
            <$> pretty0 (ac 2 Normal im doc) g
        printBody b = let (im', uses) = calcImports im b in goBody im' uses b

-- A pretty term binding, split into the type signature (possibly empty) and the term.
data PrettyBinding = PrettyBinding
  { typeSignature :: Maybe (Pretty SyntaxText),
    term :: Pretty SyntaxText
  }

-- Render a pretty binding.
renderPrettyBinding :: PrettyBinding -> Pretty SyntaxText
renderPrettyBinding PrettyBinding {typeSignature, term} =
  case typeSignature of
    Nothing -> term
    Just ty -> PP.lines [ty, term]

-- Render a pretty binding without a type signature.
renderPrettyBindingWithoutTypeSignature :: PrettyBinding -> Pretty SyntaxText
renderPrettyBindingWithoutTypeSignature PrettyBinding {term} =
  term

-- | Render a binding, producing output of the form
--
-- foo : t -> u
-- foo a = ...
--
-- The first line is only output if the term has a type annotation as the
-- outermost constructor.
--
-- Binary functions with symbolic names are output infix, as follows:
--
-- (+) : t -> t -> t
-- a + b = ...
prettyBinding ::
  (Var v) =>
  PrettyPrintEnv ->
  HQ.HashQualified Name ->
  Term2 v at ap v a ->
  Pretty SyntaxText
prettyBinding =
  prettyBinding_ renderPrettyBinding

-- | Like 'prettyBinding', but elides the type signature (if any).
prettyBindingWithoutTypeSignature ::
  (Var v) =>
  PrettyPrintEnv ->
  HQ.HashQualified Name ->
  Term2 v at ap v a ->
  Pretty SyntaxText
prettyBindingWithoutTypeSignature =
  prettyBinding_ renderPrettyBindingWithoutTypeSignature

prettyBinding_ ::
  (Var v) =>
  (PrettyBinding -> Pretty SyntaxText) ->
  PrettyPrintEnv ->
  HQ.HashQualified Name ->
  Term2 v at ap v a ->
  Pretty SyntaxText
prettyBinding_ go ppe n = runPretty ppe . fmap go . prettyBinding0 (ac (-1) Block Map.empty MaybeDoc) n

prettyBinding' ::
  (Var v) =>
  PrettyPrintEnv ->
  Width ->
  HQ.HashQualified Name ->
  Term v a ->
  ColorText
prettyBinding' ppe width v t =
  PP.render width . PP.syntaxToColor $ prettyBinding ppe v t

prettyBinding0 ::
  (MonadPretty v m) =>
  AmbientContext ->
  HQ.HashQualified Name ->
  Term2 v at ap v a ->
  m PrettyBinding
prettyBinding0 a@AmbientContext {imports = im, docContext = doc} v term =
  go (symbolic && isBinary term) term
  where
    go infix' binding = do
      env <- getPPE
      case binding of
        Ann' tm tp -> do
          -- If the term is an annotated function,
          -- we want to print the type signature on the previous line.
          -- The TypePrinter.pretty0 function prints the type, and uses a
          -- Reader monad with (Set v) in it to track which type variables are
          -- bound in the outer scope. We use that to determine if the type
          -- printer should avoid capture of those variables.
          let avoidCapture = case tp of
                ForallsNamed' vs _ -> addTypeVars vs
                _ -> id
          tp' <- TypePrinter.pretty0 im (-1) tp
          tm' <- avoidCapture (prettyBinding0 a v tm)
          pure
            PrettyBinding
              { typeSignature = Just (PP.group (renderName v <> PP.hang (fmt S.TypeAscriptionColon " :") tp')),
                term = PP.group (renderPrettyBinding tm')
              }
        (printAnnotate env -> LamsNamedMatch' vs branches) -> do
          branches' <- printCase im doc branches
          pure
            PrettyBinding
              { typeSignature = Nothing,
                term =
                  PP.group $
                    PP.group
                      ( defnLhs v vs
                          <> fmt S.BindingEquals " ="
                          <> " "
                          <> fmt
                            S.ControlKeyword
                            "cases"
                      )
                      `PP.hang` branches'
              }
        LamsNamedOrDelay' vs body -> do
          -- In the case where we're being called from inside `pretty0`, this
          -- call to printAnnotate is unfortunately repeating work we've already
          -- done.
          body' <- applyPPE2 printAnnotate body
          let (im', uses) = calcImports im body'
          prettyBody <- pretty0 (ac (-1) Block im' doc) body'
          -- Special case for 'let being on the same line
          let hang = case body' of
                Delay' (Lets' _ _) -> PP.softHang
                _ -> PP.hang
          pure
            PrettyBinding
              { typeSignature = Nothing,
                term =
                  PP.group $
                    PP.group (defnLhs v vs <> fmt S.BindingEquals " =")
                      `hang` uses [prettyBody]
              }
        t -> error ("prettyBinding0: unexpected term: " ++ show t)
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
prettyDoc :: (Var v) => PrettyPrintEnv -> Imports -> Term3 v a -> Pretty SyntaxText
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
      fmt S.DocDelimiter (l "@") <> (fmt $ S.TermReference r) (fmtTerm r)
    go (DD.DocLink (DD.LinkType (TypeLink' r))) =
      fmt S.DocDelimiter (l "@") <> (fmt $ S.TypeReference r) (fmtType r)
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
      fmt S.DocDelimiter (l "@[")
        <> fmt S.DocKeyword (l w)
        <> fmt S.DocDelimiter (l "] ")
    escaped = Text.replace "@" "\\@" . Text.replace ":]" "\\:]"
    spaceUnlessBroken = PP.orElse " " ""

paren :: Bool -> Pretty SyntaxText -> Pretty SyntaxText
paren True s = PP.group $ fmt S.Parenthesis "(" <> s <> fmt S.Parenthesis ")"
paren False s = PP.group s

parenIfInfix ::
  HQ.HashQualified Name ->
  InfixContext ->
  (Pretty SyntaxText -> Pretty SyntaxText)
parenIfInfix name ic =
  if isSymbolic name && ic == NonInfix then paren True else id

l :: (IsString s) => String -> Pretty s
l = fromString

isSymbolic :: HQ.HashQualified Name -> Bool
isSymbolic (HQ.NameOnly name) = isSymbolic' name
isSymbolic (HQ.HashQualified name _) = isSymbolic' name
isSymbolic (HQ.HashOnly _) = False

isSymbolic' :: Name -> Bool
isSymbolic' name = case symbolyId . Name.toString $ name of
  Right _ -> True
  _ -> False

emptyAc :: AmbientContext
emptyAc = ac (-1) Normal Map.empty MaybeDoc

emptyBlockAc :: AmbientContext
emptyBlockAc = ac (-1) Block Map.empty MaybeDoc

ac :: Int -> BlockContext -> Imports -> DocLiteralContext -> AmbientContext
ac prec bc im doc = AmbientContext prec bc NonInfix im doc False

fmt :: S.Element r -> Pretty (S.SyntaxText' r) -> Pretty (S.SyntaxText' r)
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

newtype PrintAnnotation = PrintAnnotation
  { -- For each suffix that appears in/under this term, the set of prefixes
    -- used with that suffix, and how many times each occurs.
    usages :: Map Suffix (Map Prefix Int)
  }
  deriving (Show)

instance Semigroup PrintAnnotation where
  PrintAnnotation {usages = a} <> PrintAnnotation {usages = b} =
    PrintAnnotation {usages = Map.unionWith f a b}
    where
      f a' b' = Map.unionWith (+) a' b'

instance Monoid PrintAnnotation where
  mempty = PrintAnnotation {usages = Map.empty}

suffixCounterTerm :: (Var v) => PrettyPrintEnv -> Term2 v at ap v a -> PrintAnnotation
suffixCounterTerm n = \case
  Var' v -> countHQ $ HQ.unsafeFromVar v
  Ref' r -> countHQ $ PrettyPrintEnv.termName n (Referent.Ref r)
  Constructor' r | noImportRefs (r ^. ConstructorReference.reference_) -> mempty
  Constructor' r -> countHQ $ PrettyPrintEnv.termName n (Referent.Con r CT.Data)
  Request' r -> countHQ $ PrettyPrintEnv.termName n (Referent.Con r CT.Effect)
  Ann' _ t -> countTypeUsages n t
  Match' _ bs ->
    let pat (MatchCase p _ _) = p
     in foldMap (countPatternUsages n . pat) bs
  _ -> mempty

suffixCounterType :: (Var v) => PrettyPrintEnv -> Type v a -> PrintAnnotation
suffixCounterType n = \case
  Type.Var' v -> countHQ $ HQ.unsafeFromVar v
  Type.Ref' r | noImportRefs r || r == Type.listRef -> mempty
  Type.Ref' r -> countHQ $ PrettyPrintEnv.typeName n r
  _ -> mempty

printAnnotate :: (Var v, Ord v) => PrettyPrintEnv -> Term2 v at ap v a -> Term3 v PrintAnnotation
printAnnotate n tm = fmap snd (go (reannotateUp (suffixCounterTerm n) tm))
  where
    go :: (Ord v) => Term2 v at ap v b -> Term2 v () () v b
    go = extraMap' id (const ()) (const ())

countTypeUsages :: (Var v, Ord v) => PrettyPrintEnv -> Type v a -> PrintAnnotation
countTypeUsages n t = snd $ annotation $ reannotateUp (suffixCounterType n) t

countPatternUsages :: PrettyPrintEnv -> Pattern loc -> PrintAnnotation
countPatternUsages n = Pattern.foldMap' f
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
      Pattern.SequenceOp {} -> mempty
      Pattern.EffectPure _ _ -> mempty
      Pattern.EffectBind _ r _ _ -> countHQ $ PrettyPrintEnv.patternName n r
      Pattern.Constructor _ r _ ->
        if noImportRefs (r ^. ConstructorReference.reference_)
          then mempty
          else countHQ $ PrettyPrintEnv.patternName n r

countHQ :: HQ.HashQualified Name -> PrintAnnotation
countHQ hq = foldMap countName (HQ.toName hq)

countName :: Name -> PrintAnnotation
countName n =
  PrintAnnotation
    { usages =
        Map.fromList do
          (p, s) <- Name.splits n
          pure (Name.toText s, Map.singleton (map NameSegment.toText p) 1)
    }

joinName :: Prefix -> Suffix -> Name
joinName p s = Name.unsafeFromText $ dotConcat $ p ++ [s]

dotConcat :: [Text] -> Text
dotConcat = Text.concat . intersperse "."

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
  r
    `elem` [ DD.pairRef,
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
      m
        |> Map.filter (\ps -> Map.size ps == 1)
        |> Map.map (head . Map.toList)
    -- Keep only names where the number of usages in this scope
    --   - is > 1, or
    --   - is 1, and S is an infix operator.
    -- Also drop names with an empty prefix.
    lookupOrDie s m = fromMaybe msg (Map.lookup s m)
      where
        msg = error $ "TermPrinter.enoughUsages " <> show (s, m)

    enoughUsages :: Map Suffix (Prefix, Int) -> Map Suffix (Prefix, Int)
    enoughUsages m =
      Map.keys m
        |> filter
          ( \s ->
              let (p, i) = lookupOrDie s m
               in (i > 1 || isRight (symbolyId (unpack s)))
                    && not (null p)
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
        Just s -> Text.length s' < Text.length s
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
      Map.elems m
        |> map (\(p, s, _) -> (p, Set.singleton s))
        |> Map.fromListWith Set.union
    render :: Map Prefix (Set Suffix) -> [Pretty SyntaxText] -> Pretty SyntaxText
    render m rest =
      let uses =
            Map.mapWithKey
              ( \p ss ->
                  fmt S.UseKeyword (l "use ")
                    <> fmt S.UsePrefix (intercalateMap (l ".") (l . unpack) p)
                    <> l " "
                    <> fmt S.UseSuffix (intercalateMap (l " ") (l . unpack) (Set.toList ss))
              )
              m
              |> Map.toList
              |> map snd
       in PP.lines (uses ++ rest)

-- Given a block term and a name (Prefix, Suffix) of interest, is there a
-- strictly smaller blockterm within it, containing all usages of that name?
-- A blockterm is a place where the syntax lets us put a use statement, like the
-- branches of an if/then/else.
-- We traverse the block terms by traversing the whole subtree with ABT.find,
-- and paying attention to those subterms that look like a blockterm.
-- This is complicated by the fact that you can't always tell if a term is a
-- blockterm just by looking at it: in some cases you can only tell when you can
-- see it in the context of the wider term that contains it. So actually we
-- traverse the tree, at each term looking for child terms that are block terms,
-- and see if any of those contain all the usages of the name.
-- Cut out the occurrences of "const id $" to get tracing.
allInSubBlock ::
  (Var v, Ord v) =>
  Term3 v PrintAnnotation ->
  Prefix ->
  Suffix ->
  Int ->
  Bool
allInSubBlock tm p s i =
  let found = concat $ ABT.find finder tm
      result = any (/= tm) found
      tr =
        id
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
                     in if i' == i && not (null found)
                          then ABT.Found found
                          else ABT.Continue
          -- children =
          --   concatMap
          --     (\t -> "child: " ++ show t ++ "\n")
          --     ( immediateChildBlockTerms t
          --     )
          tr =
            id
       in tr result
    hit t = getUsages t == i

-- Return any blockterms at or immediately under this term. Has to match the
-- places in the syntax that get a call to `calcImports` in `pretty0`.
-- AST nodes that do a calcImports in pretty0, in order to try and emit a `use`
-- statement, need to be emitted also by this function, otherwise the `use`
-- statement may come out at an enclosing scope instead.
immediateChildBlockTerms ::
  (Var vt, Var v) => Term2 vt at ap v a -> [Term2 vt at ap v a]
immediateChildBlockTerms = \case
  Handle' handler body -> [handler, body]
  If' _ t f -> [t, f]
  LetBlock bs e -> concatMap doLet bs ++ handleDelay e
  Delay' b@(Lets' _ _) -> [b]
  Match' scrute branches ->
    if isDestructuringBind scrute branches
      then [scrute]
      else concatMap doCase branches
  _ -> []
  where
    doCase (MatchCase _ _ (AbsN' _ body)) = [body]
    handleDelay (Delay' b@(Lets' _ _)) = [b]
    handleDelay _ = []
    doLet (v, Ann' tm _) = doLet (v, tm)
    doLet (v, LamsNamedOpt' _ body) = [body | not (Var.isAction v)]
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
isDestructuringBind :: (Ord v) => ABT.Term f v a -> [MatchCase loc (ABT.Term f v a)] -> Bool
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
      Pattern.Constructor _ _ ps -> any hasLiteral ps
      Pattern.As _ p -> hasLiteral p
      Pattern.EffectPure _ p -> hasLiteral p
      Pattern.EffectBind _ _ ps pk -> any hasLiteral (pk : ps)
      Pattern.SequenceLiteral _ ps -> any hasLiteral ps
      Pattern.SequenceOp _ p _ p2 -> hasLiteral p || hasLiteral p2
      Pattern.Var _ -> False
      Pattern.Unbound _ -> False
isDestructuringBind _ _ = False

isBlock :: (Ord v) => Term2 vt at ap v a -> Bool
isBlock tm =
  case tm of
    If' {} -> True
    Handle' _ _ -> True
    Match' _ _ -> True
    LetBlock _ _ -> True
    _ -> False

pattern LetBlock ::
  (Ord v) =>
  [(v, Term2 vt at ap v a)] ->
  Term2 vt at ap v a ->
  Term2 vt at ap v a
pattern LetBlock bindings body <- (unLetBlock -> Just (bindings, body))

-- Collects nested let/let rec blocks into one minimally nested block.
-- Handy because `let` and `let rec` blocks get rendered the same way.
-- We preserve nesting when the inner block shadows definitions in the
-- outer block.
unLetBlock ::
  (Ord v) =>
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

pattern LamsNamedMatch' ::
  (Var v) =>
  [v] ->
  [([Pattern ap], Maybe (Term2 vt at ap v a), Term2 vt at ap v a)] ->
  Term2 vt at ap v a
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
  (Var v) =>
  Term2 vt at ap v a ->
  Maybe ([v], [([Pattern ap], Maybe (Term2 vt at ap v a), Term2 vt at ap v a)])
unLamsMatch' t = case unLamsUntilDelay' t of
  -- x -> match x with pat -> ...
  --   becomes
  -- cases pat -> ...
  Just (reverse -> (v1 : vs), Match' (Var' v1') branches)
    | -- if `v1'` is referenced in any of the branches, we can't use lambda case
      -- syntax as we need to keep the `v1'` name that was introduced
      v1 == v1' && Set.notMember v1' (Set.unions $ freeVars <$> branches) ->
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
      isRightArity MatchCase {} = False
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
      let guardVars = maybe Set.empty ABT.freeVars g
          rhsVars = ABT.freeVars rhs
       in Set.union guardVars rhsVars

pattern Bytes' :: [Word64] -> Term3 v PrintAnnotation
pattern Bytes' bs <- (toBytes -> Just bs)

toBytes :: Term3 v PrintAnnotation -> Maybe [Word64]
toBytes (App' (Builtin' "Bytes.fromList") (List' bs)) =
  toList <$> traverse go bs
  where
    go (Nat' n) = Just n
    go _ = Nothing
toBytes _ = Nothing

prettyDoc2 ::
  forall v m.
  (MonadPretty v m) =>
  AmbientContext ->
  Term3 v PrintAnnotation ->
  m (Maybe (Pretty SyntaxText))
prettyDoc2 ac tm = do
  ppe <- getPPE
  let brace p =
        fmt S.DocDelimiter "{{"
          <> PP.softbreak
          <> p
          <> PP.softbreak
          <> fmt
            S.DocDelimiter
            "}}"
      bail tm = brace <$> pretty0 ac tm
      -- Finds the longest run of a character and return one bigger than that
      longestRun c s =
        case filter (\s -> take 2 s == [c, c]) $
          group (PP.toPlainUnbroken $ PP.syntaxToColor s) of
          [] -> 2
          x -> 1 + maximum (map length x)
      oneMore c inner = replicate (longestRun c inner) c
      makeFence inner = PP.string $ replicate (max 3 $ longestRun '`' inner) '`'
      go :: Width -> Term3 v PrintAnnotation -> m (Pretty SyntaxText)
      go hdr = \case
        (toDocTransclude ppe -> Just d) ->
          bail d
        (toDocUntitledSection ppe -> Just ds) ->
          sepBlankline ds
        (toDocSection ppe -> Just (title, ds)) -> do
          prettyTitle <- rec title
          prettyDs <- intercalateMapM "\n\n" (go (hdr + 1)) ds
          pure $
            PP.lines
              [ PP.text (Text.replicate (PP.widthToInt hdr) "#") <> " " <> prettyTitle,
                "",
                PP.indentN (hdr + 1) prettyDs
              ]
        (toDocParagraph ppe -> Just ds) ->
          PP.wrap . mconcat <$> traverse rec ds
        (toDocBulletedList ppe -> Just ds) -> do
          PP.lines <$> traverse item ds
          where
            item d = ("* " <>) . PP.indentAfterNewline "  " <$> rec d
        (toDocNumberedList ppe -> Just (n, ds)) ->
          PP.column2 <$> traverse item (zip [n ..] ds)
          where
            item (n, d) = (PP.group (PP.shown n <> "."),) <$> rec d
        (toDocWord ppe -> Just t) ->
          pure $ PP.text t
        (toDocCode ppe -> Just d) -> do
          inner <- rec d
          let quotes = PP.string $ oneMore '\'' inner
          pure $ PP.group $ quotes <> inner <> quotes
        (toDocJoin ppe -> Just ds) -> foldMapM rec ds
        (toDocItalic ppe -> Just d) -> do
          inner <- rec d
          let underscores = PP.string $ oneMore '_' inner
          pure $ PP.group $ underscores <> inner <> underscores
        (toDocBold ppe -> Just d) -> do
          inner <- rec d
          let stars = PP.string $ oneMore '*' inner
          pure $ PP.group $ stars <> inner <> stars
        (toDocStrikethrough ppe -> Just d) -> do
          inner <- rec d
          let quotes = PP.string $ oneMore '~' inner
          pure $ PP.group $ quotes <> inner <> quotes
        (toDocGroup ppe -> Just d) ->
          PP.group <$> rec d
        (toDocColumn ppe -> Just ds) ->
          PP.lines <$> traverse rec ds
        (toDocNamedLink ppe -> Just (name, target)) ->
          do
            name' <- rec name
            target' <- rec target
            pure $ PP.group $ "[" <> name' <> "](" <> target' <> ")"
        (toDocLink ppe -> Just e) -> pure . PP.group $ case e of
          Left r -> "{type " <> tyName r <> "}"
          Right r -> "{" <> tmName r <> "}"
        (toDocEval ppe -> Just tm) ->
          do
            inner <- pretty0 ac tm
            let fence = makeFence inner
            pure $ PP.lines [fence, inner, fence]
        (toDocEvalInline ppe -> Just tm) ->
          do
            inner <- pretty0 ac tm
            pure $ "@eval{" <> inner <> "}"
        (toDocExample ppe -> Just tm) ->
          do
            inner <- pretty0 ac tm
            pure $ "``" <> inner <> "``"
        (toDocExampleBlock ppe -> Just tm) ->
          do
            inner <- pretty0 ac' tm
            let fence = makeFence inner
            pure $ PP.lines ["@typecheck " <> fence, inner, fence]
          where
            ac' = ac {elideUnit = True}
        (toDocSource ppe -> Just es) ->
          pure . PP.group $ "    @source{" <> intercalateMap ", " go es <> "}"
          where
            go (Left r, _anns) = "type " <> tyName r
            go (Right r, _anns) = tmName r
        (toDocFoldedSource ppe -> Just es) ->
          pure . PP.group $ "    @foldedSource{" <> intercalateMap ", " go es <> "}"
          where
            go (Left r, _anns) = "type " <> tyName r
            go (Right r, _anns) = tmName r
        (toDocSignatureInline ppe -> Just tm) ->
          pure . PP.group $ "@inlineSignature{" <> tmName tm <> "}"
        (toDocSignature ppe -> Just tms) ->
          let name = if length tms == 1 then "@signature" else "@signatures"
           in pure . PP.group $ "    " <> name <> "{" <> intercalateMap ", " tmName tms <> "}"
        (toDocCodeBlock ppe -> Just (typ, txt)) ->
          pure $
            let txt' = PP.text txt
                fence = makeFence txt'
             in PP.group $
                  PP.lines
                    [ fence <> " " <> PP.text typ,
                      PP.group txt',
                      fence
                    ]
        (toDocVerbatim ppe -> Just txt) ->
          pure $
            PP.group $
              PP.lines
                [ "'''",
                  PP.group $ PP.text txt,
                  "'''"
                ]
        -- todo : emit fewer gratuitous columns, maybe a wrapIfMany combinator
        tm -> bail tm
        where
          im = imports ac
          tyName r = styleHashQualified'' (fmt $ S.TypeReference r) . elideFQN im $ PrettyPrintEnv.typeName ppe r
          tmName r = styleHashQualified'' (fmt $ S.TermReference r) . elideFQN im $ PrettyPrintEnv.termName ppe r
          rec = go hdr
          sepBlankline = intercalateMapM "\n\n" rec
  case tm of
    -- these patterns can introduce a {{ .. }} block
    (toDocUntitledSection ppe -> Just _) -> Just . brace <$> go 1 tm
    (toDocSection ppe -> Just _) -> Just . brace <$> go 1 tm
    (toDocParagraph ppe -> Just _) -> Just . brace <$> go 1 tm
    _ -> pure Nothing

toDocJoin :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe [Term3 v PrintAnnotation]
toDocJoin ppe (App' (Ref' r) (List' tms))
  | nameEndsWith ppe ".docJoin" r = Just (toList tms)
toDocJoin _ _ = Nothing

toDocUntitledSection :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe [Term3 v PrintAnnotation]
toDocUntitledSection ppe (App' (Ref' r) (List' tms))
  | nameEndsWith ppe ".docUntitledSection" r = Just (toList tms)
toDocUntitledSection _ _ = Nothing

toDocColumn :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe [Term3 v PrintAnnotation]
toDocColumn ppe (App' (Ref' r) (List' tms))
  | nameEndsWith ppe ".docColumn" r = Just (toList tms)
toDocColumn _ _ = Nothing

toDocGroup :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocGroup ppe (App' (Ref' r) doc)
  | nameEndsWith ppe ".docGroup" r = Just doc
toDocGroup _ _ = Nothing

toDocWord :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe Text
toDocWord ppe (App' (Ref' r) (Text' txt))
  | nameEndsWith ppe ".docWord" r = Just txt
toDocWord _ _ = Nothing

toDocBold :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocBold ppe (App' (Ref' r) doc)
  | nameEndsWith ppe ".docBold" r = Just doc
toDocBold _ _ = Nothing

toDocCode :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocCode ppe (App' (Ref' r) doc)
  | nameEndsWith ppe ".docCode" r = Just doc
toDocCode _ _ = Nothing

toDocCodeBlock :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Text, Text)
toDocCodeBlock ppe (Apps' (Ref' r) [Text' typ, Text' txt])
  | nameEndsWith ppe ".docCodeBlock" r = Just (typ, txt)
toDocCodeBlock _ _ = Nothing

toDocVerbatim :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe Text
toDocVerbatim ppe (App' (Ref' r) (toDocWord ppe -> Just txt))
  | nameEndsWith ppe ".docVerbatim" r = Just txt
toDocVerbatim _ _ = Nothing

toDocEval :: (Ord v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocEval ppe (App' (Ref' r) (Delay' tm))
  | nameEndsWith ppe ".docEval" r = Just tm
  | r == _oldDocEval = Just tm
toDocEval _ _ = Nothing

-- Old hashes for docEval, docEvalInline w/ incorrect type signatures.
-- They are still used by some existing docs so the pretty-printer
-- recognizes it.
--
-- See https://github.com/unisonweb/unison/issues/2238
_oldDocEval, _oldDocEvalInline :: Reference
_oldDocEval = Reference.unsafeFromText "#m2bmkdos2669tt46sh2gf6cmb4td5le8lcqnmsl9nfaqiv7s816q8bdtjdbt98tkk11ejlesepe7p7u8p0asu9758gdseffh0t78m2o"
_oldDocEvalInline = Reference.unsafeFromText "#7pjlvdu42gmfvfntja265dmi08afk08l54kpsuu55l9hq4l32fco2jlrm8mf2jbn61esfsi972b6e66d9on4i5bkmfchjdare1v5npg"

toDocEvalInline :: (Ord v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocEvalInline ppe (App' (Ref' r) (Delay' tm))
  | nameEndsWith ppe ".docEvalInline" r = Just tm
  | r == _oldDocEvalInline = Just tm
toDocEvalInline _ _ = Nothing

toDocExample, toDocExampleBlock :: (Ord v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocExample = toDocExample' ".docExample"
toDocExampleBlock = toDocExample' ".docExampleBlock"

toDocExample' :: (Ord v) => Text -> PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocExample' suffix ppe (Apps' (Ref' r) [Nat' n, l@(LamsNamed' vs tm)])
  | nameEndsWith ppe suffix r,
    ABT.freeVars l == mempty,
    ok tm =
      Just (lam' (ABT.annotation l) (drop (fromIntegral n + 1) vs) tm)
  where
    ok (Apps' f _) = ABT.freeVars f == mempty
    ok tm = ABT.freeVars tm == mempty
toDocExample' _ _ _ = Nothing

toDocTransclude :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocTransclude ppe (App' (Ref' r) tm)
  | nameEndsWith ppe ".docTransclude" r = Just tm
toDocTransclude _ _ = Nothing

toDocLink :: (Ord v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Either Reference Referent)
toDocLink ppe (App' (Ref' r) tm)
  | nameEndsWith ppe ".docLink" r = case tm of
      (toDocEmbedTermLink ppe -> Just tm) -> Just (Right tm)
      (toDocEmbedTypeLink ppe -> Just tm) -> Just (Left tm)
      _ -> Nothing
toDocLink _ _ = Nothing

toDocNamedLink :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation, Term3 v PrintAnnotation)
toDocNamedLink ppe (Apps' (Ref' r) [name, target])
  | nameEndsWith ppe ".docNamedLink" r = Just (name, target)
toDocNamedLink _ _ = Nothing

toDocItalic :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocItalic ppe (App' (Ref' r) doc)
  | nameEndsWith ppe ".docItalic" r = Just doc
toDocItalic _ _ = Nothing

toDocStrikethrough :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
toDocStrikethrough ppe (App' (Ref' r) doc)
  | nameEndsWith ppe ".docStrikethrough" r = Just doc
toDocStrikethrough _ _ = Nothing

toDocParagraph :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe [Term3 v PrintAnnotation]
toDocParagraph ppe (App' (Ref' r) (List' tms))
  | nameEndsWith ppe ".docParagraph" r = Just (toList tms)
toDocParagraph _ _ = Nothing

toDocEmbedTermLink :: (Ord v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe Referent
toDocEmbedTermLink ppe (App' (Ref' r) (Delay' (Referent' tm)))
  | nameEndsWith ppe ".docEmbedTermLink" r = Just tm
toDocEmbedTermLink _ _ = Nothing

toDocEmbedTypeLink :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe Reference
toDocEmbedTypeLink ppe (App' (Ref' r) (TypeLink' typeref))
  | nameEndsWith ppe ".docEmbedTypeLink" r = Just typeref
toDocEmbedTypeLink _ _ = Nothing

toDocSourceAnnotations :: (Ord v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe [Referent]
toDocSourceAnnotations _ppe _tm = Just [] -- todo fetch annotations

toDocSourceElement :: (Ord v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Either Reference Referent, [Referent])
toDocSourceElement ppe (Apps' (Ref' r) [tm, toDocSourceAnnotations ppe -> Just annotations])
  | nameEndsWith ppe ".docSourceElement" r =
      (,annotations) <$> ok tm
  where
    ok tm =
      Right <$> toDocEmbedTermLink ppe tm
        <|> Left <$> toDocEmbedTypeLink ppe tm
toDocSourceElement _ _ = Nothing

toDocSource' ::
  (Ord v) =>
  Text ->
  PrettyPrintEnv ->
  Term3 v PrintAnnotation ->
  Maybe [(Either Reference Referent, [Referent])]
toDocSource' suffix ppe (App' (Ref' r) (List' tms))
  | nameEndsWith ppe suffix r =
      case [tm | Just tm <- toDocSourceElement ppe <$> toList tms] of
        tms' | length tms' == length tms -> Just tms'
        _ -> Nothing
toDocSource' _ _ _ = Nothing

toDocSource,
  toDocFoldedSource ::
    (Ord v) =>
    PrettyPrintEnv ->
    Term3 v PrintAnnotation ->
    Maybe [(Either Reference Referent, [Referent])]
toDocSource = toDocSource' ".docSource"
toDocFoldedSource = toDocSource' ".docFoldedSource"

toDocSignatureInline :: (Ord v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe Referent
toDocSignatureInline ppe (App' (Ref' r) (toDocEmbedSignatureLink ppe -> Just tm))
  | nameEndsWith ppe ".docSignatureInline" r = Just tm
toDocSignatureInline _ _ = Nothing

toDocEmbedSignatureLink :: (Ord v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe Referent
toDocEmbedSignatureLink ppe (App' (Ref' r) (Delay' (Referent' tm)))
  | nameEndsWith ppe ".docEmbedSignatureLink" r = Just tm
toDocEmbedSignatureLink _ _ = Nothing

-- toDocEmbedAnnotation :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe (Term3 v PrintAnnotation)
-- toDocEmbedAnnotation ppe (App' (Ref' r) tm)
--   | nameEndsWith ppe ".docEmbedAnnotation" r = Just tm
-- toDocEmbedAnnotation _ _ = Nothing

-- toDocEmbedAnnotations :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe [Term3 v PrintAnnotation]
-- toDocEmbedAnnotations ppe (App' (Ref' r) (List' tms))
--   | nameEndsWith ppe ".docEmbedAnnotations" r =
--     case [ann | Just ann <- toDocEmbedAnnotation ppe <$> toList tms] of
--       tms' | length tms' == length tms -> Just tms'
--       _ -> Nothing
-- toDocEmbedAnnotations _ _ = Nothing

toDocSignature :: (Ord v) => PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe [Referent]
toDocSignature ppe (App' (Ref' r) (List' tms))
  | nameEndsWith ppe ".docSignature" r =
      case [tm | Just tm <- toDocEmbedSignatureLink ppe <$> toList tms] of
        tms' | length tms' == length tms -> Just tms'
        _ -> Nothing
toDocSignature _ _ = Nothing

toDocBulletedList :: PrettyPrintEnv -> Term3 v PrintAnnotation -> Maybe [Term3 v PrintAnnotation]
toDocBulletedList ppe (App' (Ref' r) (List' tms))
  | nameEndsWith ppe ".docBulletedList" r = Just (toList tms)
toDocBulletedList _ _ = Nothing

toDocNumberedList ::
  PrettyPrintEnv ->
  Term3 v PrintAnnotation ->
  Maybe (Word64, [Term3 v PrintAnnotation])
toDocNumberedList ppe (Apps' (Ref' r) [Nat' n, List' tms])
  | nameEndsWith ppe ".docNumberedList" r = Just (n, toList tms)
toDocNumberedList _ _ = Nothing

toDocSection ::
  PrettyPrintEnv ->
  Term3 v PrintAnnotation ->
  Maybe (Term3 v PrintAnnotation, [Term3 v PrintAnnotation])
toDocSection ppe (Apps' (Ref' r) [title, List' tms])
  | nameEndsWith ppe ".docSection" r = Just (title, toList tms)
toDocSection _ _ = Nothing

nameEndsWith :: PrettyPrintEnv -> Text -> Reference -> Bool
nameEndsWith ppe suffix r = case PrettyPrintEnv.termName ppe (Referent.Ref r) of
  HQ.NameOnly n ->
    let tn = Name.toText n
     in tn == Text.drop 1 suffix || Text.isSuffixOf suffix tn
  _ -> False
