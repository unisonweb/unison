{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Unison.Syntax.TermParser
  ( binding,
    blockTerm,
    doc2Block,
    imports,
    lam,
    substImports,
    term,
    verifyRelativeVarName,
  )
where

import Control.Comonad.Trans.Cofree (CofreeF ((:<)))
import Control.Lens (_2)
import Control.Monad.Reader (asks, local)
import Control.Monad.Trans.Writer
import Data.Bitraversable (bitraverse)
import Data.Char qualified as Char
import Data.Foldable (foldrM)
import Data.List qualified as List
import Data.List.Extra qualified as List.Extra
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Megaparsec qualified as P
import U.Codebase.Reference (ReferenceType (..))
import U.Core.ABT qualified as ABT
import Unison.ABT qualified as ABT
import Unison.Builtin.Decls qualified as DD
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Names.ResolutionResult (ResolutionError (..), ResolutionFailure (..))
import Unison.NamesWithHistory qualified as Names
import Unison.Parser.Ann (Ann (Ann))
import Unison.Parser.Ann qualified as Ann
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Syntax.Lexer.Unison qualified as L
import Unison.Syntax.Name qualified as Name (toText, toVar, unsafeParseVar)
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Syntax.Parser hiding (seq)
import Unison.Syntax.Parser qualified as Parser
import Unison.Syntax.Parser.Doc.Data qualified as Doc
import Unison.Syntax.Pattern qualified as Syntax.Pattern
import Unison.Syntax.Precedence (operatorPrecedence)
import Unison.Syntax.TypeParser qualified as TypeParser
import Unison.Term (IsTop, Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Typechecker.Components qualified as Components
import Unison.Util.Bytes qualified as Bytes
import Unison.Util.Map qualified as Map
import Unison.Util.Recursion
import Unison.Var (Var)
import Unison.Var qualified as Var
import Prelude hiding (and, or, seq, unzip)

{-
Precedence of language constructs is identical to Haskell, except that all
operators (like +, <*>, or any sequence of non-alphanumeric characters) are
left-associative and equal precedence (with a few exceptions), and operators
must have surrounding whitespace (a + b, not a+b) to distinguish from
identifiers that may contain operator characters (like empty? or fold-left).

Sections / partial application of infix operators is not implemented.
-}

type TermP v m = P v m (Term v Ann)

term :: (Monad m, Var v) => TermP v m
term = term2

term2 :: (Monad m, Var v) => TermP v m
term2 = lam term2 <|> term3

term3 :: (Monad m, Var v) => TermP v m
term3 = do
  t <- infixAppOrBooleanOp
  ot <- optional (reserved ":" *> TypeParser.computationType)
  pure case ot of
    Nothing -> t
    Just y -> Term.ann (mkAnn t y) t y

keywordBlock :: (Monad m, Var v) => TermP v m
keywordBlock = letBlock <|> handle <|> ifthen <|> match <|> lamCase <|> rewriteBlock

rewriteBlock :: (Monad m, Var v) => TermP v m
rewriteBlock = do
  t <- openBlockWith "@rewrite"
  elements <- sepBy semi (rewriteTerm <|> rewriteCase <|> rewriteType)
  b <- closeBlock
  pure (DD.rewrites (ann t <> ann b) elements)
  where
    rewriteTermlike kw mk = do
      kw <- quasikeyword kw
      lhs <- term
      (_openAnn, _spanAnn, rhs) <- layoutBlock "==>"
      pure (mk (ann kw <> ann rhs) lhs rhs)
    rewriteTerm = rewriteTermlike "term" DD.rewriteTerm
    rewriteCase = rewriteTermlike "case" DD.rewriteCase
    rewriteType = do
      kw <- quasikeyword "signature"
      vs <- P.try (some prefixDefinitionName <* reserved ".") <|> pure []
      lhs <- TypeParser.computationType
      rhs <- openBlockWith "==>" *> TypeParser.computationType <* closeBlock
      pure (DD.rewriteType (ann kw <> ann rhs) (L.payload <$> vs) lhs rhs)

typeLink' :: (Monad m, Var v) => P v m (L.Token TypeReference)
typeLink' = findUniqueType =<< hqPrefixId

findUniqueType :: (Monad m, Var v) => L.Token (HQ.HashQualified Name) -> P v m (L.Token TypeReference)
findUniqueType id =
  resolveToLocalNamespacedType id >>= \case
    Nothing -> do
      ns <- asks names
      case Names.lookupHQType Names.IncludeSuffixes (L.payload id) ns of
        s
          | Set.size s == 1 -> pure (Set.findMin s <$ id)
          | otherwise -> customFailure $ UnknownType id s
    Just ref -> pure (ref <$ id)

termLink' :: (Monad m, Var v) => P v m (L.Token Referent)
termLink' = do
  id <- hqPrefixId
  ns <- asks names
  case Names.lookupHQTerm Names.IncludeSuffixes (L.payload id) ns of
    s
      | Set.size s == 1 -> pure $ const (Set.findMin s) <$> id
      | otherwise -> customFailure $ UnknownTerm id s

link :: (Monad m, Var v) => TermP v m
link = termLink <|> typeLink
  where
    typeLink = do
      _ <- reserved "typeLink" -- type opens a block, gotta use something else
      tok <- typeLink'
      pure $ Term.typeLink (ann tok) (L.payload tok)
    termLink = do
      _ <- reserved "termLink"
      tok <- termLink'
      pure $ Term.termLink (ann tok) (L.payload tok)

resolveToLocalNamespacedType :: (Monad m, Ord v) => L.Token (HQ.HashQualified Name) -> P v m (Maybe TypeReference)
resolveToLocalNamespacedType tok =
  case L.payload tok of
    HQ.NameOnly name ->
      asks maybeNamespace >>= \case
        Nothing -> pure Nothing
        Just namespace -> do
          localNames <- asks localNamespacePrefixedTypesAndConstructors
          pure case Names.lookupHQType Names.ExactName (HQ.NameOnly (Name.joinDot namespace name)) localNames of
            refs
              | Set.null refs -> Nothing
              -- 2+ name case is impossible: we looked up exact names in the locally-bound names. Two bindings
              -- with the same name would have been a parse error. So, just take the minimum element from the set,
              -- which we know is a singleton.
              | otherwise -> Just (Set.findMin refs)
    _ -> pure Nothing

-- We disallow type annotations and lambdas,
-- just function application and operators
blockTerm :: (Monad m, Var v) => TermP v m
blockTerm = lam term <|> infixAppOrBooleanOp

match :: (Monad m, Var v) => TermP v m
match = do
  start <- openBlockWith "match"
  scrutinee <- term
  _ <- optionalCloseBlock
  _ <-
    openBlockWith "with" <|> do
      t <- anyToken
      P.customFailure (ExpectedBlockOpen "with" t)
  (_arities, cases) <- unzip <$> matchCases
  _ <- optionalCloseBlock
  let anns = foldr ((<>) . ann) (ann start) $ lastMay cases
  pure $ Term.match anns scrutinee cases

matchCases :: (Monad m, Var v) => P v m [(Int, Term.MatchCase Ann (Term v Ann))]
matchCases = sepBy semi matchCase <&> \cases_ -> [(n, c) | (n, cs) <- cases_, c <- cs]

-- Returns the arity of the pattern and the `MatchCase`. Examples:
--
--   (a, b) -> a - b -- arity 1
--   foo, hd +: tl -> foo tl -- arity 2
--
-- Cases with arity greater than 1 are desugared to matching on tuples,
-- so the following are parsed the same:
--
--   42, x -> ...
--   (42, x) -> ...
matchCase :: forall m v. (Monad m, Var v) => P v m (Int, [Term.MatchCase Ann (Term v Ann)])
matchCase = do
  pats <- sepBy1 (label "\",\"" $ reserved ",") (parsePattern >>= bindConstructorsInPattern)
  let boundVars0 = concatMap snd pats
  checkForDuplicateBinders boundVars0
  let boundVars' = map snd boundVars0
  let pat = case fst <$> pats of
        [p] -> p
        pats -> foldr pair (unit (ann . last $ pats)) pats
      unit ann = Pattern.Constructor ann (ConstructorReference DD.unitRef 0) []
      pair p1 p2 = Pattern.Constructor (ann p1 <> ann p2) (ConstructorReference DD.pairRef 0) [p1, p2]
  let guardedBlocks = label "pattern guard" . some $ do
        _ <- reserved "|"
        guard <-
          asum
            [ Nothing <$ quasikeyword "otherwise",
              Just <$> infixAppOrBooleanOp
            ]
        (_openAnn, _spanAnn, t) <- layoutBlock "->"
        pure (guard, t)
  let unguardedBlock = label "case match" do
        (_openAnn, _spanAnn, t) <- layoutBlock "->"
        pure (Nothing, t)
  -- a pattern's RHS is either one or more guards, or a single unguarded block.
  guardsAndBlocks <- guardedBlocks <|> (pure @[] <$> unguardedBlock)
  let absChain vs t = foldr (\v t -> ABT.abs' (ann t) v t) t vs
  let mk (guard, t) = Term.MatchCase pat (fmap (absChain boundVars') guard) (absChain boundVars' t)
  pure $ (length pats, mk <$> guardsAndBlocks)

-- Disallow binding the same variable twice.
checkForDuplicateBinders :: (Ord v) => [(Ann, v)] -> P v m ()
checkForDuplicateBinders =
  let go seen = \case
        [] -> pure ()
        (ann, v) : vs -> do
          seen1 <-
            Map.upsertF
              ( \case
                  Nothing -> pure ann
                  Just ann0 -> P.customFailure (DuplicateBinders ann0 ann v)
              )
              v
              seen
          go seen1 vs
   in go Map.empty

parsePattern :: forall m v. (Monad m, Var v) => P v m (Syntax.Pattern.Pattern v)
parsePattern =
  label "pattern" pRoot
  where
    pRoot :: P v m (Syntax.Pattern.Pattern v)
    pRoot =
      chainl1 (pHqNamey1 <|> pLeaf1) pInfix
      where
        pHqNamey1 :: P v m (Syntax.Pattern.Pattern v)
        pHqNamey1 = do
          pat <- pHqNamey
          let datacon name patterns =
                (Syntax.Pattern.Constructor ((ann pat <> maybe mempty ann (lastMay patterns))) name patterns)
          case pat of
            Syntax.Pattern.Constructor _ name _ {- this is [] -} -> do
              patterns <- many pLeaf
              pure (datacon name patterns)
            Syntax.Pattern.VarOrNullaryConstructor _ name ->
              many pLeaf <&> \case
                [] -> pat
                patterns -> datacon (HQ.NameOnly <$> name) patterns
            -- This is Syntax.Pattern.As
            _ -> pure pat

        pInfix :: P v m (Syntax.Pattern.Pattern v -> Syntax.Pattern.Pattern v -> Syntax.Pattern.Pattern v)
        pInfix =
          pSeqOp <&> \op l r ->
            Syntax.Pattern.SequenceOp (ann l <> ann r) l op r
          where
            pSeqOp :: (Ord v) => P v m Syntax.Pattern.SeqOp
            pSeqOp =
              Syntax.Pattern.Snoc <$ matchToken (L.SymbolyId (HQ'.fromName (Name.fromSegment NameSegment.snocSegment)))
                <|> Syntax.Pattern.Cons <$ matchToken (L.SymbolyId (HQ'.fromName (Name.fromSegment NameSegment.consSegment)))
                <|> Syntax.Pattern.Concat <$ matchToken (L.SymbolyId (HQ'.fromName (Name.fromSegment NameSegment.concatSegment)))

    pLeaf :: P v m (Syntax.Pattern.Pattern v)
    pLeaf =
      pHqNamey <|> pLeaf1

    pLeaf1 :: P v m (Syntax.Pattern.Pattern v)
    pLeaf1 =
      asum
        [ -- true or false or 5 or "text" or ?c
          pLiteral,
          -- _
          do
            tok <- blank
            pure (Syntax.Pattern.Unbound (ann tok)),
          -- [pat, pat, pat]
          Parser.seq Syntax.Pattern.SequenceLiteral pRoot,
          -- () or (pat, pat) or (pat, pat, pat) [which is actually parsed as (pat, (pat, pat)]
          pParenOrTuple,
          -- { a : b, c : d }
          P.try pRecord,
          -- { pat -> pat } or { pat }
          pEffect
        ]

    pLiteral :: P v m (Syntax.Pattern.Pattern v)
    pLiteral =
      asum [pTrue, pFalse, pNumber, pText, pChar]
      where
        pTrue :: P v m (Syntax.Pattern.Pattern v)
        pTrue = do
          tok <- reserved "true"
          pure (Syntax.Pattern.Boolean (ann tok) True)

        pFalse :: P v m (Syntax.Pattern.Pattern v)
        pFalse = do
          tok <- reserved "false"
          pure (Syntax.Pattern.Boolean (ann tok) False)

        pNumber :: P v m (Syntax.Pattern.Pattern v)
        pNumber =
          join $
            number'
              (pure . tok Syntax.Pattern.Int)
              (pure . tok Syntax.Pattern.Nat)
              (tok (const . failCommitted . FloatPattern))

        pText :: P v m (Syntax.Pattern.Pattern v)
        pText = do
          tok <- string
          pure (Syntax.Pattern.Text (ann tok) (L.payload tok))

        pChar :: P v m (Syntax.Pattern.Pattern v)
        pChar = do
          tok <- character
          pure (Syntax.Pattern.Char (ann tok) (L.payload tok))

    pParenOrTuple :: P v m (Syntax.Pattern.Pattern v)
    pParenOrTuple = do
      snd <$> tupleOrParenthesized parsePattern Syntax.Pattern.Unit mkPair
      where
        mkPair :: Syntax.Pattern.Pattern v -> Syntax.Pattern.Pattern v -> Syntax.Pattern.Pattern v
        mkPair p1 p2 =
          Syntax.Pattern.Pair (ann p1 <> ann p2) p1 p2

    pEffect :: P v m (Syntax.Pattern.Pattern v)
    pEffect = do
      start <- openBlockWith "{"

      -- After the opening curly brace, we are expecting either an EffectBind or an EffectPure:
      --
      --   EffectBind            EffectPure
      --
      --   { foo bar -> baz }    { qux }
      --     ^^^^^^^^^^^^^^        ^^^
      --
      -- We accomplish that as follows:
      --
      --   * First try EffectPure + "}"
      --     * If that fails, back the parser up and try EffectBind + "}" instaed
      --
      -- This won't always result in the best possible error messages, but it's not exactly trivial to do better,
      -- requiring more sophisticated look-ahead logic. So, this is how it works for now.
      (inner, end) <-
        asum
          [ P.try do
              inner <- pEffectPure
              end <- closeBlock
              pure (inner, end),
            do
              inner <- pEffectBind
              end <- closeBlock
              pure (inner, end)
          ]

      pure (Syntax.Pattern.setPos (ann start <> ann end) inner)
      where
        pEffectBind :: P v m (Syntax.Pattern.Pattern v)
        pEffectBind = do
          name <- hqPrefixId
          patterns <- many pLeaf
          _ <- reserved "->"
          cont <- parsePattern
          pure (Syntax.Pattern.EffectBind (ann name <> ann cont) name patterns cont)

        pEffectPure :: P v m (Syntax.Pattern.Pattern v)
        pEffectPure =
          parsePattern <&> \pat -> Syntax.Pattern.EffectPure (ann pat) pat

    pRecord :: P v m (Syntax.Pattern.Pattern v)
    pRecord = do
      start <- openBlockWith "{"
      let field = do
            fieldName <- Parser.recordFieldName
            _ <- reserved ":"
            fieldPattern <- parsePattern
            pure (L.payload fieldName, fieldPattern)
      fields <- sepBy (reserved ",") field
      end <- closeBlock
      pure (Syntax.Pattern.RecordLiteral (ann start <> ann end) (Map.fromList fields))

    -- Parse an "HQ-namey", which could either definitely be a nullary constructor (because it's either hash-only or
    -- hash-qualified or symboly), or either a variable or nullary constructor (because it's a wordy name-only). And if
    -- it's the latter, we might see that it's actually not a nullary constructor but actually a variable in an
    -- as-pattern, e.g. `Foo@Bar`.
    pHqNamey :: P v m (Syntax.Pattern.Pattern v)
    pHqNamey = do
      tok <- varOrNullaryConstructor
      case L.payload tok of
        Left name -> pure (Syntax.Pattern.Constructor (ann tok) (name <$ tok) [])
        Right name -> do
          optional (reserved "@") >>= \case
            Nothing -> pure (Syntax.Pattern.VarOrNullaryConstructor (ann tok) (name <$ tok))
            Just _ -> do
              p <- pLeaf
              pure (Syntax.Pattern.As (ann tok <> ann p) (Name.toVar name <$ tok) p)

bindConstructorsInPattern :: (Monad m, Var v) => Syntax.Pattern.Pattern v -> P v m (Pattern.Pattern Ann, [(Ann, v)])
bindConstructorsInPattern =
  fmap (over _2 (\f -> (map tokenToPair (f [])))) . runWriterT . bindConstructorsInPattern1
  where
    bindConstructorsInPattern1 ::
      forall m v.
      (Monad m, Var v) =>
      Syntax.Pattern.Pattern v ->
      WriterT ([L.Token v] -> [L.Token v]) (P v m) (Pattern.Pattern Ann)
    bindConstructorsInPattern1 = \case
      Syntax.Pattern.As pos v lpat -> do
        tell (v :)
        pat <- bindConstructorsInPattern1 lpat
        pure (Pattern.As pos pat)
      Syntax.Pattern.Boolean pos b -> pure (Pattern.Boolean pos b)
      Syntax.Pattern.Char pos c -> pure (Pattern.Char pos c)
      Syntax.Pattern.Constructor pos name pats ->
        Pattern.Constructor pos
          <$> lift (bindConstructor CT.Data name)
          <*> traverse bindConstructorsInPattern1 pats
      Syntax.Pattern.EffectBind pos name pats cont ->
        Pattern.EffectBind pos
          <$> lift (bindConstructor CT.Effect name)
          <*> traverse bindConstructorsInPattern1 pats
          <*> bindConstructorsInPattern1 cont
      Syntax.Pattern.EffectPure pos lpat -> Pattern.EffectPure pos <$> bindConstructorsInPattern1 lpat
      Syntax.Pattern.Float pos n -> pure (Pattern.Float pos n)
      Syntax.Pattern.Int pos n -> pure (Pattern.Int pos n)
      Syntax.Pattern.Nat pos n -> pure (Pattern.Nat pos n)
      Syntax.Pattern.Pair _ lpat1 lpat2 ->
        ( \pat1 pat2 ->
            Pattern.Constructor
              (ann pat1 <> ann pat2)
              (ConstructorReference DD.pairRef 0)
              [pat1, pat2]
        )
          <$> bindConstructorsInPattern1 lpat1
          <*> bindConstructorsInPattern1 lpat2
      Syntax.Pattern.RecordLiteral pos fields ->
        traverse bindConstructorsInPattern1 fields <&> Pattern.RecordLiteral pos
      Syntax.Pattern.SequenceLiteral pos pats -> Pattern.SequenceLiteral pos <$> traverse bindConstructorsInPattern1 pats
      Syntax.Pattern.SequenceOp pos lpat1 op lpat2 ->
        Pattern.SequenceOp pos
          <$> bindConstructorsInPattern1 lpat1
          <*> pure case op of
            Syntax.Pattern.Concat -> Pattern.Concat
            Syntax.Pattern.Cons -> Pattern.Cons
            Syntax.Pattern.Snoc -> Pattern.Snoc
          <*> bindConstructorsInPattern1 lpat2
      Syntax.Pattern.Text pos t -> pure (Pattern.Text pos t)
      Syntax.Pattern.Unbound pos -> pure (Pattern.Unbound pos)
      Syntax.Pattern.Unit pos -> pure (Pattern.Constructor pos (ConstructorReference DD.unitRef 0) [])
      -- Not awesome: something can be at once a syntactically valid nullary constructor and a syntactically valid
      -- variable. We currently handle this by simply looking in the namespace to determine whether it's a
      -- constructor, and if it isn't, we treat it as a variable.
      Syntax.Pattern.VarOrNullaryConstructor pos name ->
        lift (maybeBindLocalConstructor CT.Data (L.payload name)) >>= \case
          Just localCtor -> pure (Pattern.Constructor pos localCtor [])
          Nothing -> do
            names <- asks names
            let failure :: ResolutionError Referent -> P v m a
                failure err =
                  failCommitted $
                    ResolutionFailures
                      [ TermResolutionFailure
                          (HQ.NameOnly (L.payload name))
                          (ann name)
                          err
                      ]
            case Names.lookupHQPattern Names.IncludeSuffixes (HQ.NameOnly (L.payload name)) CT.Data names of
              constructors
                | Set.size constructors == 1 -> pure (Pattern.Constructor pos (Set.findMin constructors) [])
                | Set.null constructors ->
                    -- Not great thing alert :alarm: :alarm:
                    -- This is a syntactically valid variable, however, if it begins with a capital letter, we choose to
                    -- consider it a constructor-out-of-scope, since that's probably what the user meant.
                    if lastSegmentBeginsWithCapitalLetter
                      then lift (failure NotFound)
                      else do
                        tell ((Name.toVar <$> name) :)
                        pure (Pattern.Var pos)
                | otherwise ->
                    lift $
                      failure
                        ( Ambiguous
                            names
                            (Set.map (\ref -> Referent.Con ref CT.Data) constructors)
                            Set.empty
                        )
        where
          lastSegmentBeginsWithCapitalLetter :: Bool
          lastSegmentBeginsWithCapitalLetter =
            not (Char.isLower (Text.head (NameSegment.toUnescapedText (Name.lastSegment (L.payload name)))))
      where
        bindConstructor :: CT.ConstructorType -> L.Token (HQ.HashQualified Name) -> P v m ConstructorReference
        bindConstructor ct hqName = do
          -- First, if:
          --
          --   * The token isn't hash-qualified (e.g. "Foo.Bar")
          --   * We're under a namespace directive (e.g. "baz")
          --   * There's an exact match for a locally-bound constructor (e.g. "baz.Foo.Bar")
          --
          -- Then:
          --
          --   * Use that constructor reference (duh)
          --
          -- Else:
          --
          --   * Fall through to the normal logic of looking the constructor name up in all of the names (which includes
          --     the locally-bound constructors).
          maybeLocalCtor <-
            case L.payload hqName of
              HQ.NameOnly name -> maybeBindLocalConstructor ct name
              _ -> pure Nothing

          case maybeLocalCtor of
            Just localCtor -> pure localCtor
            Nothing -> do
              names <- asks names
              case Names.lookupHQPattern Names.IncludeSuffixes (L.payload hqName) ct names of
                s
                  | Set.size s == 1 -> pure (Set.findMin s)
                  | otherwise ->
                      failCommitted $
                        ResolutionFailures
                          [ ConstructorResolutionFailure
                              (L.payload hqName)
                              (ann hqName)
                              if Set.null s
                                then NotFound
                                else
                                  Ambiguous
                                    names
                                    (Set.map (,ct) s)
                                    -- Eh, here we're saying there are no "local" constructors – they're all from "the
                                    -- namespace". That's not necessarily true, but it doesn't (currently) affect the error
                                    -- message any, and we have already parsed and hashed local constructors (so they aren't
                                    -- really different from namespace constructors).
                                    Set.empty
                          ]

        maybeBindLocalConstructor :: CT.ConstructorType -> Name -> P v m (Maybe ConstructorReference)
        maybeBindLocalConstructor ct name =
          asks maybeNamespace >>= \case
            Nothing -> pure Nothing
            Just namespace -> do
              localNames <- asks localNamespacePrefixedTypesAndConstructors
              pure case Names.lookupHQPattern Names.ExactName (HQ.NameOnly (Name.joinDot namespace name)) ct localNames of
                refs
                  | Set.null refs -> Nothing
                  -- 2+ name case is impossible: we looked up exact names in the locally-bound names. Two bindings
                  -- with the same name would have been a parse error. So, just take the minimum element from the set,
                  -- which we know is a singleton.
                  | otherwise -> Just (Set.findMin refs)

lam :: (Var v) => TermP v m -> TermP v m
lam p = label "lambda" $ mkLam <$> P.try (some prefixDefinitionName <* reserved "->") <*> p
  where
    mkLam vs b =
      let annotatedArgs = vs <&> \v -> (ann v, L.payload v)
       in Term.lam' (ann (head vs) <> ann b) annotatedArgs b

letBlock, handle, ifthen :: (Monad m, Var v) => TermP v m
letBlock = label "let" $ do
  (_openAnn, _spanAnn, tm) <- layoutBlock "let"
  pure tm
handle = label "handle" do
  (_handleOpenAnn, handleSpan, b) <- block "handle"
  (_withOpenAnn, _withSpan, handler) <- layoutBlock "with"
  -- We don't use the annotation span from 'with' here because it will
  -- include a dedent if it's at the end of block.
  -- Meaning the newline gets overwritten when pretty-printing and it messes things up.
  pure $ Term.handle (handleSpan <> ann handler) handler b

checkCasesArities :: (Ord v, Annotated a) => [(Int, a)] -> P v m (Int, [a])
checkCasesArities = \case
  [] -> pure (1, [])
  cases@((i, _) : rest) -> case List.find (\(j, _) -> j /= i) rest of
    Nothing -> pure (i, snd <$> cases)
    Just (j, a) -> P.customFailure $ PatternArityMismatch i j (ann a)

lamCase :: (Monad m, Var v) => TermP v m
lamCase = do
  start <- openBlockWith "cases"
  cases <- matchCases
  (arity, cases) <- checkCasesArities cases
  _ <- optionalCloseBlock
  lamvars <- replicateM arity (Parser.uniqueName 10)
  let vars =
        Var.named <$> [tweak v i | (v, i) <- lamvars `zip` [(1 :: Int) ..]]
      tweak v 0 = v
      tweak v i = v <> Text.pack (show i)
      lamvarTerms = Term.var (ann start) <$> vars
      lamvarTerm = case lamvarTerms of
        [e] -> e
        es -> DD.tupleTerm es
      anns = foldr ((<>) . ann) (ann start) $ lastMay cases
      matchTerm = Term.match anns lamvarTerm cases
  let annotatedVars = (Ann.GeneratedFrom $ ann start,) <$> vars
  pure $ Term.lam' anns annotatedVars matchTerm

ifthen = label "if" do
  start <- peekAny
  (_ifOpenAnn, _spanAnn, c) <- block "if"
  (_thenAnn, _spanAnn, t) <- block "then"
  (_elseAnn, _spanAnn, f) <- layoutBlock "else"
  pure $ Term.iff (ann start <> ann f) c t f

text :: (Var v) => TermP v m
text = tok Term.text <$> string

char :: (Var v) => TermP v m
char = tok Term.char <$> character

boolean :: (Var v) => TermP v m
boolean =
  ((\t -> Term.boolean (ann t) True) <$> reserved "true")
    <|> ((\t -> Term.boolean (ann t) False) <$> reserved "false")

list :: (Var v) => TermP v m -> TermP v m
list = Parser.seq Term.list

hashQualifiedPrefixTerm :: (Monad m, Var v) => TermP v m
hashQualifiedPrefixTerm = resolveHashQualified =<< hqPrefixId

quasikeyword :: (Ord v) => Text -> P v m (L.Token ())
quasikeyword kw = queryToken \case
  L.WordyId (HQ'.NameOnly n) | nameIsKeyword n kw -> Just ()
  _ -> Nothing

nameIsKeyword :: Name -> Text -> Bool
nameIsKeyword name keyword =
  case (Name.isRelative name, Name.reverseSegments name) of
    (True, segment NonEmpty.:| []) -> NameSegment.toEscapedText segment == keyword
    _ -> False

-- If the hash qualified is name only, it is treated as a var, if it
-- has a short hash, we resolve that short hash immediately and fail
-- committed if that short hash can't be found in the current environment
resolveHashQualified :: (Monad m, Var v) => L.Token (HQ.HashQualified Name) -> TermP v m
resolveHashQualified tok = do
  case L.payload tok of
    HQ.NameOnly n -> pure $ Term.var (ann tok) (Name.toVar n)
    _ -> do
      names <- asks names
      case Names.lookupHQTerm Names.IncludeSuffixes (L.payload tok) names of
        s
          | Set.null s -> failCommitted $ UnknownTerm tok s
          | Set.size s > 1 -> failCommitted $ UnknownTerm tok s
          | otherwise -> pure $ Term.fromReferent (ann tok) (Set.findMin s)

termLeaf :: forall m v. (Monad m, Var v) => TermP v m
termLeaf =
  asum
    [ force,
      hashQualifiedPrefixTerm,
      text,
      char,
      number,
      bytes,
      boolean,
      link,
      recordLiteral,
      tupleOrParenthesizedTerm,
      keywordBlock,
      list term,
      delayQuote,
      (snd <$> delayBlock),
      bang,
      doc2Block <&> \(spanAnn, trm) -> trm {ABT.annotation = ABT.annotation trm <> spanAnn}
    ]

-- | Gives a parser an explicit stream to parse, so that it consumes nothing from the original stream when it runs.
--
--   This is used inside the `Doc` -> `Term` conversion, where we have chunks of Unison code embedded that need to be
--   parsed. It’s a consequence of parsing Doc in the midst of the Unison lexer.
subParse :: (Ord v, Monad m) => P v m a -> [L.Token L.Lexeme] -> P v m a
subParse p toks = do
  orig <- P.getInput
  P.setInput $ Input toks
  result <- p <* P.eof
  P.setInput orig
  pure result

-- | Syntax for documentation v2 blocks, which are surrounded by @{{@ @}}@.
-- The lexer does most of the heavy lifting so there's not a lot for
-- the parser to do. For instance, in
--
-- > {{
-- > Hi there!
-- >
-- > goodbye.
-- > }}
--
-- the lexer will produce:
--
-- > [ Doc
-- >   ( DocUntitledSection
-- >     (DocParagraph (DocWord "Hi" :| [DocWord "there!"]))
-- >     (DocParagraph (DocWord "goodbye" :| []))
-- >   )
-- > ]
--
-- The parser will parse this into the Unison expression:
--
-- > syntax.docUntitledSection [
-- >   syntax.docParagraph [syntax.docWord "Hi", syntax.docWord "there!"],
-- >   syntax.docParagraph [syntax.docWord "goodbye"]
-- > ]
--
-- Where @syntax.doc{Paragraph, UntitledSection,...}@ are all ordinary term
-- variables that will be looked up in the environment like anything else. This
-- means that the documentation syntax can have its meaning changed by
-- overriding what functions the names @syntax.doc*@ correspond to.
doc2Block :: forall m v. (Monad m, Var v) => P v m (Ann {- Annotation for the whole spanning block -}, Term v Ann)
doc2Block = do
  L.Token docContents startDoc endDoc <- doc
  let docAnn = Ann startDoc endDoc
  (docAnn,) . docUntitledSection (gann docAnn) <$> traverse foldTop docContents
  where
    foldTop = cataM \(a :< top) -> docTop a =<< bitraverse (cataM \(a :< leaf) -> docLeaf a leaf) pure top

    gann :: (Annotated a) => a -> Ann
    gann = Ann.GeneratedFrom . ann

    addDelay :: Term v Ann -> Term v Ann
    addDelay tm = Term.delay (ann tm) tm

    f :: (Annotated a) => a -> String -> Term v Ann
    f a = Term.var (gann a) . Var.nameds . ("syntax.doc" <>)

    docUntitledSection :: Ann -> Doc.UntitledSection (Term v Ann) -> Term v Ann
    docUntitledSection ann (Doc.UntitledSection tops) =
      Term.app ann (f ann "UntitledSection") $ Term.list (gann tops) tops

    docTop :: Ann -> Doc.Top [L.Token L.Lexeme] (Term v Ann) (Term v Ann) -> TermP v m
    docTop d = \case
      Doc.Section title body -> pure $ Term.apps' (f d "Section") [docParagraph d title, Term.list (gann body) body]
      Doc.Eval code ->
        let inner = do
              (_openAnn, ann, tm) <- (block' False False "syntax.docEval" (pure $ pure ()) $ Ann.External <$ P.eof)
              pure (ann, tm)
         in Term.app (gann d) (f d "Eval") . addDelay . snd
              <$> subParse inner code
      Doc.ExampleBlock code ->
        let inner = do
              (_openAnn, ann, tm) <- (block' False True "syntax.docExampleBlock" (pure $ pure ()) $ Ann.External <$ P.eof)
              pure (ann, tm)
         in Term.apps' (f d "ExampleBlock") . (Term.nat (gann d) 0 :) . pure . addDelay . snd
              <$> subParse inner code
      Doc.CodeBlock label body ->
        pure $
          Term.apps'
            (f d "CodeBlock")
            [Term.text d $ Text.pack label, Term.text d $ Text.pack body]
      Doc.List' list -> pure $ docList d list
      Doc.Paragraph' para -> pure $ docParagraph d para

    docParagraph d leaves = Term.app (gann d) (f d "Paragraph") . Term.list d $ toList leaves

    docList :: Ann -> Doc.List (Term v Ann) -> Term v Ann
    docList d = \case
      Doc.BulletedList items ->
        Term.app (gann d) (f d "BulletedList") . Term.list (gann d) . toList $ docColumn d <$> items
      Doc.NumberedList items@((n, _) :| _) ->
        Term.apps'
          (f d "NumberedList")
          [Term.nat (ann d) $ n, Term.list (gann d) . toList $ docColumn d . snd <$> items]

    docColumn :: Ann -> Doc.Column (Term v Ann) -> Term v Ann
    docColumn d (Doc.Column para sublist) =
      Term.app (gann d) (f d "Column") . Term.list (gann d) $ docParagraph d para : toList (docList d <$> sublist)

    docLeaf :: Ann -> Doc.Leaf (L.Token (ReferenceType, HQ'.HashQualified Name)) [L.Token L.Lexeme] (Term v Ann) -> TermP v m
    docLeaf d = \case
      Doc.Link link -> Term.app (gann d) (f d "Link") <$> docEmbedLink d link
      Doc.NamedLink para group -> pure $ Term.apps' (f d "NamedLink") [docParagraph d para, docGroup d group]
      Doc.Example code -> do
        trm <- subParse term code
        pure . Term.apps' (f d "Example") $ case trm of
          tm@(Term.Apps' _ xs) ->
            let fvs = List.Extra.nubOrd $ concatMap (toList . Term.freeVars) xs
                n = Term.nat (ann tm) (fromIntegral (length fvs))
                lam = addDelay $ Term.lam' (ann tm) ((mempty,) <$> fvs) tm
             in [n, lam]
          tm -> [Term.nat (ann tm) 0, addDelay tm]
      Doc.Transclude' trans -> docTransclude d trans
      Doc.Bold para -> pure . Term.app (gann d) (f d "Bold") $ docParagraph d para
      Doc.Italic para -> pure . Term.app (gann d) (f d "Italic") $ docParagraph d para
      Doc.Strikethrough para -> pure . Term.app (gann d) (f d "Strikethrough") $ docParagraph d para
      Doc.Verbatim leaf -> pure . Term.app (gann d) (f d "Verbatim") $ docWord d leaf
      Doc.Code leaf -> pure . Term.app (gann d) (f d "Code") $ docWord d leaf
      Doc.Source elems ->
        Term.app (gann d) (f d "Source") . Term.list d . toList <$> traverse (docSourceElement d) elems
      Doc.FoldedSource elems ->
        Term.app (gann d) (f d "FoldedSource") . Term.list d . toList <$> traverse (docSourceElement d) elems
      Doc.EvalInline code -> Term.app (gann d) (f d "EvalInline") . addDelay <$> subParse term code
      Doc.Signature links ->
        Term.app (gann d) (f d "Signature") . Term.list d . toList <$> traverse (docEmbedSignatureLink d) links
      Doc.SignatureInline link -> Term.app (gann d) (f d "SignatureInline") <$> docEmbedSignatureLink d link
      Doc.Word' word -> pure $ docWord d word
      Doc.Group' group -> pure $ docGroup d group

    docEmbedLink :: Ann -> Doc.EmbedLink (L.Token (ReferenceType, HQ'.HashQualified Name)) -> TermP v m
    docEmbedLink d (Doc.EmbedLink (L.Token (level, ident) start end)) = case level of
      RtType ->
        Term.app (gann d) (f d "EmbedTypeLink") . Term.typeLink (ann d) . L.payload
          <$> findUniqueType (L.Token (HQ'.toHQ ident) start end)
      RtTerm ->
        Term.app (gann d) (f d "EmbedTermLink") . addDelay <$> resolveHashQualified (L.Token (HQ'.toHQ ident) start end)

    docTransclude :: Ann -> Doc.Transclude [L.Token L.Lexeme] -> TermP v m
    docTransclude d (Doc.Transclude code) = Term.app (gann d) (f d "Transclude") <$> subParse term code

    docSourceElement ::
      Ann ->
      Doc.SourceElement (L.Token (ReferenceType, HQ'.HashQualified Name)) (Doc.Transclude [L.Token L.Lexeme]) ->
      TermP v m
    docSourceElement d (Doc.SourceElement link anns) = do
      link' <- docEmbedLink d link
      anns' <- traverse (docEmbedAnnotation d) anns
      pure $ Term.apps' (f d "SourceElement") [link', Term.list d anns']

    docEmbedSignatureLink ::
      Ann -> Doc.EmbedSignatureLink (L.Token (ReferenceType, HQ'.HashQualified Name)) -> TermP v m
    docEmbedSignatureLink d (Doc.EmbedSignatureLink (L.Token (level, ident) start end)) = case level of
      RtType -> P.customFailure . TypeNotAllowed $ L.Token (HQ'.toHQ ident) start end
      RtTerm ->
        Term.app (gann d) (f d "EmbedSignatureLink") . addDelay
          <$> resolveHashQualified (L.Token (HQ'.toHQ ident) start end)

    docEmbedAnnotation ::
      Ann ->
      Doc.EmbedAnnotation (L.Token (ReferenceType, HQ'.HashQualified Name)) (Doc.Transclude [L.Token L.Lexeme]) ->
      TermP v m
    docEmbedAnnotation d (Doc.EmbedAnnotation a) =
      -- This is the only place I’m not sure we’re doing the right thing. In the lexer, this can be an identifier or a
      -- DocLeaf, but here it could be either /text/ or a Doc element. And I don’t think there’s any way the lexemes
      -- produced for an identifier and the lexemes consumed for text line up. So, I think this is a bugfix I can’t
      -- avoid.
      Term.app (gann d) (f d "EmbedAnnotation")
        <$> either
          ( \(L.Token (level, ident) start end) -> case level of
              RtType -> P.customFailure . TypeNotAllowed $ L.Token (HQ'.toHQ ident) start end
              RtTerm -> resolveHashQualified $ L.Token (HQ'.toHQ ident) start end
          )
          (docTransclude d)
          a

    docWord :: Ann -> Doc.Word -> Term v Ann
    docWord d (Doc.Word txt) = Term.app (gann d) (f d "Word") . Term.text d $ Text.pack txt

    docGroup :: Ann -> Doc.Group (Term v Ann) -> Term v Ann
    docGroup d (Doc.Group (Doc.Join leaves)) =
      Term.app d (f d "Group") . Term.app d (f d "Join") . Term.list (ann leaves) $ toList leaves

-- Used by unbreakParas within docNormalize.  Doc literals are a joined sequence
-- segments.  This type describes a property of a segment.
data UnbreakCase
  = -- Finishes with a newline and hence does not determine whether the next
    -- line starts with whitespace.
    LineEnds
  | -- Ends with "\n something", i.e. introduces an indented line.
    StartsIndented
  | -- Ends with "\nsomething", i.e. introduces an unindented line.
    StartsUnindented
  deriving (Eq, Show)

delayQuote :: (Monad m, Var v) => TermP v m
delayQuote = P.label "quote" do
  start <- reserved "'"
  e <- termLeaf
  pure $ DD.delayTerm (ann start <> ann e) (ann start) e

delayBlock :: (Monad m, Var v) => P v m (Ann {- Ann spanning the whole block -}, Term v Ann)
delayBlock = P.label "do" do
  (openAnn, spanAnn, b) <- layoutBlock "do"
  pure $ (spanAnn, DD.delayTerm (ann b) openAnn b)

bang :: (Monad m, Var v) => TermP v m
bang = P.label "bang" do
  start <- reserved "!"
  e <- termLeaf
  pure $ DD.forceTerm (ann start <> ann e) (ann start) e

force :: forall m v. (Monad m, Var v) => TermP v m
force = P.label "force" $ P.try do
  -- `forkAt pool() blah` parses as `forkAt (pool ()) blah`
  -- That is, empty parens immediately (no space) following a symbol
  -- is treated as high precedence function application of `Unit`
  fn <- hashQualifiedPrefixTerm
  tok <- ann <$> openBlockWith "("
  guard (L.column (Ann.start tok) == L.column (Ann.end (ann fn)))
  close <- closeBlock
  pure $ DD.forceTerm (ann fn <> ann close) (tok <> ann close) fn

term4 :: (Monad m, Var v) => TermP v m
term4 = f <$> some termLeaf
  where
    f (func : args) = Term.apps func ((\a -> (ann func <> ann a, a)) <$> args)
    f [] = error "'some' shouldn't produce an empty list"

data InfixParse v
  = InfixOp (L.Token (HQ.HashQualified Name)) (Term v Ann) (InfixParse v) (InfixParse v)
  | InfixAnd (L.Token String) (InfixParse v) (InfixParse v)
  | InfixOr (L.Token String) (InfixParse v) (InfixParse v)
  | InfixOperand (Term v Ann)
  deriving (Show, Eq, Ord)

-- e.g. term4 + term4 - term4
-- or term4 || term4 && term4
-- The algorithm works as follows:
-- 1. Parse the expression left-associated
-- 2. Starting at the leftmost operator subexpression, see if the next operator
--   has higher precedence. If so, rotate the expression to the right.
--   e.g. in `a + b * c`, we first parse `(a + b) * c` then rotate to `a + (b * c)`.
-- 3. Perform the algorithm on the right-hand side if necessary, as `b` might be
--   an infix expression with lower precedence than `*`.
-- 4. Proceed to the next operator to the right in the original expression and
--    repeat steps 2-3 until we reach the end.
infixAppOrBooleanOp :: forall m v. (Monad m, Var v) => TermP v m
infixAppOrBooleanOp = do
  (p, ps) <- prelimParse
  -- traceShowM ("orig" :: String, foldl' (flip ($)) p ps)
  let p' = reassociate (p, ps)
  -- traceShowM ("reassoc" :: String, p')
  return (applyInfixOps p')
  where
    -- To handle a mix of infix operators with and without precedence rules,
    -- we first parse the expression left-associated, then reassociate it
    -- according to the precedence rules.
    prelimParse =
      chainl1Accum (InfixOperand <$> term4) genericInfixApp
    genericInfixApp =
      (InfixAnd <$> (label "and" (reserved "&&")))
        <|> (InfixOr <$> (label "or" (reserved "||")))
        <|> (uncurry InfixOp <$> parseInfix)
    shouldRotate child parent = case (child, parent) of
      (Just p1, Just p2) -> p1 < p2
      _ -> False
    parseInfix = label "infixApp" do
      op <- hqInfixId <* optional semi
      resolved <- resolveHashQualified op
      pure (op, resolved)
    reassociate (exp, ops) =
      foldl' checkOp exp ops
    checkOp exp op = fixUp (op exp)
    fixUp = \case
      InfixOp op tm lhs rhs ->
        rotate (unqualified op) (InfixOp op tm) lhs rhs
      InfixAnd op lhs rhs ->
        rotate "&&" (InfixAnd op) lhs rhs
      InfixOr op lhs rhs ->
        rotate "||" (InfixOr op) lhs rhs
      x -> x
    rotate op ctor lhs rhs =
      case lhs of
        InfixOp lop ltm ll lr
          | shouldRotate (operatorPrecedence (unqualified lop)) (operatorPrecedence op) ->
              InfixOp lop ltm ll (fixUp (ctor lr rhs))
        InfixAnd lop ll lr
          | shouldRotate (operatorPrecedence "&&") (operatorPrecedence op) ->
              InfixAnd lop ll (fixUp (ctor lr rhs))
        InfixOr lop ll lr
          | shouldRotate (operatorPrecedence "||") (operatorPrecedence op) ->
              InfixOr lop ll (fixUp (ctor lr rhs))
        _ -> ctor lhs rhs
    unqualified t = Maybe.fromJust $ NameSegment.toEscapedText . Name.lastSegment <$> (HQ.toName $ L.payload t)
    applyInfixOps :: InfixParse v -> Term v Ann
    applyInfixOps t = case t of
      InfixOp _ tm lhs rhs ->
        Term.apps' tm [applyInfixOps lhs, applyInfixOps rhs]
      InfixOperand tm -> tm
      InfixAnd op lhs rhs ->
        let lhs' = applyInfixOps lhs
            rhs' = applyInfixOps rhs
         in Term.and (ann lhs' <> ann op <> ann rhs') lhs' rhs'
      InfixOr op lhs rhs ->
        let lhs' = applyInfixOps lhs
            rhs' = applyInfixOps rhs
         in Term.or (ann lhs' <> ann op <> ann rhs') lhs' rhs'

typedecl :: (Monad m, Var v) => P v m (L.Token v, Type v Ann)
typedecl =
  (,)
    <$> P.try (prefixTermName <* reserved ":")
    <*> TypeParser.valueType
    <* semi

verifyRelativeVarName :: (Var v) => P v m (L.Token v) -> P v m (L.Token v)
verifyRelativeVarName p = do
  v <- p
  verifyRelativeName' (Name.unsafeParseVar <$> v)
  pure v

verifyRelativeName' :: (Ord v) => L.Token Name -> P v m ()
verifyRelativeName' name = do
  let txt = Name.toText . L.payload $ name
  when (Text.isPrefixOf "." txt && txt /= ".") $
    failCommitted (DisallowedAbsoluteName name)

-- example:
--   (x, y)   = foo
--   stuff
--
-- desugars to:
--
--   match foo with
--     (x,y) -> stuff
--
destructuringBind :: forall m v. (Monad m, Var v) => P v m (Ann, Term v Ann -> Term v Ann)
destructuringBind = do
  -- We have to look ahead as far as the `=` to know if this is a bind or
  -- just an action, for instance:
  --   (Some 42)
  --   vs
  --   (Some 42) = List.head elems
  pat <- P.try (parsePattern <* P.lookAhead (openBlockWith "="))
  (p, boundVars0) <- bindConstructorsInPattern pat
  checkForDuplicateBinders boundVars0
  (_eqAnn, _spanAnn, scrute) <- layoutBlock "=" -- Dwight K. Scrute ("The People's Scrutinee")
  let boundVars = map snd boundVars0
  let guard = Nothing
  let absChain vs t = foldr (\v t -> ABT.abs' (ann t) v t) t vs
      thecase t = Term.MatchCase p (fmap (absChain boundVars) guard) $ absChain boundVars t
  pure
    ( ann p,
      \t ->
        let a = ann p <> ann t
         in Term.match a scrute [thecase t]
    )

-- | Rules for the annotation of the resulting binding is as follows:
-- * If the binding has a type signature, the top level scope of the annotation for the type
-- Ann node will contain the _entire_ binding, including the type signature.
-- * The body expression of the binding contains the entire lhs (including the name of the
-- binding) and the entire body.
-- * If the binding is a lambda, the  lambda node includes the entire LHS of the binding,
-- including the name as well.
binding ::
  forall m v.
  (Monad m, Var v) =>
  P
    v
    m
    ( (Ann {- annotation for the location of 'v' -}, v),
      Term v Ann
    )
binding = label "binding" do
  typ <- optional typedecl
  -- a ++ b = ...
  let infixLhs = do
        (arg1, op) <-
          P.try $
            (,) <$> prefixDefinitionName <*> symbolyDefinitionName
        arg2 <- prefixDefinitionName
        pure (ann arg1, op, [arg1, arg2])
  let prefixLhs = do
        v <- prefixTermName
        vs <- many prefixTermName
        pure (ann v, v, vs)
  let lhs :: P v m (Ann, L.Token v, [L.Token v])
      lhs = infixLhs <|> prefixLhs
  case typ of
    Nothing -> do
      -- we haven't seen a type annotation, so lookahead to '=' before commit
      (lhsLoc, name, args) <- P.try (lhs <* P.lookAhead (openBlockWith "="))
      (_eqAnn, _bodySpanAnn, body) <- block "="
      verifyRelativeName' (fmap Name.unsafeParseVar name)
      let binding = mkBinding lhsLoc args body
      -- We don't actually use the span annotation from the block (yet) because it
      -- may contain a bunch of white-space and comments following a top-level-definition.
      -- let spanAnn = ann lhsLoc <> ann binding
      pure $ ((ann name, (L.payload name)), binding)
    Just (nameT, typ) -> do
      (lhsLoc, name, args) <- lhs
      verifyRelativeName' (fmap Name.unsafeParseVar name)
      when (L.payload name /= L.payload nameT) $
        customFailure $
          SignatureNeedsAccompanyingBody nameT
      (_eqAnn, _bodySpanAnn, body) <- block "="
      let binding = mkBinding lhsLoc args body
      -- We don't actually use the span annotation from the block (yet) because it
      -- may contain a bunch of white-space and comments following a top-level-definition.
      let spanAnn = ann nameT <> ann binding
      pure $ ((ann nameT, L.payload name), Term.ann spanAnn binding typ)
  where
    mkBinding :: Ann -> [L.Token v] -> Term.Term v Ann -> Term.Term v Ann
    mkBinding _lhsLoc [] body = body
    mkBinding lhsLoc args body =
      let annotatedArgs = args <&> \arg -> (ann arg, L.payload arg)
       in Term.lam' (lhsLoc <> ann body) annotatedArgs body

customFailure :: (P.MonadParsec e s m) => e -> m a
customFailure = P.customFailure

block ::
  forall m v.
  (Monad m, Var v) =>
  String ->
  P
    v
    m
    ( Ann {- annotation of block-open symbol, e.g. 'do', 'let' -},
      Ann {- annotation for whole block -},
      Term v Ann
    )
block s = block' False False s (openBlockWith s) closeBlock

layoutBlock ::
  forall m v.
  (Monad m, Var v) =>
  String ->
  P
    v
    m
    ( Ann {- annotation of block-open symbol, e.g. 'do', 'let' -},
      Ann {- annotation for whole layout block -},
      Term v Ann
    )
layoutBlock s = block' False False s (openBlockWith s) optionalCloseBlock

-- example: use Foo.bar.Baz + ++ x
-- + ++ and x are called the "suffixes" of the `use` statement, and
-- `Foo.bar.Baz` is called the prefix. A `use` statement has the effect
-- of allowing you to reference identifiers of the form <prefix>.<suffix>
-- using just <suffix>.
--
-- `use foo` by itself is equivalent to `use foo bar baz ...` for all
-- names in the environment prefixed by `foo`
--
-- todo: doesn't support use Foo.bar ++#abc, which lets you use `++` unqualified to refer to `Foo.bar.++#abc`
importp :: (Monad m, Ord v) => P v m [(Name, Name)]
importp = do
  kw <- reserved "use"
  -- we allow symbolyId here and parse the suffix optionaly, so we can generate
  -- a nicer error message if the suffixes are empty
  prefix <-
    optional $
      fmap Right importWordyId
        <|> fmap Left importSymbolyId
  suffixes <- optional (some (importRelativeWordyId <|> importRelativeSymbolyId))
  case (prefix, suffixes) of
    (Nothing, _) -> P.customFailure $ UseEmpty kw
    (Just prefix@(Left _), _) -> P.customFailure $ UseInvalidPrefixSuffix prefix suffixes
    (Just (Right prefix), Nothing) -> do
      -- `wildcard import`
      names <- asks names
      pure $ Names.expandWildcardImport (L.payload prefix) names
    (Just (Right prefix), Just suffixes) -> pure do
      suffix <- L.payload <$> suffixes
      pure (suffix, Name.joinDot (L.payload prefix) suffix)

data BlockElement v
  = Binding ((Ann {- span for the binding name -}, v), Term v Ann)
  | DestructuringBind (Ann, Term v Ann -> Term v Ann)
  | Action (Term v Ann)

instance (Show v) => Show (BlockElement v) where
  show (Binding ((pos, name), _)) = show ("binding: " :: Text, pos, name)
  show (DestructuringBind (pos, _)) = show ("destructuring bind: " :: Text, pos)
  show (Action tm) = show ("action: " :: Text, ann tm)

-- subst
-- use Foo.Bar + blah
-- use Bar.Baz zonk zazzle
imports :: (Monad m, Var v) => P v m (Names, [(v, v)])
imports = do
  let sem = P.try (semi <* P.lookAhead (reserved "use"))
  imported <- mconcat . reverse <$> sepBy sem importp
  ns' <- Names.importing imported <$> asks names
  pure (ns', [(Name.toVar suffix, Name.toVar full) | (suffix, full) <- imported])

-- A key feature of imports is we want to be able to say:
-- `use foo.bar Baz qux` without having to specify whether `Baz` or `qux` are
-- terms or types.
substImports :: (Var v) => Names -> [(v, v)] -> Term v Ann -> Term v Ann
substImports ns imports =
  ABT.substsInheritAnnotation
    [ (suffix, Term.var () full)
    | (suffix, full) <- imports
    ]
    . Term.substTypeVars -- no guard here, as `full` could be bound
    -- not in Names, but in a later term binding
      [ (suffix, Type.var () full)
      | (suffix, full) <- imports,
        Names.hasTypeNamed Names.IncludeSuffixes (Name.unsafeParseVar full) ns
      ]

block' ::
  forall m v end.
  (Monad m, Var v, Annotated end) =>
  IsTop ->
  -- | `True` means insert `()` at end of block if it ends with a statement
  Bool ->
  String ->
  P v m (L.Token ()) ->
  P v m end ->
  P v m (Ann {- span for the opening token, e.g. the "do" or opening bracket -}, Ann {- ann which spans the whole block -}, Term v Ann)
block' isTop implicitUnitAtEnd s openBlock closeBlock = do
  open <- openBlock
  (names, imports) <- imports
  _ <- optional semi
  statements <- local (\e -> e {names}) $ sepBy semi statement
  end <- closeBlock
  body <- substImports names imports <$> go open statements
  pure (ann open, ann open <> ann end, body)
  where
    statement = asum [Binding <$> binding, DestructuringBind <$> destructuringBind, Action <$> blockTerm]
    go :: L.Token () -> [BlockElement v] -> P v m (Term v Ann)
    go open =
      let finish :: Term.Term v Ann -> TermP v m
          finish tm = case Components.minimize' tm of
            Left dups -> customFailure $ DuplicateTermNames (toList (fmap (second toList) dups))
            Right tm -> pure tm
          toTm :: [BlockElement v] -> TermP v m
          toTm [] = customFailure $ EmptyBlock (const s <$> open)
          toTm (be : bes) = do
            let (bs, blockResult) = determineBlockResult (be :| bes)
            finish =<< foldrM step blockResult bs
            where
              step :: BlockElement v -> Term v Ann -> TermP v m
              step elem result = case elem of
                Binding ((a, v), tm) -> do
                  let fullLetRecSpan = ann a <> ann result
                  pure $
                    Term.consLetRec
                      isTop
                      fullLetRecSpan
                      (a, v, tm)
                      result
                Action tm -> do
                  let fullLetRecSpan = (ann tm <> ann result)
                  pure $
                    Term.consLetRec
                      isTop
                      fullLetRecSpan
                      (Ann.External, positionalVar (ann tm) (Var.named "_"), tm)
                      result
                DestructuringBind (_, f) ->
                  f <$> finish result
          determineBlockResult :: NonEmpty (BlockElement v) -> ([BlockElement v], Term v Ann)
          determineBlockResult bs = case NonEmpty.reverse bs of
            Binding ((a, _v), _) :| _ ->
              if implicitUnitAtEnd
                then (toList bs, DD.unitTerm a)
                else (toList bs, Term.var a (positionalVar a Var.missingResult))
            Action e :| bs -> (reverse (toList bs), e)
            DestructuringBind (a, _) :| _ ->
              if implicitUnitAtEnd
                then (toList bs, DD.unitTerm a)
                else (toList bs, Term.var a (positionalVar a Var.missingResult))
       in toTm

number :: (Var v) => TermP v m
number = number' (tok Term.int) (tok Term.nat) (tok Term.float)

bytes :: (Var v) => TermP v m
bytes = do
  b <- bytesToken
  let a = ann b
  pure $
    Term.app
      a
      (Term.builtin a "Bytes.fromList")
      (Term.list a $ Term.nat a . fromIntegral <$> Bytes.toWord8s (L.payload b))

number' ::
  (Ord v) =>
  (L.Token Int64 -> a) ->
  (L.Token Word64 -> a) ->
  (L.Token Double -> a) ->
  P v m a
number' i u f = fmap go numeric
  where
    go num@(L.payload -> p)
      | any (\c -> c == '.' || c == 'e') p && take 1 p == "+" = f (read . drop 1 <$> num)
      | any (\c -> c == '.' || c == 'e') p = f (read <$> num)
      | take 1 p == "+" = i (read . drop 1 <$> num)
      | take 1 p == "-" = i (read <$> num)
      | otherwise = u (read <$> num)

-- E.g. { name = "Steve", age = 30 }
recordLiteral ::
  forall v m.
  (Var v, Ord v, Monad m) =>
  TermP v m
recordLiteral = do
  seq' "{" finalize keyValueP
  where
    keyValueP :: P v m (L.Token Text, Term v Ann)
    keyValueP = do
      key <- recordFieldName
      _ <- reserved ":"
      value <- term
      pure (key, value)
    finalize :: Ann -> [(L.Token Text, Term v Ann)] -> (Term v Ann)
    finalize spanAnn kvs = Term.record spanAnn (Map.fromList (first L.payload <$> kvs))

tupleOrParenthesizedTerm :: (Monad m, Var v) => TermP v m
tupleOrParenthesizedTerm = label "tuple" $ do
  (spanAnn, tm) <- tupleOrParenthesized term DD.unitTerm pair
  pure $ tm {ABT.annotation = spanAnn}
  where
    pair t1 t2 =
      Term.app
        (ann t1 <> ann t2)
        ( Term.app
            (ann t1)
            (Term.constructor (ann t1 <> ann t2) (ConstructorReference DD.pairRef 0))
            t1
        )
        t2
