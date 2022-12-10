{-# LANGUAGE PartialTypeSignatures #-}

module Unison.Syntax.TermParser where

import Control.Monad.Reader (asks, local)
import qualified Data.Char as Char
import Data.Foldable (foldrM)
import qualified Data.List as List
import qualified Data.List.Extra as List.Extra
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Sequence
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Tuple.Extra as TupleE
import qualified Text.Megaparsec as P
import qualified Unison.ABT as ABT
import qualified Unison.Builtin.Decls as DD
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import qualified Unison.ConstructorType as CT
import qualified Unison.HashQualified as HQ
import Unison.Name (Name)
import qualified Unison.Name as Name
import qualified Unison.Names as Names
import Unison.NamesWithHistory (NamesWithHistory)
import qualified Unison.NamesWithHistory as NamesWithHistory
import Unison.Parser.Ann (Ann)
import Unison.Pattern (Pattern)
import qualified Unison.Pattern as Pattern
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Syntax.Lexer as L
import qualified Unison.Syntax.Name as Name (toText, toVar, unsafeFromVar)
import Unison.Syntax.Parser hiding (seq)
import qualified Unison.Syntax.Parser as Parser (seq, uniqueName)
import qualified Unison.Syntax.TypeParser as TypeParser
import Unison.Term (IsTop, Term)
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.Typechecker.Components as Components
import qualified Unison.Util.Bytes as Bytes
import Unison.Util.List (intercalateMapWith, quenchRuns)
import Unison.Var (Var)
import qualified Unison.Var as Var
import Prelude hiding (and, or, seq)

watch :: Show a => String -> a -> a
watch msg a = let !_ = trace (msg ++ ": " ++ show a) () in a

{-
Precedence of language constructs is identical to Haskell, except that all
operators (like +, <*>, or any sequence of non-alphanumeric characters) are
left-associative and equal precedence, and operators must have surrounding
whitespace (a + b, not a+b) to distinguish from identifiers that may contain
operator characters (like empty? or fold-left).

Sections / partial application of infix operators is not implemented.
-}

type TermP v = P v (Term v Ann)

term :: Var v => TermP v
term = term2

term2 :: Var v => TermP v
term2 = lam term2 <|> term3

term3 :: Var v => TermP v
term3 = do
  t <- infixAppOrBooleanOp
  ot <- optional (reserved ":" *> TypeParser.computationType)
  pure $ case ot of
    Nothing -> t
    Just y -> Term.ann (mkAnn t y) t y

keywordBlock :: Var v => TermP v
keywordBlock = letBlock <|> handle <|> ifthen <|> match <|> lamCase

typeLink' :: Var v => P v (L.Token Reference)
typeLink' = do
  id <- hqPrefixId
  ns <- asks names
  case NamesWithHistory.lookupHQType (L.payload id) ns of
    s
      | Set.size s == 1 -> pure $ const (Set.findMin s) <$> id
      | otherwise -> customFailure $ UnknownType id s

termLink' :: Var v => P v (L.Token Referent)
termLink' = do
  id <- hqPrefixId
  ns <- asks names
  case NamesWithHistory.lookupHQTerm (L.payload id) ns of
    s
      | Set.size s == 1 -> pure $ const (Set.findMin s) <$> id
      | otherwise -> customFailure $ UnknownTerm id s

link' :: Var v => P v (Either (L.Token Reference) (L.Token Referent))
link' = do
  id <- hqPrefixId
  ns <- asks names
  case (NamesWithHistory.lookupHQTerm (L.payload id) ns, NamesWithHistory.lookupHQType (L.payload id) ns) of
    (s, s2) | Set.size s == 1 && Set.null s2 -> pure . Right $ const (Set.findMin s) <$> id
    (s, s2) | Set.size s2 == 1 && Set.null s -> pure . Left $ const (Set.findMin s2) <$> id
    (s, s2) -> customFailure $ UnknownId id s s2

link :: Var v => TermP v
link = termLink <|> typeLink
  where
    typeLink = do
      P.try (reserved "typeLink") -- type opens a block, gotta use something else
      tok <- typeLink'
      pure $ Term.typeLink (ann tok) (L.payload tok)
    termLink = do
      P.try (reserved "termLink")
      tok <- termLink'
      pure $ Term.termLink (ann tok) (L.payload tok)

-- We disallow type annotations and lambdas,
-- just function application and operators
blockTerm :: Var v => TermP v
blockTerm = lam term <|> infixAppOrBooleanOp

match :: Var v => TermP v
match = do
  start <- openBlockWith "match"
  scrutinee <- term
  _ <- closeBlock
  _ <-
    P.try (openBlockWith "with") <|> do
      t <- anyToken
      P.customFailure (ExpectedBlockOpen "with" t)
  (_arities, cases) <- NonEmpty.unzip <$> matchCases1 start
  _ <- closeBlock
  pure $
    Term.match
      (ann start <> ann (NonEmpty.last cases))
      scrutinee
      (toList cases)

matchCases1 :: Var v => L.Token () -> P v (NonEmpty (Int, Term.MatchCase Ann (Term v Ann)))
matchCases1 start = do
  cases <-
    sepBy1 semi matchCase
      <&> \cases -> [(n, c) | (n, cs) <- cases, c <- cs]
  case cases of
    [] -> P.customFailure (EmptyMatch start)
    (c : cs) -> pure (c NonEmpty.:| cs)

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
matchCase :: Var v => P v (Int, [Term.MatchCase Ann (Term v Ann)])
matchCase = do
  pats <- sepBy1 (reserved ",") parsePattern
  let boundVars' = [v | (_, vs) <- pats, (_ann, v) <- vs]
      pat = case fst <$> pats of
        [p] -> p
        pats -> foldr pair (unit (ann . last $ pats)) pats
      unit ann = Pattern.Constructor ann (ConstructorReference DD.unitRef 0) []
      pair p1 p2 = Pattern.Constructor (ann p1 <> ann p2) (ConstructorReference DD.pairRef 0) [p1, p2]
  guardsAndBlocks <- many $ do
    guard <-
      asum
        [ Nothing <$ P.try (reserved "|" *> quasikeyword "otherwise"),
          optional $ reserved "|" *> infixAppOrBooleanOp
        ]
    t <- block "->"
    pure (guard, t)
  let absChain vs t = foldr (\v t -> ABT.abs' (ann t) v t) t vs
  let mk (guard, t) = Term.MatchCase pat (fmap (absChain boundVars') guard) (absChain boundVars' t)
  pure $ (length pats, mk <$> guardsAndBlocks)

parsePattern :: forall v. Var v => P v (Pattern Ann, [(Ann, v)])
parsePattern = root
  where
    root = chainl1 patternCandidates patternInfixApp
    patternCandidates = constructor <|> leaf
    patternInfixApp ::
      P
        v
        ( (Pattern Ann, [(Ann, v)]) ->
          (Pattern Ann, [(Ann, v)]) ->
          (Pattern Ann, [(Ann, v)])
        )
    patternInfixApp = f <$> seqOp
      where
        f op (l, lvs) (r, rvs) =
          (Pattern.SequenceOp (ann l <> ann r) l op r, lvs ++ rvs)

    -- note: nullaryCtor comes before var patterns, since (for better or worse)
    -- they can overlap (a variable could be called 'Foo' in the current grammar).
    -- This order treats ambiguous patterns as nullary constructors if there's
    -- a constructor with a matching name.
    leaf =
      literal <|> nullaryCtor <|> varOrAs <|> unbound <|> seqLiteral
        <|> parenthesizedOrTuplePattern
        <|> effect
    literal = (,[]) <$> asum [true, false, number, text, char]
    true = (\t -> Pattern.Boolean (ann t) True) <$> reserved "true"
    false = (\t -> Pattern.Boolean (ann t) False) <$> reserved "false"
    number =
      join $
        number'
          (pure . tok Pattern.Int)
          (pure . tok Pattern.Nat)
          (tok (const . failCommitted . FloatPattern))
    text = (\t -> Pattern.Text (ann t) (L.payload t)) <$> string
    char = (\c -> Pattern.Char (ann c) (L.payload c)) <$> character
    parenthesizedOrTuplePattern :: P v (Pattern Ann, [(Ann, v)])
    parenthesizedOrTuplePattern = tupleOrParenthesized parsePattern unit pair
    unit ann = (Pattern.Constructor ann (ConstructorReference DD.unitRef 0) [], [])
    pair (p1, v1) (p2, v2) =
      ( Pattern.Constructor (ann p1 <> ann p2) (ConstructorReference DD.pairRef 0) [p1, p2],
        v1 ++ v2
      )
    -- Foo x@(Blah 10)
    varOrAs :: P v (Pattern Ann, [(Ann, v)])
    varOrAs = do
      v <- wordyPatternName
      o <- optional (reserved "@")
      if isJust o
        then (\(p, vs) -> (Pattern.As (ann v) p, tokenToPair v : vs)) <$> leaf
        else pure (Pattern.Var (ann v), [tokenToPair v])
    unbound :: P v (Pattern Ann, [(Ann, v)])
    unbound = (\tok -> (Pattern.Unbound (ann tok), [])) <$> blank
    ctor :: CT.ConstructorType -> (L.Token (HQ.HashQualified Name) -> Set ConstructorReference -> Error v) -> P v (L.Token ConstructorReference)
    ctor ct err = do
      -- this might be a var, so we avoid consuming it at first
      tok <- P.try (P.lookAhead hqPrefixId)
      names <- asks names
      -- probably should avoid looking up in `names` if `L.payload tok`
      -- starts with a lowercase
      case NamesWithHistory.lookupHQPattern (L.payload tok) ct names of
        s
          | Set.null s -> die tok s
          | Set.size s > 1 -> die tok s
          | otherwise -> -- matched ctor name, consume the token
              do anyToken; pure (Set.findMin s <$ tok)
      where
        isLower = Text.all Char.isLower . Text.take 1 . Name.toText
        die hq s = case L.payload hq of
          -- if token not hash qualified or uppercase,
          -- fail w/out consuming it to allow backtracking
          HQ.NameOnly n
            | Set.null s
                && isLower n ->
                fail $ "not a constructor name: " <> show n
          -- it was hash qualified, and wasn't found in the env, that's a failure!
          _ -> failCommitted $ err hq s

    unzipPatterns f elems = case unzip elems of (patterns, vs) -> f patterns (join vs)

    effectBind0 = do
      tok <- ctor CT.Effect UnknownAbilityConstructor
      leaves <- many leaf
      _ <- reserved "->"
      pure (tok, leaves)

    effectBind = do
      (tok, leaves) <- P.try effectBind0
      (cont, vsp) <- parsePattern
      pure $
        let f patterns vs = (Pattern.EffectBind (ann tok <> ann cont) (L.payload tok) patterns cont, vs ++ vsp)
         in unzipPatterns f leaves

    effectPure = go <$> parsePattern
      where
        go (p, vs) = (Pattern.EffectPure (ann p) p, vs)

    effect = do
      start <- openBlockWith "{"
      (inner, vs) <- effectBind <|> effectPure
      end <- closeBlock
      pure (Pattern.setLoc inner (ann start <> ann end), vs)

    -- ex: unique type Day = Mon | Tue | ...
    nullaryCtor = P.try $ do
      tok <- ctor CT.Data UnknownDataConstructor
      pure (Pattern.Constructor (ann tok) (L.payload tok) [], [])

    constructor = do
      tok <- ctor CT.Data UnknownDataConstructor
      let f patterns vs =
            let loc = foldl (<>) (ann tok) $ map ann patterns
             in (Pattern.Constructor loc (L.payload tok) patterns, vs)
      unzipPatterns f <$> many leaf

    seqLiteral = Parser.seq f root
      where
        f loc = unzipPatterns ((,) . Pattern.SequenceLiteral loc)

lam :: Var v => TermP v -> TermP v
lam p = label "lambda" $ mkLam <$> P.try (some prefixDefinitionName <* reserved "->") <*> p
  where
    mkLam vs b = Term.lam' (ann (head vs) <> ann b) (map L.payload vs) b

letBlock, handle, ifthen :: Var v => TermP v
letBlock = label "let" $ block "let"
handle = label "handle" $ do
  b <- block "handle"
  handler <- block "with"
  pure $ Term.handle (ann b) handler b

checkCasesArities :: (Ord v, Annotated a) => NonEmpty (Int, a) -> P v (Int, NonEmpty a)
checkCasesArities cases@((i, _) NonEmpty.:| rest) =
  case List.find (\(j, _) -> j /= i) rest of
    Nothing -> pure (i, snd <$> cases)
    Just (j, a) -> P.customFailure $ PatternArityMismatch i j (ann a)

lamCase :: Var v => TermP v
lamCase = do
  start <- openBlockWith "cases"
  cases <- matchCases1 start
  (arity, cases) <- checkCasesArities cases
  _ <- closeBlock
  lamvars <- replicateM arity (Parser.uniqueName 10)
  let vars =
        Var.named <$> [tweak v i | (v, i) <- lamvars `zip` [(1 :: Int) ..]]
      tweak v 0 = v
      tweak v i = v <> Text.pack (show i)
      lamvarTerms = Term.var (ann start) <$> vars
      lamvarTerm = case lamvarTerms of
        [e] -> e
        es -> DD.tupleTerm es
      anns = ann start <> ann (NonEmpty.last cases)
      matchTerm = Term.match anns lamvarTerm (toList cases)
  pure $ Term.lam' anns vars matchTerm

ifthen = label "if" $ do
  start <- peekAny
  c <- block "if"
  t <- block "then"
  f <- block "else"
  pure $ Term.iff (ann start <> ann f) c t f

text :: Var v => TermP v
text = tok Term.text <$> string

char :: Var v => TermP v
char = tok Term.char <$> character

boolean :: Var v => TermP v
boolean =
  ((\t -> Term.boolean (ann t) True) <$> reserved "true")
    <|> ((\t -> Term.boolean (ann t) False) <$> reserved "false")

list :: Var v => TermP v -> TermP v
list = Parser.seq Term.list

hashQualifiedPrefixTerm :: Var v => TermP v
hashQualifiedPrefixTerm = resolveHashQualified =<< hqPrefixId

hashQualifiedInfixTerm :: Var v => TermP v
hashQualifiedInfixTerm = resolveHashQualified =<< hqInfixId

quasikeyword :: Ord v => String -> P v (L.Token ())
quasikeyword kw = queryToken $ \case
  L.WordyId s Nothing | s == kw -> Just ()
  _ -> Nothing

-- If the hash qualified is name only, it is treated as a var, if it
-- has a short hash, we resolve that short hash immediately and fail
-- committed if that short hash can't be found in the current environment
resolveHashQualified :: Var v => L.Token (HQ.HashQualified Name) -> TermP v
resolveHashQualified tok = do
  names <- asks names
  case L.payload tok of
    HQ.NameOnly n -> pure $ Term.var (ann tok) (Name.toVar n)
    _ -> case NamesWithHistory.lookupHQTerm (L.payload tok) names of
      s
        | Set.null s -> failCommitted $ UnknownTerm tok s
        | Set.size s > 1 -> failCommitted $ UnknownTerm tok s
        | otherwise -> pure $ Term.fromReferent (ann tok) (Set.findMin s)

termLeaf :: forall v. Var v => TermP v
termLeaf =
  asum
    [ hashQualifiedPrefixTerm,
      text,
      char,
      number,
      bytes,
      boolean,
      link,
      tupleOrParenthesizedTerm,
      keywordBlock,
      list term,
      delayQuote,
      delayBlock,
      bang,
      docBlock,
      doc2Block
    ]

-- Syntax for documentation v2 blocks, which are surrounded by {{ }}.
-- The lexer does most of the heavy lifting so there's not a lot for
-- the parser to do. For instance, in
--
--   {{
--   Hi there!
--
--   goodbye.
--   }}
--
-- the lexer will produce:
--
-- [Open "syntax.docUntitledSection",
--    Open "syntax.docParagraph",
--      Open "syntax.docWord", Textual "Hi", Close,
--      Open "syntax.docWord", Textual "there!", Close,
--    Close
--    Open "syntax.docParagraph",
--      Open "syntax.docWord", Textual "goodbye", Close,
--    Close
--  Close]
--
-- The parser will parse this into the Unison expression:
--
--   syntax.docUntitledSection [
--     syntax.docParagraph [syntax.docWord "Hi", syntax.docWord "there!"],
--     syntax.docParagraph [syntax.docWord "goodbye"]
--   ]
--
-- Where `syntax.doc{Paragraph, UntitledSection,...}` are all ordinary term
-- variables that will be looked up in the environment like anything else. This
-- means that the documentation syntax can have its meaning changed by
-- overriding what functions the names `syntax.doc*` correspond to.
doc2Block :: forall v. Var v => TermP v
doc2Block =
  P.lookAhead (openBlockWith "syntax.docUntitledSection") *> elem
  where
    elem :: TermP v
    elem =
      text <|> do
        t <- openBlock
        let -- here, `t` will be something like `Open "syntax.docWord"`
            -- so `f` will be a term var with the name "syntax.docWord".
            f = f' t
            f' t = Term.var (ann t) (Var.nameds (L.payload t))

            -- follows are some common syntactic forms used for parsing child elements

            -- regular is parsed into `f child1 child2 child3` for however many children
            regular = do
              cs <- P.many elem <* closeBlock
              pure $ Term.apps' f cs

            -- variadic is parsed into: `f [child1, child2, ...]`
            variadic = variadic' f
            variadic' f = do
              cs <- P.many elem <* closeBlock
              pure $ Term.apps' f [Term.list (ann cs) cs]

            -- sectionLike is parsed into: `f tm [child1, child2, ...]`
            sectionLike = do
              arg1 <- elem
              cs <- P.many elem <* closeBlock
              pure $ Term.apps' f [arg1, Term.list (ann cs) cs]

            evalLike wrap = do
              tm <- term <* closeBlock
              pure $ Term.apps' f [wrap tm]

            -- converts `tm` to `'tm`
            --
            -- Embedded examples like ``1 + 1`` are represented as terms,
            -- but are wrapped in delays so they are left unevaluated for the
            -- code which renders documents. (We want the doc display to get
            -- the unevaluated expression `1 + 1` and not `2`)
            addDelay tm = Term.delay (ann tm) tm

        case L.payload t of
          "syntax.docJoin" -> variadic
          "syntax.docUntitledSection" -> variadic
          "syntax.docColumn" -> variadic
          "syntax.docParagraph" -> variadic
          "syntax.docSignature" -> variadic
          "syntax.docSource" -> variadic
          "syntax.docFoldedSource" -> variadic
          "syntax.docBulletedList" -> variadic
          "syntax.docSourceAnnotations" -> variadic
          "syntax.docSourceElement" -> do
            link <- elem
            anns <- P.optional $ reserved "@" *> elem
            closeBlock $> Term.apps' f [link, fromMaybe (Term.list (ann link) mempty) anns]
          "syntax.docNumberedList" -> do
            nitems@((n, _) : _) <- P.some nitem <* closeBlock
            let items = snd <$> nitems
            pure $ Term.apps' f [n, Term.list (ann items) items]
            where
              nitem = do
                n <- number
                t <- openBlockWith "syntax.docColumn"
                let f = f' ("syntax.docColumn" <$ t)
                child <- variadic' f
                pure (n, child)
          "syntax.docSection" -> sectionLike
          -- @source{ type Blah, foo, type Bar }
          "syntax.docEmbedTermLink" -> do
            tm <- addDelay <$> (hashQualifiedPrefixTerm <|> hashQualifiedInfixTerm)
            closeBlock $> Term.apps' f [tm]
          "syntax.docEmbedSignatureLink" -> do
            tm <- addDelay <$> (hashQualifiedPrefixTerm <|> hashQualifiedInfixTerm)
            closeBlock $> Term.apps' f [tm]
          "syntax.docEmbedTypeLink" -> do
            r <- typeLink'
            closeBlock $> Term.apps' f [Term.typeLink (ann r) (L.payload r)]
          "syntax.docExample" ->
            (term <* closeBlock) <&> \case
              tm@(Term.Apps' _ xs) ->
                let fvs = List.Extra.nubOrd $ concatMap (toList . Term.freeVars) xs
                    n = Term.nat (ann tm) (fromIntegral (length fvs))
                    lam = addDelay $ Term.lam' (ann tm) fvs tm
                 in Term.apps' f [n, lam]
              tm -> Term.apps' f [Term.nat (ann tm) 0, addDelay tm]
          "syntax.docTransclude" -> evalLike id
          "syntax.docEvalInline" -> evalLike addDelay
          "syntax.docExampleBlock" -> do
            tm <- block'' False True "syntax.docExampleBlock" (pure (void t)) closeBlock
            pure $ Term.apps' f [Term.nat (ann tm) 0, addDelay tm]
          "syntax.docEval" -> do
            tm <- block' False "syntax.docEval" (pure (void t)) closeBlock
            pure $ Term.apps' f [addDelay tm]
          _ -> regular

docBlock :: Var v => TermP v
docBlock = do
  openTok <- openBlockWith "[:"
  segs <- many segment
  closeTok <- closeBlock
  let a = ann openTok <> ann closeTok
  pure . docNormalize $ Term.app a (Term.constructor a (ConstructorReference DD.docRef DD.docJoinId)) (Term.list a segs)
  where
    segment = blob <|> linky
    blob = do
      s <- string
      pure $
        Term.app
          (ann s)
          (Term.constructor (ann s) (ConstructorReference DD.docRef DD.docBlobId))
          (Term.text (ann s) (L.payload s))
    linky = asum [include, signature, evaluate, source, link]
    include = do
      _ <- P.try (reserved "include")
      hashQualifiedPrefixTerm
    signature = do
      _ <- P.try (reserved "signature")
      tok <- termLink'
      pure $
        Term.app
          (ann tok)
          (Term.constructor (ann tok) (ConstructorReference DD.docRef DD.docSignatureId))
          (Term.termLink (ann tok) (L.payload tok))
    evaluate = do
      _ <- P.try (reserved "evaluate")
      tok <- termLink'
      pure $
        Term.app
          (ann tok)
          (Term.constructor (ann tok) (ConstructorReference DD.docRef DD.docEvaluateId))
          (Term.termLink (ann tok) (L.payload tok))
    source = do
      _ <- P.try (reserved "source")
      l <- link''
      pure $
        Term.app
          (ann l)
          (Term.constructor (ann l) (ConstructorReference DD.docRef DD.docSourceId))
          l
    link'' = either ty t <$> link'
      where
        t tok =
          Term.app
            (ann tok)
            (Term.constructor (ann tok) (ConstructorReference DD.linkRef DD.linkTermId))
            (Term.termLink (ann tok) (L.payload tok))
        ty tok =
          Term.app
            (ann tok)
            (Term.constructor (ann tok) (ConstructorReference DD.linkRef DD.linkTypeId))
            (Term.typeLink (ann tok) (L.payload tok))
    link = d <$> link''
      where
        d tm = Term.app (ann tm) (Term.constructor (ann tm) (ConstructorReference DD.docRef DD.docLinkId)) tm

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

-- Doc literal normalization
--
-- This normalization allows the pretty-printer and doc display code to do
-- indenting, and to do line-wrap of paragraphs, but without the inserted
-- newlines being then frozen into the text for ever more over subsequent
-- edit/update cycles.
--
-- The alternative would be to stop line-wrapping docs on view/display by adding
-- newlines in the pretty-printer, and instead leave wrapping to the
-- terminal/editor.  Might be worth considering if this code ends up being
-- too buggy and fragile to maintain.  Maybe display could add newlines,
-- and view could refrain from doing so.
--
-- Operates on the text of the Blobs within a doc (as parsed by docBlock):
-- - reduces the whitespace after all newlines so that at least one of the
--   non-initial lines has zero indent (important because the pretty-printer adds
--   indenting when displaying doc literals)
-- - removes trailing whitespace from each line
-- - removes newlines between any sequence of non-empty zero-indent lines
--   (i.e. undo line-breaking within paragraphs).
--
-- Should be understood in tandem with Util.Pretty.paragraphyText, which
-- outputs doc text for display/edit/view.
-- See also unison-src/transcripts/doc-formatting.md.
--
-- There is some heuristic/approximate logic in here - see the comment flagged
-- with ** below.
--
-- This function is a bit painful - it's trying to act on a sequence of lines,
-- but that sequence is split up between the various blobs in the doc, which
-- are separated by the elements tracking things like @[source] etc.  It
-- would be simplified if the doc representation was something like
-- [Either Char EnrichedElement].
--
-- This function has some tracing which you can enable by deleting some calls to
-- 'const id' below.
docNormalize :: (Ord v, Show v) => Term v a -> Term v a
docNormalize tm = case tm of
  -- This pattern is just `DD.DocJoin seqs`, but exploded in order to grab
  -- the annotations.  The aim is just to map `normalize` over it.
  a@(Term.App' c@(Term.Constructor' (ConstructorReference DD.DocRef DD.DocJoinId)) s@(Term.List' seqs)) ->
    join
      (ABT.annotation a)
      (ABT.annotation c)
      (ABT.annotation s)
      (normalize seqs)
    where

  _ -> error $ "unexpected doc structure: " ++ show tm
  where
    normalize =
      Sequence.fromList . (map TupleE.fst3)
        . (tracing "after unbreakParas")
        . unbreakParas
        . (tracing "after full preprocess")
        . preProcess
        . (tracing "after unindent")
        . unIndent
        . (tracing "initial parse")
        . miniPreProcess
    preProcess xs =
      zip3
        seqs
        (lineStarteds $ Sequence.fromList seqs)
        (followingLines $ Sequence.fromList seqs)
      where
        seqs = map fst xs
    miniPreProcess seqs = zip (toList seqs) (lineStarteds seqs)
    unIndent ::
      Ord v =>
      [(Term v a, UnbreakCase)] ->
      [(Term v a, UnbreakCase)]
    unIndent tms = map go tms
      where
        go (b, previous) =
          ((mapBlob $ (reduceIndent includeFirst minIndent)) b, previous)
          where
            -- Since previous was calculated before unindenting, it will often be wrongly
            -- StartsIndented instead of StartsUnindented - but that's OK just for the test
            -- below.  And we'll recalculate it later in preProcess.
            includeFirst = previous == LineEnds
        concatenatedBlobs :: Text
        concatenatedBlobs = mconcat (toList (fmap (getBlob . fst) tms))
        getBlob (DD.DocBlob txt) = txt
        getBlob _ = "."
        -- Note we exclude the first line when calculating the minimum indent - the lexer
        -- already stripped leading spaces from it, and anyway it would have been sharing
        -- its line with the [: and maybe other stuff.
        nonInitialNonEmptyLines =
          filter (not . Text.null) $
            map Text.stripEnd $
              drop 1 $
                Text.lines
                  concatenatedBlobs
        minIndent =
          minimumOrZero $
            map
              (Text.length . (Text.takeWhile Char.isSpace))
              nonInitialNonEmptyLines
        minimumOrZero xs = if length xs == 0 then 0 else minimum xs
        reduceIndent :: Bool -> Int -> Text -> Text
        reduceIndent includeFirst n t =
          fixup $
            Text.unlines $
              mapExceptFirst reduceLineIndent onFirst $
                Text.lines t
          where
            onFirst = if includeFirst then reduceLineIndent else id
            reduceLineIndent l = result
              where
                currentIndent = Text.length $ (Text.takeWhile Char.isSpace) l
                remainder = (Text.dropWhile Char.isSpace) l
                newIndent = maximum [0, currentIndent - n]
                result = Text.replicate newIndent " " `mappend` remainder
            -- unlines . lines adds a trailing newline if one was not present: undo that.
            fixup = if Text.takeEnd 1 t == "\n" then id else Text.dropEnd 1
    -- Remove newlines between any sequence of non-empty zero-indent lines.
    -- This is made more complicated by Doc elements (e.g. links) which break up a
    -- blob but don't break a line of output text**.  We sometimes need to refer back to the
    -- previous blob to see whether a newline is between two zero-indented lines.
    -- For example...
    -- "This link to @foo makes it harder to see\n
    --  that the newline should be removed."
    -- Whether an element does this (breaks a blob but not a line of output text) really
    -- depends on some things we don't know here: does an @[include] target doc occupy
    -- just one line or several; whether this doc is going to be viewed or displayed.
    -- So we'll get it wrong sometimes.  The impact of this is that we may sometimes
    -- misjudge whether a newline is separating two non-indented lines, and should therefore
    -- be removed.
    unbreakParas ::
      (Show v, Ord v) =>
      [(Term v a, UnbreakCase, Bool)] ->
      [(Term v a, UnbreakCase, Bool)]
    unbreakParas = map go
      where
        -- 'candidate' means 'candidate to be joined with an adjacent line as part of a
        -- paragraph'.
        go (b, previous, nextIsCandidate) =
          (mapBlob go b, previous, nextIsCandidate)
          where
            go txt = if Text.null txt then txt else tr result'
              where
                tr =
                  const id $
                    trace $
                      "\nprocessElement on blob " ++ (show txt) ++ ", result' = "
                        ++ (show result')
                        ++ ", lines: "
                        ++ (show ls)
                        ++ ", candidates = "
                        ++ (show candidates)
                        ++ ", previous = "
                        ++ (show previous)
                        ++ ", firstIsCandidate = "
                        ++ (show firstIsCandidate)
                        ++ "\n\n"
                -- remove trailing whitespace
                -- ls is non-empty thanks to the Text.null check above
                -- Don't cut the last line's trailing whitespace - there's an assumption here
                -- that it's followed by something which will put more text on the same line.
                ls = mapExceptLast Text.stripEnd id $ Text.lines txt
                -- Work out which lines are candidates to be joined as part of a paragraph, i.e.
                -- are not indented.
                candidate l = case Text.uncons l of
                  Just (initial, _) -> not . Char.isSpace $ initial
                  Nothing -> False -- empty line
                  -- The segment of this blob that runs up to the first newline may not itself
                  -- be the start of a line of the doc - for example if it's preceded by a link.
                  -- So work out whether the line of which it is a part is a candidate.
                firstIsCandidate = case previous of
                  LineEnds -> candidate (head ls)
                  StartsIndented -> False
                  StartsUnindented -> True
                candidates = firstIsCandidate : (tail (map candidate ls))
                result = mconcat $ intercalateMapWith sep fst (zip ls candidates)
                sep (_, candidate1) (_, candidate2) =
                  if candidate1 && candidate2 then " " else "\n"
                -- Text.lines forgets whether there was a trailing newline.
                -- If there was one, then either add it back or convert it to a space.
                result' =
                  if (Text.takeEnd 1 txt) == "\n"
                    then
                      if (last candidates) && nextIsCandidate
                        then result `Text.append` " "
                        else result `Text.append` "\n"
                    else result
    -- A list whose entries match those of tms.  `Nothing` is used for elements
    -- which just continue a line, and so need to be ignored when looking back
    -- for how the last line started.  Otherwise describes whether the last
    -- line of this entry is indented (or maybe terminated by a newline.)
    -- A value of `Nothing` protects ensuing text from having its leading
    -- whitespace removed by `unindent`.
    -- Note that some elements render over multiple lines when displayed.
    -- See test2 in transcript doc-formatting.md for an example of how
    -- this looks when there is whitespace immediately following @[source]
    -- or @[evaluate].
    lastLines :: Show v => Sequence.Seq (Term v a) -> [Maybe UnbreakCase]
    lastLines tms = (flip fmap) (toList tms) $ \case
      DD.DocBlob txt -> unbreakCase txt
      DD.DocLink _ -> Nothing
      DD.DocSource _ -> Nothing
      DD.DocSignature _ -> Nothing
      DD.DocEvaluate _ -> Nothing
      Term.Var' _ -> Nothing -- @[include]
      e@_ -> error ("unexpected doc element: " ++ show e)
    -- Work out whether the last line of this blob is indented (or maybe
    -- terminated by a newline.)
    unbreakCase :: Text -> Maybe UnbreakCase
    unbreakCase txt =
      let (startAndNewline, afterNewline) = Text.breakOnEnd "\n" txt
       in if Text.null startAndNewline
            then Nothing
            else
              if Text.null afterNewline
                then Just LineEnds
                else
                  if Char.isSpace (Text.head afterNewline)
                    then Just StartsIndented
                    else Just StartsUnindented
    -- A list whose entries match those of tms.  Describes how the current
    -- line started (the line including the start of this entry) - or LineEnds
    -- if this entry is starting a line itself.
    -- Calculated as the UnbreakCase of the previous entry that included a newline.
    -- Really there's a function of type (a -> Bool) -> a -> [a] -> [a] in here
    -- fighting to break free - overwriting elements that are 'shadowed' by
    -- a preceding element for which the predicate is true, with a copy of
    -- that element.
    lineStarteds :: Show v => Sequence.Seq (Term v a) -> [UnbreakCase]
    lineStarteds tms = tr $ quenchRuns LineEnds StartsUnindented $ xs''
      where
        tr =
          const id $
            trace $
              "lineStarteds: xs = " ++ (show xs) ++ ", xss = "
                ++ (show xss)
                ++ ", xs' = "
                ++ (show xs')
                ++ ", xs'' = "
                ++ (show xs'')
                ++ "\n\n"
        -- Make sure there's a Just at the start of the list so we always find
        -- one when searching back.
        -- Example: xs = [J1,N2,J3]
        xs :: [Maybe UnbreakCase]
        xs = Just LineEnds : (lastLines tms)
        -- Example: xss = [[J1],[J1,N2],[J1,N2,J3]]
        xss :: [[Maybe UnbreakCase]]
        xss = drop 1 $ List.inits xs
        -- Example: after each step of the map...
        --            [[J1],[N2,J1],[J3,N2,J1]]   -- after reverse
        --            [Just J1, Just J1, Just J3] -- after find
        --            ...
        --   result = [1,1,3]
        xs' =
          map (Maybe.fromJust . Maybe.fromJust . (List.find isJust) . reverse) xss
        xs'' = List.Extra.dropEnd 1 xs'
    -- For each element, can it be a line-continuation of a preceding blob?
    continuesLine :: Sequence.Seq (Term v a) -> [Bool]
    continuesLine tms = (flip fmap) (toList tms) $ \case
      DD.DocBlob _ -> False -- value doesn't matter - you don't get adjacent blobs
      DD.DocLink _ -> True
      DD.DocSource _ -> False
      DD.DocSignature _ -> False
      DD.DocEvaluate _ -> False
      Term.Var' _ -> False -- @[include]
      _ -> error ("unexpected doc element" ++ show tm)
    -- A list whose entries match those of tms.  Can the subsequent entry by a
    -- line continuation of this one?
    followingLines tms = drop 1 ((continuesLine tms) ++ [False])
    mapExceptFirst :: (a -> b) -> (a -> b) -> [a] -> [b]
    mapExceptFirst fRest fFirst = \case
      [] -> []
      x : rest -> (fFirst x) : (map fRest rest)
    mapExceptLast fRest fLast = reverse . (mapExceptFirst fRest fLast) . reverse
    tracing :: Show a => [Char] -> a -> a
    tracing when x =
      (const id $ trace ("at " ++ when ++ ": " ++ (show x) ++ "\n")) x
    blob aa ac at txt =
      Term.app aa (Term.constructor ac (ConstructorReference DD.docRef DD.docBlobId)) (Term.text at txt)
    join aa ac as segs =
      Term.app aa (Term.constructor ac (ConstructorReference DD.docRef DD.docJoinId)) (Term.list' as segs)
    mapBlob :: Ord v => (Text -> Text) -> Term v a -> Term v a
    -- this pattern is just `DD.DocBlob txt` but exploded to capture the annotations as well
    mapBlob f (aa@(Term.App' ac@(Term.Constructor' (ConstructorReference DD.DocRef DD.DocBlobId)) at@(Term.Text' txt))) =
      blob (ABT.annotation aa) (ABT.annotation ac) (ABT.annotation at) (f txt)
    mapBlob _ t = t

delayQuote :: Var v => TermP v
delayQuote = P.label "quote" $ do
  start <- reserved "'"
  e <- termLeaf
  pure $ DD.delayTerm (ann start <> ann e) e

delayBlock :: Var v => TermP v
delayBlock = P.label "do" $ do
  b <- block "do"
  pure $ DD.delayTerm (ann b) b

bang :: Var v => TermP v
bang = P.label "bang" $ do
  start <- reserved "!"
  e <- termLeaf
  pure $ DD.forceTerm (ann start <> ann e) (ann start) e

var :: Var v => L.Token v -> Term v Ann
var t = Term.var (ann t) (L.payload t)

seqOp :: Ord v => P v Pattern.SeqOp
seqOp =
  (Pattern.Snoc <$ matchToken (L.SymbolyId ":+" Nothing))
    <|> (Pattern.Cons <$ matchToken (L.SymbolyId "+:" Nothing))
    <|> (Pattern.Concat <$ matchToken (L.SymbolyId "++" Nothing))

term4 :: Var v => TermP v
term4 = f <$> some termLeaf
  where
    f (func : args) = Term.apps func ((\a -> (ann func <> ann a, a)) <$> args)
    f [] = error "'some' shouldn't produce an empty list"

-- e.g. term4 + term4 - term4
-- or term4 || term4 && term4
infixAppOrBooleanOp :: Var v => TermP v
infixAppOrBooleanOp = chainl1 term4 (or <|> and <|> infixApp)
  where
    or = orf <$> label "or" (reserved "||")
    orf op lhs rhs = Term.or (ann lhs <> ann op <> ann rhs) lhs rhs
    and = andf <$> label "and" (reserved "&&")
    andf op lhs rhs = Term.and (ann lhs <> ann op <> ann rhs) lhs rhs
    infixApp = infixAppf <$> label "infixApp" (hashQualifiedInfixTerm <* optional semi)
    infixAppf op lhs rhs = Term.apps' op [lhs, rhs]

typedecl :: Var v => P v (L.Token v, Type v Ann)
typedecl =
  (,) <$> P.try (prefixDefinitionName <* reserved ":")
    <*> TypeParser.valueType
    <* semi

verifyRelativeVarName :: Var v => P v (L.Token v) -> P v (L.Token v)
verifyRelativeVarName p = do
  v <- p
  verifyRelativeName' (Name.unsafeFromVar <$> v)
  pure v

verifyRelativeName :: Ord v => P v (L.Token Name) -> P v (L.Token Name)
verifyRelativeName name = do
  name <- name
  verifyRelativeName' name
  pure name

verifyRelativeName' :: Ord v => L.Token Name -> P v ()
verifyRelativeName' name = do
  let txt = Name.toText . L.payload $ name
  when (Text.isPrefixOf "." txt && txt /= ".") $
    failCommitted (DisallowedAbsoluteName name)

-- example:
--   (x, y)   = foo
--   hd +: tl | hd < 10 = [1,2,3]
--   stuff
--
-- desugars to:
--
--   match foo with
--     (x,y) -> match [1,2,3] with
--       hd +: tl | hd < 10 -> stuff
--
destructuringBind :: forall v. Var v => P v (Ann, Term v Ann -> Term v Ann)
destructuringBind = do
  -- We have to look ahead as far as the `=` to know if this is a bind or
  -- just an action, for instance:
  --   Some 42
  --   vs
  --   Some 42 = List.head elems
  (p, boundVars, guard) <- P.try $ do
    (p, boundVars) <- parsePattern
    let boundVars' = snd <$> boundVars
    guard <- optional $ reserved "|" *> infixAppOrBooleanOp
    P.lookAhead (openBlockWith "=")
    pure (p, boundVars', guard)
  scrute <- block "=" -- Dwight K. Scrute ("The People's Scrutinee")
  let absChain vs t = foldr (\v t -> ABT.abs' (ann t) v t) t vs
      thecase t = Term.MatchCase p (fmap (absChain boundVars) guard) $ absChain boundVars t
  pure $
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
binding :: forall v. Var v => P v ((Ann, v), Term v Ann)
binding = label "binding" do
  typ <- optional typedecl
  -- a ++ b = ...
  let infixLhs = do
        (arg1, op) <-
          P.try $
            (,) <$> prefixDefinitionName <*> infixDefinitionName
        arg2 <- prefixDefinitionName
        pure (ann arg1, op, [arg1, arg2])
  let prefixLhs = do
        v <- prefixDefinitionName
        vs <- many prefixDefinitionName
        pure (ann v, v, vs)
  let lhs :: P v (Ann, L.Token v, [L.Token v])
      lhs = infixLhs <|> prefixLhs
  case typ of
    Nothing -> do
      -- we haven't seen a type annotation, so lookahead to '=' before commit
      (lhsLoc, name, args) <- P.try (lhs <* P.lookAhead (openBlockWith "="))
      body <- block "="
      verifyRelativeName' (fmap Name.unsafeFromVar name)
      pure $ mkBinding (lhsLoc <> ann body) (L.payload name) args body
    Just (nameT, typ) -> do
      (lhsLoc, name, args) <- lhs
      verifyRelativeName' (fmap Name.unsafeFromVar name)
      when (L.payload name /= L.payload nameT) $
        customFailure $ SignatureNeedsAccompanyingBody nameT
      body <- block "="
      pure $
        fmap
          (\e -> Term.ann (ann nameT <> ann e) e typ)
          (mkBinding (ann lhsLoc <> ann body) (L.payload name) args body)
  where
    mkBinding loc f [] body = ((loc, f), body)
    mkBinding loc f args body =
      ((loc, f), Term.lam' (loc <> ann body) (L.payload <$> args) body)

customFailure :: P.MonadParsec e s m => e -> m a
customFailure = P.customFailure

block :: forall v. Var v => String -> TermP v
block s = block' False s (openBlockWith s) closeBlock

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
importp :: Ord v => P v [(Name, Name)]
importp = do
  kw <- reserved "use"
  -- we allow symbolyId here and parse the suffix optionaly, so we can generate
  -- a nicer error message if the suffixes are empty
  prefix <-
    optional $
      fmap Right (importWordyId <|> importDotId) -- use . Nat
        <|> fmap Left importSymbolyId
  suffixes <- optional (some (importWordyId <|> importSymbolyId))
  case (prefix, suffixes) of
    (Nothing, _) -> P.customFailure $ UseEmpty kw
    (Just prefix@(Left _), _) -> P.customFailure $ UseInvalidPrefixSuffix prefix suffixes
    (Just (Right prefix), Nothing) -> do
      -- `wildcard import`
      names <- asks names
      pure $ Names.expandWildcardImport (L.payload prefix) (NamesWithHistory.currentNames names)
    (Just (Right prefix), Just suffixes) -> pure $ do
      suffix <- L.payload <$> suffixes
      pure (suffix, Name.joinDot (L.payload prefix) suffix)

data BlockElement v
  = Binding ((Ann, v), Term v Ann)
  | DestructuringBind (Ann, Term v Ann -> Term v Ann)
  | Action (Term v Ann)

instance Show v => Show (BlockElement v) where
  show (Binding ((pos, name), _)) = show ("binding: " :: Text, pos, name)
  show (DestructuringBind (pos, _)) = show ("destructuring bind: " :: Text, pos)
  show (Action tm) = show ("action: " :: Text, ann tm)

-- subst
-- use Foo.Bar + blah
-- use Bar.Baz zonk zazzle
imports :: Var v => P v (NamesWithHistory, [(v, v)])
imports = do
  let sem = P.try (semi <* P.lookAhead (reserved "use"))
  imported <- mconcat . reverse <$> sepBy sem importp
  ns' <- NamesWithHistory.importing imported <$> asks names
  pure (ns', [(Name.toVar suffix, Name.toVar full) | (suffix, full) <- imported])

-- A key feature of imports is we want to be able to say:
-- `use foo.bar Baz qux` without having to specify whether `Baz` or `qux` are
-- terms or types.
substImports :: Var v => NamesWithHistory -> [(v, v)] -> Term v Ann -> Term v Ann
substImports ns imports =
  ABT.substsInheritAnnotation
    [ (suffix, Term.var () full)
      | (suffix, full) <- imports
    ]
    . Term.substTypeVars -- no guard here, as `full` could be bound
    -- not in Names, but in a later term binding
      [ (suffix, Type.var () full)
        | (suffix, full) <- imports,
          NamesWithHistory.hasTypeNamed (Name.unsafeFromVar full) ns
      ]

block' :: Var v => IsTop -> String -> P v (L.Token ()) -> P v (L.Token ()) -> TermP v
block' isTop = block'' isTop False

block'' ::
  forall v b.
  Var v =>
  IsTop ->
  Bool -> -- `True` means insert `()` at end of block if it ends with a statement
  String ->
  P v (L.Token ()) ->
  P v b ->
  TermP v
block'' isTop implicitUnitAtEnd s openBlock closeBlock = do
  open <- openBlock
  (names, imports) <- imports
  _ <- optional semi
  statements <- local (\e -> e {names = names}) $ sepBy semi statement
  _ <- closeBlock
  theBlock <- substImports names imports <$> go open statements
  pure $ ABT.annotate (ann open <> ann theBlock) theBlock
  where
    statement = asum [Binding <$> binding, DestructuringBind <$> destructuringBind, Action <$> blockTerm]
    go :: L.Token () -> [BlockElement v] -> P v (Term v Ann)
    go open bs =
      let finish :: Term.Term v Ann -> TermP v
          finish tm = case Components.minimize' tm of
            Left dups -> customFailure $ DuplicateTermNames (toList dups)
            Right tm -> pure tm
          toTm :: [BlockElement v] -> TermP v
          toTm [] = customFailure $ EmptyBlock (const s <$> open)
          toTm (be : bes) = do
            let (bs, blockResult) = determineBlockResult (be :| bes)
            finish =<< foldrM step blockResult bs
            where
              step :: BlockElement v -> Term v Ann -> TermP v
              step elem result = case elem of
                Binding ((a, v), tm) ->
                  pure $
                    Term.consLetRec
                      isTop
                      (ann a <> ann result)
                      (a, v, tm)
                      result
                Action tm ->
                  pure $
                    Term.consLetRec
                      isTop
                      (ann tm <> ann result)
                      (ann tm, positionalVar (ann tm) (Var.named "_"), tm)
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
       in toTm bs

number :: Var v => TermP v
number = number' (tok Term.int) (tok Term.nat) (tok Term.float)

bytes :: Var v => TermP v
bytes = do
  b <- bytesToken
  let a = ann b
  pure $
    Term.app
      a
      (Term.builtin a "Bytes.fromList")
      (Term.list a $ Term.nat a . fromIntegral <$> Bytes.toWord8s (L.payload b))

number' ::
  Ord v =>
  (L.Token Int64 -> a) ->
  (L.Token Word64 -> a) ->
  (L.Token Double -> a) ->
  P v a
number' i u f = fmap go numeric
  where
    go num@(L.payload -> p)
      | any (\c -> c == '.' || c == 'e') p && take 1 p == "+" = f (read . drop 1 <$> num)
      | any (\c -> c == '.' || c == 'e') p = f (read <$> num)
      | take 1 p == "+" = i (read . drop 1 <$> num)
      | take 1 p == "-" = i (read <$> num)
      | otherwise = u (read <$> num)

tupleOrParenthesizedTerm :: Var v => TermP v
tupleOrParenthesizedTerm = label "tuple" $ tupleOrParenthesized term DD.unitTerm pair
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
