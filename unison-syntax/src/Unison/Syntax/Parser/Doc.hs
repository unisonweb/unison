module Unison.Syntax.Parser.Doc
  ( Tree,
    untitledSection,
    sectionElem,
    leaf,

    -- * section elements
    section,
    eval,
    exampleBlock,
    codeBlock,
    list,
    bulletedList,
    numberedList,
    paragraph,

    -- * leaves
    link,
    namedLink,
    example,
    transclude,
    bold,
    italic,
    strikethrough,
    verbatim,
    source,
    foldedSource,
    evalInline,
    signatures,
    signatureInline,
    group,
    word,

    -- * other components
    column',
    embedTypeLink,
    embedTermLink,
    embedSignatureLink,
    join,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad.State qualified as S
import Data.Char (isControl, isSpace)
import Data.List qualified as List
import Data.List.Extra qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char qualified as CP
import Text.Megaparsec.Char.Lexer qualified as LP
import Unison.Parser.Ann (Ann, Annotated (..))
import Unison.Prelude hiding (join)
import Unison.Syntax.Lexer
  ( P,
    ParsingEnv (..),
    column,
    identifierP,
    line,
    lit,
    local,
    sepBy1',
    separated,
    some',
    someTill',
    typeOrAbilityAlt,
    wordySep,
    (<+>),
  )
import Unison.Syntax.Lexer.Token (Token (Token), posP, tokenP)
import Unison.Syntax.Parser.Doc.Data

type Tree code = Cofree (Top code) Ann

-- | This is the actual `Doc` lexer. Unlike `doc2`, it doesn’t do any Unison-side lexing (i.e., it doesn’t know that
--   Unison wraps `Doc` literals in `}}`).
untitledSection :: P a -> P (UntitledSection a)
untitledSection a = UntitledSection <$> P.many (a <* CP.space)

wordyKw :: String -> P String
wordyKw kw = separated wordySep (lit kw)

sectionElem :: (Annotated code) => (P () -> P code) -> P () -> P (Tree code)
sectionElem code docClose =
  fmap wrap' $
    section code docClose
      <|> P.label "block eval (syntax: a fenced code block)" (eval code <|> exampleBlock code <|> codeBlock)
      <|> list code docClose
      <|> paragraph code docClose

paragraph :: (Annotated code) => (P () -> P code) -> P () -> P (Top code (Tree code))
paragraph code = fmap Paragraph . spaced . leafy code

word :: P end -> P (Leaf code void)
word closing = fmap Word . tokenP . P.try $ do
  let end = P.lookAhead $ void (P.satisfy isSpace) <|> void closing
  word <- P.manyTill (P.satisfy (\ch -> not (isSpace ch))) end
  guard (not $ reserved word || null word)
  pure word
  where
    reserved word = List.isPrefixOf "}}" word || all (== '#') word

leaf :: (Annotated code) => (P () -> P code) -> P () -> P (Leaf code (Tree code))
leaf code closing =
  do
    link
      <|> namedLink code closing
      <|> example code
      <|> transclude code
    <|> bold code closing
    <|> italic code closing
    <|> strikethrough code closing
    <|> verbatim
    <|> source code
    <|> foldedSource code
    <|> evalInline code
    <|> signatures
    <|> signatureInline
    <|> word closing

leafy :: (Annotated code) => (P () -> P code) -> P () -> P (Leaf code (Tree code))
leafy code closing = do
  p <- leaf code closing
  after <- P.optional . P.try $ leafy code closing
  case after of
    Nothing -> pure p
    Just after -> group . pure $ p :| pure after

comma :: P String
comma = lit "," <* CP.space

source :: (P () -> P code) -> P (Leaf code a)
source = fmap Source . (lit "@source" *>) . sourceElements

foldedSource :: (P () -> P code) -> P (Leaf code a)
foldedSource = fmap FoldedSource . (lit "@foldedSource" *>) . sourceElements

sourceElements :: (P () -> P code) -> P (NonEmpty (SourceElement (Leaf code Void)))
sourceElements code = do
  _ <- (lit " {" <|> lit "{") *> CP.space
  s <- sepBy1' srcElem comma
  _ <- lit "}"
  pure s
  where
    srcElem =
      SourceElement
        <$> embedLink
        <*> ( fmap (fromMaybe []) . P.optional $
                (lit "@") *> (CP.space *> annotations)
            )
      where
        annotation = fmap Left (tokenP identifierP) <|> fmap Right (transclude code) <* CP.space
        annotations =
          P.some (EmbedAnnotation <$> annotation)

signatures :: P (Leaf code a)
signatures = fmap Signature $ do
  _ <- (lit "@signatures" <|> lit "@signature") *> (lit " {" <|> lit "{") *> CP.space
  s <- sepBy1' embedSignatureLink comma
  _ <- lit "}"
  pure s

signatureInline :: P (Leaf code a)
signatureInline = fmap SignatureInline $ do
  _ <- lit "@inlineSignature" *> (lit " {" <|> lit "{") *> CP.space
  s <- embedSignatureLink
  _ <- lit "}"
  pure s

evalInline :: (P () -> P a1) -> P (Leaf a1 a2)
evalInline code = fmap EvalInline $ do
  _ <- lit "@eval" *> (lit " {" <|> lit "{") *> CP.space
  let inlineEvalClose = void $ lit "}"
  s <- code inlineEvalClose
  pure s

embedTypeLink :: P EmbedLink
embedTypeLink =
  EmbedTypeLink <$> do
    _ <- typeOrAbilityAlt (wordyKw . Text.unpack) <* CP.space
    tokenP identifierP <* CP.space

embedTermLink :: P EmbedLink
embedTermLink = EmbedTermLink <$> tokenP identifierP <* CP.space

embedSignatureLink :: P EmbedSignatureLink
embedSignatureLink = EmbedSignatureLink <$> tokenP identifierP <* CP.space

verbatim :: P (Leaf code a)
verbatim =
  P.label "code (examples: ''**unformatted**'', `words` or '''_words_''')" $ do
    Token originalText start stop <- tokenP do
      -- a single backtick followed by a non-backtick is treated as monospaced
      let tick = P.try (lit "`" <* P.lookAhead (P.satisfy (/= '`')))
      -- also two or more ' followed by that number of closing '
      quotes <- tick <|> (lit "''" <+> P.takeWhileP Nothing (== '\''))
      P.someTill P.anySingle (lit quotes)
    let isMultiLine = line start /= line stop
    if isMultiLine
      then do
        let trimmed = (trimAroundDelimiters originalText)
        let txt = trimIndentFromVerbatimBlock (column start - 1) trimmed
        -- If it's a multi-line verbatim block we trim any whitespace representing
        -- indentation from the pretty-printer. See 'trimIndentFromVerbatimBlock'
        pure . Verbatim $
          Word $
            Token txt start stop
      else
        pure . Code $
          Word $
            Token originalText start stop

example :: (P () -> P code) -> P (Leaf code void)
example code =
  P.label "inline code (examples: ``List.map f xs``, ``[1] :+ 2``)" $
    fmap Example $ do
      n <- P.try $ do
        _ <- lit "`"
        length <$> P.takeWhile1P (Just "backticks") (== '`')
      let end = void . lit $ replicate (n + 1) '`'
      CP.space *> code end

link :: P (Leaf a b)
link = P.label "link (examples: {type List}, {Nat.+})" $ Link <$> P.try (lit "{" *> embedLink <* lit "}")

transclude :: (P () -> P code) -> P (Leaf code x)
transclude code =
  fmap Transclude . P.label "transclusion (examples: {{ doc2 }}, {{ sepBy s [doc1, doc2] }})" $
    lit "{{" *> code (void $ lit "}}")

nonNewlineSpaces :: P String
nonNewlineSpaces = P.takeWhileP Nothing nonNewlineSpace
  where
    nonNewlineSpace ch = isSpace ch && ch /= '\n' && ch /= '\r'

eval :: (Annotated code) => (P () -> P code) -> P (Top code (Tree code))
eval code =
  Eval <$> do
    -- commit after seeing that ``` is on its own line
    fence <- P.try $ do
      fence <- lit "```" <+> P.takeWhileP Nothing (== '`')
      b <- all isSpace <$> P.lookAhead (P.takeWhileP Nothing (/= '\n'))
      fence <$ guard b
    CP.space *> code (void $ lit fence)

exampleBlock :: (Annotated code) => (P () -> P code) -> P (Top code (Tree code))
exampleBlock code =
  ExampleBlock
    <$> do
      void $ lit "@typecheck" <* CP.space
      fence <- lit "```" <+> P.takeWhileP Nothing (== '`')
      code . void $ lit fence

codeBlock :: P (Top code (Tree code))
codeBlock = do
  column <- (\x -> x - 1) . toInteger . P.unPos <$> LP.indentLevel
  let tabWidth = toInteger . P.unPos $ P.defaultTabWidth
  fence <- lit "```" <+> P.takeWhileP Nothing (== '`')
  name <-
    nonNewlineSpaces
      *> tokenP (P.takeWhile1P Nothing (not . isSpace))
      <* nonNewlineSpaces
  _ <- void CP.eol
  verbatim <-
    tokenP $
      uncolumn column tabWidth . trimAroundDelimiters
        <$> P.someTill P.anySingle ([] <$ lit fence)
  pure $ CodeBlock name verbatim
  where
    uncolumn column tabWidth s =
      let skip col r | col < 1 = r
          skip col s@('\t' : _) | col < tabWidth = s
          skip col ('\t' : r) = skip (col - tabWidth) r
          skip col (c : r)
            | isSpace c && (not $ isControl c) =
                skip (col - 1) r
          skip _ s = s
       in List.intercalate "\n" $ skip column <$> lines s

emphasis :: (Annotated code) => Char -> (P () -> P code) -> P () -> P (Tree code)
emphasis delimiter code closing = do
  let start = some (P.satisfy (== delimiter))
  end <- P.try $ do
    end <- start
    P.lookAhead (P.satisfy (not . isSpace))
    pure end
  wrap' . Paragraph
    <$> someTill'
      (leafy code (closing <|> (void $ lit end)) <* void whitespaceWithoutParagraphBreak)
      (lit end)
  where
    -- Allows whitespace or a newline, but not more than two newlines in a row.
    whitespaceWithoutParagraphBreak :: P ()
    whitespaceWithoutParagraphBreak = void do
      void nonNewlineSpaces
      optional newline >>= \case
        Just _ -> void nonNewlineSpaces
        Nothing -> pure ()

bold :: (Annotated code) => (P () -> P code) -> P () -> P (Leaf code (Tree code))
bold code = fmap Bold . emphasis '*' code

italic :: (Annotated code) => (P () -> P code) -> P () -> P (Leaf code (Tree code))
italic code = fmap Italic . emphasis '_' code

strikethrough :: (Annotated code) => (P () -> P code) -> P () -> P (Leaf code (Tree code))
strikethrough code = fmap Strikethrough . emphasis '~' code

namedLink :: (Annotated code) => (P () -> P code) -> P () -> P (Leaf code (Tree code))
namedLink code docClose =
  P.label "hyperlink (example: [link name](https://destination.com))" do
    _ <- lit "["
    p <- spaced . leafy code . void $ char ']'
    _ <- lit "]"
    _ <- lit "("
    target <- group $ fmap pure link <|> some' (transclude code <|> word (docClose <|> void (char ')')))
    _ <- lit ")"
    pure $ NamedLink (wrap' $ Paragraph p) target

sp :: P String
sp = P.try $ do
  spaces <- P.takeWhile1P (Just "space") isSpace
  close <- P.optional (P.lookAhead (lit "}}"))
  case close of
    Nothing -> guard $ ok spaces
    Just _ -> pure ()
  pure spaces
  where
    ok s = length [() | '\n' <- s] < 2

spaced :: P a -> P (NonEmpty a)
spaced p = some' (p <* P.optional sp)

-- | Not an actual node, but this pattern is referenced in multiple places
list :: (Annotated code) => (P () -> P code) -> P () -> P (Top code (Tree code))
list code docClose = bulletedList code docClose <|> numberedList code docClose

listSep :: P ()
listSep = P.try $ newline *> nonNewlineSpaces *> P.lookAhead (void bulletedStart <|> void numberedStart)

bulletedStart :: P (Int, [a])
bulletedStart = P.try $ do
  r <- listItemStart $ [] <$ P.satisfy bulletChar
  P.lookAhead (P.satisfy isSpace)
  pure r
  where
    bulletChar ch = ch == '*' || ch == '-' || ch == '+'

listItemStart :: P a -> P (Int, a)
listItemStart gutter = P.try $ do
  nonNewlineSpaces
  col <- column <$> posP
  parentCol <- S.gets parentListColumn
  guard (col > parentCol)
  (col,) <$> gutter

numberedStart :: P (Int, Token Word64)
numberedStart = listItemStart $ P.try (tokenP $ LP.decimal <* lit ".")

-- | FIXME: This should take a @`P` a@
numberedList :: (Annotated code) => (P () -> P code) -> P () -> P (Top code (Tree code))
numberedList code docClose = NumberedList <$> sepBy1' numberedItem listSep
  where
    numberedItem = P.label "numbered list (examples: 1. item1, 8. start numbering at '8')" do
      (col, s) <- numberedStart
      (s,) <$> column' code docClose col

-- | FIXME: This should take a @`P` a@
bulletedList :: (Annotated code) => (P () -> P code) -> P () -> P (Top code (Tree code))
bulletedList code docClose = BulletedList <$> sepBy1' bullet listSep
  where
    bullet = P.label "bullet (examples: * item1, - item2)" do
      (col, _) <- bulletedStart
      column' code docClose col

column' :: (Annotated code) => (P () -> P code) -> P () -> Int -> P (Column (Tree code))
column' code docClose col =
  Column . wrap'
    <$> (nonNewlineSpaces *> listItemParagraph)
    <*> local (\e -> e {parentListColumn = col}) (P.optional $ listSep *> fmap wrap' (list code docClose))
  where
    listItemParagraph =
      Paragraph <$> do
        col <- column <$> posP
        some' (leafy code docClose <* sep col)
      where
        -- Trickiness here to support hard line breaks inside of
        -- a bulleted list, so for instance this parses as expected:
        --
        --   * uno dos
        --     tres quatro
        --   * alice bob
        --     carol dave eve
        sep col = do
          _ <- nonNewlineSpaces
          _ <-
            P.optional . P.try $
              newline
                *> nonNewlineSpaces
                *> do
                  col2 <- column <$> posP
                  guard $ col2 >= col
                  (P.notFollowedBy $ void numberedStart <|> void bulletedStart)
          pure ()

newline :: P String
newline = P.label "newline" $ lit "\n" <|> lit "\r\n"

-- |
--
-- > ## Section title
-- >
-- > A paragraph under this section.
-- > Part of the same paragraph. Blanklines separate paragraphs.
-- >
-- > ### A subsection title
-- >
-- > A paragraph under this subsection.
-- >
-- > # A section title (not a subsection)
section :: (Annotated code) => (P () -> P code) -> P () -> P (Top code (Tree code))
section code docClose = do
  ns <- S.gets parentSections
  hashes <- P.try $ lit (replicate (head ns) '#') *> P.takeWhile1P Nothing (== '#') <* sp
  title <- paragraph code docClose <* CP.space
  let m = length hashes + head ns
  body <-
    local (\env -> env {parentSections = (m : (tail ns))}) $
      P.many (sectionElem code docClose <* CP.space)
  pure $ Section (wrap' title) body

-- | Not an actual node, but this pattern is referenced in multiple places
embedLink :: P EmbedLink
embedLink = embedTypeLink <|> embedTermLink

-- | FIXME: This should just take a @`P` code@ and @`P` a@.
group :: P (NonEmpty (Leaf code a)) -> P (Leaf code a)
group = fmap Group . join

-- | FIXME: This should just take a @`P` a@
join :: P (NonEmpty a) -> P (Join a)
join = fmap Join

-- * utility functions

wrap' :: (Annotated code) => Top code (Tree code) -> Tree code
wrap' doc = ann doc :< doc

-- | If it's a multi-line verbatim block we trim any whitespace representing
-- indentation from the pretty-printer.
--
-- E.g.
--
-- @@
-- {{
--   # Heading
--     '''
--     code
--       indented
--     '''
-- }}
-- @@
--
-- Should lex to the text literal "code\n  indented".
--
-- If there's text in the literal that has LESS trailing whitespace than the
-- opening delimiters, we don't trim it at all. E.g.
--
-- @@
-- {{
--   # Heading
--     '''
--   code
--     '''
-- }}
-- @@
--
--  Is parsed as "  code".
--
--  Trim the expected amount of whitespace from a text literal:
--  >>> trimIndentFromVerbatimBlock 2 "  code\n    indented"
-- "code\n  indented"
--
-- If the text literal has less leading whitespace than the opening delimiters,
-- leave it as-is
-- >>> trimIndentFromVerbatimBlock 2 "code\n  indented"
-- "code\n  indented"
trimIndentFromVerbatimBlock :: Int -> String -> String
trimIndentFromVerbatimBlock leadingSpaces txt = fromMaybe txt $ do
  List.intercalate "\n" <$> for (lines txt) \line -> do
    -- If any 'stripPrefix' fails, we fail and return the unaltered text
    case List.stripPrefix (replicate leadingSpaces ' ') line of
      Just stripped -> Just stripped
      Nothing ->
        -- If it was a line with all white-space, just use an empty line,
        -- this can happen easily in editors which trim trailing whitespace.
        if all isSpace line
          then Just ""
          else Nothing

-- | Trim leading/trailing whitespace from around delimiters, e.g.
--
-- {{
--   '''___ <- whitespace here including newline
--   text block
-- 👇 or here
-- __'''
-- }}
-- >>> trimAroundDelimiters "  \n  text block \n  "
-- "  text block "
--
-- Should leave leading and trailing line untouched if it contains non-whitespace, e.g.:
--
-- '''  leading whitespace
--   text block
-- trailing whitespace:  '''
-- >>> trimAroundDelimiters "  leading whitespace\n  text block \ntrailing whitespace:  "
-- "  leading whitespace\n  text block \ntrailing whitespace:  "
--
-- Should keep trailing newline if it's the only thing on the line, e.g.:
--
-- '''
-- newline below
--
-- '''
-- >>> trimAroundDelimiters "\nnewline below\n\n"
-- "newline below\n\n"
trimAroundDelimiters :: String -> String
trimAroundDelimiters txt =
  txt
    & ( \s ->
          List.breakOn "\n" s
            & \case
              (prefix, suffix)
                | all isSpace prefix -> drop 1 suffix
                | otherwise -> prefix <> suffix
      )
    & ( \s ->
          List.breakOnEnd "\n" s
            & \case
              (_prefix, "") -> s
              (prefix, suffix)
                | all isSpace suffix -> dropTrailingNewline prefix
                | otherwise -> prefix <> suffix
      )
  where
    dropTrailingNewline = \case
      [] -> []
      (x : xs) -> NonEmpty.init (x NonEmpty.:| xs)
