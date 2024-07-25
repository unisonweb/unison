module Unison.Syntax.Parser.Doc where

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
import Unison.Prelude
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
untitledSection :: forall code. (Annotated code) => (P () -> P code) -> P () -> P (UntitledSection (Tree code))
untitledSection code docClose = UntitledSection <$> P.many (sectionElem <* CP.space)
  where
    wordyKw kw = separated wordySep (lit kw)
    sectionElem = section <|> fencedBlock <|> list <|> paragraph
    paragraph = wrap' . Paragraph <$> spaced leaf
    reserved word = List.isPrefixOf "}}" word || all (== '#') word

    wordy :: P end -> P (Leaf code void)
    wordy closing = fmap Word . tokenP . P.try $ do
      let end =
            P.lookAhead $
              docClose
                <|> void (P.satisfy isSpace)
                <|> void closing
      word <- P.manyTill (P.satisfy (\ch -> not (isSpace ch))) end
      guard (not $ reserved word || null word)
      pure word

    leafy closing = groupy closing gs
      where
        gs =
          link
            <|> externalLink
            <|> exampleInline
            <|> expr
            <|> boldOrItalicOrStrikethrough closing
            <|> verbatim
            <|> atDoc
            <|> wordy closing

    leaf = leafy mzero

    atDoc = src <|> evalInline <|> signature <|> signatureInline
      where
        comma = lit "," <* CP.space
        src =
          src' Source "@source"
            <|> src' FoldedSource "@foldedSource"
        srcElem =
          SourceElement
            <$> (typeLink <|> termLink)
            <*> ( fmap (fromMaybe []) . P.optional $
                    (lit "@") *> (CP.space *> annotations)
                )
          where
            annotation = fmap Left (tokenP identifierP) <|> fmap Right expr <* CP.space
            annotations =
              P.some (EmbedAnnotation <$> annotation)
        src' name atName = fmap name $ do
          _ <- lit atName *> (lit " {" <|> lit "{") *> CP.space
          s <- sepBy1' srcElem comma
          _ <- lit "}"
          pure s
        signature = fmap Signature $ do
          _ <- (lit "@signatures" <|> lit "@signature") *> (lit " {" <|> lit "{") *> CP.space
          s <- sepBy1' signatureLink comma
          _ <- lit "}"
          pure s
        signatureInline = fmap SignatureInline $ do
          _ <- lit "@inlineSignature" *> (lit " {" <|> lit "{") *> CP.space
          s <- signatureLink
          _ <- lit "}"
          pure s
        evalInline = fmap EvalInline $ do
          _ <- lit "@eval" *> (lit " {" <|> lit "{") *> CP.space
          let inlineEvalClose = () <$ lit "}"
          s <- code inlineEvalClose
          pure s

    typeLink = fmap EmbedTypeLink $ do
      _ <- typeOrAbilityAlt (wordyKw . Text.unpack) <* CP.space
      tokenP identifierP <* CP.space

    termLink =
      fmap EmbedTermLink $
        tokenP identifierP <* CP.space

    signatureLink =
      fmap EmbedSignatureLink $
        tokenP identifierP <* CP.space

    groupy closing p = do
      Token p _ _ <- tokenP p
      after <- P.optional . P.try $ leafy closing
      pure $ case after of
        Nothing -> p
        Just after ->
          Group
            . Join
            $ p
              :| pure after

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

    exampleInline =
      P.label "inline code (examples: ``List.map f xs``, ``[1] :+ 2``)" $
        fmap Example $ do
          n <- P.try $ do
            _ <- lit "`"
            length <$> P.takeWhile1P (Just "backticks") (== '`')
          let end = () <$ lit (replicate (n + 1) '`')
          ex <- CP.space *> code end
          pure ex

    link =
      P.label "link (examples: {type List}, {Nat.+})" $
        fmap Link $
          P.try $
            lit "{" *> (typeLink <|> termLink) <* lit "}"

    expr :: P (Leaf code x)
    expr =
      fmap Transclude . P.label "transclusion (examples: {{ doc2 }}, {{ sepBy s [doc1, doc2] }})" $
        lit "{{" *> code (() <$ lit "}}")

    nonNewlineSpace ch = isSpace ch && ch /= '\n' && ch /= '\r'
    nonNewlineSpaces = P.takeWhileP Nothing nonNewlineSpace

    -- Allows whitespace or a newline, but not more than two newlines in a row.
    whitespaceWithoutParagraphBreak :: P ()
    whitespaceWithoutParagraphBreak = void do
      void nonNewlineSpaces
      optional newline >>= \case
        Just _ -> void nonNewlineSpaces
        Nothing -> pure ()

    fencedBlock =
      P.label "block eval (syntax: a fenced code block)" $
        evalUnison <|> exampleBlock <|> other
      where
        evalUnison = fmap (wrap' . Eval) $ do
          -- commit after seeing that ``` is on its own line
          fence <- P.try $ do
            fence <- lit "```" <+> P.takeWhileP Nothing (== '`')
            b <- all isSpace <$> P.lookAhead (P.takeWhileP Nothing (/= '\n'))
            fence <$ guard b
          CP.space
            *> code (() <$ lit fence)

        exampleBlock = fmap (wrap' . ExampleBlock) $ do
          void $ lit "@typecheck" <* CP.space
          fence <- lit "```" <+> P.takeWhileP Nothing (== '`')
          code (() <$ lit fence)

        uncolumn column tabWidth s =
          let skip col r | col < 1 = r
              skip col s@('\t' : _) | col < tabWidth = s
              skip col ('\t' : r) = skip (col - tabWidth) r
              skip col (c : r)
                | isSpace c && (not $ isControl c) =
                    skip (col - 1) r
              skip _ s = s
           in List.intercalate "\n" $ skip column <$> lines s

        other = fmap (uncurry $ wrapSimple2 CodeBlock) $ do
          column <- (\x -> x - 1) . toInteger . P.unPos <$> LP.indentLevel
          let tabWidth = toInteger . P.unPos $ P.defaultTabWidth
          fence <- lit "```" <+> P.takeWhileP Nothing (== '`')
          name <-
            P.takeWhileP Nothing nonNewlineSpace
              *> tokenP (P.takeWhile1P Nothing (not . isSpace))
              <* P.takeWhileP Nothing nonNewlineSpace
          _ <- void CP.eol
          verbatim <-
            tokenP $
              uncolumn column tabWidth . trimAroundDelimiters
                <$> P.someTill P.anySingle ([] <$ lit fence)
          pure (name, verbatim)

    boldOrItalicOrStrikethrough closing = do
      let start =
            some (P.satisfy (== '*'))
              <|> some (P.satisfy (== '_'))
              <|> some
                (P.satisfy (== '~'))
          name s =
            if take 1 s == "~"
              then Strikethrough
              else if take 1 s == "*" then Bold else Italic
      end <- P.try $ do
        end <- start
        P.lookAhead (P.satisfy (not . isSpace))
        pure end
      name end . wrap' . Paragraph
        <$> someTill'
          (leafy (closing <|> (void $ lit end)) <* whitespaceWithoutParagraphBreak)
          (lit end)

    externalLink =
      P.label "hyperlink (example: [link name](https://destination.com))" $
        fmap (uncurry NamedLink) $ do
          _ <- lit "["
          p <- leafies (void $ char ']')
          _ <- lit "]"
          _ <- lit "("
          target <-
            fmap (Group . Join) $
              fmap pure link <|> some' (expr <|> wordy (char ')'))
          _ <- lit ")"
          pure (p, target)

    -- newline = P.optional (lit "\r") *> lit "\n"

    sp = P.try $ do
      spaces <- P.takeWhile1P (Just "space") isSpace
      close <- P.optional (P.lookAhead (lit "}}"))
      case close of
        Nothing -> guard $ ok spaces
        Just _ -> pure ()
      pure spaces
      where
        ok s = length [() | '\n' <- s] < 2

    spaced p = some' (p <* P.optional sp)
    leafies close = wrap' . Paragraph <$> spaced (leafy close)

    list = bulletedList <|> numberedList

    bulletedList = wrap' . BulletedList <$> sepBy1' bullet listSep
    numberedList = wrap' . NumberedList <$> sepBy1' numberedItem listSep

    listSep = P.try $ newline *> nonNewlineSpaces *> P.lookAhead (void bulletedStart <|> void numberedStart)

    bulletedStart = P.try $ do
      r <- listItemStart' $ [] <$ P.satisfy bulletChar
      P.lookAhead (P.satisfy isSpace)
      pure r
      where
        bulletChar ch = ch == '*' || ch == '-' || ch == '+'

    listItemStart' :: P a -> P (Int, a)
    listItemStart' gutter = P.try $ do
      nonNewlineSpaces
      col <- column <$> posP
      parentCol <- S.gets parentListColumn
      guard (col > parentCol)
      (col,) <$> gutter

    numberedStart =
      listItemStart' $ P.try (tokenP $ LP.decimal <* lit ".")

    listItemParagraph = fmap (wrap' . Paragraph) $ do
      col <- column <$> posP
      some' (leaf <* sep col)
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

    numberedItem = P.label msg $ do
      (col, s) <- numberedStart
      (s,)
        <$> ( fmap (uncurry Column) $ do
                p <- nonNewlineSpaces *> listItemParagraph
                subList <-
                  local (\e -> e {parentListColumn = col}) (P.optional $ listSep *> list)
                pure (p, subList)
            )
      where
        msg = "numbered list (examples: 1. item1, 8. start numbering at '8')"

    bullet = fmap (uncurry Column) . P.label "bullet (examples: * item1, - item2)" $ do
      (col, _) <- bulletedStart
      p <- nonNewlineSpaces *> listItemParagraph
      subList <-
        local
          (\e -> e {parentListColumn = col})
          (P.optional $ listSep *> list)
      pure (p, subList)

    newline = P.label "newline" $ lit "\n" <|> lit "\r\n"

    -- ## Section title
    --
    -- A paragraph under this section.
    -- Part of the same paragraph. Blanklines separate paragraphs.
    --
    -- ### A subsection title
    --
    -- A paragraph under this subsection.

    -- # A section title (not a subsection)
    section :: P (Tree code)
    section = fmap (wrap' . uncurry Section) $ do
      ns <- S.gets parentSections
      hashes <- P.try $ lit (replicate (head ns) '#') *> P.takeWhile1P Nothing (== '#') <* sp
      title <- paragraph <* CP.space
      let m = length hashes + head ns
      body <-
        local (\env -> env {parentSections = (m : (tail ns))}) $
          P.many (sectionElem <* CP.space)
      pure $ (title, body)

    wrap' :: Top code (Tree code) -> Tree code
    wrap' doc = ann doc :< doc

    wrapSimple2 :: (Annotated a, Annotated b) => (a -> b -> Top code (Tree code)) -> a -> b -> Tree code
    wrapSimple2 fn a b = ann a <> ann b :< fn a b

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

-- Trim leading/trailing whitespace from around delimiters, e.g.
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
