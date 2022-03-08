{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Lexer
  ( Token (..),
    Line,
    Column,
    Err (..),
    Pos (..),
    Lexeme (..),
    lexer,
    simpleWordyId,
    simpleSymbolyId,
    line,
    column,
    escapeChars,
    debugFileLex,
    debugLex',
    debugLex'',
    debugLex''',
    showEscapeChar,
    touches,
    typeModifiers,
    typeOrAbilityAlt,
    typeModifiersAlt,
    -- todo: these probably don't belong here
    wordyIdChar,
    wordyIdStartChar,
    wordyId,
    symbolyId,
    wordyId0,
    symbolyId0,
  )
where

import Control.Lens.TH (makePrisms)
import qualified Control.Monad.State as S
import Data.Char
import Data.List
import qualified Data.List.NonEmpty as Nel
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import GHC.Exts (sortWith)
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char as CP
import qualified Text.Megaparsec.Char.Lexer as LP
import Text.Megaparsec.Error (ShowToken (..))
import qualified Text.Megaparsec.Error as EP
import qualified Text.Megaparsec.Internal as PI
import Unison.Lexer.Pos (Column, Line, Pos (Pos), column, line)
import Unison.Prelude
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH
import qualified Unison.Util.Bytes as Bytes
import Unison.Util.Monoid (intercalateMap)

type BlockName = String

type Layout = [(BlockName, Column)]

data Token a = Token
  { payload :: a,
    start :: !Pos,
    end :: !Pos
  }
  deriving (Eq, Ord, Show, Functor)

data ParsingEnv = ParsingEnv
  { layout :: !Layout, -- layout stack
    opening :: Maybe BlockName, -- `Just b` if a block of type `b` is being opened
    inLayout :: Bool, -- are we inside a construct that uses layout?
    parentSection :: Int, -- 1 means we are inside a # Heading 1
    parentListColumn :: Int -- 4 means we are inside a list starting
    -- at the fourth column
  }
  deriving (Show)

type P = P.ParsecT (Token Err) String (S.State ParsingEnv)

local :: (ParsingEnv -> ParsingEnv) -> P a -> P a
local f p = do
  env0 <- S.get
  S.put (f env0)
  e <- P.observing p
  S.put env0
  case e of
    Left e -> parseFailure e
    Right a -> pure a

parseFailure :: EP.ParseError Char (Token Err) -> P a
parseFailure e = PI.ParsecT $ \s _ _ _ eerr -> eerr e s

data Err
  = InvalidWordyId String
  | ReservedWordyId String
  | InvalidSymbolyId String
  | ReservedSymbolyId String
  | InvalidShortHash String
  | InvalidBytesLiteral String
  | InvalidHexLiteral
  | InvalidOctalLiteral
  | Both Err Err
  | MissingFractional String -- ex `1.` rather than `1.04`
  | MissingExponent String -- ex `1e` rather than `1e3`
  | UnknownLexeme
  | TextLiteralMissingClosingQuote String
  | InvalidEscapeCharacter Char
  | LayoutError
  | CloseWithoutMatchingOpen String String -- open, close
  | UnexpectedDelimiter String
  | Opaque String -- Catch-all failure type, generally these will be
  -- automatically generated errors coming from megaparsec
  -- Try to avoid this for common errors a user is likely to see.
  deriving (Eq, Ord, Show) -- richer algebra

-- Design principle:
--   `[Lexeme]` should be sufficient information for parsing without
--   further knowledge of spacing or indentation levels
--   any knowledge of comments
data Lexeme
  = Open String -- start of a block
  | Semi IsVirtual -- separator between elements of a block
  | Close -- end of a block
  | Reserved String -- reserved tokens such as `{`, `(`, `type`, `of`, etc
  | Textual String -- text literals, `"foo bar"`
  | Character Char -- character literals, `?X`
  | Backticks String (Maybe ShortHash) -- an identifier in backticks
  | WordyId String (Maybe ShortHash) -- a (non-infix) identifier
  | SymbolyId String (Maybe ShortHash) -- an infix identifier
  | Blank String -- a typed hole or placeholder
  | Numeric String -- numeric literals, left unparsed
  | Bytes Bytes.Bytes -- bytes literals
  | Hash ShortHash -- hash literals
  | Err Err
  deriving (Eq, Show, Ord)

type IsVirtual = Bool -- is it a virtual semi or an actual semi?

makePrisms ''Lexeme

space :: P ()
space =
  LP.space
    CP.space1
    (fold <|> LP.skipLineComment "--")
    (LP.skipBlockCommentNested "{-" "-}")
  where
    fold = P.try $ lit "---" *> P.takeRest *> pure ()

lit :: String -> P String
lit = P.try . LP.symbol (pure ())

token :: P Lexeme -> P [Token Lexeme]
token = token' (\a start end -> [Token a start end])

pos :: P Pos
pos = do
  p <- P.getPosition
  pure $ Pos (P.unPos (P.sourceLine p)) (P.unPos (P.sourceColumn p))

-- Token parser: strips trailing whitespace and comments after a
-- successful parse, and also takes care of emitting layout tokens
-- (such as virtual semicolons and closing tokens).
token' :: (a -> Pos -> Pos -> [Token Lexeme]) -> P a -> P [Token Lexeme]
token' tok p = LP.lexeme space (token'' tok p)

-- Committed failure
err :: Pos -> Err -> P x
err start t = do
  stop <- pos
  -- This consumes a character and therefore produces committed failure,
  -- so `err s t <|> p2` won't try `p2`
  _ <- void CP.anyChar <|> P.eof
  P.customFailure (Token t start stop)

{-
commitAfter :: P a -> (a -> P b) -> P b
commitAfter a f = do
  a <- P.try a
  f a
-}

commitAfter2 :: P a -> P b -> (a -> b -> P c) -> P c
commitAfter2 a b f = do
  (a, b) <- P.try $ liftA2 (,) a b
  f a b

-- Token parser implementation which leaves trailing whitespace and comments
-- but does emit layout tokens such as virtual semicolons and closing tokens.
token'' :: (a -> Pos -> Pos -> [Token Lexeme]) -> P a -> P [Token Lexeme]
token'' tok p = do
  start <- pos
  -- We save the current state so we can backtrack the state if `p` fails.
  env <- S.get
  layoutToks <- case opening env of
    -- If we're opening a block named b, we push (b, currentColumn) onto
    -- the layout stack. Example:
    --
    --   blah = cases
    --       {- A comment -}
    --          -- A one-line comment
    --     0 -> "hi"
    --     1 -> "bye"
    --
    -- After the `cases` token, the state will be opening = Just "cases",
    -- meaning the parser is searching for the next non-whitespace/comment
    -- character to determine the leftmost column of the `cases` block.
    -- That will be the column of the `0`.
    Just blockname ->
      -- special case - handling of empty blocks, as in:
      --   foo =
      --   bar = 42
      if blockname == "=" && column start <= top l && not (null l)
        then do
          S.put (env {layout = (blockname, column start + 1) : l, opening = Nothing})
          pops start
        else [] <$ S.put (env {layout = layout', opening = Nothing})
      where
        layout' = (blockname, column start) : l
        l = layout env
    -- If we're not opening a block, we potentially pop from
    -- the layout stack and/or emit virtual semicolons.
    Nothing -> if inLayout env then pops start else pure []
  a <- p <|> (S.put env >> fail "resetting state")
  end <- pos
  pure $ layoutToks ++ tok a start end
  where
    pops :: Pos -> P [Token Lexeme]
    pops p = do
      env <- S.get
      let l = layout env
      if top l == column p
        then pure [Token (Semi True) p p]
        else
          if column p > top l || topHasClosePair l
            then pure []
            else
              if column p < top l
                then S.put (env {layout = pop l}) >> ((Token Close p p :) <$> pops p)
                else error "impossible"

    topHasClosePair :: Layout -> Bool
    topHasClosePair [] = False
    topHasClosePair ((name, _) : _) =
      name `elem` ["{", "(", "[", "handle", "match", "if", "then"]

lexer0' :: String -> String -> [Token Lexeme]
lexer0' scope rem =
  case flip S.evalState env0 $ P.runParserT lexemes scope rem of
    Left e -> case e of
      P.FancyError _ (customErrs -> es) | not (null es) -> es
      P.FancyError (top Nel.:| _) es ->
        let msg = intercalateMap "\n" P.showErrorComponent es
         in [Token (Err (Opaque msg)) (toPos top) (toPos top)]
      P.TrivialError (top Nel.:| _) _ _ ->
        let msg = Opaque $ EP.parseErrorPretty e
         in [Token (Err msg) (toPos top) (toPos top)]
    Right ts -> Token (Open scope) topLeftCorner topLeftCorner : tweak ts
  where
    customErrs es = [Err <$> e | P.ErrorCustom e <- toList es]
    toPos (P.SourcePos _ line col) = Pos (P.unPos line) (P.unPos col)
    env0 = ParsingEnv [] (Just scope) True 0 0
    -- hacky postprocessing pass to do some cleanup of stuff that's annoying to
    -- fix without adding more state to the lexer:
    --   - 1+1 lexes as [1, +1], convert this to [1, +, 1]
    --   - when a semi followed by a virtual semi, drop the virtual, lets you
    --     write
    --       foo x = action1;
    --               2
    --   - semi immediately after first Open is ignored
    tweak [] = []
    tweak (h@(payload -> Semi False) : (payload -> Semi True) : t) = h : tweak t
    tweak (h@(payload -> Reserved _) : t) = h : tweak t
    tweak (t1 : t2@(payload -> Numeric num) : rem)
      | notLayout t1 && touches t1 t2 && isSigned num =
        t1 :
        Token
          (SymbolyId (take 1 num) Nothing)
          (start t2)
          (inc $ start t2) :
        Token (Numeric (drop 1 num)) (inc $ start t2) (end t2) :
        tweak rem
    tweak (h : t) = h : tweak t
    isSigned num = all (\ch -> ch == '-' || ch == '+') $ take 1 num

infixl 2 <+>

(<+>) :: Monoid a => P a -> P a -> P a
p1 <+> p2 = do a1 <- p1; a2 <- p2; pure (a1 <> a2)

lexemes :: P [Token Lexeme]
lexemes = lexemes' eof
  where
    eof :: P [Token Lexeme]
    eof = P.try $ do
      p <- P.eof >> pos
      n <- maybe 0 (const 1) <$> S.gets opening
      l <- S.gets layout
      pure $ replicate (length l + n) (Token Close p p)

-- Runs the parser `p`, then:
--   1. resets the layout stack to be what it was before `p`.
--   2. emits enough closing tokens to reach `lbl` but not pop it.
--      (you can think of this as just dealing with a final "unclosed"
--       block at the end of `p`)
restoreStack :: String -> P [Token Lexeme] -> P [Token Lexeme]
restoreStack lbl p = do
  layout1 <- S.gets layout
  p <- p
  s2 <- S.get
  let (pos1, pos2) = foldl' (\_ b -> (start b, end b)) mempty p
      unclosed = takeWhile (\(lbl', _) -> lbl' /= lbl) (layout s2)
      closes = replicate (length unclosed) (Token Close pos1 pos2)
  S.put (s2 {layout = layout1})
  pure $ p <> closes

lexemes' :: P [Token Lexeme] -> P [Token Lexeme]
lexemes' eof =
  P.optional space >> do
    hd <- join <$> P.manyTill toks (P.lookAhead eof)
    tl <- eof
    pure $ hd <> tl
  where
    toks =
      doc2 <|> doc <|> token numeric <|> token character <|> reserved
        <|> token symbolyId
        <|> token blank
        <|> token wordyId
        <|> (asum . map token) [semi, textual, hash]

    wordySep c = isSpace c || not (wordyIdChar c)
    positioned p = do start <- pos; a <- p; stop <- pos; pure (start, a, stop)

    tok :: P a -> P [Token a]
    tok p = do
      (start, a, stop) <- positioned p
      pure [Token a start stop]

    doc2 :: P [Token Lexeme]
    doc2 = do
      let start = token'' ignore (lit "{{")
      startToks <- start <* CP.space
      env0 <- S.get
      docToks0 <-
        wrap "syntax.docUntitledSection" $
          local (\env -> env {inLayout = False}) (body <* lit "}}")
      let docToks = startToks <> docToks0
      endToks <- token' ignore (pure ())
      -- Hack to allow anonymous doc blocks before type decls
      --   {{ Some docs }}             Foo.doc = {{ Some docs }}
      --   ability Foo where      =>   ability Foo where
      tn <- subsequentTypeName
      pure $ case (tn, docToks) of
        (Just (WordyId tname _), ht : _)
          | isTopLevel ->
            startToks
              <> [WordyId (tname <> ".doc") Nothing <$ ht, Open "=" <$ ht]
              <> docToks0
              <> [Close <$ last docToks]
              <> endToks
          where
            isTopLevel = length (layout env0) + maybe 0 (const 1) (opening env0) == 1
        _ -> docToks <> endToks
      where
        wordyKw kw = separated wordySep (lit kw)
        subsequentTypeName = P.lookAhead . P.optional $ do
          let lit' s = lit s <* sp
          let modifier = typeModifiersAlt lit'
          let typeOrAbility' = typeOrAbilityAlt wordyKw
          _ <- modifier <* typeOrAbility' *> sp
          wordyId
        ignore _ _ _ = []
        body = join <$> P.many (sectionElem <* CP.space)
        sectionElem = section <|> fencedBlock <|> list <|> paragraph
        paragraph = wrap "syntax.docParagraph" $ join <$> spaced leaf
        reserved word =
          isPrefixOf "}}" word
            || all (== '#') word

        wordy closing = wrap "syntax.docWord" . tok . fmap Textual . P.try $ do
          let end =
                P.lookAhead $
                  void docClose
                    <|> void docOpen
                    <|> void (CP.satisfy isSpace)
                    <|> void closing
          word <- P.manyTill (CP.satisfy (\ch -> not (isSpace ch))) end
          guard (not $ reserved word || null word)
          pure word

        leafy closing = groupy closing gs
          where
            gs =
              link <|> externalLink <|> exampleInline <|> expr
                <|> boldOrItalicOrStrikethrough closing
                <|> verbatim
                <|> atDoc
                <|> wordy closing

        leaf = leafy mzero

        atDoc = src <|> evalInline <|> signature <|> signatureInline
          where
            comma = lit "," <* CP.space
            src =
              src' "syntax.docSource" "@source"
                <|> src' "syntax.docFoldedSource" "@foldedSource"
            srcElem =
              wrap "syntax.docSourceElement" $
                (typeLink <|> termLink)
                  <+> ( fmap (fromMaybe []) . P.optional $
                          (tok (Reserved <$> lit "@") <+> (CP.space *> annotations))
                      )
              where
                annotation = tok (symbolyId <|> wordyId) <|> expr <* CP.space
                annotations =
                  join <$> P.some (wrap "syntax.docEmbedAnnotation" annotation)
            src' name atName = wrap name $ do
              _ <- lit atName *> (lit " {" <|> lit "{") *> CP.space
              s <- P.sepBy1 srcElem comma
              _ <- lit "}"
              pure (join s)
            signature = wrap "syntax.docSignature" $ do
              _ <- (lit "@signatures" <|> lit "@signature") *> (lit " {" <|> lit "{") *> CP.space
              s <- join <$> P.sepBy1 signatureLink comma
              _ <- lit "}"
              pure s
            signatureInline = wrap "syntax.docSignatureInline" $ do
              _ <- lit "@inlineSignature" *> (lit " {" <|> lit "{") *> CP.space
              s <- signatureLink
              _ <- lit "}"
              pure s
            evalInline = wrap "syntax.docEvalInline" $ do
              _ <- lit "@eval" *> (lit " {" <|> lit "{") *> CP.space
              let inlineEvalClose = [] <$ lit "}"
              s <- lexemes' inlineEvalClose
              pure s

        typeLink = wrap "syntax.docEmbedTypeLink" $ do
          _ <- typeOrAbilityAlt wordyKw <* CP.space
          tok (symbolyId <|> wordyId) <* CP.space

        termLink =
          wrap "syntax.docEmbedTermLink" $
            tok (symbolyId <|> wordyId) <* CP.space

        signatureLink =
          wrap "syntax.docEmbedSignatureLink" $
            tok (symbolyId <|> wordyId) <* CP.space

        groupy closing p = do
          (start, p, stop) <- positioned p
          after <- P.optional . P.try $ leafy closing
          pure $ case after of
            Nothing -> p
            Just after ->
              [ Token (Open "syntax.docGroup") start stop',
                Token (Open "syntax.docJoin") start stop'
              ]
                <> p
                <> after
                <> (take 2 $ repeat (Token Close stop' stop'))
              where
                stop' = maybe stop end (lastMay after)

        verbatim =
          P.label "code (examples: ''**unformatted**'', '''_words_''')" $ do
            (start, txt, stop) <- positioned $ do
              quotes <- lit "''" <+> many (CP.satisfy (== '\''))
              P.someTill CP.anyChar (lit quotes)
            if all isSpace $ takeWhile (/= '\n') txt
              then
                wrap "syntax.docVerbatim" $
                  wrap "syntax.docWord" $
                    pure [Token (Textual (trim txt)) start stop]
              else
                wrap "syntax.docCode" $
                  wrap "syntax.docWord" $
                    pure [Token (Textual txt) start stop]

        trim = f . f
          where
            f = reverse . dropThru
            dropThru = dropNl . dropWhile (\ch -> isSpace ch && ch /= '\n')
            dropNl ('\n' : t) = t
            dropNl as = as

        exampleInline =
          P.label "inline code (examples: ``List.map f xs``, ``[1] :+ 2``)" $
            wrap "syntax.docExample" $ do
              n <- P.try $ do
                _ <- lit "`"
                length <$> P.takeWhile1P (Just "backticks") (== '`')
              let end :: P [Token Lexeme] = [] <$ lit (replicate (n + 1) '`')
              ex <- CP.space *> lexemes' end
              pure ex

        docClose = [] <$ lit "}}"
        docOpen = [] <$ lit "{{"

        link =
          P.label "link (examples: {type List}, {Nat.+})" $
            wrap "syntax.docLink" $
              P.try $ lit "{" *> (typeLink <|> termLink) <* lit "}"

        expr =
          P.label "transclusion (examples: {{ doc2 }}, {{ sepBy s [doc1, doc2] }})" $
            wrap "syntax.docTransclude" $
              docOpen *> lexemes' docClose

        nonNewlineSpace ch = isSpace ch && ch /= '\n' && ch /= '\r'
        nonNewlineSpaces = P.takeWhileP Nothing nonNewlineSpace

        fencedBlock =
          P.label "block eval (syntax: a fenced code block)" $
            evalUnison <|> exampleBlock <|> other
          where
            evalUnison = wrap "syntax.docEval" $ do
              -- commit after seeing that ``` is on its own line
              fence <- P.try $ do
                fence <- lit "```" <+> P.many (CP.satisfy (== '`'))
                b <- all isSpace <$> P.lookAhead (P.takeWhileP Nothing (/= '\n'))
                fence <$ guard b
              CP.space
                *> local
                  (\env -> env {inLayout = True, opening = Just "docEval"})
                  (restoreStack "docEval" $ lexemes' ([] <$ lit fence))

            exampleBlock = wrap "syntax.docExampleBlock" $ do
              void $ lit "@typecheck" <* CP.space
              fence <- lit "```" <+> P.many (CP.satisfy (== '`'))
              local
                (\env -> env {inLayout = True, opening = Just "docExampleBlock"})
                (restoreStack "docExampleBlock" $ lexemes' ([] <$ lit fence))

            uncolumn column tabWidth s =
              let skip col r | col < 1 = r
                  skip col s@('\t' : _) | col < tabWidth = s
                  skip col ('\t' : r) = skip (col - tabWidth) r
                  skip col (c : r)
                    | isSpace c && (not $ isControl c) =
                      skip (col - 1) r
                  skip _ s = s
               in intercalate "\n" $ skip column <$> lines s

            other = wrap "syntax.docCodeBlock" $ do
              column <- (\x -> x - 1) . toInteger . P.unPos <$> LP.indentLevel
              tabWidth <- toInteger . P.unPos <$> P.getTabWidth
              fence <- lit "```" <+> P.many (CP.satisfy (== '`'))
              name <-
                P.many (CP.satisfy nonNewlineSpace)
                  *> tok (Textual <$> P.takeWhile1P Nothing (not . isSpace))
                  <* P.many (CP.satisfy nonNewlineSpace)
              _ <- void CP.eol
              verbatim <-
                tok $
                  Textual . uncolumn column tabWidth . trim
                    <$> P.someTill CP.anyChar ([] <$ lit fence)
              pure (name <> verbatim)

        boldOrItalicOrStrikethrough closing = do
          let start =
                some (CP.satisfy (== '*')) <|> some (CP.satisfy (== '_'))
                  <|> some
                    (CP.satisfy (== '~'))
              name s =
                if take 1 s == "~"
                  then "syntax.docStrikethrough"
                  else if take 1 s == "*" then "syntax.docBold" else "syntax.docItalic"
          end <- P.try $ do
            end <- start
            P.lookAhead (CP.satisfy (not . isSpace))
            pure end
          wrap (name end) . wrap "syntax.docParagraph" $
            join
              <$> P.someTill
                (leafy (closing <|> (void $ lit end)) <* nonNewlineSpaces)
                (lit end)

        externalLink =
          P.label "hyperlink (example: [link name](https://destination.com))" $
            wrap "syntax.docNamedLink" $ do
              _ <- lit "["
              p <- leafies (void $ char ']')
              _ <- lit "]"
              _ <- lit "("
              target <-
                wrap "syntax.docGroup" . wrap "syntax.docJoin" $
                  link <|> fmap join (P.some (expr <|> wordy (char ')')))
              _ <- lit ")"
              pure (p <> target)

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

        spaced p = P.some (p <* P.optional sp)
        leafies close = wrap "syntax.docParagraph" $ join <$> spaced (leafy close)

        list = bulletedList <|> numberedList

        bulletedList = wrap "syntax.docBulletedList" $ join <$> P.sepBy1 bullet listSep
        numberedList = wrap "syntax.docNumberedList" $ join <$> P.sepBy1 numberedItem listSep

        listSep = P.try $ newline *> nonNewlineSpaces *> P.lookAhead (bulletedStart <|> numberedStart)

        bulletedStart = P.try $ do
          r <- listItemStart' $ [] <$ CP.satisfy bulletChar
          P.lookAhead (CP.satisfy isSpace)
          pure r
          where
            bulletChar ch = ch == '*' || ch == '-' || ch == '+'

        listItemStart' gutter = P.try $ do
          nonNewlineSpaces
          col <- column <$> pos
          parentCol <- S.gets parentListColumn
          guard (col > parentCol)
          (col,) <$> gutter

        numberedStart =
          listItemStart' $ P.try (tok . fmap num $ LP.decimal <* lit ".")
          where
            num :: Word -> Lexeme
            num n = Numeric (show n)

        listItemParagraph = wrap "syntax.docParagraph" $ do
          col <- column <$> pos
          join <$> P.some (leaf <* sep col)
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
                      col2 <- column <$> pos
                      guard $ col2 >= col
                      (P.notFollowedBy $ numberedStart <|> bulletedStart)
              pure ()

        numberedItem = P.label msg $ do
          (col, s) <- numberedStart
          pure s
            <+> ( wrap "syntax.docColumn" $ do
                    p <- nonNewlineSpaces *> listItemParagraph
                    subList <-
                      local (\e -> e {parentListColumn = col}) (P.optional $ listSep *> list)
                    pure (p <> fromMaybe [] subList)
                )
          where
            msg = "numbered list (examples: 1. item1, 8. start numbering at '8')"

        bullet = wrap "syntax.docColumn" . P.label "bullet (examples: * item1, - item2)" $ do
          (col, _) <- bulletedStart
          p <- nonNewlineSpaces *> listItemParagraph
          subList <-
            local
              (\e -> e {parentListColumn = col})
              (P.optional $ listSep *> list)
          pure (p <> fromMaybe [] subList)

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
        section :: P [Token Lexeme]
        section = wrap "syntax.docSection" $ do
          n <- S.gets parentSection
          hashes <- P.try $ lit (replicate n '#') *> P.takeWhile1P Nothing (== '#') <* sp
          title <- paragraph <* CP.space
          let m = length hashes + n
          body <-
            local (\env -> env {parentSection = m}) $
              P.many (sectionElem <* CP.space)
          pure $ title <> join body

        wrap :: String -> P [Token Lexeme] -> P [Token Lexeme]
        wrap o p = do
          start <- pos
          lexemes <- p
          pure $ go start lexemes
          where
            go start [] = [Token (Open o) start start, Token Close start start]
            go start ts@(Token _ x _ : _) =
              Token (Open o) start x : (ts ++ [Token Close (end final) (end final)])
              where
                final = last ts

    doc :: P [Token Lexeme]
    doc = open <+> (CP.space *> fmap fixup body) <+> (close <* space)
      where
        open = token'' (\t _ _ -> t) $ tok (Open <$> lit "[:")
        close = tok (Close <$ lit ":]")
        at = lit "@"
        -- this removes some trailing whitespace from final textual segment
        fixup [] = []
        fixup (Token (Textual (reverse -> txt)) start stop : []) =
          [Token (Textual txt') start stop]
          where
            txt' = reverse (dropWhile (\c -> isSpace c && not (c == '\n')) txt)
        fixup (h : t) = h : fixup t

        body :: P [Token Lexeme]
        body = txt <+> (atk <|> pure [])
          where
            ch = (":]" <$ lit "\\:]") <|> ("@" <$ lit "\\@") <|> (pure <$> CP.anyChar)
            txt = tok (Textual . join <$> P.manyTill ch (P.lookAhead sep))
            sep = void at <|> void close
            ref = at *> (tok wordyId <|> tok symbolyId <|> docTyp)
            atk = (ref <|> docTyp) <+> body
            docTyp = do
              _ <- lit "["
              typ <- tok (P.manyTill CP.anyChar (P.lookAhead (lit "]")))
              _ <- lit "]" *> CP.space
              t <- tok wordyId <|> tok symbolyId
              pure $ (fmap Reserved <$> typ) <> t

    blank =
      separated wordySep $
        char '_' *> P.optional wordyIdSeg <&> (Blank . fromMaybe "")

    semi = char ';' $> Semi False
    textual = Textual <$> quoted
    quoted = char '"' *> P.manyTill (LP.charLiteral <|> sp) (char '"')
      where
        sp = lit "\\s" $> ' '
    character = Character <$> (char '?' *> (spEsc <|> LP.charLiteral))
      where
        spEsc = P.try (char '\\' *> char 's' $> ' ')

    -- symboly Ids and wordy Ids differ only in their final segment, this parses the initial
    -- path.
    idPrefix :: P (Maybe String, [String])
    idPrefix = do
      dot <- P.optional (lit ".")
      nonTerminalSegments <- P.many (P.try (segmentP <* char '.'))
      pure (dot, nonTerminalSegments)
      where
        segmentP = symbolyIdSeg <|> wordyIdSeg

    wordyId :: P Lexeme
    wordyId = P.label wordyMsg . P.try $ do
      (dot, segs) <- idPrefix
      traceM $ "Wordy" <> (show segs)
      baseName <- wordyIdSeg
      shorthash <- P.optional shorthash
      pure . traceShowId $ WordyId (fromMaybe "" dot <> intercalate "." (segs ++ [baseName])) shorthash
      where
        wordyMsg = "identifier (ex: abba1, snake_case, .foo.bar#xyz, .Nat.-.doc or 🌻)"

    symbolyId :: P Lexeme
    symbolyId = P.label symbolMsg . P.try $ do
      (dot, segs) <- idPrefix
      traceM $ "Symboly" <> (show segs)
      baseName <- symbolyIdSeg
      shorthash <- P.optional shorthash
      pure . traceShowId $ SymbolyId (fromMaybe "" dot <> intercalate "." (segs ++ [baseName])) shorthash

    symbolyIdSeg :: P String
    symbolyIdSeg = do
      start <- pos
      id <- P.takeWhile1P (Just symbolMsg) symbolyIdChar
      when (Set.member id reservedOperators) $ do
        stop <- pos
        P.customFailure (Token (ReservedSymbolyId id) start stop)
      pure id

    symbolMsg :: String
    symbolMsg = "operator (examples: +, Float./, List.++#xyz)"

    wordyIdSeg :: P String
    -- wordyIdSeg = litSeg <|> (P.try do -- todo
    wordyIdSeg = P.try $ do
      start <- pos
      ch <- CP.satisfy wordyIdStartChar
      rest <- P.many (CP.satisfy wordyIdChar)
      let word = ch : rest
      when (Set.member word keywords) $ do
        stop <- pos
        P.customFailure (Token (ReservedWordyId word) start stop)
      pure (ch : rest)

    {-
    -- ``an-identifier-with-dashes``
    -- ```an identifier with spaces```
    litSeg :: P String
    litSeg = P.try $ do
      ticks1 <- lit "``"
      ticks2 <- P.many (char '`')
      let ticks = ticks1 <> ticks2
      let escTick = lit "\\`" $> '`'
      P.manyTill (LP.charLiteral <|> escTick) (lit ticks)
    -}

    hashMsg = "hash (ex: #af3sj3)"
    shorthash = P.label hashMsg $ do
      P.lookAhead (char '#')
      -- `foo#xyz` should parse
      (start, potentialHash, _) <- positioned $ P.takeWhile1P (Just hashMsg) (\ch -> not (isSep ch) && ch /= '`')
      case SH.fromString potentialHash of
        Nothing -> err start (InvalidShortHash potentialHash)
        Just sh -> pure sh

    separated :: (Char -> Bool) -> P a -> P a
    separated ok p = P.try $ p <* P.lookAhead (void (CP.satisfy ok) <|> P.eof)

    numeric = bytes <|> otherbase <|> float <|> intOrNat
      where
        intOrNat = P.try $ num <$> sign <*> LP.decimal
        float = do
          _ <- P.try (P.lookAhead (sign >> LP.decimal >> (char '.' <|> char 'e' <|> char 'E'))) -- commit after this
          start <- pos
          sign <- fromMaybe "" <$> sign
          base <- P.takeWhile1P (Just "base") isDigit
          decimals <-
            P.optional $
              let missingFractional = err start (MissingFractional $ base <> ".")
               in liftA2 (<>) (lit ".") (P.takeWhile1P (Just "decimals") isDigit <|> missingFractional)
          exp <- P.optional $ do
            e <- map toLower <$> (lit "e" <|> lit "E")
            sign <- fromMaybe "" <$> optional (lit "+" <|> lit "-")
            let missingExp = err start (MissingExponent $ base <> fromMaybe "" decimals <> e <> sign)
            exp <- P.takeWhile1P (Just "exponent") isDigit <|> missingExp
            pure $ e <> sign <> exp
          pure $ Numeric (sign <> base <> fromMaybe "" decimals <> fromMaybe "" exp)

        bytes = do
          start <- pos
          _ <- lit "0xs"
          s <- map toLower <$> P.takeWhileP (Just "hexidecimal character") isAlphaNum
          case Bytes.fromBase16 $ Bytes.fromWord8s (fromIntegral . ord <$> s) of
            Left _ -> err start (InvalidBytesLiteral $ "0xs" <> s)
            Right bs -> pure (Bytes bs)
        otherbase = octal <|> hex
        octal = do
          start <- pos
          commitAfter2 sign (lit "0o") $ \sign _ ->
            fmap (num sign) LP.octal <|> err start InvalidOctalLiteral
        hex = do
          start <- pos
          commitAfter2 sign (lit "0x") $ \sign _ ->
            fmap (num sign) LP.hexadecimal <|> err start InvalidHexLiteral

        num :: Maybe String -> Integer -> Lexeme
        num sign n = Numeric (fromMaybe "" sign <> show n)
        sign = P.optional (lit "+" <|> lit "-")

    hash = Hash <$> P.try shorthash

    reserved :: P [Token Lexeme]
    reserved =
      token' (\ts _ _ -> ts) $
        braces
          <|> parens
          <|> brackets
          <|> commaSeparator
          <|> delim
          <|> delayOrForce
          <|> keywords
          <|> layoutKeywords
      where
        keywords =
          symbolyKw ":"
            <|> symbolyKw "@"
            <|> symbolyKw "||"
            <|> symbolyKw "|"
            <|> symbolyKw "&&"
            <|> wordyKw "true"
            <|> wordyKw "false"
            <|> wordyKw "use"
            <|> wordyKw "forall"
            <|> wordyKw "∀"
            <|> wordyKw "termLink"
            <|> wordyKw "typeLink"

        wordyKw s = separated wordySep (kw s)
        symbolyKw s = separated (not . symbolyIdChar) (kw s)

        kw :: String -> P [Token Lexeme]
        kw s = positioned (lit s) <&> \(pos1, s, pos2) -> [Token (Reserved s) pos1 pos2]

        layoutKeywords :: P [Token Lexeme]
        layoutKeywords =
          ifElse <|> withKw <|> openKw "match" <|> openKw "handle" <|> typ <|> arr <|> eq
            <|> openKw "cases"
            <|> openKw "where"
            <|> openKw "let"
          where
            ifElse =
              openKw "if" <|> closeKw' (Just "then") ["if"] (lit "then")
                <|> closeKw' (Just "else") ["then"] (lit "else")
            modKw = typeModifiersAlt (openKw1 wordySep)
            typeOrAbilityKw = typeOrAbilityAlt openTypeKw1
            typ = modKw <|> typeOrAbilityKw

            withKw = do
              [Token _ pos1 pos2] <- wordyKw "with"
              env <- S.get
              let l = layout env
              case findClose ["handle", "match"] l of
                Nothing -> err pos1 (CloseWithoutMatchingOpen msgOpen "'with'")
                  where
                    msgOpen = "'handle' or 'match'"
                Just (withBlock, n) -> do
                  let b = withBlock <> "-with"
                  S.put (env {layout = drop n l, opening = Just b})
                  let opens = [Token (Open "with") pos1 pos2]
                  pure $ replicate n (Token Close pos1 pos2) ++ opens

            -- In `structural/unique type` and `structural/unique ability`,
            -- only the `structural` or `unique` opens a layout block,
            -- and `ability` and `type` are just keywords.
            openTypeKw1 t = do
              b <- S.gets (topBlockName . layout)
              case b of
                Just mod | Set.member mod typeModifiers -> wordyKw t
                _ -> openKw1 wordySep t

            -- layout keyword which bumps the layout column by 1, rather than looking ahead
            -- to the next token to determine the layout column
            openKw1 :: (Char -> Bool) -> String -> P [Token Lexeme]
            openKw1 sep kw = do
              (pos0, kw, pos1) <- positioned $ separated sep (lit kw)
              S.modify (\env -> env {layout = (kw, column $ inc pos0) : layout env})
              pure [Token (Open kw) pos0 pos1]

            eq = do
              [Token _ start end] <- symbolyKw "="
              env <- S.get
              case topBlockName (layout env) of
                -- '=' does not open a layout block if within a type declaration
                Just t | t == "type" || Set.member t typeModifiers -> pure [Token (Reserved "=") start end]
                Just _ -> S.put (env {opening = Just "="}) >> pure [Token (Open "=") start end]
                _ -> err start LayoutError

            arr = do
              [Token _ start end] <- symbolyKw "->"
              env <- S.get
              -- -> introduces a layout block if we're inside a `match with` or `cases`
              case topBlockName (layout env) of
                Just match | match `elem` matchWithBlocks -> do
                  S.put (env {opening = Just "->"})
                  pure [Token (Open "->") start end]
                _ -> pure [Token (Reserved "->") start end]

        -- a bit of lookahead here to reserve }} for closing a documentation block
        braces = open "{" <|> close ["{"] p
          where
            p = do
              l <- lit "}"
              -- if we're within an existing {{ }} block, inLayout will be false
              -- so we can actually allow }} to appear in normal code
              inLayout <- S.gets inLayout
              when (not inLayout) $ void $ P.lookAhead (CP.satisfy (/= '}'))
              pure l
        matchWithBlocks = ["match-with", "cases"]
        parens = open "(" <|> close ["("] (lit ")")
        brackets = open "[" <|> close ["["] (lit "]")
        -- `allowCommaToClose` determines if a comma should close inner blocks.
        -- Currently there is a set of blocks where `,` is not treated specially
        -- and it just emits a Reserved ",". There are currently only three:
        -- `cases`, `match-with`, and `{`
        allowCommaToClose match = not $ match `elem` ("{" : matchWithBlocks)
        commaSeparator = do
          env <- S.get
          case topBlockName (layout env) of
            Just match
              | allowCommaToClose match ->
                blockDelimiter ["[", "("] (lit ",")
            _ -> fail "this comma is a pattern separator"

        delim = P.try $ do
          ch <- CP.satisfy (\ch -> ch /= ';' && Set.member ch delimiters)
          pos <- pos
          pure [Token (Reserved [ch]) pos (inc pos)]

        delayOrForce = separated ok $ do
          (start, op, end) <- positioned $ CP.satisfy isDelayOrForce
          pure [Token (Reserved [op]) start end]
          where
            ok c = isDelayOrForce c || isSpace c || isAlphaNum c || Set.member c delimiters || c == '\"'

        open :: String -> P [Token Lexeme]
        open b = do
          (start, _, end) <- positioned $ lit b
          env <- S.get
          S.put (env {opening = Just b})
          pure [Token (Open b) start end]

        openKw :: String -> P [Token Lexeme]
        openKw s = separated wordySep $ do
          (pos1, s, pos2) <- positioned $ lit s
          env <- S.get
          S.put (env {opening = Just s})
          pure [Token (Open s) pos1 pos2]

        close = close' Nothing

        closeKw' :: Maybe String -> [String] -> P String -> P [Token Lexeme]
        closeKw' reopenBlockname open closeP = close' reopenBlockname open (separated wordySep closeP)

        blockDelimiter :: [String] -> P String -> P [Token Lexeme]
        blockDelimiter open closeP = do
          (pos1, close, pos2) <- positioned $ closeP
          env <- S.get
          case findClose open (layout env) of
            Nothing -> err pos1 (UnexpectedDelimiter (quote close))
              where
                quote s = "'" <> s <> "'"
            Just (_, n) -> do
              S.put (env {layout = drop (n - 1) (layout env)})
              let delims = [Token (Reserved close) pos1 pos2]
              pure $ replicate (n - 1) (Token Close pos1 pos2) ++ delims

        close' :: Maybe String -> [String] -> P String -> P [Token Lexeme]
        close' reopenBlockname open closeP = do
          (pos1, close, pos2) <- positioned $ closeP
          env <- S.get
          case findClose open (layout env) of
            Nothing -> err pos1 (CloseWithoutMatchingOpen msgOpen (quote close))
              where
                msgOpen = intercalate " or " (quote <$> open)
                quote s = "'" <> s <> "'"
            Just (_, n) -> do
              S.put (env {layout = drop n (layout env), opening = reopenBlockname})
              let opens = maybe [] (const $ [Token (Open close) pos1 pos2]) reopenBlockname
              pure $ replicate n (Token Close pos1 pos2) ++ opens

        findClose :: [String] -> Layout -> Maybe (String, Int)
        findClose _ [] = Nothing
        findClose s ((h, _) : tl) = if h `elem` s then Just (h, 1) else fmap (1 +) <$> findClose s tl

simpleWordyId :: String -> Lexeme
simpleWordyId = flip WordyId Nothing

simpleSymbolyId :: String -> Lexeme
simpleSymbolyId = flip SymbolyId Nothing

notLayout :: Token Lexeme -> Bool
notLayout t = case payload t of
  Close -> False
  Semi _ -> False
  Open _ -> False
  _ -> True

-- `True` if the tokens are adjacent, with no space separating the two
touches :: Token a -> Token b -> Bool
touches (end -> t) (start -> t2) =
  line t == line t2 && column t == column t2

top :: Layout -> Column
top [] = 1
top ((_, h) : _) = h

-- todo: make Layout a NonEmpty
topBlockName :: Layout -> Maybe BlockName
topBlockName [] = Nothing
topBlockName ((name, _) : _) = Just name

pop :: [a] -> [a]
pop = drop 1

topLeftCorner :: Pos
topLeftCorner = Pos 1 1

data T a = T a [T a] [a] | L a deriving (Functor, Foldable, Traversable)

headToken :: T a -> a
headToken (T a _ _) = a
headToken (L a) = a

instance Show a => Show (T a) where
  show (L a) = show a
  show (T open mid close) =
    show open ++ "\n"
      ++ indent "  " (intercalateMap "\n" show mid)
      ++ "\n"
      ++ intercalateMap "" show close
    where
      indent by s = by ++ (s >>= go by)
      go by '\n' = '\n' : by
      go _ c = [c]

reorderTree :: ([T a] -> [T a]) -> T a -> T a
reorderTree _ l@(L _) = l
reorderTree f (T open mid close) = T open (f (reorderTree f <$> mid)) close

tree :: [Token Lexeme] -> T (Token Lexeme)
tree toks = one toks const
  where
    one (open@(payload -> Open _) : ts) k = many (T open) [] ts k
    one (t : ts) k = k (L t) ts
    one [] k = k lastErr []
      where
        lastErr = case drop (length toks - 1) toks of
          [] -> L (Token (Err LayoutError) topLeftCorner topLeftCorner)
          (t : _) -> L $ t {payload = Err LayoutError}

    many open acc [] k = k (open (reverse acc) []) []
    many open acc (t@(payload -> Close) : ts) k = k (open (reverse acc) [t]) ts
    many open acc ts k = one ts $ \t ts -> many open (t : acc) ts k

stanzas :: [T (Token Lexeme)] -> [[T (Token Lexeme)]]
stanzas = go []
  where
    go acc [] = [reverse acc]
    go acc (t : ts) = case payload $ headToken t of
      Semi _ -> reverse (t : acc) : go [] ts
      _ -> go (t : acc) ts

-- Moves type and ability declarations to the front of the token stream
-- and move `use` statements to the front of each block
reorder :: [T (Token Lexeme)] -> [T (Token Lexeme)]
reorder = join . sortWith f . stanzas
  where
    f [] = 3 :: Int
    f (t0 : _) = case payload $ headToken t0 of
      Open mod | Set.member mod typeModifiers -> 1
      Open typOrA | Set.member typOrA typeOrAbility -> 1
      Reserved "use" -> 0
      _ -> 3 :: Int

lexer :: String -> String -> [Token Lexeme]
lexer scope rem =
  let t = tree $ lexer0' scope rem
      -- after reordering can end up with trailing semicolon at the end of
      -- a block, which we remove with this pass
      fixup ((payload -> Semi _) : t@(payload -> Close) : tl) = t : fixup tl
      fixup [] = []
      fixup (h : t) = h : fixup t
   in fixup . toList $ reorderTree reorder t

isDelayOrForce :: Char -> Bool
isDelayOrForce op = op == '\'' || op == '!'

-- Mapping between characters and their escape codes. Use parse/showEscapeChar
-- to convert.
escapeChars :: [(Char, Char)]
escapeChars =
  [ ('0', '\0'),
    ('a', '\a'),
    ('b', '\b'),
    ('f', '\f'),
    ('n', '\n'),
    ('r', '\r'),
    ('t', '\t'),
    ('v', '\v'),
    ('s', ' '),
    ('\'', '\''),
    ('"', '"'),
    ('\\', '\\')
  ]

-- Inverse of parseEscapeChar; map a character to its escaped version:
showEscapeChar :: Char -> Maybe Char
showEscapeChar c =
  Map.lookup c (Map.fromList [(x, y) | (y, x) <- escapeChars])

isSep :: Char -> Bool
isSep c = isSpace c || Set.member c delimiters

-- Not a keyword, '.' delimited list of wordyId0 (should not include a trailing '.')
wordyId0 :: String -> Either Err (String, String)
wordyId0 s = span' wordyIdChar s $ \case
  (id@(ch : _), rem)
    | not (Set.member id keywords)
        && wordyIdStartChar ch ->
      Right (id, rem)
  (id, _rem) -> Left (InvalidWordyId id)

wordyIdStartChar :: Char -> Bool
wordyIdStartChar ch = isAlpha ch || isEmoji ch || ch == '_'

wordyIdChar :: Char -> Bool
wordyIdChar ch =
  isAlphaNum ch || isEmoji ch || ch `elem` ['_', '!', '\'']

isEmoji :: Char -> Bool
isEmoji c = c >= '\x1F300' && c <= '\x1FAFF'

symbolyId :: String -> Either Err (String, String)
symbolyId r@('.' : s)
  | s == "" = symbolyId0 r --
  | isSpace (head s) = symbolyId0 r -- lone dot treated as an operator
  | isDelimiter (head s) = symbolyId0 r --
  | otherwise = (\(s, rem) -> ('.' : s, rem)) <$> symbolyId' s
symbolyId s = symbolyId' s

-- Is a '.' delimited list of wordyId, with a final segment of `symbolyId0`
symbolyId' :: String -> Either Err (String, String)
symbolyId' s = case wordyId0 s of
  Left _ -> symbolyId0 s
  Right (wid, '.' : rem) -> case symbolyId rem of
    Left e -> Left e
    Right (rest, rem) -> Right (wid <> "." <> rest, rem)
  Right (w, _) -> Left (InvalidSymbolyId w)

wordyId :: String -> Either Err (String, String)
wordyId ('.' : s) = (\(s, rem) -> ('.' : s, rem)) <$> wordyId' s
wordyId s = wordyId' s

-- Is a '.' delimited list of wordyId
wordyId' :: String -> Either Err (String, String)
wordyId' s = case wordyId0 s of
  Left e -> Left e
  Right (wid, '.' : rem@(ch : _)) | wordyIdStartChar ch -> case wordyId rem of
    Left e -> Left e
    Right (rest, rem) -> Right (wid <> "." <> rest, rem)
  Right (w, rem) -> Right (w, rem)

-- Returns either an error or an id and a remainder
symbolyId0 :: String -> Either Err (String, String)
symbolyId0 s = span' symbolyIdChar s $ \case
  (id@(_ : _), rem) | not (Set.member id reservedOperators) -> Right (id, rem)
  (id, _rem) -> Left (InvalidSymbolyId id)

symbolyIdChar :: Char -> Bool
symbolyIdChar ch = Set.member ch symbolyIdChars

symbolyIdChars :: Set Char
symbolyIdChars = Set.fromList "!$%^&*-=+<>~\\/|:"

keywords :: Set String
keywords =
  Set.fromList
    [ "if",
      "then",
      "else",
      "forall",
      "∀",
      "handle",
      "with",
      "where",
      "use",
      "true",
      "false",
      "alias",
      "typeLink",
      "termLink",
      "let",
      "namespace",
      "match",
      "cases"
    ]
    <> typeModifiers
    <> typeOrAbility

typeOrAbility :: Set String
typeOrAbility = Set.fromList ["type", "ability"]

typeOrAbilityAlt :: Alternative f => (String -> f a) -> f a
typeOrAbilityAlt f =
  asum $ map f (toList typeOrAbility)

typeModifiers :: Set String
typeModifiers = Set.fromList ["structural", "unique"]

typeModifiersAlt :: Alternative f => (String -> f a) -> f a
typeModifiersAlt f =
  asum $ map f (toList typeModifiers)

delimiters :: Set Char
delimiters = Set.fromList "()[]{},?;"

isDelimiter :: Char -> Bool
isDelimiter ch = Set.member ch delimiters

reservedOperators :: Set String
reservedOperators = Set.fromList ["=", "->", ":", "&&", "||", "|", "!", "'"]

inc :: Pos -> Pos
inc (Pos line col) = Pos line (col + 1)

debugFileLex :: String -> IO ()
debugFileLex file = do
  contents <- readUtf8 file
  let s = debugLex'' (lexer file (Text.unpack contents))
  putStrLn s

debugLex'' :: [Token Lexeme] -> String
debugLex'' [Token (Err (Opaque msg)) start end] =
  (if start == end then msg1 else msg2) <> ":\n" <> msg
  where
    msg1 = "Error on line " <> show (line start) <> ", column " <> show (column start)
    msg2 =
      "Error on line " <> show (line start) <> ", column " <> show (column start)
        <> " - line "
        <> show (line end)
        <> ", column "
        <> show (column end)
debugLex'' ts = show . fmap payload . tree $ ts

debugLex' :: String -> String
debugLex' = debugLex'' . lexer "debugLex"

debugLex''' :: String -> String -> String
debugLex''' s = debugLex'' . lexer s

span' :: (a -> Bool) -> [a] -> (([a], [a]) -> r) -> r
span' f a k = k (span f a)

instance EP.ShowErrorComponent (Token Err) where
  showErrorComponent (Token err _ _) = go err
    where
      go = \case
        Opaque msg -> msg
        CloseWithoutMatchingOpen open close -> "I found a closing " <> close <> " but no matching " <> open <> "."
        Both e1 e2 -> go e1 <> "\n" <> go e2
        LayoutError -> "Indentation error"
        TextLiteralMissingClosingQuote s -> "This text literal missing a closing quote: " <> excerpt s
        e -> show e
      excerpt s = if length s < 15 then s else take 15 s <> "..."

instance ShowToken (Token Lexeme) where
  showTokens xs =
    join . Nel.toList . S.evalState (traverse go xs) . end $ Nel.head xs
    where
      go :: Token Lexeme -> S.State Pos String
      go tok = do
        prev <- S.get
        S.put $ end tok
        pure $ pad prev (start tok) ++ pretty (payload tok)
      pretty (Open s) = s
      pretty (Reserved w) = w
      pretty (Textual t) = '"' : t ++ ['"']
      pretty (Character c) =
        case showEscapeChar c of
          Just c -> "?\\" ++ [c]
          Nothing -> '?' : [c]
      pretty (Backticks n h) =
        '`' : n ++ (toList h >>= SH.toString) ++ ['`']
      pretty (WordyId n h) = n ++ (toList h >>= SH.toString)
      pretty (SymbolyId n h) = n ++ (toList h >>= SH.toString)
      pretty (Blank s) = "_" ++ s
      pretty (Numeric n) = n
      pretty (Hash sh) = show sh
      pretty (Err e) = show e
      pretty (Bytes bs) = "0xs" <> show bs
      pretty Close = "<outdent>"
      pretty (Semi True) = "<virtual semicolon>"
      pretty (Semi False) = ";"
      pad (Pos line1 col1) (Pos line2 col2) =
        if line1 == line2
          then replicate (col2 - col1) ' '
          else replicate (line2 - line1) '\n' ++ replicate col2 ' '

instance Applicative Token where
  pure a = Token a (Pos 0 0) (Pos 0 0)
  Token f start _ <*> Token a _ end = Token (f a) start end
