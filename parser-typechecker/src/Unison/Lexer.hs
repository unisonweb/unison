{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Unison.Lexer where

import Unison.Prelude

import           Control.Lens.TH (makePrisms)
import qualified Control.Monad.State as S
import           Data.Char
import           Data.List
import qualified Data.List.NonEmpty as Nel
import Unison.Util.Monoid (intercalateMap)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           GHC.Exts (sortWith)
import           Text.Megaparsec.Error (ShowToken(..))
import           Unison.ShortHash ( ShortHash )
import qualified Unison.ShortHash as SH
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as CP
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as LP
import qualified Unison.Util.Bytes as Bytes

type Line = Int
type Column = Int
data Pos = Pos {-# Unpack #-} !Line {-# Unpack #-} !Column deriving (Eq,Ord,Show)
type BlockName = String
type Layout = [(BlockName,Column)]

data Token a = Token {
  payload :: a,
  start :: !Pos,
  end :: !Pos
} deriving (Eq, Ord, Show, Functor)

data ParsingEnv =
  ParsingEnv { layout :: !Layout
             , position :: !Pos
             , inLayout :: !Bool
             , opening :: Maybe String }

type P = P.ParsecT (Token Err) String (S.State ParsingEnv)

data Err
  = InvalidWordyId String
  | InvalidSymbolyId String
  | InvalidShortHash String
  | Both Err Err
  | MissingFractional String -- ex `1.` rather than `1.04`
  | MissingExponent String -- ex `1e` rather than `1e3`
  | UnknownLexeme
  | TextLiteralMissingClosingQuote String
  | InvalidEscapeCharacter Char
  | LayoutError
  | CloseWithoutMatchingOpen String String -- open, close
  deriving (Eq,Ord,Show) -- richer algebra

-- Design principle:
--   `[Lexeme]` should be sufficient information for parsing without
--   further knowledge of spacing or indentation levels
--   any knowledge of comments
data Lexeme
  = Open String      -- start of a block
  | Semi IsVirtual   -- separator between elements of a block
  | Close            -- end of a block
  | Reserved String  -- reserved tokens such as `{`, `(`, `type`, `of`, etc
  | Textual String   -- text literals, `"foo bar"`
  | Character Char   -- character literals, `?X`
  | Backticks String (Maybe ShortHash) -- an identifier in backticks
  | WordyId String   (Maybe ShortHash) -- a (non-infix) identifier
  | SymbolyId String (Maybe ShortHash) -- an infix identifier
  | Blank String     -- a typed hole or placeholder
  | Numeric String   -- numeric literals, left unparsed
  | Bytes Bytes.Bytes -- bytes literals
  | Hash ShortHash   -- hash literals
  | Err Err
  deriving (Eq,Show,Ord)

type IsVirtual = Bool -- is it a virtual semi or an actual semi?

makePrisms ''Lexeme

space :: P ()
space = LP.space CP.space1 (LP.skipLineComment "--") (LP.skipBlockCommentNested "{-" "-}")

lit :: String -> P String
lit = P.try . LP.symbol space

token :: P Lexeme -> P [Token Lexeme]
token = token' (\a start end -> [Token a start end])

pos :: P Pos
pos = do
  p <- P.getPosition
  pure $ Pos (P.unPos (P.sourceLine p)) (P.unPos (P.sourceColumn p))

token' :: (a -> Pos -> Pos -> [Token Lexeme]) -> P a -> P [Token Lexeme]
token' tok p = LP.lexeme space $ do
  start <- pos
  env <- S.get
  case opening env of
    Nothing -> pure ()
    Just blockname -> S.put $
      env { layout = (blockname, column start) : layout env, opening = Nothing }
  a <- p
  layoutToks <- pops start
  end <- pos
  pure $ layoutToks ++ (tok a start end)
  where
    pops :: Pos -> P [Token Lexeme]
    pops p = do
      env <- S.get
      let l = layout env
      if inLayout env then
        if top l == column p then pure [Token (Semi True) p p]
        else if top l < column p || topHasClosePair l then pure []
        else if top l > column p then
          S.put (env { layout = (pop l) }) >> ((Token Close p p :) <$> pops p)
        else error "impossible"
      else
        pure []

-- todo: implement function with same signature as the existing lexer function
-- to set up initial state, run the parser, etc
lexer0' :: P [Token Lexeme]
lexer0' = do
  toks <- P.many lexemes
  pure (join toks)

lexemes :: P [Token Lexeme]
lexemes = reserved
      <|> eof
      <|> (asum . map token) [semi, textual, character, backticks, wordyId, symbolyId, numeric, hash]
  where
  semi = char ';' $> Semi False

  textual = Textual <$> quoted
  quoted = char '"' *> P.manyTill LP.charLiteral (char '"')

  character =
    Character <$> (char '?' *> (spEsc <|> LP.charLiteral))
    where spEsc = P.try (char '\\' *> char 's')

  backticks =
    tick <$> (char '`' *> wordyId <* char '`')
    where tick (WordyId n sh) = Backticks n sh
          tick t = t

  wordyId :: P Lexeme
  wordyId = P.label wordyMsg . P.try $ do
    dot <- P.optional (lit ".")
    segs <- P.sepBy1 wordyIdSeg (char '.')
    shorthash <- P.optional shorthash
    pure $ WordyId (fromMaybe "" dot <> intercalate "." segs) shorthash

  symbolyId :: P Lexeme
  symbolyId = P.label symbolMsg . P.try $ do
    dot <- P.optional (lit ".")
    segs <- segs
    shorthash <- P.optional shorthash
    pure $ SymbolyId (fromMaybe "" dot <> segs) shorthash
    where
    segs = symbolyIdSeg <|> do
      hd <- wordyIdSeg
      _ <- char '.'
      tl <- segs
      pure (hd ++ tl)

  wordyMsg = "identifier (ex: abba1, _zoink, .foo.bar#xyz, or 🚀)"
  symbolMsg = "operator (ex: +, Float./, List.++#xyz)"

  symbolyIdSeg :: P String
  symbolyIdSeg = P.try $ do
    id <- P.takeWhile1P (Just symbolMsg) symbolyIdChar
    if Set.member id reservedOperators then fail "reserved operation"
    else pure id

  wordyIdSeg :: P String
  wordyIdSeg = litSeg <|> (P.try do
    ch <- CP.satisfy wordyIdStartChar
    rest <- P.many (CP.satisfy wordyIdChar)
    pure (ch : rest))

  -- ``an-identifier-with-dashes``
  -- ```an identifier with dashes```
  litSeg :: P String
  litSeg = P.try $ do
    ticks1 <- lit "``"
    ticks2 <- P.many (char '`')
    let ticks = ticks1 <> ticks2
    P.manyTill (CP.satisfy (const True)) (lit ticks)

  hashMsg = "hash (ex: #af3sj3)"
  shorthash = P.label hashMsg $ do
    P.lookAhead (char '#')
    -- `foo#xyz` should parse
    potentialHash <- P.takeWhile1P (Just hashMsg) (\ch -> not (isSep ch) && ch /= '`')
    case SH.fromString potentialHash of
      Nothing -> fail "invalid shorthash"
      Just sh -> pure sh

  numeric = intOrNat <|> float <|> bytes <|> otherbase
    where
      intOrNat = P.try $ num <$> sign <*> LP.decimal
      float = P.try $ Numeric . show @Double <$> LP.float
      bytes = P.try $ do
        _ <- lit "0xs"
        s <- map toLower <$> P.takeWhileP (Just "hexidecimal character") isHex
        case Bytes.fromBase16 $ Bytes.fromWord8s (fromIntegral . ord <$> s) of
          Left e -> fail (show e)
          Right bs -> pure (Bytes bs)
        where isHex ch | ch >= '0' && ch <= '9' = True
                       | ch >= 'a' && ch <= 'f' = True
                       | otherwise = False
      otherbase = P.try $ num <$> sign <*> ((lit "0o" >> LP.octal) <|> (lit "0x" >> LP.hexadecimal))

      num :: Maybe Char -> Integer -> Lexeme
      num sign n = Numeric (maybe "" (:[]) sign ++ show n)
      sign = P.optional (char '+' <|> char '-')

  hash = Hash <$> P.try shorthash

  reserved :: P [Token Lexeme]
  reserved =
    token' (\ts _ _ -> ts) $
    braces <|> parens <|> delim <|> delayOrForce <|> keywords <|> layoutKeywords
    where
    keywords = kw "@" <|> kw "||" <|> kw "|" <|> kw "&&" <|> kw "true" <|> kw "false"
           <|> kw "use" <|> kw "if" <|> kw "forall" <|> kw "∀"
           <|> kw "termLink" <|> kw "typeLink"

    kw' k = do
      pos <- pos
      k <- lit k
      P.lookAhead (CP.satisfy ok)
      pure (k, pos, incBy k pos)
      where
        ok c = isDelayOrForce c || isSpace c || isAlphaNum c || Set.member c delimiters

    layoutKeywords :: P [Token Lexeme]
    layoutKeywords =
      ifElse <|> matchWith <|> handle <|> typ <|> arr <|> eq <|>
      openKw "cases" <|> openKw "where" <|> openKw "let"
      where
        matchWith = openKw "match" <|> close' True "match" "with"
        ifElse = openKw "if" <|> close' True "if" "then" <|> close' True "then" "else"
        handle = openKw "handle" <|> close' True "handle" "with"
        typ = openKw1 "unique" <|> openKw1 "type" <|> openKw1 "ability"

        -- layout keyword which bumps the layout column by 1, rather than looking ahead
        -- to the next token to determine the layout column
        openKw1 :: String -> P [Token Lexeme]
        openKw1 kw = do
          pos0 <- pos
          kw <- lit kw
          pos1 <- pos
          S.modify (\env -> env { layout = (kw, column $ inc pos0) : layout env })
          pure [Token (Open kw) pos0 pos1]

        eq = do
          (_, start, end) <- kw' "="
          env <- S.get
          case topBlockName (layout env) of
            -- '=' does not open a layout block if within a type declaration
            Just t | t == "type" || t == "unique" -> pure [Token (Reserved "=") start end]
            Just _ -> S.put (env { opening = Just "=" }) >> pure [Token (Open "=") start end]
            _ -> P.customFailure (Token LayoutError start end)

        arr = do
          (_, start, end) <- kw' "->"
          env <- S.get
          -- -> introduces a layout block if we're inside a `match with` or `cases`
          case topBlockName (layout env) of
            Just match | match == "match-with" || match == "cases" -> do
              S.put (env { opening = Just "->" })
              pure [Token (Open "->") start end]
            _ -> pure [Token (Reserved "->") start end]

    braces = open "{" <|> close "{" "}"
    parens = open "(" <|> close "(" ")"
    delim = do
      ch <- CP.satisfy (`Set.member` delimiters)
      pos <- pos
      pure $ [Token (Reserved [ch]) pos (inc pos)]
    delayOrForce = P.try $ do
      op <- CP.satisfy isDelayOrForce
      P.lookAhead (CP.satisfy ok)
      pos <- pos
      pure [Token (Reserved [op]) pos (inc pos)]
      where
        ok c = isDelayOrForce c || isSpace c || isAlphaNum c || Set.member c delimiters

    kw :: String -> P [Token Lexeme]
    kw s = do
      pos1 <- pos
      s <- lit s
      P.lookAhead (P.eof <|> void (CP.satisfy ok))
      pos2 <- pos
      pure [Token (Reserved s) pos1 pos2]
      where
        ok :: Char -> Bool
        ok c = isSpace c || Set.member c delimiters || isAlphaNum c

    openKw :: String -> P [Token Lexeme]
    openKw s = do
      pos1 <- pos
      s <- lit s
      P.lookAhead (P.eof <|> void (CP.satisfy ok))
      pos2 <- pos
      env <- S.get
      S.put (env { opening = Just s })
      pure [Token (Open s) pos1 pos2]
      where
        ok :: Char -> Bool
        ok c = isSpace c || Set.member c delimiters

    close = close' False

    close' :: Bool -> String -> String -> P [Token Lexeme]
    close' reopen open close = do
      pos1 <- pos
      close <- lit close
      pos2 <- pos
      env <- S.get
      case findClose open (layout env) of
        Nothing -> P.customFailure (Token (CloseWithoutMatchingOpen open close) pos1 pos2)
        Just n -> do
          if reopen then
            S.put (env { layout = drop n (layout env), opening = Just close })
          else
            S.put (env { layout = drop n (layout env) })
          let opens = if reopen then [Token (Open close) pos1 pos2] else []
          pure $ replicate n (Token Close pos1 pos2) ++ opens

    open :: String -> P [Token Lexeme]
    open b = do
      pos <- pos
      _ <- lit b
      env <- S.get
      S.put (env { opening = Just b })
      pure [Token (Open b) pos (incBy b pos)]

  eof :: P [Token Lexeme]
  eof = P.try $ do
    p <- P.eof >> pos
    l <- S.gets layout
    pure $ replicate (length l) (Token Close p p)

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

line :: Pos -> Line
line (Pos line _) = line

column :: Pos -> Column
column (Pos _ column) = column

-- `True` if the tokens are adjacent, with no space separating the two
touches :: Token a -> Token b -> Bool
touches (end -> t) (start -> t2) =
  line t == line t2 && column t == column t2

top :: Layout -> Column
top []    = 1
top ((_,h):_) = h

-- todo: make Layout a NonEmpty
topBlockName :: Layout -> Maybe BlockName
topBlockName [] = Nothing
topBlockName ((name,_):_) = Just name

topHasClosePair :: Layout -> Bool
topHasClosePair [] = False
topHasClosePair ((name,_):_) = name == "{" || name == "("

findNearest :: Layout -> Set BlockName -> Maybe BlockName
findNearest l ns =
  case topBlockName l of
    Just n -> if Set.member n ns then Just n else findNearest (pop l) ns
    Nothing -> Nothing

-- Figure out how many elements must be popped from the layout stack
-- before finding a matching `Open` token
findClose :: String -> Layout -> Maybe Int
findClose _ [] = Nothing
findClose s ((h,_):tl) = if s == h then Just 1 else (1+) <$> findClose s tl


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
              ++ indent "  " (intercalateMap "\n" show mid) ++ "\n"
              ++ intercalateMap "" show close
    where
      indent by s = by ++ (s >>= go by)
      go by '\n' = '\n' : by
      go _ c = [c]

reorderTree :: ([T a] -> [T a]) -> T a -> T a
reorderTree _ l@(L _) = l
reorderTree f (T open mid close) = T open (f (reorderTree f <$> mid)) close

tree :: [Token Lexeme] -> T (Token Lexeme)
tree toks = one toks const where
  one (open@(payload -> Open _) : ts) k = many (T open) [] ts k
  one (t : ts) k = k (L t) ts
  one [] k = k lastErr [] where
    lastErr = case drop (length toks - 1) toks of
      [] -> L (Token (Err LayoutError) topLeftCorner topLeftCorner)
      (t : _) -> L $ t { payload = Err LayoutError }

  many open acc [] k = k (open (reverse acc) []) []
  many open acc (t@(payload -> Close) : ts) k = k (open (reverse acc) [t]) ts
  many open acc ts k = one ts $ \t ts -> many open (t:acc) ts k

stanzas :: [T (Token Lexeme)] -> [[T (Token Lexeme)]]
stanzas = go [] where
  go acc [] = [reverse acc]
  go acc (t:ts) = case payload $ headToken t of
    Semi _ -> reverse (t : acc) : go [] ts
    _      -> go (t:acc) ts

-- Moves type and effect declarations to the front of the token stream
-- and move `use` statements to the front of each block
reorder :: [T (Token Lexeme)] -> [T (Token Lexeme)]
reorder = join . sortWith f . stanzas
  where
    f [] = 3 :: Int
    f (t : _) = case payload $ headToken t of
      Open "type" -> 1
      Open "unique" -> 1
      Open "ability" -> 1
      Reserved "use" -> 0
      _ -> 3 :: Int

lexer :: String -> String -> [Token Lexeme]
lexer scope rem =
  let t = tree $ lexer0 scope rem
      -- after reordering can end up with trailing semicolon at the end of
      -- a block, which we remove with this pass
      fixup ((payload -> Semi _) : t@(payload -> Close) : tl) = t : fixup tl
      fixup [] = []
      fixup (h : t) = h : fixup t
  in fixup . toList $ reorderTree reorder t

lexer0 :: String -> String -> [Token Lexeme]
lexer0 scope rem =
    tweak $ Token (Open scope) topLeftCorner topLeftCorner
      : pushLayout scope [] topLeftCorner rem
  where
    -- hacky postprocessing pass to do some cleanup of stuff that's annoying to
    -- fix without adding more state to the lexer:
    --   - 1+1 lexes as [1, +1], convert this to [1, +, 1]
    --   - when a semi followed by a virtual semi, drop the virtual, lets you
    --     write
    --       foo x = action1;
    --               2
    tweak [] = []
    tweak (h@(payload -> Semi False):(payload -> Semi True):t) = h : tweak t
    tweak (h@(payload -> Reserved _):t) = h : tweak t
    tweak (t1:t2@(payload -> Numeric num):rem)
      | notLayout t1 && touches t1 t2 && isSigned num =
        t1 : Token (SymbolyId (take 1 num) Nothing)
                   (start t2)
                   (inc $ start t2)
           : Token (Numeric (drop 1 num)) (inc $ start t2) (end t2)
           : tweak rem
    tweak (h:t) = h : tweak t
    isSigned num = all (\ch -> ch == '-' || ch == '+') $ take 1 num
    -- skip whitespace and comments
    goWhitespace :: Layout -> Pos -> String -> [Token Lexeme]
    goWhitespace l pos rem = span' isSpace rem $ \case
      (_spaces, '-':'-':'-':_rem) -> popLayout0 l pos []
      (spaces, '-':'-':rem) -> spanThru' (/= '\n') rem $ \(ignored, rem) ->
        goWhitespace l (incBy ('-':'-':ignored) . incBy spaces $ pos) rem
      (spaces, rem) -> popLayout l (incBy spaces pos) rem

    popLayout :: Layout -> Pos -> String -> [Token Lexeme]
    popLayout l pos rem = case matchKeyword' layoutCloseAndOpenKeywords rem of
      Nothing -> case matchKeyword' layoutCloseOnlyKeywords rem of
        Nothing -> popLayout0 l pos rem
        Just (kw, rem) ->
          let end = incBy kw pos
          in Token Close pos end
               : Token (Reserved kw) pos end
               : goWhitespace (pop l) (incBy kw pos) rem
      Just (kw, rem) ->
        let kw' = layoutCloseAndOpenKeywordMap kw l in
        case closes (openingKeyword kw') kw' l pos of
          (Nothing, ts) -> ts ++ recover l (incBy kw pos) rem
          (Just l, ts) ->
            let end = incBy kw pos
            in ts ++ [Token (Open kw) pos end] ++ pushLayout kw' l end rem

    -- Examine current column and pop the layout stack
    -- and emit `Semi` / `Close` tokens as needed
    popLayout0 :: Layout -> Pos -> String -> [Token Lexeme]
    popLayout0 l p [] = replicate (length l) $ Token Close p p
    popLayout0 l p@(Pos _ c2) rem
      | top l == c2 = Token (Semi True) p p : go l p rem
      | top l <  c2 || topHasClosePair l = go l p rem
      | top l >  c2 = Token Close p p : popLayout0 (pop l) p rem
      | otherwise   = error "impossible"

    -- todo: is there a reason we want this to be more than just:
    -- go1 (top l + 1 : l) pos rem
    -- looks for the next non whitespace, non-comment character, and
    -- pushes its column onto the layout stack
    pushLayout :: BlockName -> Layout -> Pos -> String -> [Token Lexeme]
    pushLayout b l pos rem = span' isSpace rem $ \case
      (_spaces, '-':'-':'-':_rem) ->
        -- short circuit - everything after `---` is ignored
        popLayout0 ((b,column pos):l) pos []
      (spaces, '-':'-':rem) -> spanThru' (/= '\n') rem $ \(ignored, rem) ->
        pushLayout b l (incBy ('-':'-':ignored) . incBy spaces $ pos) rem
      (spaces, rem) ->
        let topcol = top l
            pos'   = incBy spaces pos
            col'   = column pos'
        in
          if b == "=" && col' <= topcol then
            -- force closing by introducing a fake col +1 layout
            popLayout0 ((b, col' + 1) : l) pos' rem
          else
            go ((b, col') : l) pos' rem

    -- Closes a layout block with the given open/close pair, e.g `close "(" ")"`
    close :: String -> String -> Layout -> Pos -> String -> [Token Lexeme]
    close open close l pos rem = case findClose open l of
      Nothing -> [Token (Err $ CloseWithoutMatchingOpen open close) pos pos]
      Just n ->
        let closes = replicate n $ Token Close pos (incBy close pos)
        in closes ++ goWhitespace (drop n l) (incBy close pos) rem

    -- If the close is well-formed, returns a new layout stack and the correct
    -- number of `Close` tokens. If the close isn't well-formed (has no match),
    -- `Nothing` is returned along an error token.
    closes :: String -> String -> Layout -> Pos
          -> (Maybe Layout, [Token Lexeme])
    closes open close l pos = case findClose open l of
      Nothing -> (Nothing,
        [Token (Err $ CloseWithoutMatchingOpen open close) pos (incBy close pos)])
      Just n ->
        (Just $ drop n l, replicate n $ Token Close pos (incBy close pos))

    -- assuming we've dealt with whitespace and layout, read a token
    go :: Layout -> Pos -> String -> [Token Lexeme]
    go l pos rem = case rem of
      [] -> popLayout0 l pos []
      '?' : '\\' : c : rem ->
        case parseEscapeChar c of
          Just c ->
            let end = inc $ inc $ inc pos in
            Token (Character c) pos end : goWhitespace l end rem
          Nothing ->
            [Token (Err $ InvalidEscapeCharacter c) pos pos]
      '?' : c : rem ->
        let end = inc $ inc pos in
        Token (Character c) pos end : goWhitespace l end rem
      '[' : ':' : rem ->
        let end = inc . inc $ pos in
        Token (Open "[:") pos (inc . inc $ pos) : lexDoc l end rem
      -- '{' and '(' both introduce a block, which is closed by '}' and ')'
      -- The lexer doesn't distinguish among closing blocks: all the ways of
      -- closing a block emit the same sort of token, `Close`.
      --
      -- Note: within {}'s, `->` does not open a block, since `->` is used
      -- inside request patterns like `{State.set s -> k}`
      '{' : rem -> Token (Open "{") pos (inc pos) : pushLayout "{" l (inc pos) rem
      '}' : rem -> close "{" "}" l pos rem
      '(' : rem -> Token (Open "(") pos (inc pos) : pushLayout "(" l (inc pos) rem
      ')' : rem -> close "(" ")" l pos rem
      ';' : rem -> Token (Semi False) pos (inc pos) : goWhitespace l (inc pos) rem
      ch : rem | Set.member ch delimiters ->
        Token (Reserved [ch]) pos (inc pos) : goWhitespace l (inc pos) rem
      op : rem@(c : _)
        | isDelayOrForce op
        && (isSpace c || isAlphaNum c
            || Set.member c delimiters || isDelayOrForce c) ->
          Token (Reserved [op]) pos (inc pos) : goWhitespace l (inc pos) rem
      ':' : rem@(c : _) | isSpace c || isAlphaNum c ->
        Token (Reserved ":") pos (inc pos) : goWhitespace l (inc pos) rem
      '@' : rem ->
        Token (Reserved "@") pos (inc pos) : goWhitespace l (inc pos) rem
      '_' : rem | hasSep rem ->
        Token (Blank "") pos (inc pos) : goWhitespace l (inc pos) rem
      '_' : (wordyId -> Right (id, rem)) ->
        let pos' = incBy id $ inc pos
        in Token (Blank id) pos pos' : goWhitespace l pos' rem
      '&' : '&' : rem ->
        let end = incBy "&&" pos
        in Token (Reserved "&&") pos end : goWhitespace l end rem
      '|' : '|' : rem ->
        let end = incBy "||" pos
        in Token (Reserved "||") pos end : goWhitespace l end rem
      '|' : c : rem | isSpace c || isAlphaNum c ->
        Token (Reserved "|") pos (inc pos) : goWhitespace l (inc pos) (c:rem)
      '=' : rem@(c : _) | isSpace c || isAlphaNum c ->
        let end = inc pos
        in case topBlockName l of
          -- '=' does not open a layout block if within a type declaration
          Just "type"   -> Token (Reserved "=") pos end : goWhitespace l end rem
          Just "unique" -> Token (Reserved "=") pos end : goWhitespace l end rem
          Just _      -> Token (Open "=") pos end : pushLayout "=" l end rem
          Nothing     -> Token (Err LayoutError) pos pos : recover l pos rem
      '-' : '>' : rem@(c : _)
        | isSpace c || isAlphaNum c || Set.member c delimiters ->
          let end = incBy "->" pos
          in case topBlockName l of
              Just "match-with" -> -- `->` opens a block when pattern-matching only
                Token (Open "->") pos end : pushLayout "->" l end rem
              Just "cases" -> -- `->` opens a block when pattern-matching only
                Token (Open "->") pos end : pushLayout "->" l end rem
              Just _ -> Token (Reserved "->") pos end : goWhitespace l end rem
              Nothing -> Token (Err LayoutError) pos pos : recover l pos rem

      -- string literals and backticked identifiers
      '"' : rem -> case splitStringLit rem of
        Right (delta, lit, rem) -> let end = pos <> delta in
          Token (Textual lit) pos end : goWhitespace l end rem
        Left (TextLiteralMissingClosingQuote _) ->
          [Token (Err $ TextLiteralMissingClosingQuote rem) pos pos]
        Left err -> [Token (Err err) pos pos]
      '`' : rem -> case wordyId rem of
        Left e -> Token (Err e) pos pos : recover l pos rem
        Right (id, rem) ->
          if ['#'] `isPrefixOf` rem then
             case shortHash rem of
               Left e -> Token (Err e) pos pos : recover l pos rem
               Right (h, rem) ->
                 let end = inc . incBy id . incBy (SH.toString h) . inc $ pos
                  in Token (Backticks id (Just h)) pos end
                     : goWhitespace l end (pop rem)
          else
           let end = inc . incBy id . inc $ pos
            in Token (Backticks id Nothing) pos end
                 : goWhitespace l end (pop rem)

      rem@('#' : _) -> case shortHash rem of
         Left e -> Token (Err e) pos pos : recover l pos rem
         Right (h, rem) ->
           let end = incBy (SH.toString h) pos
           in Token (Hash h) pos end : goWhitespace l end rem
      -- keywords and identifiers
      (symbolyId -> Right (id, rem')) -> case numericLit rem of
        Right (Just (num, rem)) ->
          let end = incBy num pos
          in Token (Numeric num) pos end : goWhitespace l end rem
        _ -> if ['#'] `isPrefixOf` rem' then
               case shortHash rem' of
                 Left e -> Token (Err e) pos pos : recover l pos rem'
                 Right (h, rem) ->
                   let end = incBy id . incBy (SH.toString h) $ pos
                    in Token (SymbolyId id (Just h)) pos end
                       : goWhitespace l end rem
             else
              let end = incBy id pos
               in Token (SymbolyId id Nothing) pos end : goWhitespace l end rem'
      (wordyId -> Right (id, rem)) ->
        if ['#'] `isPrefixOf` rem then
          case shortHash rem of
            Left e -> Token (Err e) pos pos : recover l pos rem
            Right (h, rem) ->
              let end = incBy id . incBy (SH.toString h) $ pos
               in Token (WordyId id (Just h)) pos end
                  : goWhitespace l end rem
        else let end = incBy id pos
              in Token (WordyId id Nothing) pos end : goWhitespace l end rem
      (matchKeyword -> Just (kw,rem)) ->
        let end = incBy kw pos in
              case kw of
                -- `unique type` lexes as [Open "unique", Reserved "type"]
                -- `type` lexes as [Open "type"]
                -- `unique ability` lexes as [Open "unique", Reserved "ability"]
                -- `ability` lexes as [Open "ability"]
                kw@"unique" ->
                  Token (Open kw) pos end
                    : goWhitespace ((kw, column $ inc pos) : l) end rem
                kw@"ability" | topBlockName l /= Just "unique" ->
                  Token (Open kw) pos end
                    : goWhitespace ((kw, column $ inc pos) : l) end rem
                kw@"type" | topBlockName l /= Just "unique" ->
                  Token (Open kw) pos end
                    : goWhitespace ((kw, column $ inc pos) : l) end rem
                kw | Set.member kw layoutKeywords ->
                       Token (Open kw) pos end : pushLayout kw l end rem
                   | otherwise -> Token (Reserved kw) pos end : goWhitespace l end rem

      -- numeric literals
      rem -> case numericLit rem of
        Right (Just (num, rem)) ->
          let end = incBy num pos in Token (Numeric num) pos end : goWhitespace l end rem
        Right Nothing -> Token (Err UnknownLexeme) pos pos : recover l pos rem
        Left e -> Token (Err e) pos pos : recover l pos rem

    lexDoc l pos rem = case span (\c -> isSpace c && not (c == '\n')) rem of
      (spaces,rem) -> docBlob l pos' rem pos' []
        where pos' = incBy spaces pos

    docBlob l pos rem blobStart acc = case rem of
      '@' : (hqToken (inc pos) -> Just (tok, rem)) ->
        let pos' = inc $ end tok in
        Token (Textual (reverse acc)) blobStart pos :
        tok :
        docBlob l pos' rem pos' []
      '@' : (docType (inc pos) -> Just (typTok, pos', rem)) ->
        Token (Textual (reverse acc)) blobStart pos : case rem of
         (hqToken pos' -> Just (tok, rem)) ->
           let pos'' = inc (end tok) in
           typTok : tok : docBlob l pos'' rem pos' []
         _ -> recover l pos rem
      '\\' : '@' : rem -> docBlob l (incBy "\\@" pos) rem blobStart ('@':acc)
      '\\' : ':' : ']' : rem -> docBlob l (incBy "\\:]" pos) rem blobStart (']':':':acc)
      ':' : ']' : rem ->
        let pos' = inc . inc $ pos in
        (if null acc then id
         else (Token (Textual (reverse
          $ dropWhile (\c -> isSpace c && not (c == '\n')) acc)) blobStart pos :)) $
          Token Close pos pos' : goWhitespace l pos' rem
      [] -> recover l pos rem
      ch : rem -> docBlob l (incBy [ch] pos) rem blobStart (ch:acc)

    docType :: Pos -> String -> Maybe (Token Lexeme, Pos, String)
    docType pos rem = case rem of
      -- this crazy one liner parses [<stuff>]<whitespace>, as a pattern match
      '[' : (span (/= ']') -> (typ, ']' : (span isSpace -> (spaces, rem)))) ->
         -- advance past [, <typ>, ], <whitespace>
         let pos' = incBy typ . inc . incBy spaces . inc $ pos in
         -- the reserved token doesn't include the `[]` chars
         Just (Token (Reserved typ) (inc pos) (incBy typ . inc $ pos), pos', rem)
      _ -> Nothing

    hqToken :: Pos -> String -> Maybe (Token Lexeme, String)
    hqToken pos rem = case rem of
      (shortHash -> Right (h, rem)) ->
        Just (Token (Hash h) pos (incBy (SH.toString h) pos), rem)
      (wordyId -> Right (id, rem)) -> case rem of
        (shortHash -> Right (h, rem)) ->
          Just (Token (WordyId id $ Just h) pos (incBy id . incBy (SH.toString h) $ pos), rem)
        _ -> Just (Token (WordyId id Nothing) pos (incBy id pos), rem)
      (symbolyId -> Right (id, rem)) -> case rem of
        (shortHash -> Right (h, rem)) ->
          Just (Token (SymbolyId id $ Just h) pos (incBy id . incBy (SH.toString h) $ pos), rem)
        _ -> Just (Token (SymbolyId id Nothing) pos (incBy id pos), rem)
      _ -> Nothing

    recover _l _pos _rem = []

isDelayOrForce :: Char -> Bool
isDelayOrForce op = op == '\''|| op == '!'

matchKeyword :: String -> Maybe (String,String)
matchKeyword = matchKeyword' keywords

matchKeyword' :: Set String -> String -> Maybe (String,String)
matchKeyword' keywords s = case break isSep s of
  (kw, rem) | Set.member kw keywords -> Just (kw, rem)
  _ -> Nothing

-- Split into a string literal and the remainder, and a delta which includes
-- both the starting and ending `"` character
-- The input string should only start with a '"' if the string literal is empty
splitStringLit :: String -> Either Err (Pos, String, String)
splitStringLit = go (inc mempty) "" where
  -- n tracks the raw character delta of this literal
  go !n !acc ('\\':s:rem) = case parseEscapeChar s of
    Just e -> go (inc . inc $ n) (e:acc) rem
    Nothing  -> Left $ InvalidEscapeCharacter s
  go !n !acc ('"':rem)    = Right (inc n, reverse acc, rem)
  go !n !acc (x:rem)      = go (inc n) (x:acc) rem
  go _ _ []               = Left $ TextLiteralMissingClosingQuote ""

-- Mapping between characters and their escape codes. Use parse/showEscapeChar
-- to convert.
escapeChars :: [(Char, Char)]
escapeChars =
  [ ('0', '\0')
  , ('a', '\a')
  , ('b', '\b')
  , ('f', '\f')
  , ('n', '\n')
  , ('r', '\r')
  , ('t', '\t')
  , ('v', '\v')
  , ('s', ' ')
  , ('\'', '\'')
  , ('"', '"')
  , ('\\', '\\')
  ]

-- Map a escape symbol to it's character literal
parseEscapeChar :: Char -> Maybe Char
parseEscapeChar c =
  Map.lookup c (Map.fromList escapeChars)

-- Inverse of parseEscapeChar; map a character to its escaped version:
showEscapeChar :: Char -> Maybe Char
showEscapeChar c =
  Map.lookup c (Map.fromList [(x, y) | (y, x) <- escapeChars])

numericLit :: String -> Either Err (Maybe (String,String))
numericLit = go
  where
  go ('+':s) = go2 "+" s
  go ('-':s) = go2 "-" s
  go s = go2 "" s
  go2 sign s = case span isDigit s of
    (num@(_:_), []) -> pure $ pure (sign ++ num, [])
    (num@(_:_), '.':rem) -> case span isDigit rem of
      (fractional@(_:_), []) ->
        pure $ pure (sign ++ num ++ "." ++ fractional, [])
      (fractional@(_:_), c:rem)
        | c `elem` "eE" -> goExp (sign ++ num ++ "." ++ fractional) rem
        | isSep c -> pure $ pure (sign ++ num ++ "." ++ fractional, c:rem)
        | otherwise -> pure Nothing
      ([], _) -> Left (MissingFractional (sign ++ num ++ "."))
    (num@(_:_), c:rem) | c `elem` "eE" -> goExp (sign ++ num) rem
    (num@(_:_), c:rem) -> pure $ pure (sign ++ num, c:rem)
    ([], _) -> pure Nothing
  goExp signNum rem = case rem of
    ('+':s) -> goExp' signNum "+" s
    ('-':s) -> goExp' signNum "-" s
    s       -> goExp' signNum ""  s
  goExp' signNum expSign exp = case span isDigit exp of
    (_:_, []) ->
      pure $ pure (signNum ++ "e" ++ expSign ++ exp, [])
    (exp'@(_:_), c:rem)
      | isSep c -> pure $ pure (signNum ++ "e" ++ expSign ++ exp', c:rem)
      | otherwise -> pure Nothing
    ([], _) -> Left (MissingExponent (signNum ++ "e" ++ expSign))

isSep :: Char -> Bool
isSep c = isSpace c || Set.member c delimiters

hasSep :: String -> Bool
hasSep [] = True
hasSep (ch:_) = isSep ch

-- Not a keyword, '.' delimited list of wordyId0 (should not include a trailing '.')
wordyId0 :: String -> Either Err (String, String)
wordyId0 s = span' wordyIdChar s $ \case
  (id@(ch:_), rem) | not (Set.member id keywords)
                    && wordyIdStartChar ch
                    -> Right (id, rem)
  (id, _rem) -> Left (InvalidWordyId id)

wordyIdStartChar :: Char -> Bool
wordyIdStartChar ch = isAlpha ch || isEmoji ch || ch == '_'

wordyIdChar :: Char -> Bool
wordyIdChar ch =
  isAlphaNum ch || isEmoji ch || ch `elem` "_!'"

isEmoji :: Char -> Bool
isEmoji c = c >= '\x1F300' && c <= '\x1FAFF'

symbolyId :: String -> Either Err (String, String)
symbolyId r@('.':s)
  | s == ""              = symbolyId0 r --
  | isSpace (head s)     = symbolyId0 r -- lone dot treated as an operator
  | isDelimiter (head s) = symbolyId0 r --
  | otherwise            = (\(s, rem) -> ('.':s, rem)) <$> symbolyId' s
symbolyId s = symbolyId' s

-- Is a '.' delimited list of wordyId, with a final segment of `symbolyId0`
symbolyId' :: String -> Either Err (String, String)
symbolyId' s = case wordyId0 s of
  Left _ -> symbolyId0 s
  Right (wid, '.':rem) -> case symbolyId rem of
    Left e -> Left e
    Right (rest, rem) -> Right (wid <> "." <> rest, rem)
  Right (w,_) -> Left (InvalidSymbolyId w)

wordyId :: String -> Either Err (String, String)
wordyId ('.':s) = (\(s,rem) -> ('.':s,rem)) <$> wordyId' s
wordyId s = wordyId' s

-- Is a '.' delimited list of wordyId
wordyId' :: String -> Either Err (String, String)
wordyId' s = case wordyId0 s of
  Left e -> Left e
  Right (wid, '.':rem@(ch:_)) | wordyIdStartChar ch -> case wordyId rem of
    Left e -> Left e
    Right (rest, rem) -> Right (wid <> "." <> rest, rem)
  Right (w,rem) -> Right (w,rem)

-- Is a `ShortHash`
shortHash :: String -> Either Err (ShortHash, String)
shortHash s = case SH.fromString potentialHash of
  Nothing -> Left (InvalidShortHash potentialHash)
  Just x  -> Right (x, rem)
  where (potentialHash, rem) = break ((||) <$> isSpace <*> (== '`')) s

-- Returns either an error or an id and a remainder
symbolyId0 :: String -> Either Err (String, String)
symbolyId0 s = span' symbolyIdChar s $ \case
  (id@(_:_), rem) | not (Set.member id reservedOperators) -> Right (id, rem)
  (id, _rem) -> Left (InvalidSymbolyId id)

symbolyIdChar :: Char -> Bool
symbolyIdChar ch = Set.member ch symbolyIdChars

symbolyIdChars :: Set Char
symbolyIdChars = Set.fromList "!$%^&*-=+<>.~\\/|:"

keywords :: Set String
keywords = Set.fromList [
  "if", "then", "else", "forall", "∀",
  "handle", "with", "unique",
  "where", "use",
  "true", "false",
  "type", "ability", "alias", "typeLink", "termLink",
  "let", "namespace", "match", "cases"]

-- These keywords introduce a layout block
layoutKeywords :: Set String
layoutKeywords =
  Set.fromList [
    "if", "handle", "let", "where", "match", "cases"
  ]

-- These keywords end a layout block and begin another layout block
layoutCloseAndOpenKeywords :: Set String
layoutCloseAndOpenKeywords = Set.fromList ["then", "else", "with"]

-- Use a transformed block name to disambiguate certain keywords
layoutCloseAndOpenKeywordMap :: String    -- close-and-open keyword
                             -> Layout    -- layout
                             -> BlockName -- transformed blockname for keyword
layoutCloseAndOpenKeywordMap "with" l =
  case findNearest l (Set.fromList ["handle", "match"]) of
    Just "match"  -> "match-with"
    Just "handle" -> "handle-with"
    _ -> "with"
layoutCloseAndOpenKeywordMap kw _ = kw

openingKeyword :: BlockName -> String
openingKeyword        "then" = "if"
openingKeyword        "else" = "then"
openingKeyword        "with" = "match or handle" -- hack!!
openingKeyword  "match-with" = "match"
openingKeyword "handle-with" = "handle"
openingKeyword kw = error $ "Not sure what the opening keyword is for: " <> kw

-- These keywords end a layout block
layoutCloseOnlyKeywords :: Set String
layoutCloseOnlyKeywords = Set.fromList ["}"]

delimiters :: Set Char
delimiters = Set.fromList "()[]{},?;"

isDelimiter :: Char -> Bool
isDelimiter ch = Set.member ch delimiters

reservedOperators :: Set String
reservedOperators = Set.fromList ["->", ":", "&&", "||"]

inc :: Pos -> Pos
inc (Pos line col) = Pos line (col + 1)

incBy :: String -> Pos -> Pos
incBy rem pos@(Pos line col) = case rem of
  []       -> pos
  '\r':rem -> incBy rem $ Pos line col
  '\n':rem -> incBy rem $ Pos (line + 1) 1
  _:rem    -> incBy rem $ Pos line (col + 1)

debugLex'' :: [Token Lexeme] -> String
debugLex'' = show . fmap payload . tree

debugLex' :: String -> String
debugLex' =  debugLex'' . lexer "debugLex"

debugLex''' :: String -> String -> String
debugLex''' s =  debugLex'' . lexer s

span' :: (a -> Bool) -> [a] -> (([a],[a]) -> r) -> r
span' f a k = k (span f a)

spanThru' :: (a -> Bool) -> [a] -> (([a],[a]) -> r) -> r
spanThru' f a k = case span f a of
  (l, []) -> k (l, [])
  (l, lz:r) -> k (l ++ [lz], r)

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

instance Semigroup Pos where (<>) = mappend

instance Monoid Pos where
  mempty = Pos 0 0
  Pos line col `mappend` Pos line2 col2 =
    if line2 == 0 then Pos line (col + col2)
    else Pos (line + line2) col2

