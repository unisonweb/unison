{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Unison.Lexer (
  Token(..), Line, Column, Err(..), Pos(..), Lexeme(..),
  lexer, simpleWordyId, simpleSymbolyId,
  line, column,
  escapeChars,
  debugLex', debugLex'', debugLex''', showEscapeChar, touches,
  -- todo: these probably don't belong here
  wordyIdChar, wordyIdStartChar,
  wordyId, symbolyId, wordyId0, symbolyId0)
  where

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
import qualified Text.Megaparsec.Error as EP
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
  ParsingEnv { layout :: !Layout -- layout stack
             , opening :: Maybe BlockName } -- `Just b` if a block of type `b` is being opened

type P = P.ParsecT (Token Err) String (S.State ParsingEnv)

data Err
  = InvalidWordyId String
  | InvalidSymbolyId String
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
  | Opaque String -- Catch-all failure type, generally these will be
                  -- automatically generated errors coming from megaparsec
                  -- Try to avoid this for common errors a user is likely to see.
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
space = LP.space CP.space1 (fold <|> LP.skipLineComment "--")
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
  (a,b) <- P.try $ liftA2 (,) a b
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
      if blockname == "=" && column start <= top l && not (null l) then do
        S.put (env { layout = (blockname, column start + 1) : l, opening = Nothing })
        pops start
      else [] <$ S.put (env { layout = layout', opening = Nothing })
      where layout' = (blockname, column start) : l
            l = layout env
    -- If we're not opening a block, we potentially pop from
    -- the layout stack and/or emit virtual semicolons.
    Nothing -> pops start
  a <- p <|> (S.put env >> fail "resetting state")
  end <- pos
  pure $ layoutToks ++ tok a start end
  where
    pops :: Pos -> P [Token Lexeme]
    pops p = do
      env <- S.get
      let l = layout env
      if top l == column p then pure [Token (Semi True) p p]
      else if column p > top l || topHasClosePair l then pure []
      else if column p < top l then
        -- traceShow (l, p) $
        S.put (env { layout = pop l }) >> ((Token Close p p :) <$> pops p)
      else error "impossible"

    topHasClosePair :: Layout -> Bool
    topHasClosePair [] = False
    topHasClosePair ((name,_):_) = name `elem` ["{", "(", "handle", "match", "if", "then"]

-- todo: implement function with same signature as the existing lexer function
-- to set up initial state, run the parser, etc
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
  customErrs es = [ Err <$> e | P.ErrorCustom e <- toList es ]
  toPos (P.SourcePos _ line col) = Pos (P.unPos line) (P.unPos col)
  env0 = ParsingEnv [] (Just scope)
  -- hacky postprocessing pass to do some cleanup of stuff that's annoying to
  -- fix without adding more state to the lexer:
  --   - 1+1 lexes as [1, +1], convert this to [1, +, 1]
  --   - when a semi followed by a virtual semi, drop the virtual, lets you
  --     write
  --       foo x = action1;
  --               2
  --   - semi immediately after first Open is ignored
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

infixl 2 <+>
(<+>) :: Monoid a => P a -> P a -> P a
p1 <+> p2 = do a1 <- p1; a2 <- p2; pure (a1 <> a2)

lexemes :: P [Token Lexeme]
lexemes = P.optional space >> do
  hd <- join <$> P.manyTill toks (P.lookAhead P.eof)
  tl <- eof
  pure $ hd <> tl
  where
  toks = doc <|> token numeric <|> token character <|> reserved <|> token symbolyId
     <|> token blank <|> token wordyId
     <|> (asum . map token) [ semi, textual, backticks, hash ]

  wordySep c = isSpace c || not (isAlphaNum c)
  positioned p = do start <- pos; a <- p; stop <- pos; pure (start, a, stop)

  tok :: P a -> P [Token a]
  tok p = do (start,a,stop) <- positioned p
             pure [Token a start stop]

  doc :: P [Token Lexeme]
  doc = open <+> (CP.space *> fmap fixup body) <+> (close <* space) where
    open = token'' (\t _ _ -> t) $ tok (Open <$> lit "[:")
    close = tok (Close <$ lit ":]")
    at = lit "@"
    -- this removes some trailing whitespace from final textual segment
    fixup [] = []
    fixup (Token (Textual (reverse -> txt)) start stop : [])
      = [Token (Textual txt') start stop]
      where txt' = reverse (dropWhile (\c -> isSpace c && not (c == '\n')) txt)
    fixup (h:t) = h : fixup t

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

  blank = separated wordySep $
    char '_' *> P.optional wordyIdSeg <&> (Blank . fromMaybe "")

  semi = char ';' $> Semi False
  textual = Textual <$> quoted
  quoted = char '"' *> P.manyTill (LP.charLiteral <|> sp) (char '"')
           where sp = lit "\\s" $> ' '
  character = Character <$> (char '?' *> (spEsc <|> LP.charLiteral))
              where spEsc = P.try (char '\\' *> char 's' $> ' ')
  backticks = tick <$> (char '`' *> wordyId <* char '`')
              where tick (WordyId n sh) = Backticks n sh
                    tick t = t

  wordyId :: P Lexeme
  wordyId = P.label wordyMsg . P.try $ do
    dot <- P.optional (lit ".")
    segs <- P.sepBy1 wordyIdSeg (P.try (char '.' <* P.lookAhead (CP.satisfy wordyIdChar)))
    shorthash <- P.optional shorthash
    pure $ WordyId (fromMaybe "" dot <> intercalate "." segs) shorthash
    where
      wordyMsg = "identifier (ex: abba1, snake_case, .foo.bar#xyz, or 🌻)"

  symbolyId :: P Lexeme
  symbolyId = P.label symbolMsg . P.try $ do
    dot <- P.optional (lit ".")
    segs <- P.optional segs
    shorthash <- P.optional shorthash
    case (dot, segs) of
      (_, Just segs)      -> pure $ SymbolyId (fromMaybe "" dot <> segs) shorthash
      -- a single . or .#somehash is parsed as a symboly id
      (Just dot, Nothing) -> pure $ SymbolyId dot shorthash
      (Nothing, Nothing)  -> fail symbolMsg
    where
    segs = symbolyIdSeg <|> (wordyIdSeg <+> lit "." <+> segs)

  symbolMsg = "operator (ex: +, Float./, List.++#xyz)"

  symbolyIdSeg :: P String
  symbolyIdSeg = do
    id <- P.takeWhile1P (Just symbolMsg) symbolyIdChar
    if Set.member id reservedOperators then fail "reserved operator"
    else pure id

  wordyIdSeg :: P String
  -- wordyIdSeg = litSeg <|> (P.try do -- todo
  wordyIdSeg = do
    ch <- CP.satisfy wordyIdStartChar
    rest <- P.many (CP.satisfy wordyIdChar)
    when (Set.member (ch : rest) keywords) $ fail "identifier segment can't be a keyword"
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
        decimals <- P.optional $ let
          missingFractional = err start (MissingFractional $ base <> ".")
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
      octal = do start <- pos
                 commitAfter2 sign (lit "0o") $ \sign _ ->
                   fmap (num sign) LP.octal <|> err start InvalidOctalLiteral
      hex = do start <- pos
               commitAfter2 sign (lit "0x") $ \sign _ ->
                 fmap (num sign) LP.hexadecimal <|> err start InvalidHexLiteral

      num :: Maybe String -> Integer -> Lexeme
      num sign n = Numeric (fromMaybe "" sign <> show n)
      sign = P.optional (lit "+" <|> lit "-")

  hash = Hash <$> P.try shorthash

  reserved :: P [Token Lexeme]
  reserved =
    token' (\ts _ _ -> ts) $
    braces <|> parens <|> delim <|> delayOrForce <|> keywords <|> layoutKeywords
    where
    keywords = symbolyKw ":" <|> symbolyKw "@" <|> symbolyKw "||" <|> symbolyKw "|" <|> symbolyKw "&&"
           <|> wordyKw "true" <|> wordyKw "false"
           <|> wordyKw "use" <|> wordyKw "forall" <|> wordyKw "∀"
           <|> wordyKw "termLink" <|> wordyKw "typeLink"

    wordyKw s = separated wordySep (kw s)
    symbolyKw s = separated (not . symbolyIdChar) (kw s)

    kw :: String -> P [Token Lexeme]
    kw s = positioned (lit s) <&> \(pos1,s,pos2) -> [Token (Reserved s) pos1 pos2]

    layoutKeywords :: P [Token Lexeme]
    layoutKeywords =
      ifElse <|> withKw <|> openKw "match" <|> openKw "handle" <|> typ <|> arr <|> eq <|>
      openKw "cases" <|> openKw "where" <|> openKw "let"
      where
        ifElse = openKw "if" <|> close' (Just "then") ["if"] "then" <|> close' (Just "else") ["then"] "else"
        typ = openKw1 "unique" <|> openTypeKw1 "type" <|> openTypeKw1 "ability"

        withKw = do
          [Token _ pos1 pos2] <- wordyKw "with"
          env <- S.get
          let l = layout env
          case findClose ["handle","match"] l of
            Nothing -> err pos1 (CloseWithoutMatchingOpen msgOpen "'with'")
                       where msgOpen = "'handle' or 'match'"
            Just (withBlock, n) -> do
              let b = withBlock <> "-with"
              S.put (env { layout = drop n l, opening = Just b })
              let opens = [Token (Open "with") pos1 pos2]
              pure $ replicate n (Token Close pos1 pos2) ++ opens

        -- In `unique type` and `unique ability`, only the `unique` opens a layout block,
        -- and `ability` and `type` are just keywords.
        openTypeKw1 t = do
          b <- S.gets (topBlockName . layout)
          case b of Just "unique" -> wordyKw t
                    _             -> openKw1 t

        -- layout keyword which bumps the layout column by 1, rather than looking ahead
        -- to the next token to determine the layout column
        openKw1 :: String -> P [Token Lexeme]
        openKw1 kw = do
          (pos0, kw, pos1) <- positioned $ lit kw
          S.modify (\env -> env { layout = (kw, column $ inc pos0) : layout env })
          pure [Token (Open kw) pos0 pos1]

        eq = do
          [Token _ start end] <- symbolyKw "="
          env <- S.get
          case topBlockName (layout env) of
            -- '=' does not open a layout block if within a type declaration
            Just t | t == "type" || t == "unique" -> pure [Token (Reserved "=") start end]
            Just _ -> S.put (env { opening = Just "=" }) >> pure [Token (Open "=") start end]
            _ -> err start LayoutError

        arr = do
          [Token _ start end] <- symbolyKw "->"
          env <- S.get
          -- -> introduces a layout block if we're inside a `match with` or `cases`
          case topBlockName (layout env) of
            Just match | match == "match-with" || match == "cases" -> do
              S.put (env { opening = Just "->" })
              pure [Token (Open "->") start end]
            _ -> pure [Token (Reserved "->") start end]

    braces = open "{" <|> close ["{"] "}"
    parens = open "(" <|> close ["("] ")"

    delim = P.try $ do
      ch <- CP.satisfy (\ch -> ch /= ';' && Set.member ch delimiters)
      pos <- pos
      pure [Token (Reserved [ch]) pos (inc pos)]

    delayOrForce = separated ok $ do
      (start, op, end) <- positioned $ CP.satisfy isDelayOrForce
      pure [Token (Reserved [op]) start end]
      where ok c = isDelayOrForce c || isSpace c || isAlphaNum c || Set.member c delimiters

    open :: String -> P [Token Lexeme]
    open b = do
      (start, _, end) <- positioned $ lit b
      env <- S.get
      S.put (env { opening = Just b })
      pure [Token (Open b) start end]

    openKw :: String -> P [Token Lexeme]
    openKw s = separated wordySep $ do
      (pos1, s, pos2) <- positioned $ lit s
      env <- S.get
      S.put (env { opening = Just s })
      pure [Token (Open s) pos1 pos2]

    close = close' Nothing

    close' :: Maybe String -> [String] -> String -> P [Token Lexeme]
    close' reopenBlockname open close = do
      (pos1, close, pos2) <- positioned $ lit close
      env <- S.get
      case findClose open (layout env) of
        Nothing -> err pos1 (CloseWithoutMatchingOpen msgOpen (quote close))
          where msgOpen = intercalate " or " (quote <$> open)
                quote s = "'" <> s <> "'"
        Just (_, n) -> do
          S.put (env { layout = drop n (layout env), opening = reopenBlockname })
          let opens = maybe [] (const $ [Token (Open close) pos1 pos2]) reopenBlockname
          pure $ replicate n (Token Close pos1 pos2) ++ opens

    findClose :: [String] -> Layout -> Maybe (String, Int)
    findClose _ [] = Nothing
    findClose s ((h,_):tl) = if h `elem` s then Just (h, 1) else fmap (1+) <$> findClose s tl

  eof :: P [Token Lexeme]
  eof = P.try $ do
    p <- P.eof >> pos
    n <- maybe 0 (const 1) <$> S.gets opening
    l <- S.gets layout
    pure $ replicate (length l + n) (Token Close p p)

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
  let t = tree $ lexer0' scope rem
      -- after reordering can end up with trailing semicolon at the end of
      -- a block, which we remove with this pass
      fixup ((payload -> Semi _) : t@(payload -> Close) : tl) = t : fixup tl
      fixup [] = []
      fixup (h : t) = h : fixup t
  in fixup . toList $ reorderTree reorder t

isDelayOrForce :: Char -> Bool
isDelayOrForce op = op == '\''|| op == '!'

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

-- Inverse of parseEscapeChar; map a character to its escaped version:
showEscapeChar :: Char -> Maybe Char
showEscapeChar c =
  Map.lookup c (Map.fromList [(x, y) | (y, x) <- escapeChars])

isSep :: Char -> Bool
isSep c = isSpace c || Set.member c delimiters

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

delimiters :: Set Char
delimiters = Set.fromList "()[]{},?;"

isDelimiter :: Char -> Bool
isDelimiter ch = Set.member ch delimiters

reservedOperators :: Set String
reservedOperators = Set.fromList ["=", "->", ":", "&&", "||", "|", "!", "'"]

inc :: Pos -> Pos
inc (Pos line col) = Pos line (col + 1)

debugLex'' :: [Token Lexeme] -> String
debugLex'' = show . fmap payload . tree

debugLex' :: String -> String
debugLex' =  debugLex'' . lexer "debugLex"

debugLex''' :: String -> String -> String
debugLex''' s =  debugLex'' . lexer s

span' :: (a -> Bool) -> [a] -> (([a],[a]) -> r) -> r
span' f a k = k (span f a)

instance EP.ShowErrorComponent (Token Err) where
  showErrorComponent (Token err _ _) = go err where
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

instance Semigroup Pos where (<>) = mappend

instance Monoid Pos where
  mempty = Pos 0 0
  Pos line col `mappend` Pos line2 col2 =
    if line2 == 0 then Pos line (col + col2)
    else Pos (line + line2) col2
