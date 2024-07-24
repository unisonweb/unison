{-# LANGUAGE UndecidableInstances #-}

module Unison.Syntax.Parser
  ( Annotated (..),
    Err,
    Error (..),
    Input,
    P,
    ParsingEnv (..),
    UniqueName,
    anyToken,
    blank,
    bytesToken,
    chainl1,
    chainr1,
    character,
    closeBlock,
    failCommitted,
    failureIf,
    hqInfixId,
    hqPrefixId,
    importSymbolyId,
    importWordyId,
    label,
    matchToken,
    mkAnn,
    numeric,
    openBlock,
    openBlockWith,
    peekAny,
    positionalVar,
    prefixDefinitionName,
    prefixTermName,
    queryToken,
    reserved,
    root,
    rootFile,
    run',
    run,
    semi,
    Unison.Syntax.Parser.seq,
    Unison.Syntax.Parser.seq',
    sepBy,
    sepBy1,
    string,
    symbolyDefinitionName,
    tok,
    tokenToPair,
    tupleOrParenthesized,
    uniqueBase32Namegen,
    uniqueName,
    wordyDefinitionName,
    wordyPatternName,
  )
where

import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Reader.Class (asks)
import Crypto.Random qualified as Random
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (serialize)
import Data.Bytes.VarInt (VarInt (..))
import Data.Char qualified as Char
import Data.Kind (Type)
import Data.List.NonEmpty qualified as Nel
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Megaparsec (runParserT)
import Text.Megaparsec qualified as P
import U.Util.Base32Hex qualified as Base32Hex
import Unison.ABT qualified as ABT
import Unison.ConstructorReference (ConstructorReference)
import Unison.Hash qualified as Hash
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Hashable qualified as Hashable
import Unison.Name as Name
import Unison.Names (Names)
import Unison.Names.ResolutionResult qualified as Names
import Unison.Parser.Ann (Ann (..))
import Unison.Pattern (Pattern)
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Syntax.Lexer qualified as L
import Unison.Syntax.Name qualified as Name (toVar, unsafeParseText)
import Unison.Term (MatchCase (..))
import Unison.UnisonFile.Error qualified as UF
import Unison.Util.Bytes (Bytes)
import Unison.Var (Var)
import Unison.Var qualified as Var

debug :: Bool
debug = False

type P v m = P.ParsecT (Error v) Input (ReaderT (ParsingEnv m) m)

type Err v = P.ParseError Input (Error v)

data ParsingEnv (m :: Type -> Type) = ParsingEnv
  { uniqueNames :: UniqueName,
    -- | Return a GUID to reuse for a unique type of the given name, if any.
    --
    -- This callback is called for every `unique type` declaration that does not explicitly specify a GUID.
    --
    -- The name (e.g. `Foo` in `unique type Foo`) is passed in, and if the function returns a Just, that GUID is used;
    -- otherwise, a random one is generated from `uniqueNames`.
    uniqueTypeGuid :: Name -> m (Maybe Text),
    names :: Names
  }

newtype UniqueName = UniqueName (L.Pos -> Int -> Maybe Text)

instance Semigroup UniqueName where
  UniqueName f <> UniqueName g =
    UniqueName \pos len -> f pos len <|> g pos len

instance Monoid UniqueName where
  mempty = UniqueName (\_ _ -> Nothing)

uniqueBase32Namegen :: forall gen. (Random.DRG gen) => gen -> UniqueName
uniqueBase32Namegen rng =
  UniqueName \pos lenInBase32Hex -> go pos lenInBase32Hex rng
  where
    -- if the identifier starts with a number, try again, since
    -- we want the name to work as a valid wordyId
    go :: L.Pos -> Int -> gen -> Maybe Text
    go pos lenInBase32Hex rng0 =
      let (bytes, rng) = Random.randomBytesGenerate 32 rng0
          posBytes = runPutS do
            serialize $ VarInt (L.line pos)
            serialize $ VarInt (L.column pos)
          h = Hashable.accumulate' $ bytes <> posBytes
          b58 = Hash.toBase32HexText h
       in if Char.isDigit (Text.head b58)
            then go pos lenInBase32Hex rng
            else Just . Text.take lenInBase32Hex $ b58

uniqueName :: (Monad m, Var v) => Int -> P v m Text
uniqueName lenInBase32Hex = do
  UniqueName mkName <- asks uniqueNames
  pos <- L.start <$> P.lookAhead anyToken
  let none = Base32Hex.toText . Base32Hex.fromByteString . encodeUtf8 . Text.pack $ show pos
  pure . fromMaybe none $ mkName pos lenInBase32Hex

data Error v
  = SignatureNeedsAccompanyingBody (L.Token v)
  | DisallowedAbsoluteName (L.Token Name)
  | EmptyBlock (L.Token String)
  | UnknownAbilityConstructor (L.Token (HQ.HashQualified Name)) (Set ConstructorReference)
  | UnknownDataConstructor (L.Token (HQ.HashQualified Name)) (Set ConstructorReference)
  | UnknownTerm (L.Token (HQ.HashQualified Name)) (Set Referent)
  | UnknownType (L.Token (HQ.HashQualified Name)) (Set Reference)
  | UnknownId (L.Token (HQ.HashQualified Name)) (Set Referent) (Set Reference)
  | ExpectedBlockOpen String (L.Token L.Lexeme)
  | -- Indicates a cases or match/with which doesn't have any patterns
    EmptyMatch (L.Token ())
  | EmptyWatch Ann
  | UseInvalidPrefixSuffix (Either (L.Token Name) (L.Token Name)) (Maybe [L.Token Name])
  | UseEmpty (L.Token String) -- an empty `use` statement
  | DidntExpectExpression (L.Token L.Lexeme) (Maybe (L.Token L.Lexeme))
  | TypeDeclarationErrors [UF.Error v Ann]
  | -- MissingTypeModifier (type|ability) name
    MissingTypeModifier (L.Token String) (L.Token v)
  | ResolutionFailures [Names.ResolutionFailure v Ann]
  | DuplicateTypeNames [(v, [Ann])]
  | DuplicateTermNames [(v, [Ann])]
  | PatternArityMismatch Int Int Ann -- PatternArityMismatch expectedArity actualArity location
  | FloatPattern Ann
  deriving (Show, Eq, Ord)

tokenToPair :: L.Token a -> (Ann, a)
tokenToPair t = (ann t, L.payload t)

newtype Input = Input {inputStream :: [L.Token L.Lexeme]}
  deriving stock (Eq, Ord, Show)
  deriving newtype (P.Stream, P.VisualStream)

class Annotated a where
  ann :: a -> Ann

instance Annotated Ann where
  ann = id

instance Annotated (L.Token a) where
  ann (L.Token _ s e) = Ann s e

instance (Annotated a) => Annotated (ABT.Term f v a) where
  ann = ann . ABT.annotation

instance (Annotated a) => Annotated (Pattern a) where
  ann = ann . Pattern.loc

instance (Annotated a) => Annotated [a] where
  ann [] = mempty
  ann (h : t) = foldl' (\acc a -> acc <> ann a) (ann h) t

instance (Annotated a, Annotated b) => Annotated (MatchCase a b) where
  ann (MatchCase p _ b) = ann p <> ann b

label :: (Ord v, Show a) => String -> P v m a -> P v m a
label = P.label

-- label = P.dbg

traceRemainingTokens :: (Ord v) => String -> P v m ()
traceRemainingTokens label = do
  remainingTokens <- lookAhead $ many anyToken
  let _ =
        trace ("REMAINDER " ++ label ++ ":\n" ++ L.debugLex'' remainingTokens) ()
  pure ()

mkAnn :: (Annotated a, Annotated b) => a -> b -> Ann
mkAnn x y = ann x <> ann y

tok :: (Ann -> a -> b) -> L.Token a -> b
tok f (L.Token a start end) = f (Ann start end) a

peekAny :: (Ord v) => P v m (L.Token L.Lexeme)
peekAny = P.lookAhead P.anySingle

lookAhead :: (Ord v) => P v m a -> P v m a
lookAhead = P.lookAhead

anyToken :: (Ord v) => P v m (L.Token L.Lexeme)
anyToken = P.anySingle

failCommitted :: (Ord v) => Error v -> P v m x
failCommitted e = do
  void anyToken <|> void P.eof
  P.customFailure e

root :: (Ord v) => P v m a -> P v m a
root p = (openBlock *> p) <* closeBlock <* P.eof

rootFile :: (Ord v) => P v m a -> P v m a
rootFile p = p <* P.eof

run' :: (Monad m, Ord v) => P v m a -> String -> String -> ParsingEnv m -> m (Either (Err v) a)
run' p s name env =
  let lex =
        if debug
          then L.lexer name (trace (L.debugLex''' "lexer receives" s) s)
          else L.lexer name s
      pTraced = traceRemainingTokens "parser receives" *> p
   in runReaderT (runParserT pTraced name (Input lex)) env <&> \case
        Left err -> Left (Nel.head (P.bundleErrors err))
        Right x -> Right x

run :: (Monad m, Ord v) => P v m a -> String -> ParsingEnv m -> m (Either (Err v) a)
run p s = run' p s ""

-- Virtual pattern match on a lexeme.
queryToken :: (Ord v) => (L.Lexeme -> Maybe a) -> P v m (L.Token a)
queryToken f = P.token (traverse f) Set.empty

-- Consume a block opening and return the string that opens the block.
openBlock :: (Ord v) => P v m (L.Token String)
openBlock = queryToken getOpen
  where
    getOpen (L.Open s) = Just s
    getOpen _ = Nothing

openBlockWith :: (Ord v) => String -> P v m (L.Token ())
openBlockWith s = void <$> P.satisfy ((L.Open s ==) . L.payload)

-- Match a particular lexeme exactly, and consume it.
matchToken :: (Ord v) => L.Lexeme -> P v m (L.Token L.Lexeme)
matchToken x = P.satisfy ((==) x . L.payload)

-- Consume a virtual semicolon
semi :: (Ord v) => P v m (L.Token ())
semi = label "newline or semicolon" $ queryToken go
  where
    go (L.Semi _) = Just ()
    go _ = Nothing

-- Consume the end of a block
closeBlock :: (Ord v) => P v m (L.Token ())
closeBlock = void <$> matchToken L.Close

wordyPatternName :: (Var v) => P v m (L.Token v)
wordyPatternName = queryToken \case
  L.WordyId (HQ'.NameOnly n) -> Just $ Name.toVar n
  _ -> Nothing

-- Parse an prefix identifier e.g. Foo or (+), discarding any hash
prefixDefinitionName :: (Var v) => P v m (L.Token v)
prefixDefinitionName =
  wordyDefinitionName <|> parenthesize symbolyDefinitionName

-- Parse a prefix identifier e.g. Foo or (+), rejecting any hash
-- This is useful for term declarations, where type signatures and term names should not have hashes.
prefixTermName :: (Var v) => P v m (L.Token v)
prefixTermName = wordyTermName <|> parenthesize symbolyTermName
  where
    wordyTermName = queryToken \case
      L.WordyId (HQ'.NameOnly n) -> Just $ Name.toVar n
      L.Blank s -> Just $ Var.nameds ("_" <> s)
      _ -> Nothing
    symbolyTermName = queryToken \case
      L.SymbolyId (HQ'.NameOnly n) -> Just $ Name.toVar n
      _ -> Nothing

-- Parse a wordy identifier e.g. Foo, discarding any hash
wordyDefinitionName :: (Var v) => P v m (L.Token v)
wordyDefinitionName = queryToken $ \case
  L.WordyId n -> Just $ Name.toVar (HQ'.toName n)
  L.Blank s -> Just $ Var.nameds ("_" <> s)
  _ -> Nothing

-- Parse a wordyId as a Name, rejecting any hash
importWordyId :: (Ord v) => P v m (L.Token Name)
importWordyId = queryToken \case
  L.WordyId (HQ'.NameOnly n) -> Just n
  L.Blank s | not (null s) -> Just $ Name.unsafeParseText (Text.pack ("_" <> s))
  _ -> Nothing

-- The `+` in: use Foo.bar + as a Name
importSymbolyId :: (Ord v) => P v m (L.Token Name)
importSymbolyId = queryToken \case
  L.SymbolyId (HQ'.NameOnly n) -> Just n
  _ -> Nothing

-- Parse a symboly ID like >>= or &&, discarding any hash
symbolyDefinitionName :: (Var v) => P v m (L.Token v)
symbolyDefinitionName = queryToken $ \case
  L.SymbolyId n -> Just $ Name.toVar (HQ'.toName n)
  _ -> Nothing

-- | Expect parentheses around a token, includes the parentheses within the start/end
-- annotations of the resulting token.
parenthesize :: (Ord v) => P v m (L.Token a) -> P v m (L.Token a)
parenthesize p = do
  (start, a) <- P.try do
    start <- L.start <$> openBlockWith "("
    a <- p
    pure (start, a)
  end <- L.end <$> closeBlock
  pure (L.Token {payload = L.payload a, start, end})

hqPrefixId, hqInfixId :: (Ord v) => P v m (L.Token (HQ.HashQualified Name))
hqPrefixId = hqWordyId_ <|> parenthesize hqSymbolyId_
hqInfixId = hqSymbolyId_

-- Parse a hash-qualified alphanumeric identifier
hqWordyId_ :: (Ord v) => P v m (L.Token (HQ.HashQualified Name))
hqWordyId_ = queryToken \case
  L.WordyId n -> Just $ HQ'.toHQ n
  L.Hash h -> Just $ HQ.HashOnly h
  L.Blank s | not (null s) -> Just $ HQ.NameOnly (Name.unsafeParseText (Text.pack ("_" <> s)))
  _ -> Nothing

-- Parse a hash-qualified symboly ID like >>=#foo or &&
hqSymbolyId_ :: (Ord v) => P v m (L.Token (HQ.HashQualified Name))
hqSymbolyId_ = queryToken \case
  L.SymbolyId n -> Just (HQ'.toHQ n)
  _ -> Nothing

-- Parse a reserved word
reserved :: (Ord v) => String -> P v m (L.Token String)
reserved w = label w $ queryToken getReserved
  where
    getReserved (L.Reserved w') | w == w' = Just w
    getReserved _ = Nothing

-- Parse a placeholder or typed hole
blank :: (Ord v) => P v m (L.Token String)
blank = label "blank" $ queryToken getBlank
  where
    getBlank (L.Blank s) = Just ('_' : s)
    getBlank _ = Nothing

numeric :: (Ord v) => P v m (L.Token String)
numeric = queryToken getNumeric
  where
    getNumeric (L.Numeric s) = Just s
    getNumeric _ = Nothing

bytesToken :: (Ord v) => P v m (L.Token Bytes)
bytesToken = queryToken getBytes
  where
    getBytes (L.Bytes bs) = Just bs
    getBytes _ = Nothing

sepBy :: (Ord v) => P v m a -> P v m b -> P v m [b]
sepBy sep pb = P.sepBy pb sep

sepBy1 :: (Ord v) => P v m a -> P v m b -> P v m [b]
sepBy1 sep pb = P.sepBy1 pb sep

sepEndBy :: (Ord v) => P v m a -> P v m b -> P v m [b]
sepEndBy sep pb = P.sepEndBy pb sep

character :: (Ord v) => P v m (L.Token Char)
character = queryToken getChar
  where
    getChar (L.Character c) = Just c
    getChar _ = Nothing

string :: (Ord v) => P v m (L.Token Text)
string = queryToken getString
  where
    getString (L.Textual s) = Just (Text.pack s)
    getString _ = Nothing

-- | Parses a tuple of 'a's, or a single parenthesized 'a'
--
-- returns the result of combining elements with 'pair', alongside the annotation containing
-- the full parenthesized expression.
tupleOrParenthesized :: (Ord v) => P v m a -> (Ann -> a) -> (a -> a -> a) -> P v m (Ann {- spanAnn -}, a)
tupleOrParenthesized p unit pair = do
  seq' "(" go p
  where
    go ann [t] = (ann, t)
    go ann (t : ts) = (ann, foldr pair (unit mempty) (t Nel.:| ts))
    go ann [] = (ann, unit ann)

seq :: (Ord v) => (Ann -> [a] -> a) -> P v m a -> P v m a
seq = seq' "["

seq' :: (Ord v) => String -> (Ann -> [a] -> b) -> P v m a -> P v m b
seq' openStr f p = do
  open <- openBlockWith openStr <* redundant
  es <- sepEndBy (P.try $ optional semi *> reserved "," <* redundant) p
  close <- redundant *> closeBlock
  pure (f (ann open <> ann close) es)
  where
    redundant = P.skipMany (P.eitherP (reserved ",") semi)

chainr1 :: (Ord v) => P v m a -> P v m (a -> a -> a) -> P v m a
chainr1 p op = go1
  where
    go1 = p >>= go2
    go2 hd = do { op <- op; op hd <$> go1 } <|> pure hd

-- Parse `p` 1+ times, combining with `op`
chainl1 :: (Ord v) => P v m a -> P v m (a -> a -> a) -> P v m a
chainl1 p op = foldl (flip ($)) <$> p <*> P.many (flip <$> op <*> p)

-- If `p` would succeed, this fails uncommitted.
-- Otherwise, `failIfOk` used to produce the output
failureIf :: (Ord v) => P v m (P v m b) -> P v m a -> P v m b
failureIf failIfOk p = do
  dontwant <- P.try . P.lookAhead $ failIfOk
  p <- P.try $ P.lookAhead (optional p)
  when (isJust p) $ fail "failureIf"
  dontwant

-- Gives this var an id based on its position - a useful trick to
-- obtain a variable whose id won't match any other id in the file
-- `positionalVar a Var.missingResult`
positionalVar :: (Annotated a, Var v) => a -> v -> v
positionalVar a v =
  let s = start (ann a)
      line = fromIntegral $ L.line s
      col = fromIntegral $ L.column s
   in -- this works as long as no lines more than 50k characters
      Var.freshenId (line * 50000 + col) v
