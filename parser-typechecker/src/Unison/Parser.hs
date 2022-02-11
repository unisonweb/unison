{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE DeriveAnyClass #-}

module Unison.Parser where

import Unison.Prelude
    ( trace,
      join,
      foldl',
      Text,
      optional,
      Alternative((<|>), many),
      Set,
      void,
      when,
      fromMaybe,
      isJust,
      listToMaybe,
      encodeUtf8,
      lastMay )

import qualified Crypto.Random        as Random
import           Data.Bytes.Put                 (runPutS)
import           Data.Bytes.Serial              ( serialize )
import           Data.Bytes.VarInt              ( VarInt(..) )
import           Data.Bifunctor       (bimap)
import qualified Data.Char            as Char
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.Set             as Set
import qualified Data.Text            as Text
import           Data.Typeable        (Proxy (..))
import           Text.Megaparsec      (runParserT)
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P
import qualified Unison.ABT           as ABT
import Unison.ConstructorReference (ConstructorReference)
import qualified Unison.Hash          as Hash
import qualified Unison.HashQualified as HQ
import qualified Unison.Lexer         as L
import           Unison.Pattern       (Pattern)
import qualified Unison.Pattern      as Pattern
import           Unison.Term          (MatchCase (..))
import           Unison.Var           (Var)
import qualified Unison.Var           as Var
import qualified Unison.UnisonFile.Error as UF
import qualified U.Util.Base32Hex as Base32Hex
import Unison.Util.Bytes              (Bytes)
import Unison.Name as Name
import Unison.NamesWithHistory (NamesWithHistory)
import qualified Unison.Names.ResolutionResult as Names
import Control.Monad.Reader.Class (asks)
import qualified Unison.Hashable as Hashable
import Unison.Referent (Referent)
import Unison.Reference (Reference)
import Unison.Parser.Ann (Ann(..))
import Text.Megaparsec.Error (ShowErrorComponent)

debug :: Bool
debug = False

type P v = P.ParsecT (Error v) Input ((->) ParsingEnv)
type Token s = P.Token s
type Err v = P.ParseError (Token Input) (Error v)

data ParsingEnv =
  ParsingEnv { uniqueNames :: UniqueName
             , names :: NamesWithHistory
             }

newtype UniqueName = UniqueName (L.Pos -> Int -> Maybe Text)

instance Semigroup UniqueName where (<>) = mappend
instance Monoid UniqueName where
  mempty = UniqueName (\_ _ -> Nothing)
  mappend (UniqueName f) (UniqueName g) =
    UniqueName $ \pos len -> f pos len <|> g pos len


uniqueBase32Namegen :: forall gen. Random.DRG gen => gen -> UniqueName
uniqueBase32Namegen rng =
  UniqueName $ \pos lenInBase32Hex -> go pos lenInBase32Hex rng
  where
  -- if the identifier starts with a number, try again, since
  -- we want the name to work as a valid wordyId
  go :: L.Pos -> Int -> gen -> Maybe Text
  go pos lenInBase32Hex rng0 = let
    (bytes,rng) = Random.randomBytesGenerate 32 rng0
    posBytes = runPutS $ do
      serialize $ VarInt (L.line pos)
      serialize $ VarInt (L.column pos)
    h = Hashable.accumulate' $ bytes <> posBytes
    b58 = Hash.base32Hex h
    in if Char.isDigit (Text.head b58) then go pos lenInBase32Hex rng
       else Just . Text.take lenInBase32Hex $ b58


uniqueName :: Var v => Int -> P v Text
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
  | EmptyMatch
  | EmptyWatch
  | UseInvalidPrefixSuffix (Either (L.Token Name) (L.Token Name)) (Maybe [L.Token Name])
  | UseEmpty (L.Token String) -- an empty `use` statement
  | DidntExpectExpression (L.Token L.Lexeme) (Maybe (L.Token L.Lexeme))
  | TypeDeclarationErrors [UF.Error v Ann]
  -- MissingTypeModifier (type|ability) name
  | MissingTypeModifier (L.Token String) (L.Token v)
  | ResolutionFailures [Names.ResolutionFailure v Ann]
  | DuplicateTypeNames [(v, [Ann])]
  | DuplicateTermNames [(v, [Ann])]
  | PatternArityMismatch Int Int Ann -- PatternArityMismatch expectedArity actualArity location
  | FloatPattern Ann
  deriving (Show, Eq, Ord)

instance (Ord v, Show v) => ShowErrorComponent (Error v) where
  showErrorComponent e = show e

tokenToPair :: L.Token a -> (Ann, a)
tokenToPair t = (ann t, L.payload t)

newtype Input = Input { inputStream :: [L.Token L.Lexeme] }
  deriving (Eq, Ord, Show)

instance P.Stream Input where
  type Token Input = L.Token L.Lexeme
  type Tokens Input = Input

  tokenToChunk pxy = P.tokensToChunk pxy . pure

  tokensToChunk _ = Input

  chunkToTokens _ = inputStream

  chunkLength pxy = length . P.chunkToTokens pxy

  chunkEmpty pxy = null . P.chunkToTokens pxy

  positionAt1 _ sp t = setPos sp (L.start t)

  positionAtN pxy sp =
    maybe sp (setPos sp . L.start) . listToMaybe . P.chunkToTokens pxy

  advance1 _ _ cp = setPos cp . L.end

  advanceN _ _ cp = maybe cp (setPos cp . L.end) . lastMay . inputStream

  take1_ (P.chunkToTokens proxy -> [])   = Nothing
  take1_ (P.chunkToTokens proxy -> t:ts) = Just (t, P.tokensToChunk proxy ts)
  take1_ _                               = error "Unpossible"

  takeN_ n (P.chunkToTokens proxy -> []) | n > 0 = Nothing
  takeN_ n ts =
    Just
      . join bimap (P.tokensToChunk proxy)
      . splitAt n $ P.chunkToTokens proxy ts

  takeWhile_ p = join bimap (P.tokensToChunk proxy) . span p . inputStream

setPos :: P.SourcePos -> L.Pos -> P.SourcePos
setPos sp lp =
  P.SourcePos (P.sourceName sp) (P.mkPos $ L.line lp) (P.mkPos $ L.column lp)

class Annotated a where
  ann :: a -> Ann

instance Annotated Ann where
  ann = id

instance Annotated (L.Token a) where
  ann (L.Token _ s e) = Ann s e

instance Annotated a => Annotated (ABT.Term f v a) where
  ann = ann . ABT.annotation

instance Annotated a => Annotated (Pattern a) where
  ann = ann . Pattern.loc

instance Annotated a => Annotated [a] where
  ann [] = mempty
  ann (h:t) = foldl' (\acc a -> acc <> ann a) (ann h) t

instance (Annotated a, Annotated b) => Annotated (MatchCase a b) where
  ann (MatchCase p _ b) = ann p <> ann b

label :: (Ord v, Show a) => String -> P v a -> P v a
label = P.label
-- label = P.dbg

traceRemainingTokens :: Ord v => String -> P v ()
traceRemainingTokens label = do
  remainingTokens <- lookAhead $ many anyToken
  let
    _ =
      trace ("REMAINDER " ++ label ++ ":\n" ++ L.debugLex'' remainingTokens) ()
  pure ()

mkAnn :: (Annotated a, Annotated b) => a -> b -> Ann
mkAnn x y = ann x <> ann y

tok :: (Ann -> a -> b) -> L.Token a -> b
tok f (L.Token a start end) = f (Ann start end) a

peekAny :: Ord v => P v (L.Token L.Lexeme)
peekAny = P.lookAhead P.anyChar

lookAhead :: Ord v => P v a -> P v a
lookAhead = P.lookAhead

anyToken :: Ord v => P v (L.Token L.Lexeme)
anyToken = P.anyChar

failCommitted :: Ord v => Error v -> P v x
failCommitted e = do
  void anyToken <|> void P.eof
  P.customFailure e

proxy :: Proxy Input
proxy = Proxy

root :: Ord v => P v a -> P v a
root p = (openBlock *> p) <* closeBlock <* P.eof

-- |
rootFile :: Ord v => P v a -> P v a
rootFile p = p <* P.eof

run' :: Ord v => P v a -> String -> String -> ParsingEnv -> Either (Err v) a
run' p s name env =
  let lex = if debug
            then L.lexer name (trace (L.debugLex''' "lexer receives" s) s)
            else L.lexer name s
      pTraced = traceRemainingTokens "parser receives" *> p
  in runParserT pTraced name (Input lex) env

run :: Ord v => P v a -> String -> ParsingEnv -> Either (Err v) a
run p s = run' p s ""

-- Virtual pattern match on a lexeme.
queryToken :: Ord v => (L.Lexeme -> Maybe a) -> P v (L.Token a)
queryToken f = P.token go Nothing
  where go t@(f . L.payload -> Just s) = Right $ fmap (const s) t
        go x = Left (pure (P.Tokens (x:|[])), Set.empty)

-- Consume a block opening and return the string that opens the block.
openBlock :: Ord v => P v (L.Token String)
openBlock = queryToken getOpen
  where
    getOpen (L.Open s) = Just s
    getOpen _          = Nothing

openBlockWith :: Ord v => String -> P v (L.Token ())
openBlockWith s = void <$> P.satisfy ((L.Open s ==) . L.payload)

-- Match a particular lexeme exactly, and consume it.
matchToken :: Ord v => L.Lexeme -> P v (L.Token L.Lexeme)
matchToken x = P.satisfy ((==) x . L.payload)

-- The package name that refers to the root, literally just `.`
importDotId :: Ord v => P v (L.Token Name)
importDotId = queryToken go where
  go (L.SymbolyId "." Nothing) = Just (Name.unsafeFromString ".")
  go _ = Nothing

-- Consume a virtual semicolon
semi :: Ord v => P v (L.Token ())
semi = queryToken go where
  go (L.Semi _) = Just ()
  go _ = Nothing

-- Consume the end of a block
closeBlock :: Ord v => P v (L.Token ())
closeBlock = void <$> matchToken L.Close

wordyPatternName :: Var v => P v (L.Token v)
wordyPatternName = queryToken $ \case
  L.WordyId s Nothing -> Just $ Var.nameds s
  _                   -> Nothing

-- Parse an prefix identifier e.g. Foo or (+), discarding any hash
prefixDefinitionName :: Var v => P v (L.Token v)
prefixDefinitionName =
  wordyDefinitionName <|> parenthesize symbolyDefinitionName

-- Parse a wordy identifier e.g. Foo, discarding any hash
wordyDefinitionName :: Var v => P v (L.Token v)
wordyDefinitionName = queryToken $ \case
  L.WordyId s _            -> Just $ Var.nameds s
  L.Blank s                -> Just $ Var.nameds ("_" <> s)
  _                        -> Nothing

-- Parse a wordyId as a String, rejecting any hash
wordyIdString :: Ord v => P v (L.Token String)
wordyIdString = queryToken $ \case
  L.WordyId s Nothing -> Just s
  _                   -> Nothing

-- Parse a wordyId as a Name, rejecting any hash
importWordyId :: Ord v => P v (L.Token Name)
importWordyId = (fmap . fmap) Name.unsafeFromString wordyIdString

-- The `+` in: use Foo.bar + as a Name
importSymbolyId :: Ord v => P v (L.Token Name)
importSymbolyId = (fmap . fmap) Name.unsafeFromString symbolyIdString

-- Parse a symbolyId as a String, rejecting any hash
symbolyIdString :: Ord v => P v (L.Token String)
symbolyIdString = queryToken $ \case
  L.SymbolyId s Nothing -> Just s
  _                     -> Nothing

-- Parse an infix id e.g. + or Docs.++, discarding any hash
infixDefinitionName :: Var v => P v (L.Token v)
infixDefinitionName = symbolyDefinitionName

-- Parse a symboly ID like >>= or &&, discarding any hash
symbolyDefinitionName :: Var v => P v (L.Token v)
symbolyDefinitionName = queryToken $ \case
  L.SymbolyId s _ -> Just $ Var.nameds s
  _               -> Nothing

parenthesize :: Ord v => P v a -> P v a
parenthesize p = P.try (openBlockWith "(" *> p) <* closeBlock

hqPrefixId, hqInfixId :: Ord v => P v (L.Token (HQ.HashQualified Name))
hqPrefixId = hqWordyId_ <|> parenthesize hqSymbolyId_
hqInfixId = hqSymbolyId_

-- Parse a hash-qualified alphanumeric identifier
hqWordyId_ :: Ord v => P v (L.Token (HQ.HashQualified Name))
hqWordyId_ = queryToken $ \case
  L.WordyId "" (Just h) -> Just $ HQ.HashOnly h
  L.WordyId s  (Just h) -> Just $ HQ.HashQualified (Name.unsafeFromString s) h
  L.WordyId s  Nothing  -> Just $ HQ.NameOnly (Name.unsafeFromString s)
  L.Hash h              -> Just $ HQ.HashOnly h
  L.Blank s | not (null s) -> Just $ HQ.NameOnly (Name.unsafeFromString ("_" <> s))
  _ -> Nothing

-- Parse a hash-qualified symboly ID like >>=#foo or &&
hqSymbolyId_ :: Ord v => P v (L.Token (HQ.HashQualified Name))
hqSymbolyId_ = queryToken $ \case
  L.SymbolyId "" (Just h) -> Just $ HQ.HashOnly h
  L.SymbolyId s  (Just h) -> Just $ HQ.HashQualified (Name.unsafeFromString s) h
  L.SymbolyId s  Nothing  -> Just $ HQ.NameOnly (Name.unsafeFromString s)
  _ -> Nothing

-- Parse a reserved word
reserved :: Ord v => String -> P v (L.Token String)
reserved w = label w $ queryToken getReserved
  where getReserved (L.Reserved w') | w == w' = Just w
        getReserved _               = Nothing

-- Parse a placeholder or typed hole
blank :: Ord v => P v (L.Token String)
blank = label "blank" $ queryToken getBlank
  where getBlank (L.Blank s) = Just ('_' : s)
        getBlank _           = Nothing

numeric :: Ord v => P v (L.Token String)
numeric = queryToken getNumeric
  where getNumeric (L.Numeric s) = Just s
        getNumeric _             = Nothing

bytesToken :: Ord v => P v (L.Token Bytes)
bytesToken = queryToken getBytes
  where getBytes (L.Bytes bs) = Just bs
        getBytes _ = Nothing

sepBy :: Ord v => P v a -> P v b -> P v [b]
sepBy sep pb = P.sepBy pb sep

sepBy1 :: Ord v => P v a -> P v b -> P v [b]
sepBy1 sep pb = P.sepBy1 pb sep

sepEndBy :: Ord v => P v a -> P v b -> P v [b]
sepEndBy sep pb = P.sepEndBy pb sep

character :: Ord v => P v (L.Token Char)
character = queryToken getChar
  where getChar (L.Character c) = Just c
        getChar _ = Nothing

string :: Ord v => P v (L.Token Text)
string = queryToken getString
  where getString (L.Textual s) = Just (Text.pack s)
        getString _             = Nothing

tupleOrParenthesized :: Ord v => P v a -> (Ann -> a) -> (a -> a -> a) -> P v a
tupleOrParenthesized p unit pair = seq' "(" go p
 where
  go _ [t] = t
  go a xs  = foldr pair (unit a) xs

seq :: Ord v => (Ann -> [a] -> a) -> P v a -> P v a
seq = seq' "["

seq' :: Ord v => String -> (Ann -> [a] -> a) -> P v a -> P v a
seq' openStr f p = do
  open  <- openBlockWith openStr <* redundant
  es    <- sepEndBy (P.try $ optional semi *> reserved "," <* redundant) p
  close <- redundant *> closeBlock
  pure $ go open es close
  where go open elems close = f (ann open <> ann close) elems
        redundant = P.skipMany (P.eitherP (reserved ",") semi)

chainr1 :: Ord v => P v a -> P v (a -> a -> a) -> P v a
chainr1 p op = go1 where
  go1 = p >>= go2
  go2 hd = do { op <- op; op hd <$> go1 } <|> pure hd

-- Parse `p` 1+ times, combining with `op`
chainl1 :: Ord v => P v a -> P v (a -> a -> a) -> P v a
chainl1 p op = foldl (flip ($)) <$> p <*> P.many (flip <$> op <*> p)

-- If `p` would succeed, this fails uncommitted.
-- Otherwise, `failIfOk` used to produce the output
failureIf :: Ord v => P v (P v b) -> P v a -> P v b
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
  -- this works as long as no lines more than 50k characters
  in Var.freshenId (line * 50000 + col) v
