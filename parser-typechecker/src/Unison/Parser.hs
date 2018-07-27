{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Parser where

import           Control.Applicative
import           Control.Monad (join)
import           Data.Bifunctor (bimap)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable (Proxy(..))
import           Text.Megaparsec (runParserT)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import           Text.Megaparsec.Error (ShowErrorComponent(..))
import qualified Unison.ABT as ABT
import           Unison.Hash
import qualified Unison.Lexer as L
import           Unison.Pattern (PatternP)
import qualified Unison.PatternP as Pattern
import           Unison.Term (MatchCase(..))
import qualified Unison.UnisonFile as UnisonFile
import           Unison.Var (Var)
import qualified Unison.Var as Var

type PEnv = UnisonFile.CtorLookup
type P v = P.ParsecT (Error v) Input ((->) PEnv)
type Token s = P.Token s
type Err v = P.ParseError (Token Input) (Error v)

data Error v
  = SignatureNeedsAccompanyingBody (L.Token v)
  -- we would include the last binding term if we didn't have to have an Ord instance for it
  | BlockMustEndWithExpression { blockAnn :: Ann, lastBindingAnn :: Ann }
  | EmptyBlock (L.Token String)
  | UnknownEffectConstructor (L.Token String)
  | UnknownDataConstructor (L.Token String)
  deriving (Show, Eq, Ord)

instance Var v => ShowErrorComponent (Error v) where
  showErrorComponent e = case e of
    SignatureNeedsAccompanyingBody t ->
      showLineCol t ++ ": You provided a type signature, but I " ++
      "didn't find an accompanying definition after it."
    BlockMustEndWithExpression bann lbann ->
      showLineCol lbann ++
      ": The last line of the block starting at " ++
      showLineCol bann ++
      " has to be an expression, not a binding or an import."
    EmptyBlock t ->
      showLineCol t ++
      ": I expected a block after `" ++ L.payload t ++
      "`, but there wasn't one. Maybe check your indentation."
    UnknownEffectConstructor t ->
      showLineCol t ++
      ": I don't know about any effect constructor named `" ++ L.payload t ++
      "`. Maybe make sure it's correctly spelled, and that you've imported it."
    UnknownDataConstructor t ->
      showLineCol t ++
      ": I don't know about any data constructor named `" ++ L.payload t ++
      "`. Maybe make sure it's correctly spelled, and that you've imported it."

data Ann =
  Intrinsic |
  Ann { start :: L.Pos, end :: L.Pos } deriving (Eq, Ord, Show)

instance Semigroup Ann where
  Ann s1 _ <> Ann _ e2 = Ann s1 e2
  Intrinsic <> Intrinsic = error "FUN SURPRISE!"
  x <> y = error $ "Compiler bug! Tried to combine terms annotated with ("
                   ++ show x ++ ") and (" ++ show y ++ ")"

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
    fromMaybe sp . fmap (setPos sp . L.start) . listToMaybe . P.chunkToTokens pxy

  advance1 _ _ cp = setPos cp . L.end

  advanceN _ _ cp = setPos cp . L.end . last . inputStream

  take1_ (P.chunkToTokens proxy -> []) = Nothing
  take1_ (P.chunkToTokens proxy -> t:ts) = Just (t, P.tokensToChunk proxy ts)
  take1_ _ = error "Unpossible"

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

instance Annotated a => Annotated (PatternP a) where
  ann = ann . Pattern.loc

instance (Annotated a, Annotated b) => Annotated (MatchCase a b) where
  ann (MatchCase p _ b) = ann p <> ann b

mkAnn :: (Annotated a, Annotated b) => a -> b -> Ann
mkAnn x y = ann x <> ann y

showLineCol :: Annotated a => a -> String
showLineCol a =
  let L.Pos line col = start $ ann a
  in "Line " ++ show line ++ ", column " ++ show col

tok :: (Ann -> a -> b) -> L.Token a -> b
tok f (L.Token a start end) = f (Ann start end) a

peekAny :: Var v => P v (L.Token L.Lexeme)
peekAny = P.lookAhead P.anyChar

proxy :: Proxy Input
proxy = Proxy

root :: Var v => P v a -> P v a
root p = openBlock *> p <* closeBlock <* P.eof

run' :: P v a -> String -> String -> PEnv -> Either (Err v) a
run' p s name = runParserT p name (Input $ L.lexer name s) -- todo: L.reorder

run :: P v a -> String -> PEnv -> Either (Err v) a
run p s = run' p s ""

penv0 :: PEnv
penv0 = Map.empty

-- Virtual pattern match on a lexeme.
queryToken :: Var v => (L.Lexeme -> Maybe a) -> P v (L.Token a)
queryToken f = P.token go Nothing
  where go t@((f . L.payload) -> Just s) = Right $ fmap (const s) t
        go x = Left (pure (P.Tokens (x:|[])), Set.empty)

-- Consume a block opening and return the string that opens the block.
openBlock :: Var v => P v (L.Token String)
openBlock = queryToken getOpen
  where
    getOpen (L.Open s) = Just s
    getOpen _ = Nothing

openBlockWith :: Var v => String -> P v (L.Token ())
openBlockWith s = fmap (const ()) <$> P.satisfy ((L.Open s ==) . L.payload)

-- Match a particular lexeme exactly, and consume it.
matchToken :: Var v => L.Lexeme -> P v (L.Token L.Lexeme)
matchToken x = P.satisfy ((==) x . L.payload)

-- Consume a virtual semicolon
semi :: Var v => P v (L.Token ())
semi = fmap (const ()) <$> matchToken L.Semi

-- Consume the end of a block
closeBlock :: Var v => P v (L.Token ())
closeBlock = fmap (const ()) <$> matchToken L.Close

-- Parse an alphanumeric identifier
wordyId :: Var v => P v (L.Token String)
wordyId = queryToken getWordy
  where getWordy (L.WordyId s) = Just s
        getWordy _ = Nothing

-- Parse a symboly ID like >>= or &&
symbolyId :: Var v => P v (L.Token String)
symbolyId = queryToken getSymboly
  where getSymboly (L.SymbolyId s) = Just s
        getSymboly _ = Nothing

backticks :: Var v => P v (L.Token String)
backticks = queryToken getBackticks
  where getBackticks (L.Backticks s) = Just s
        getBackticks _ = Nothing

-- Parse a reserved word
reserved :: Var v => String -> P v (L.Token String)
reserved w = P.label w $ queryToken getReserved
  where getReserved (L.Reserved w') | w == w' = Just w
        getReserved _ = Nothing

numeric :: Var v => P v (L.Token String)
numeric = queryToken getNumeric
  where getNumeric (L.Numeric s) = Just s
        getNumeric _ = Nothing

sepBy :: Var v => P v a -> P v b -> P v [b]
sepBy sep pb = P.sepBy pb sep

sepBy1 :: Var v => P v a -> P v b -> P v [b]
sepBy1 sep pb = P.sepBy1 pb sep

prefixVar :: Var v => P v (L.Token v)
prefixVar = fmap (Var.named . Text.pack) <$> P.label "symbol" prefixOp
  where
    prefixOp = wordyId <|> P.label "prefix-operator" (P.try (reserved "(" *> symbolyId) <* reserved ")")

infixVar :: Var v => P v (L.Token v)
infixVar =
  fmap (Var.named . Text.pack) <$> (symbolyId <|> backticks)

hashLiteral :: Var v => P v (L.Token Hash)
hashLiteral = queryToken getHash
  where getHash (L.Hash s) = Just s
        getHash _ = Nothing

string :: Var v => P v (L.Token Text)
string = queryToken getString
  where getString (L.Textual s) = Just (Text.pack s)
        getString _ = Nothing

tupleOrParenthesized :: Var v => P v a -> (Ann -> a) -> (a -> a -> a) -> P v a
tupleOrParenthesized p unit pair = do
    open <- reserved "("
    es <- sepBy (reserved ",") p
    close <- optional semi *> reserved ")"
    pure $ go es open close
  where
    go [t] _ _ = t
    go as s e = foldr pair (unit (ann s <> ann e)) as

chainl1 :: Var v => P v a -> P v (a -> a -> a) -> P v a
chainl1 p op = foldl (flip ($)) <$> p <*> P.many (flip <$> op <*> p)
