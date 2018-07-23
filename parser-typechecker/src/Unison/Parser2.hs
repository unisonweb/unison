{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Parser2 where

import           Control.Monad (join)
import           Data.Bifunctor (bimap)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Typeable (Proxy(..))
import           Text.Megaparsec (ParseError, runParserT)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Unison.Lexer as L
import qualified Unison.UnisonFile as UnisonFile

type PEnv = UnisonFile.CtorLookup

type P = P.ParsecT Error Input ((->) PEnv)

type Parser a = P (L.Token a)

data Error = Error deriving (Show, Eq, Ord)

data Ann = Ann { start :: L.Pos, end :: L.Pos }

instance Semigroup Ann where
  Ann s1 _ <> Ann _ e2 = Ann s1 e2

newtype Input = Input { inputStream :: [L.Token L.Lexeme] }
  deriving (Eq, Ord, Show)

type Err s = ParseError (P.Token s) Error

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

ann :: L.Token a -> Ann
ann (L.Token _ s e) = Ann s e

proxy :: Proxy Input
proxy = Proxy

root :: P a -> P a
root p = p <* P.eof

run' :: P a -> Input -> String -> PEnv -> Either (Err Input) a
run' p s name = runParserT p name s

run :: P a -> Input -> PEnv -> Either (Err Input) a
run p s = run' p s ""

-- Virtual pattern match on a lexeme.
queryToken :: (L.Lexeme -> Maybe a) -> Parser a
queryToken f = P.token go Nothing
  where go t@((f . L.payload) -> Just s) = Right $ fmap (const s) t
        go x = Left (pure (P.Tokens (x:|[])), Set.empty)

-- Consume a block opening and return the string that opens the block.
openBlock :: Parser String
openBlock = queryToken getOpen
  where
    getOpen (L.Open s) = Just s
    getOpen _ = Nothing

-- Match a particular lexeme exactly, and consume it.
matchToken :: L.Lexeme -> Parser L.Lexeme
matchToken x = P.satisfy ((==) x . L.payload)

-- Consume a virtual semicolon
semi :: Parser ()
semi = fmap (const ()) <$> matchToken L.Semi

-- Consume the end of a block
closeBlock :: Parser ()
closeBlock = fmap (const ()) <$> matchToken L.Close

-- Parse an alphanumeric identifier
wordyId :: Parser String
wordyId = queryToken getWordy
  where getWordy (L.WordyId s) = Just s
        getWordy _ = Nothing

-- Parse a symboly ID like >>= or &&
symbolyId :: Parser String
symbolyId = queryToken getSymboly
  where getSymboly (L.SymbolyId s) = Just s
        getSymboly _ = Nothing

-- Parse a reserved word
reserved :: String -> Parser String
reserved w = queryToken getReserved
  where getReserved (L.Reserved w') | w == w' = Just w
        getReserved _ = Nothing

-- Parse a pair of parentheses around an expression
parenthesized :: Parser a -> Parser a
parenthesized = P.between (reserved "(") (reserved ")")

sepBy :: P a -> P b -> P [b]
sepBy sep pb = P.sepBy pb sep

sepBy1 :: P a -> P b -> P [b]
sepBy1 sep pb = P.sepBy pb sep

