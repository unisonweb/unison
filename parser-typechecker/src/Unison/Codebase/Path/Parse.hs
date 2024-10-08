module Unison.Codebase.Path.Parse
  ( -- * Path parsing functions
    parsePath,
    parsePath',
    parseSplit,
    parseSplit',
    parseHQSplit,
    parseHQSplit',
    parseShortHashOrHQSplit',

    -- * Path parsers
    pathP,
    splitP,
    splitP',
  )
where

import Data.Text qualified as Text
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Internal qualified as P (withParsecT)
import Unison.Codebase.Path
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Prelude hiding (empty, toList)
import Unison.ShortHash (ShortHash)
import Unison.Syntax.Lexer qualified as Lexer
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.NameSegment qualified as NameSegment (renderParseErr)
import Unison.Syntax.ShortHash qualified as ShortHash

------------------------------------------------------------------------------------------------------------------------
-- Path parsing functions

parsePath :: String -> Either Text Path
parsePath =
  runParser pathP

parsePath' :: String -> Either Text Path'
parsePath' = \case
  "" -> Right relativeEmpty'
  "." -> Right absoluteEmpty'
  path -> unsplit' <$> parseSplit' path

parseSplit :: String -> Either Text Split
parseSplit =
  runParser splitP

parseSplit' :: String -> Either Text Split'
parseSplit' =
  runParser splitP'

parseShortHashOrHQSplit' :: String -> Either Text (Either ShortHash HQSplit')
parseShortHashOrHQSplit' =
  runParser shortHashOrHqSplitP'

parseHQSplit :: String -> Either Text HQSplit
parseHQSplit s =
  parseHQSplit' s >>= \case
    (RelativePath' (Relative p), hqseg) -> Right (p, hqseg)
    _ -> Left $ "Sorry, you can't use an absolute name like " <> Text.pack s <> " here."

parseHQSplit' :: String -> Either Text HQSplit'
parseHQSplit' =
  runParser hqSplitP'

runParser :: Parsec (Lexer.Token Text) [Char] a -> String -> Either Text a
runParser p =
  mapLeft (Text.pack . P.errorBundlePretty) . P.runParser (p <* P.eof) ""

------------------------------------------------------------------------------------------------------------------------
-- Path parsers

pathP :: Parsec (Lexer.Token Text) [Char] Path
pathP =
  (unsplit <$> splitP) <|> pure empty

splitP :: Parsec (Lexer.Token Text) [Char] Split
splitP =
  splitFromName <$> P.withParsecT (fmap NameSegment.renderParseErr) Name.relativeNameP

splitP' :: Parsec (Lexer.Token Text) [Char] Split'
splitP' =
  splitFromName' <$> P.withParsecT (fmap NameSegment.renderParseErr) Name.nameP

shortHashOrHqSplitP' :: Parsec (Lexer.Token Text) [Char] (Either ShortHash HQSplit')
shortHashOrHqSplitP' =
  Left <$> ShortHash.shortHashP <|> Right <$> hqSplitP'

hqSplitP' :: Parsec (Lexer.Token Text) [Char] HQSplit'
hqSplitP' = do
  (segs, seg) <- splitP'
  P.optional (P.withParsecT (fmap ("invalid hash: " <>)) ShortHash.shortHashP) <&> \case
    Nothing -> (segs, HQ'.fromName seg)
    Just hash -> (segs, HQ'.HashQualified seg hash)
