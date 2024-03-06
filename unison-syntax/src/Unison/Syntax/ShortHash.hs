-- | Utilities related to the parsing and printing of short hashes using the default syntax.
module Unison.Syntax.ShortHash
  ( -- * Short hash parsers
    shortHashP,
  )
where

import Data.Char qualified as Char
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Megaparsec (ParsecT)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Unison.Prelude
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as ShortHash
import Unison.Syntax.Lexer.Token (Token (..), tokenP)
import Unison.Syntax.ReservedWords (delimiters)

-- | A short hash parser.
--
-- Throws the parsed hash as an error if it's invalid.
shortHashP :: ParsecT (Token Text) [Char] m ShortHash
shortHashP =
  P.label hashMsg do
    P.lookAhead (P.char '#')
    token <-
      tokenP do
        Text.pack <$> P.takeWhile1P (Just hashMsg) (\ch -> not (isSep ch) && ch /= '`')
    case ShortHash.fromText (payload token) of
      Nothing -> P.customFailure token
      Just sh -> pure sh
  where
    hashMsg = "hash (ex: #af3sj3)"

    isSep :: Char -> Bool
    isSep c =
      Char.isSpace c || Set.member c delimiters
