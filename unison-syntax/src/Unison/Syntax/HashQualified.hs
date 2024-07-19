{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Syntax-related combinators for HashQualified (to/from string types).
module Unison.Syntax.HashQualified
  ( -- * String conversions
    parseText,
    parseTextWith,
    unsafeParseText,
    toText,
    unsafeFromVar,
    toVar,

    -- * Parsers
    hashQualifiedP,
  )
where

import Data.Text qualified as Text
import Text.Megaparsec (ParsecT)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Internal qualified as P (withParsecT)
import Unison.HashQualified (HashQualified (..))
import Unison.HashQualified qualified as HashQualified
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Prelude hiding (fromString)
import Unison.Syntax.HashQualifiedPrime qualified as HQ'
import Unison.Syntax.Lexer.Token (Token)
import Unison.Syntax.Name qualified as Name (nameP, toText)
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Syntax.ShortHash qualified as ShortHash
import Unison.Var (Var)
import Unison.Var qualified as Var
import Prelude hiding (take)

parseText :: Text -> Maybe (HashQualified Name)
parseText text =
  eitherToMaybe (P.runParser parser "" (Text.unpack text))
  where
    parser =
      hashQualifiedP (P.withParsecT (fmap NameSegment.renderParseErr) Name.nameP) <* P.eof

parseTextWith :: P.Parsec (Token Text) [Char] name -> Text -> Maybe (HashQualified name)
parseTextWith parser text =
  eitherToMaybe (P.runParser (hashQualifiedP parser <* P.eof) "" (Text.unpack text))

unsafeParseText :: Text -> HashQualified Name
unsafeParseText txt = fromMaybe msg . parseText $ txt
  where
    msg = error $ "HashQualified.unsafeParseText " <> show txt

toText :: HashQualified Name -> Text
toText =
  HashQualified.toTextWith Name.toText

unsafeFromVar :: (Var v) => v -> HashQualified Name
unsafeFromVar =
  unsafeParseText . Var.name

toVar :: (Var v) => HashQualified Name -> v
toVar =
  Var.named . toText

------------------------------------------------------------------------------------------------------------------------
-- Hash-qualified parsers

-- | A hash-qualified parser.
hashQualifiedP ::
  (Monad m) =>
  ParsecT (Token Text) [Char] m name ->
  ParsecT (Token Text) [Char] m (HashQualified name)
hashQualifiedP nameP =
  P.try do
    optional ShortHash.shortHashP >>= \case
      Nothing -> HQ'.toHQ <$> HQ'.hashQualifiedP nameP
      Just hash -> pure (HashOnly hash)
