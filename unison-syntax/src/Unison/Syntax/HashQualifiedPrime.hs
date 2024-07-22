{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Syntax-related combinators for HashQualified' (to/from string types).
module Unison.Syntax.HashQualifiedPrime
  ( -- * String conversions
    parseText,
    unsafeParseText,
    toText,

    -- * Parsers
    hashQualifiedP,
  )
where

import Data.Text qualified as Text
import Text.Megaparsec (ParsecT)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Internal qualified as P (withParsecT)
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Prelude hiding (fromString)
import Unison.Syntax.Lexer.Token (Token)
import Unison.Syntax.Name qualified as Name (nameP, toText)
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Syntax.ShortHash qualified as ShortHash (shortHashP)

------------------------------------------------------------------------------------------------------------------------
-- String conversions

parseText :: Text -> Maybe (HQ'.HashQualified Name)
parseText text =
  eitherToMaybe (P.runParser parser "" (Text.unpack text))
  where
    parser =
      hashQualifiedP (P.withParsecT (fmap NameSegment.renderParseErr) Name.nameP) <* P.eof

unsafeParseText :: (HasCallStack) => Text -> HQ'.HashQualified Name
unsafeParseText txt = fromMaybe msg (parseText txt)
  where
    msg = error ("HashQualified.unsafeFromText " <> show txt)

toText :: HQ'.HashQualified Name -> Text
toText =
  HQ'.toTextWith Name.toText

------------------------------------------------------------------------------------------------------------------------
-- Hash-qualified parsers

-- | A hash-qualified parser.
hashQualifiedP ::
  (Monad m) =>
  ParsecT (Token Text) [Char] m name ->
  ParsecT (Token Text) [Char] m (HQ'.HashQualified name)
hashQualifiedP nameP =
  P.try do
    name <- nameP
    optional ShortHash.shortHashP <&> \case
      Nothing -> HQ'.NameOnly name
      Just hash -> HQ'.HashQualified name hash
