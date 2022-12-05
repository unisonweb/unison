{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Syntax-related combinators for HashQualified' (to/from string types).
module Unison.Syntax.HashQualified'
  ( fromString,
    fromText,
    unsafeFromText,
    toString,
    toText,
  )
where

import qualified Data.Text as Text
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name, Parse)
import qualified Unison.Name as Name
import Unison.Prelude hiding (fromString)
import qualified Unison.Prelude
import qualified Unison.ShortHash as SH
import qualified Unison.Syntax.Name as Name (toText, unsafeFromText)

instance IsString (HQ'.HashQualified Name) where
  fromString = unsafeFromText . Text.pack

instance Parse Text (HQ'.HashQualified Name) where
  parse = fromText

fromString :: String -> Maybe (HQ'.HashQualified Name)
fromString = fromText . Text.pack

-- Parses possibly-hash-qualified into structured type.
fromText :: Text -> Maybe (HQ'.HashQualified Name)
fromText t = case Text.breakOn "#" t of
  (name, "") -> Just $ HQ'.NameOnly (Name.unsafeFromText name) -- safe bc breakOn #
  (name, hash) -> HQ'.HashQualified (Name.unsafeFromText name) <$> SH.fromText hash

unsafeFromText :: HasCallStack => Text -> HQ'.HashQualified Name
unsafeFromText txt = fromMaybe msg (fromText txt)
  where
    msg = error ("HashQualified.unsafeFromText " <> show txt)

toString :: HQ'.HashQualified Name -> String
toString =
  Text.unpack . toText

toText :: HQ'.HashQualified Name -> Text
toText =
  HQ'.toTextWith Name.toText
