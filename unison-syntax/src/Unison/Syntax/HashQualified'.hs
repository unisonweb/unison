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

import Data.Text qualified as Text
import U.Codebase.ShortHash qualified as SH
import Unison.HashQualified' qualified as HQ'
import Unison.Name (Name, Parse)
import Unison.Name qualified as Name
import Unison.Prelude hiding (fromString)
import Unison.Prelude qualified
import Unison.Syntax.Name qualified as Name (toText, unsafeFromText)

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

unsafeFromText :: (HasCallStack) => Text -> HQ'.HashQualified Name
unsafeFromText txt = fromMaybe msg (fromText txt)
  where
    msg = error ("HashQualified.unsafeFromText " <> show txt)

toString :: HQ'.HashQualified Name -> String
toString =
  Text.unpack . toText

toText :: HQ'.HashQualified Name -> Text
toText =
  HQ'.toTextWith Name.toText
