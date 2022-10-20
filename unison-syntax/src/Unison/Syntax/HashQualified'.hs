{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Syntax-related combinators for HashQualified' (to/from string types).
module Unison.Syntax.HashQualified'
  ( fromString,
    fromText,
    unsafeFromText,
  )
where

import qualified Data.Text as Text
import Unison.HashQualified' (HashQualified (..))
import Unison.Name (Name, Parse)
import qualified Unison.Name as Name
import Unison.Prelude hiding (fromString)
import qualified Unison.Prelude
import qualified Unison.ShortHash as SH
import qualified Unison.Syntax.Name as Name (unsafeFromText)

instance IsString (HashQualified Name) where
  fromString = unsafeFromText . Text.pack

instance Parse Text (HashQualified Name) where
  parse = fromText

fromString :: String -> Maybe (HashQualified Name)
fromString = fromText . Text.pack

-- Parses possibly-hash-qualified into structured type.
fromText :: Text -> Maybe (HashQualified Name)
fromText t = case Text.breakOn "#" t of
  (name, "") -> Just $ NameOnly (Name.unsafeFromText name) -- safe bc breakOn #
  (name, hash) -> HashQualified (Name.unsafeFromText name) <$> SH.fromText hash

unsafeFromText :: HasCallStack => Text -> HashQualified Name
unsafeFromText txt = fromMaybe msg (fromText txt)
  where
    msg = error ("HashQualified.unsafeFromText " <> show txt)
