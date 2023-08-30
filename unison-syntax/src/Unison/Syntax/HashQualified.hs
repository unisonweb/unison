{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Syntax-related combinators for HashQualified (to/from string types).
module Unison.Syntax.HashQualified
  ( fromString,
    fromText,
    unsafeFromString,
    unsafeFromText,
    unsafeFromVar,
    toString,
    toText,
    toVar,
  )
where

import Data.Text qualified as Text
import U.Codebase.ShortHash qualified as SH
import Unison.HashQualified (HashQualified (..))
import Unison.HashQualified qualified as HashQualified
import Unison.Name (Name, Parse)
import Unison.Name qualified as Name
import Unison.Prelude hiding (fromString)
import Unison.Syntax.Name qualified as Name (fromText, toText)
import Unison.Var (Var)
import Unison.Var qualified as Var
import Prelude hiding (take)

instance Parse Text (HashQualified Name) where
  parse = fromText

fromString :: String -> Maybe (HashQualified Name)
fromString = fromText . Text.pack

-- Parses possibly-hash-qualified into structured type.
-- Doesn't validate against base58 or the codebase.
fromText :: Text -> Maybe (HashQualified Name)
fromText t = case Text.breakOn "#" t of -- breakOn leaves the '#' on the RHS
  ("", "") -> Nothing
  (name, "") -> NameOnly <$> Name.fromText name
  ("", hash) -> HashOnly <$> SH.fromText hash
  (name, hash) -> HashQualified <$> Name.fromText name <*> SH.fromText hash

unsafeFromString :: String -> HashQualified Name
unsafeFromString s = fromMaybe msg . fromString $ s
  where
    msg = error $ "HashQualified.unsafeFromString " <> show s

-- Won't crash as long as SH.unsafeFromText doesn't crash on any input that
-- starts with '#', which is true as of the time of this writing, but not great.
unsafeFromText :: Text -> HashQualified Name
unsafeFromText txt = fromMaybe msg . fromText $ txt
  where
    msg = error $ "HashQualified.unsafeFromText " <> show txt

unsafeFromVar :: (Var v) => v -> HashQualified Name
unsafeFromVar = unsafeFromText . Var.name

toString :: HashQualified Name -> String
toString =
  Text.unpack . toText

toText :: HashQualified Name -> Text
toText =
  HashQualified.toTextWith Name.toText

toVar :: (Var v) => HashQualified Name -> v
toVar =
  Var.named . toText
