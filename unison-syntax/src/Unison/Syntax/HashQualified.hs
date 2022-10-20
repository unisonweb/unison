{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Syntax-related combinators for HashQualified (to/from string types).
module Unison.Syntax.HashQualified
  ( fromString,
    fromText,
    unsafeFromString,
    unsafeFromText,
    unsafeFromVar,
    toVar,
  )
where

import qualified Data.Text as Text
import Unison.HashQualified (HashQualified (..))
import qualified Unison.HashQualified as HashQualified
import Unison.Name (Name, Parse)
import qualified Unison.Name as Name
import Unison.Prelude hiding (fromString)
import qualified Unison.ShortHash as SH
import qualified Unison.Syntax.Name as Name (fromText, toText)
import Unison.Var (Var)
import qualified Unison.Var as Var
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

unsafeFromVar :: Var v => v -> HashQualified Name
unsafeFromVar = unsafeFromText . Var.name

toVar :: Var v => HashQualified Name -> v
toVar = Var.named . HashQualified.toTextWith Name.toText
