{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.V1.Reference where

import Data.Char (isDigit)
import qualified Data.Text as Text
import Data.Word (Word64)
import Data.Text (Text)
import U.Util.Base32Hex (Base32Hex)
import qualified U.Util.Hash as H
import qualified U.Util.Base32Hex as Base32Hex
import qualified Data.Set as Set
import Data.Set (Set)

data Reference
  = Builtin Text.Text
  | -- `Derived` can be part of a strongly connected component.
    -- The `Pos` refers to a particular element of the component
    -- and the `Size` is the number of elements in the component.
    -- Using an ugly name so no one tempted to use this
    DerivedId Id
  deriving (Eq, Ord, Show)

pattern Derived :: H.Hash -> Pos -> Size -> Reference
pattern Derived h i n = DerivedId (Id h i n)

type Pos = Word64
type Size = Word64

-- todo: don't read or return size; must also update showSuffix and fromText
data Id = Id H.Hash Pos Size deriving (Eq, Ord, Show)

readSuffix :: Text -> Either String (Pos, Size)
readSuffix t = case Text.breakOn "c" t of
  (pos, Text.drop 1 -> size)
    | Text.all isDigit pos && Text.all isDigit size ->
      Right (read (Text.unpack pos), read (Text.unpack size))
  _ -> Left "suffix decoding error"

toText :: Reference -> Text
toText (Builtin b) = "##" <> b
toText (DerivedId (Id h i n)) =
  "#" <> (Base32Hex.toText . H.toBase32Hex) h
    <> "."
    <> (Text.pack . show) i
    <> "c"
    <> (Text.pack . show) n

newtype Component = Component {members :: Set Reference}

-- Gives the component (dependency cycle) that the reference is a part of
componentFor :: Reference -> Component
componentFor b@(Builtin _) = Component (Set.singleton b)
componentFor (DerivedId (Id h _ n)) =
  Component (Set.fromList [DerivedId (Id h i n) | i <- take (fromIntegral n) [0 ..]])

derivedBase32Hex :: Base32Hex -> Pos -> Size -> Reference
derivedBase32Hex h i n = DerivedId (Id (H.fromBase32Hex h) i n)
