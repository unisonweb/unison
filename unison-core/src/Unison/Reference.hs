{-# LANGUAGE DataKinds #-}

module Unison.Reference
  ( Reference,
    Reference'
      ( ReferenceBuiltin,
        ReferenceDerived,
        Builtin,
        DerivedId,
        Derived
      ),
    _DerivedId,
    Id,
    Id' (..),
    Pos,
    CycleSize,
    Size,
    TermReference,
    TermReferenceId,
    TypeReference,
    TypeReferenceId,
    derivedBase32Hex,
    componentFor,
    componentFromLength,
    unsafeFromText,
    isPrefixOf,
    fromText,
    readSuffix,
    showShort,
    showSuffix,
    toHash,
    toId,
    fromId,
    toText,
    idToText,
    unsafeId,
    toShortHash,
    idToHash,
    idToShortHash,
    isBuiltin,
  )
where

import Control.Lens (Prism')
import Data.Char (isDigit)
import Data.Generics.Sum (_Ctor)
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.Reference
  ( Id,
    Id' (..),
    Reference,
    Reference' (..),
    TermReference,
    TermReferenceId,
    TypeReference,
    TypeReferenceId,
    idToHash,
    idToShortHash,
    isBuiltin,
    toId,
    toShortHash,
    unsafeId,
    pattern Derived,
  )
import Unison.Hash qualified as H
import Unison.Prelude
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as SH

pattern Builtin :: t -> Reference' t h
pattern Builtin x = ReferenceBuiltin x

pattern DerivedId :: Id' h -> Reference' t h
pattern DerivedId x = ReferenceDerived x

{-# COMPLETE Builtin, DerivedId #-}

{-# COMPLETE Builtin, Derived #-}

{-# COMPLETE Builtin, ReferenceDerived #-}

{-# COMPLETE ReferenceBuiltin, DerivedId #-}

_DerivedId :: Prism' Reference Id
_DerivedId = _Ctor @"ReferenceDerived"

showSuffix :: Pos -> Text
showSuffix = Text.pack . show

readSuffix :: Text -> Either String Pos
readSuffix = \case
  pos
    | Text.all isDigit pos,
      Just pos' <- readMaybe (Text.unpack pos) ->
        Right pos'
  t -> Left $ "Invalid reference suffix: " <> show t

isPrefixOf :: ShortHash -> Reference -> Bool
isPrefixOf sh r = SH.isPrefixOf sh (toShortHash r)

toText :: Reference -> Text
toText = SH.toText . toShortHash

idToText :: Id -> Text
idToText = toText . ReferenceDerived

showShort :: Int -> Reference -> Text
showShort numHashChars = SH.toText . SH.shortenTo numHashChars . toShortHash

type Pos = Word64

type Size = CycleSize

type CycleSize = Word64

-- enumerate the `a`s and associates them with corresponding `Reference.Id`s
componentFor :: H.Hash -> [a] -> [(Id, a)]
componentFor h as = [(Id h i, a) | (i, a) <- zip [0 ..] as]

componentFromLength :: H.Hash -> CycleSize -> Set Id
componentFromLength h size = Set.fromList [Id h i | i <- [0 .. size - 1]]

derivedBase32Hex :: Text -> Pos -> Maybe Reference
derivedBase32Hex b32Hex i = mayH <&> \h -> Derived h i
  where
    mayH = H.fromBase32HexText b32Hex

unsafeFromText :: Text -> Reference
unsafeFromText = either error id . fromText

fromId :: Id -> Reference
fromId = ReferenceDerived

toHash :: Reference -> Maybe H.Hash
toHash r = idToHash <$> toId r

-- |
-- todo: take a (Reference -> CycleSize) so that `readSuffix` doesn't have to parse the size from the text.
-- examples:
--
-- builtins don’t have cycles
-- >>> fromText "##Text.take"
-- Right ##Text.take
--
-- derived, no cycle
-- >>> fromText "#dqp2oi4iderlrgp2h11sgkff6drk92omo4c84dncfhg9o0jn21cli4lhga72vlchmrb2jk0b3bdc2gie1l06sqdli8ego4q0akm3au8"
-- Right #dqp2o
--
-- derived, part of cycle
-- >>> fromText "#dqp2oi4iderlrgp2h11sgkff6drk92omo4c84dncfhg9o0jn21cli4lhga72vlchmrb2jk0b3bdc2gie1l06sqdli8ego4q0akm3au8.12345"
-- Right #dqp2o.12345
--
-- Errors with 'Left' on invalid hashes
-- >>> fromText "#invalid_hash.12345"
-- Left "Invalid hash: \"invalid_hash\""
fromText :: Text -> Either String Reference
fromText t = case Text.split (== '#') t of
  [_, "", b] -> Right (ReferenceBuiltin b)
  [_, h] -> case Text.split (== '.') h of
    [hash] ->
      case derivedBase32Hex hash 0 of
        Nothing -> Left $ "Invalid hash: " <> show hash
        Just r -> Right r
    [hash, suffix] -> do
      pos <- readSuffix suffix
      maybe (Left $ "Invalid hash: " <> show hash) Right (derivedBase32Hex hash pos)
    _ -> bail
  _ -> bail
  where
    bail = Left $ "couldn't parse a Reference from " <> Text.unpack t
