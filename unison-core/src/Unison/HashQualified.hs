{-# LANGUAGE OverloadedStrings #-}

module Unison.HashQualified where

import qualified Data.Text as Text
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Prelude hiding (fromString)
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.ShortHash (ShortHash)
import qualified Unison.ShortHash as SH
import Unison.Var (Var)
import qualified Unison.Var as Var
import Prelude hiding (take)

data HashQualified' n
  = NameOnly n
  | HashOnly ShortHash
  | HashQualified n ShortHash
  deriving (Eq, Functor, Show)

type HashQualified = HashQualified' Name

stripNamespace :: Text -> HashQualified -> HashQualified
stripNamespace namespace hq = case hq of
  NameOnly name -> NameOnly $ strip name
  HashQualified name sh -> HashQualified (strip name) sh
  ho -> ho
  where
    strip name =
      fromMaybe name $ Name.stripNamePrefix (Name.unsafeFromText namespace) name

toName :: HashQualified' n -> Maybe n
toName = \case
  NameOnly name -> Just name
  HashQualified name _ -> Just name
  HashOnly _ -> Nothing

-- Sort the list of names by length of segments: smaller number of
-- segments is listed first. NameOnly < Hash qualified < Hash only
--
-- Examples:
--   [foo.bar.baz, bar.baz] -> [bar.baz, foo.bar.baz]
--   [#a29dj2k91, foo.bar.baz] -> [foo.bar.baz, #a29dj2k91]
--   [foo.bar#abc, foo.bar] -> [foo.bar, foo.bar#abc]
--   [.foo.bar, foo.bar] -> [foo.bar, .foo.bar]
sortByLength :: [HashQualified' Name] -> [HashQualified' Name]
sortByLength hs = sortOn f hs
  where
    f (NameOnly n) = (countDots n, 0, Left n)
    f (HashQualified n _h) = (countDots n, 1, Left n)
    f (HashOnly h) = (maxBound, 0, Right h)
    countDots n = Text.count "." (Text.dropEnd 1 (Name.toText n))

hasName, hasHash :: HashQualified -> Bool
hasName = isJust . toName
hasHash = isJust . toHash

toHash :: HashQualified' n -> Maybe ShortHash
toHash = \case
  NameOnly _ -> Nothing
  HashQualified _ sh -> Just sh
  HashOnly sh -> Just sh

-- partial: assumes either a name or hash is provided (or both)
fromNameHash :: Maybe Name -> Maybe ShortHash -> HashQualified
fromNameHash n h = case n of
  Just name -> case h of
    Just hash -> HashQualified name hash
    Nothing -> NameOnly name
  Nothing -> case h of
    Just hash -> HashOnly hash
    Nothing -> error "bad HQ construction"

take :: Int -> HashQualified' n -> HashQualified' n
take i = \case
  n@(NameOnly _) -> n
  HashOnly s -> HashOnly (SH.take i s)
  HashQualified n s -> if i == 0 then NameOnly n else HashQualified n (SH.take i s)

toString :: Show n => HashQualified' n -> String
toString = Text.unpack . toText

fromString :: String -> Maybe HashQualified
fromString = fromText . Text.pack

unsafeFromString :: String -> HashQualified
unsafeFromString s = fromMaybe msg . fromString $ s
  where
    msg = error $ "HashQualified.unsafeFromString " <> show s

-- Parses possibly-hash-qualified into structured type.
-- Doesn't validate against base58 or the codebase.
fromText :: Text -> Maybe HashQualified
fromText t = case Text.breakOn "#" t of -- breakOn leaves the '#' on the RHS
  (name, "") -> Just $ NameOnly (Name.unsafeFromText name) -- safe bc breakOn #
  ("", hash) -> HashOnly <$> SH.fromText hash
  (name, hash) -> HashQualified (Name.unsafeFromText name) <$> SH.fromText hash

-- Won't crash as long as SH.unsafeFromText doesn't crash on any input that
-- starts with '#', which is true as of the time of this writing, but not great.
unsafeFromText :: Text -> HashQualified
unsafeFromText txt = fromMaybe msg . fromText $ txt
  where
    msg = error $ "HashQualified.unsafeFromText " <> show txt

toText :: Show n => HashQualified' n -> Text
toText = \case
  NameOnly name -> Text.pack (show name)
  HashQualified name hash -> Text.pack (show name) <> SH.toText hash
  HashOnly hash -> SH.toText hash

-- Returns the full referent in the hash.  Use HQ.take to just get a prefix
fromNamedReferent :: n -> Referent -> HashQualified' n
fromNamedReferent n r = HashQualified n (Referent.toShortHash r)

-- Returns the full reference in the hash.  Use HQ.take to just get a prefix
fromNamedReference :: n -> Reference -> HashQualified' n
fromNamedReference n r = HashQualified n (Reference.toShortHash r)

fromReferent :: Referent -> HashQualified
fromReferent = HashOnly . Referent.toShortHash

fromReference :: Reference -> HashQualified
fromReference = HashOnly . Reference.toShortHash

fromPattern :: Reference -> Int -> HashQualified
fromPattern r cid = HashOnly $ Referent.patternShortHash r cid

fromName :: n -> HashQualified' n
fromName = NameOnly

unsafeFromVar :: Var v => v -> HashQualified
unsafeFromVar = unsafeFromText . Var.name

fromVar :: Var v => v -> Maybe HashQualified
fromVar = fromText . Var.name

toVar :: Var v => HashQualified -> v
toVar = Var.named . toText

-- todo: find this logic elsewhere and replace with call to this
matchesNamedReferent :: Name -> Referent -> HashQualified -> Bool
matchesNamedReferent n r = \case
  NameOnly n' -> n' == n
  HashOnly sh -> sh `SH.isPrefixOf` Referent.toShortHash r
  HashQualified n' sh -> n' == n && sh `SH.isPrefixOf` Referent.toShortHash r

matchesNamedReference :: Name -> Reference -> HashQualified -> Bool
matchesNamedReference n r = \case
  NameOnly n' -> n' == n
  HashOnly sh -> sh `SH.isPrefixOf` Reference.toShortHash r
  HashQualified n' sh -> n' == n && sh `SH.isPrefixOf` Reference.toShortHash r

-- Use `requalify hq . Referent.Ref` if you want to pass in a `Reference`.
requalify :: HashQualified -> Referent -> HashQualified
requalify hq r = case hq of
  NameOnly n -> fromNamedReferent n r
  HashQualified n _ -> fromNamedReferent n r
  HashOnly _ -> fromReferent r

-- this implementation shows HashOnly before the others, because None < Some.
-- Flip it around carefully if HashOnly should come last.
instance Ord n => Ord (HashQualified' n) where
  compare a b = case compare (toName a) (toName b) of
    EQ -> compare (toHash a) (toHash b)
    o -> o

--instance Show n => Show (HashQualified' n) where
--  show = Text.unpack . toText
