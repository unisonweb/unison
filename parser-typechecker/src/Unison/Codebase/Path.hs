{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Unison.Codebase.Path
  -- ( Name(..)
  -- , unsafeFromText
  -- , toString
  -- , fromString
  -- , toVar
  -- , unsafeFromVar
  -- , isPrefixOf
  -- , stripPrefix
  -- )
where

import           Data.List                (intercalate)
-- import           Data.String                    ( IsString
--                                                 , fromString
--                                                 )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Unison.Hashable               as H
import           Unison.Name                    ( Name )
import qualified Unison.Name                   as Name

-- Represents the parts of a name between the `.`s
newtype NameSegment = NameSegment { toText :: Text } deriving (Eq, Ord, Show)

-- `Foo.Bar.baz` becomes ["Foo", "Bar", "baz"]
newtype Path = Path { toList :: [NameSegment] } deriving (Eq, Ord)

asIdentifier :: Path -> Text
asIdentifier = Text.intercalate "." . fmap toText . toList

asDirectory :: Path -> Text
asDirectory p = case toList p of
  NameSegment "_root_" : tail -> "/" <> asDirectory (Path tail)
  other -> Text.intercalate "/" . fmap toText $ other

fromName :: Name -> Path
fromName = Path . fmap NameSegment . Text.splitOn "." . Name.toText

-- Returns the nearest common ancestor, along with the
-- two inputs relativized to that ancestor.
relativeToAncestor :: Path -> Path -> (Path, Path, Path)
relativeToAncestor (Path a) (Path b) = case (a, b) of
  (ha : ta, hb : tb) | ha == hb ->
    let (ancestor, relA, relB) = relativeToAncestor (Path ta) (Path tb)
    in (ha `cons` ancestor, relA, relB)
  -- nothing in common
  _ -> (empty, Path a, Path b)

pattern Parent h t = Path (NameSegment h : t)




empty :: Path
empty = mempty

instance Monoid Path where
  mappend (Path a) (Path b) = Path (a <> b)
  mempty = Path []

instance Semigroup Path where
  (<>) = mappend

cons :: NameSegment -> Path -> Path
cons ns (Path p) = Path (ns : p)

instance Show Path where
  show (Path nss) = intercalate "/" $ fmap escape1 nss
    where escape1 ns = escape =<< (Text.unpack . toText $ ns)
          escape = \case '/' -> "\\/"; c -> [c]

-- unsafeFromString :: NameSegment ->
-- toString :: NameSegment -> String
-- toString = Text.unpack . toText
--
-- isPrefixOf :: Name -> Name -> Bool
-- a `isPrefixOf` b = toText a `Text.isPrefixOf` toText b
--
-- stripPrefix :: Name -> Name -> Maybe Name
-- stripPrefix prefix name =
--   Name <$> Text.stripPrefix (toText prefix) (toText name)
--
-- instance Show Name where
--   show = toString
--
-- instance IsString Name where
--   fromString = unsafeFromText . Text.pack
--
instance H.Hashable NameSegment where
  tokens s = [H.Text (toText s)]
