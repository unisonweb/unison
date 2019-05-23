{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns #-}

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

import qualified Data.Foldable as Foldable
import           Data.List                (intercalate)
-- import           Data.String                    ( IsString
--                                                 , fromString
--                                                 )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Sequence                  (Seq((:<|),(:|>) ))
import qualified Data.Sequence                 as Seq
import qualified Unison.Hashable               as H
import           Unison.Name                    ( Name )
import qualified Unison.Name                   as Name
import qualified Unison.HashQualified          as HQ

-- Represents the parts of a name between the `.`s
newtype NameSegment = NameSegment { toText :: Text } deriving (Eq, Ord, Show)

-- `Foo.Bar.baz` becomes ["Foo", "Bar", "baz"]
newtype Path = Path { toSeq :: Seq NameSegment } deriving (Eq, Ord)

newtype Absolute = Absolute Path deriving (Eq,Ord,Show)
newtype Relative = Relative Path deriving (Eq,Ord,Show)
newtype Path' = Path' (Either Absolute Relative) deriving (Eq,Ord,Show)

type HQSegment = HQ.HashQualified' NameSegment

type Split = (Path, NameSegment)
type HQSplit = (Path, HQSegment)

type Split' = (Path', NameSegment)
type HQSplit' = (Path', HQSegment)

type SplitAbsolute = (Absolute, NameSegment)
type HQSplitAbsolute = (Absolute, HQSegment)

parsePath :: Text -> Either String Path'
parsePath = error "todo"

parseHashQualified :: Text -> Either String HQSplit'
parseHashQualified = error "todo"

toAbsoluteSplit :: Absolute -> (Path', a) -> (Absolute, a)
toAbsoluteSplit a (p, s) = (toAbsolutePath a p, s)

fromSplit' :: (Path', a) -> (Path, a)
fromSplit' (Path' (Left (Absolute p)), a) = (p, a)
fromSplit' (Path' (Right (Relative p)), a) = (p, a)

fromAbsoluteSplit :: (Absolute, a) -> (Path, a)
fromAbsoluteSplit (Absolute p, a) = (p, a)

absoluteEmpty :: Absolute
absoluteEmpty = Absolute (Path mempty)

toAbsolutePath :: Absolute -> Path' -> Absolute
toAbsolutePath (Absolute cur) (Path' p) = case p of
  Left a -> a
  Right (Relative rel) -> Absolute (Path $ toSeq cur <> toSeq rel)

toPath' :: Path -> Path'
toPath' = \case
  Path (NameSegment "" :<| tail) -> Path' . Left . Absolute . Path $ tail
  p -> Path' . Right . Relative $ p

toList :: Path -> [NameSegment]
toList = Foldable.toList . toSeq

fromList :: [NameSegment] -> Path
fromList = Path . Seq.fromList

singleton :: NameSegment -> Path
singleton n = fromList [n]

snoc :: Path -> NameSegment -> Path
snoc (Path p) ns = Path (p <> pure ns)

unsnoc :: Path -> Maybe (Path, NameSegment)
unsnoc p = case p of
  Path (init :|> last) -> Just (Path init, last)
  _ -> Nothing

uncons :: Path -> Maybe (NameSegment, Path)
uncons p = case p of
  Path (hd :<| tl) -> Just (hd, Path tl)
  _ -> Nothing

asIdentifier :: Path -> Text
asIdentifier = Text.intercalate "." . fmap toText . toList

asDirectory :: Path -> Text
asDirectory p = case toList p of
  NameSegment "_root_" : (Seq.fromList -> tail) ->
    "/" <> asDirectory (Path tail)
  other -> Text.intercalate "/" . fmap toText $ other

-- > Path.fromName . Name.unsafeFromText $ ".Foo.bar"
-- /Foo/bar
-- Int./  -> "Int"/"/"
-- pkg/Int.. -> "pkg"/"Int"/"."
-- Int./foo -> error because "/foo" is not a valid NameSegment
--                      and "Int." is not a valid NameSegment
--                      and "Int" / "" / "foo" is not a valid path (internal "")
-- todo: fromName needs to be a little more complicated if we want to allow
--       identifiers called Function.(.)
fromName :: Name -> Path
fromName = fromList . fmap NameSegment . Text.splitOn "." . Name.toText

toName :: Path -> Name
toName = Name.unsafeFromText . asIdentifier

-- Returns the nearest common ancestor, along with the
-- two inputs relativized to that ancestor.
relativeToAncestor :: Path -> Path -> (Path, Path, Path)
relativeToAncestor (Path a) (Path b) = case (a, b) of
  (ha :<| ta, hb :<| tb) | ha == hb ->
    let (ancestor, relA, relB) = relativeToAncestor (Path ta) (Path tb)
    in (ha `cons` ancestor, relA, relB)
  -- nothing in common
  _ -> (empty, Path a, Path b)

pattern Parent h t = Path (NameSegment h :<| t)

empty :: Path
empty = Path mempty

cons :: NameSegment -> Path -> Path
cons ns (Path p) = Path (ns :<| p)

instance Show Path where
  show (Path nss) = intercalate "/" $ fmap escape1 (Foldable.toList nss)
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
