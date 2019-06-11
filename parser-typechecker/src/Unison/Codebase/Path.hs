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

--import Debug.Trace
import qualified Data.Foldable as Foldable
-- import           Data.String                    ( IsString
--                                                 , fromString
--                                                 )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Sequence                  (Seq((:<|),(:|>) ))
import qualified Data.Sequence                 as Seq
import           Unison.Name                    ( Name )
import qualified Unison.Name                   as Name
import Unison.Util.Monoid (intercalateMap)
import qualified Unison.Lexer                  as Lexer

import Unison.Codebase.NameSegment (NameSegment(NameSegment), HQSegment, HQ'Segment)
import qualified Unison.Codebase.NameSegment as NameSegment

-- `Foo.Bar.baz` becomes ["Foo", "Bar", "baz"]
newtype Path = Path { toSeq :: Seq NameSegment } deriving (Eq, Ord)

newtype Absolute = Absolute { unabsolute :: Path } deriving (Eq,Ord)
newtype Relative = Relative { unrelative :: Path } deriving (Eq,Ord)
newtype Path' = Path' (Either Absolute Relative) deriving (Eq,Ord,Show)

instance Show Absolute where
  show s = "." ++ show (unabsolute s)

instance Show Relative where
  show = show . unrelative

unsplit' :: Split' -> Path'
unsplit' (Path' (Left (Absolute p)), seg) = Path' (Left (Absolute (unsplit (p, seg))))
unsplit' (Path' (Right (Relative p)), seg) = Path' (Right (Relative (unsplit (p, seg))))

unsplit :: Show a => (Path, a) -> Path
unsplit (Path p, a) = Path (p :|> NameSegment (Text.pack (show a)))

type Split = (Path, NameSegment)
type HQSplit = (Path, HQSegment)
type HQ'Split = (Path, HQ'Segment)

type Split' = (Path', NameSegment)
type HQSplit' = (Path', HQSegment)
type HQ'Split' = (Path', HQ'Segment)

type SplitAbsolute = (Absolute, NameSegment)
type HQSplitAbsolute = (Absolute, HQSegment)

-- .libs.blah.poo is Absolute
-- libs.blah.poo is Relative
-- Left is some parse error tbd
parsePath' :: String -> Either String Path'
parsePath' p = case p of
  '.' : p -> Path' . Left . Absolute . fromList <$> segs p
  p -> Path' . Right . Relative . fromList <$> segs p
  where
  segs p = traverse validate
         . filter (not . Text.null)
         . Text.splitOn "."
         $ Text.pack p
  validate seg =
    case (fst <$> Lexer.wordyId0 (Text.unpack seg),
          fst <$> Lexer.symbolyId0 (Text.unpack seg)) of
      (Left e, Left _) -> Left (show e)
      (Right a, _) -> Right (NameSegment $ Text.pack a)
      (_, Right a) -> Right (NameSegment $ Text.pack a)

parseSplit' :: String -> Either String Split'
parseSplit' p = case parsePath' p of
  Left e -> Left e
  Right (Path' e) -> case e of
    Left (Absolute p) -> case unsnoc p of
      Nothing -> Left "empty path"
      Just (p, seg) -> pure (Path' . Left . Absolute $ p, seg)
    Right (Relative p) -> case unsnoc p of
      Nothing -> Left "empty path"
      Just (p, seg) -> pure (Path' . Right . Relative $ p, seg)

parseHQSplit' :: String -> Either String HQSplit'
parseHQSplit' = error "todo"

parseHQ'Split' :: String -> Either String HQ'Split'
parseHQ'Split' = error "todo"

-- this might be useful in implementing the above
-- hqToPathSeg :: HashQualified -> (Path.Path', HQSegment)
-- hqToPathSeg = \case
--   HQ.NameOnly n -> (p', HQ.NameOnly n') where (p', n') = splitName n
--   HQ.HashOnly h -> (Path.Path' (Left Path.absoluteEmpty), HQ.HashOnly h)
--   HQ.HashQualified n h -> (p',HQ.HashQualified n' h) where (p',n') = splitName n
--   where
--   splitName n = (Path.toPath' p, n') where
--     (p, n') = fromMaybe (error "hq name can't be empty")
--                         (Path.unsnoc (Path.fromName n))

toAbsoluteSplit :: Absolute -> (Path', a) -> (Absolute, a)
toAbsoluteSplit a (p, s) = (toAbsolutePath a p, s)

fromSplit' :: (Path', a) -> (Path, a)
fromSplit' (Path' (Left (Absolute p)), a) = (p, a)
fromSplit' (Path' (Right (Relative p)), a) = (p, a)

fromAbsoluteSplit :: (Absolute, a) -> (Path, a)
fromAbsoluteSplit (Absolute p, a) = (p, a)

absoluteEmpty :: Absolute
absoluteEmpty = Absolute empty

relativeEmpty' :: Path'
relativeEmpty' = Path' (Right (Relative empty))

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

splitFromName :: Name -> Maybe Split
splitFromName = unsnoc . fromName

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

--asDirectory :: Path -> Text
--asDirectory p = case toList p of
--  NameSegment "_root_" : (Seq.fromList -> tail) ->
--    "/" <> asDirectory (Path tail)
--  other -> Text.intercalate "/" . fmap NameSegment.toText $ other

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
toName = Name.unsafeFromText . toText

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
  show = Text.unpack . toText

toText :: Path -> Text
toText (Path nss) = intercalateMap "." NameSegment.toText nss