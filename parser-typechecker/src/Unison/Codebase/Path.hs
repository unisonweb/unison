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
import Control.Lens hiding (unsnoc, cons)
import qualified Control.Lens as Lens
import Data.Either.Combinators (maybeToRight)
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
import qualified Unison.HashQualified as HQ
import qualified Unison.HashQualified' as HQ'
import qualified Unison.ShortHash as SH

import Unison.Codebase.NameSegment (NameSegment(NameSegment), HQSegment, HQ'Segment)
import qualified Unison.Codebase.NameSegment as NameSegment

-- `Foo.Bar.baz` becomes ["Foo", "Bar", "baz"]
newtype Path = Path { toSeq :: Seq NameSegment } deriving (Eq, Ord)

newtype Absolute = Absolute { unabsolute :: Path } deriving (Eq,Ord)
newtype Relative = Relative { unrelative :: Path } deriving (Eq,Ord)
newtype Path' = Path' (Either Absolute Relative) deriving (Eq,Ord)

instance Show Path' where
  show (Path' (Left abs)) = show abs
  show (Path' (Right rel)) = show rel

instance Show Absolute where
  show s = "." ++ show (unabsolute s)

instance Show Relative where
  show = show . unrelative

unsplit' :: Split' -> Path'
unsplit' (Path' (Left (Absolute p)), seg) = Path' (Left (Absolute (unsplit (p, seg))))
unsplit' (Path' (Right (Relative p)), seg) = Path' (Right (Relative (unsplit (p, seg))))

unsplit :: (Path, NameSegment) -> Path
unsplit (Path p, a) = Path (p :|> a)

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
-- All the segments must be wordyIds
parsePath' :: String -> Either String Path'
parsePath' p = case parsePath'Impl p of
  Left e -> Left e
  Right (p, "") -> Right p
  Right (p, rem) -> case Lexer.wordyId0 rem of
    Right (seg, "") ->
      Right (unsplit' (p, NameSegment . Text.pack $ seg))
    Right (_, rem) ->
      Left ("extra characters after " <> show p <> ": " ++ show rem)
    Left e -> Left (show e)

-- implementation detail of parsePath' and parseSplit'
-- foo.bar.baz.34 becomes `Right (foo.bar.baz, "34")
-- foo.bar.baz    becomes `Right (foo.bar, "baz")
-- foo.bar.baz#a8fj becomes `Right (foo.bar, "baz#a8fj")`
parsePath'Impl :: String -> Either String (Path', String)
parsePath'Impl p = case p of
  "." -> Right (Path' . Left $ absoluteEmpty, "")
  '.' : p -> over _1 (Path' . Left  . Absolute . fromList) <$> segs p
  p       -> over _1 (Path' . Right . Relative . fromList) <$> segs p
  where
  segs p = case Lexer.wordyId p of
    Left e         -> Left (show e)
    Right (a, "") -> case Lens.unsnoc (Text.splitOn "." $ Text.pack a) of
      Nothing -> Left "empty path"
      Just (segs, last) ->
        Right (NameSegment <$> segs, Text.unpack last)
    Right (segs, '.':rem) ->
      let segs' = Text.splitOn "." (Text.pack segs)
      in Right (NameSegment <$> segs', rem)
    Right (segs, rem) ->
      Left $ "extra characters after " <> segs <> ": " <> show rem

wordyNameSegment, definitionNameSegment :: String -> Either String NameSegment
wordyNameSegment s = case Lexer.wordyId0 s of
  Left e -> Left (show e)
  Right (a, "") -> Right (NameSegment (Text.pack a))
  Right (a, rem) ->
    Left $ "trailing characters after " <> show a <> ": " <> show rem

definitionNameSegment s = wordyNameSegment s <> symbolyNameSegment s
  where
  symbolyNameSegment s = case Lexer.symbolyId0 s of
    Left e -> Left (show e)
    Right (a, "") -> Right (NameSegment (Text.pack a))
    Right (a, rem) ->
      Left $ "trailing characters after " <> show a <> ": " <> show rem

-- parseSplit' wordyNameSegment "foo.bar.baz" returns Right (foo.bar, baz)
-- parseSplit' wordyNameSegment "foo.bar.+" returns Left err
-- parseSplit' wordyOrSymbolyNameSegment "foo.bar.+" returns Right (foo.bar, +)
parseSplit' :: (String -> Either String NameSegment)
            -> String
            -> Either String Split'
parseSplit' lastSegment p = do
  (p', rem) <- parsePath'Impl p
  seg <- lastSegment rem
  pure (p', seg)

parseHQSplit' :: String -> Either String HQSplit'
parseHQSplit' s = do
  (p, rem) <- parsePath'Impl s
  case Text.splitOn "#" (Text.pack rem) of
    [] -> error $ "encountered empty string parsing '" <> s <> "'"
    [n] -> do
      seg <- definitionNameSegment (Text.unpack n)
      pure (p, HQ.NameOnly seg)
    ["", sh] ->
      maybeToRight (shError s) . fmap (\sh -> (p, HQ.HashOnly sh))
      . SH.fromText $ "#" <> sh
    [n, sh] -> do
      seg <- definitionNameSegment (Text.unpack n)
      maybeToRight (shError s) .
        fmap (\sh -> (p, HQ.HashQualified seg sh)) .
        SH.fromText $ "#" <> sh
    _ -> Left $ s <> " has too many #."
  where
  shError s = "couldn't parse shorthash from " <> s

parseHQ'Split' :: String -> Either String HQ'Split'
parseHQ'Split' s = do
  (p, rem) <- parsePath'Impl s
  case Text.splitOn "#" (Text.pack rem) of
    [] -> error $ "encountered empty string parsing '" <> s <> "'"
    [n] -> do
      seg <- definitionNameSegment (Text.unpack n)
      pure (p, HQ'.NameOnly $ seg)
    [n, sh] -> do
      seg <- definitionNameSegment (Text.unpack n)
      maybeToRight (shError s) .
        fmap (\sh -> (p, HQ'.HashQualified seg sh)) .
        SH.fromText $ "#" <> sh
    _ -> Left $ s <> " has too many #."
  where
  shError s = "couldn't parse shorthash from " <> s


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
