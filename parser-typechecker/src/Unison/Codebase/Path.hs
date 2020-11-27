{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Path where

import Control.Lens hiding (cons, snoc, unsnoc)
import qualified Control.Lens as Lens
import Data.Bifunctor (first)
import qualified Data.Foldable as Foldable
import Data.List.Extra (dropPrefix, stripPrefix)
import Data.Sequence (Seq ((:<|), (:|>)))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Unison.HashQualified' as HQ'
import qualified Unison.Lexer as Lexer
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment (NameSegment))
import qualified Unison.NameSegment as NameSegment
import Unison.Prelude hiding (empty, toList)
import qualified Unison.ShortHash as SH
import Unison.Util.Monoid (intercalateMap)

-- `Foo.Bar.baz` becomes ["Foo", "Bar", "baz"]
newtype Path = Path {toSeq :: Seq NameSegment} deriving (Eq, Ord)

newtype Absolute = Absolute {unabsolute :: Path} deriving (Eq, Ord)

newtype Relative = Relative {unrelative :: Path} deriving (Eq, Ord)

newtype Path' = Path' {unPath' :: Either Absolute Relative}
  deriving (Eq, Ord)

isCurrentPath :: Path' -> Bool
isCurrentPath p = p == currentPath

currentPath :: Path'
currentPath = Path' (Right (Relative (Path mempty)))

isRoot' :: Path' -> Bool
isRoot' = either isRoot (const False) . unPath'

isRoot :: Absolute -> Bool
isRoot = Seq.null . toSeq . unabsolute

absoluteToPath' :: Absolute -> Path'
absoluteToPath' abs = Path' (Left abs)

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

unsplit :: Split -> Path
unsplit (Path p, a) = Path (p :|> a)

unsplitHQ :: HQSplit -> HQ'.HashQualified' Path
unsplitHQ (p, a) = fmap (snoc p) a

unsplitHQ' :: HQSplit' -> HQ'.HashQualified' Path'
unsplitHQ' (p, a) = fmap (snoc' p) a

type Split = (Path, NameSegment)

type HQSplit = (Path, HQ'.HQSegment)

type Split' = (Path', NameSegment)

type HQSplit' = (Path', HQ'.HQSegment)

type SplitAbsolute = (Absolute, NameSegment)

type HQSplitAbsolute = (Absolute, HQ'.HQSegment)

-- examples:
--   unprefix .foo.bar .blah == .blah (absolute paths left alone)
--   unprefix .foo.bar id    == id    (relative paths starting w/ nonmatching prefix left alone)
--   unprefix .foo.bar foo.bar.baz == baz (relative paths w/ common prefix get stripped)
unprefix :: Absolute -> Path' -> Path
unprefix (Absolute prefix) (Path' p) = case p of
  Left abs -> unabsolute abs
  Right (unrelative -> rel) -> fromList $ dropPrefix (toList prefix) (toList rel)

-- too many types
prefix :: Absolute -> Path' -> Path
prefix (Absolute (Path prefix)) (Path' p) = case p of
  Left (unabsolute -> abs) -> abs
  Right (unrelative -> rel) -> Path $ prefix <> toSeq rel

-- .libs.blah.poo is Absolute
-- libs.blah.poo is Relative
-- Left is some parse error tbd
parsePath' :: String -> Either String Path'
parsePath' p = case parsePathImpl' p of
  Left e -> Left e
  Right (p, "") -> Right p
  Right (p, rem) ->
    case (first show . (Lexer.wordyId0 <> Lexer.symbolyId0) <> unit') rem of
      Right (seg, "") -> Right (unsplit' (p, NameSegment . Text.pack $ seg))
      Right (_, rem) ->
        Left ("extra characters after " <> show p <> ": " <> show rem)
      Left e -> Left e

-- implementation detail of parsePath' and parseSplit'
-- foo.bar.baz.34 becomes `Right (foo.bar.baz, "34")
-- foo.bar.baz    becomes `Right (foo.bar, "baz")
-- baz            becomes `Right (, "baz")
-- foo.bar.baz#a8fj becomes `Left`; we don't hash-qualify paths.
-- TODO: Get rid of this thing.
parsePathImpl' :: String -> Either String (Path', String)
parsePathImpl' p = case p of
  "." -> Right (Path' . Left $ absoluteEmpty, "")
  '.' : p -> over _1 (Path' . Left . Absolute . fromList) <$> segs p
  p -> over _1 (Path' . Right . Relative . fromList) <$> segs p
  where
    go f p = case f p of
      Right (a, "") -> case Lens.unsnoc (Name.segments' $ Text.pack a) of
        Nothing -> Left "empty path"
        Just (segs, last) -> Right (NameSegment <$> segs, Text.unpack last)
      Right (segs, '.' : rem) ->
        let segs' = Name.segments' (Text.pack segs)
         in Right (NameSegment <$> segs', rem)
      Right (segs, rem) ->
        Left $ "extra characters after " <> segs <> ": " <> show rem
      Left e -> Left e
    segs p = go (first show . (Lexer.symbolyId <> Lexer.wordyId) <> unit') p

wordyNameSegment, definitionNameSegment :: String -> Either String NameSegment
wordyNameSegment s = case Lexer.wordyId0 s of
  Left e -> Left (show e)
  Right (a, "") -> Right (NameSegment (Text.pack a))
  Right (a, rem) ->
    Left $ "trailing characters after " <> show a <> ": " <> show rem

optionalWordyNameSegment :: String -> Either String NameSegment
optionalWordyNameSegment "" = Right $ NameSegment ""
optionalWordyNameSegment s = wordyNameSegment s

-- Parse a name segment like "()"
unit' :: String -> Either String (String, String)
unit' s = case stripPrefix "()" s of
  Nothing -> Left $ "Expected () but found: " <> s
  Just rem -> Right ("()", rem)

unit :: String -> Either String NameSegment
unit s = case unit' s of
  Right (_, "") -> Right $ NameSegment "()"
  Right (_, rem) -> Left $ "trailing characters after (): " <> show rem
  Left _ -> Left $ "I don't know how to parse " <> s

definitionNameSegment s = wordyNameSegment s <> symbolyNameSegment s <> unit s
  where
    symbolyNameSegment s = case Lexer.symbolyId0 s of
      Left e -> Left (show e)
      Right (a, "") -> Right (NameSegment (Text.pack a))
      Right (a, rem) ->
        Left $ "trailing characters after " <> show a <> ": " <> show rem

-- parseSplit' wordyNameSegment "foo.bar.baz" returns Right (foo.bar, baz)
-- parseSplit' wordyNameSegment "foo.bar.+" returns Left err
-- parseSplit' definitionNameSegment "foo.bar.+" returns Right (foo.bar, +)
parseSplit' ::
  (String -> Either String NameSegment) ->
  String ->
  Either String Split'
parseSplit' lastSegment p = do
  (p', rem) <- parsePathImpl' p
  seg <- lastSegment rem
  pure (p', seg)

parseShortHashOrHQSplit' :: String -> Either String (Either SH.ShortHash HQSplit')
parseShortHashOrHQSplit' s =
  case Text.breakOn "#" $ Text.pack s of
    ("", "") -> error $ "encountered empty string parsing '" <> s <> "'"
    (n, "") -> do
      (p, rem) <- parsePathImpl' (Text.unpack n)
      seg <- definitionNameSegment rem
      pure $ Right (p, HQ'.NameOnly seg)
    ("", sh) -> do
      sh <- maybeToRight (shError s) . SH.fromText $ sh
      pure $ Left sh
    (n, sh) -> do
      (p, rem) <- parsePathImpl' (Text.unpack n)
      seg <- definitionNameSegment rem
      hq <-
        maybeToRight (shError s)
          . fmap (\sh -> (p, HQ'.HashQualified seg sh))
          . SH.fromText
          $ sh
      pure $ Right hq
  where
    shError s = "couldn't parse shorthash from " <> s

parseHQSplit :: String -> Either String HQSplit
parseHQSplit s = case parseHQSplit' s of
  Right (Path' (Right (Relative p)), hqseg) -> Right (p, hqseg)
  Right (Path' Left {}, _) ->
    Left $ "Sorry, you can't use an absolute name like " <> s <> " here."
  Left e -> Left e

parseHQSplit' :: String -> Either String HQSplit'
parseHQSplit' s = case Text.breakOn "#" $ Text.pack s of
  ("", "") -> error $ "encountered empty string parsing '" <> s <> "'"
  ("", _) -> Left "Sorry, you can't use a hash-only reference here."
  (n, "") -> do
    (p, rem) <- parsePath n
    seg <- definitionNameSegment rem
    pure (p, HQ'.NameOnly seg)
  (n, sh) -> do
    (p, rem) <- parsePath n
    seg <- definitionNameSegment rem
    maybeToRight (shError s)
      . fmap (\sh -> (p, HQ'.HashQualified seg sh))
      . SH.fromText
      $ sh
  where
    shError s = "couldn't parse shorthash from " <> s
    parsePath n = do
      x <- parsePathImpl' $ Text.unpack n
      pure $ case x of
        (Path' (Left e), "") | e == absoluteEmpty -> (relativeEmpty', ".")
        x -> x

toAbsoluteSplit :: Absolute -> (Path', a) -> (Absolute, a)
toAbsoluteSplit a (p, s) = (resolve a p, s)

fromSplit' :: (Path', a) -> (Path, a)
fromSplit' (Path' (Left (Absolute p)), a) = (p, a)
fromSplit' (Path' (Right (Relative p)), a) = (p, a)

fromAbsoluteSplit :: (Absolute, a) -> (Path, a)
fromAbsoluteSplit (Absolute p, a) = (p, a)

absoluteEmpty :: Absolute
absoluteEmpty = Absolute empty

relativeEmpty' :: Path'
relativeEmpty' = Path' (Right (Relative empty))

relativeSingleton :: NameSegment -> Relative
relativeSingleton = Relative . Path . Seq.singleton

toPath' :: Path -> Path'
toPath' = \case
  Path (NameSegment "" :<| tail) -> Path' . Left . Absolute . Path $ tail
  p -> Path' . Right . Relative $ p

toList :: Path -> [NameSegment]
toList = Foldable.toList . toSeq

fromList :: [NameSegment] -> Path
fromList = Path . Seq.fromList

ancestors :: Absolute -> Seq Absolute
ancestors (Absolute (Path segments)) = Absolute . Path <$> Seq.inits segments

hqSplitFromName' :: Name -> Maybe HQSplit'
hqSplitFromName' = fmap (fmap HQ'.fromName) . Lens.unsnoc . fromName'

splitFromName :: Name -> Maybe Split
splitFromName = unsnoc . fromName

unprefixName :: Absolute -> Name -> Name
unprefixName prefix = toName . unprefix prefix . fromName'

prefixName :: Absolute -> Name -> Name
prefixName p = toName . prefix p . fromName'

singleton :: NameSegment -> Path
singleton n = fromList [n]

cons :: NameSegment -> Path -> Path
cons = Lens.cons

snoc :: Path -> NameSegment -> Path
snoc = Lens.snoc

snoc' :: Path' -> NameSegment -> Path'
snoc' = Lens.snoc

unsnoc :: Path -> Maybe (Path, NameSegment)
unsnoc = Lens.unsnoc

uncons :: Path -> Maybe (NameSegment, Path)
uncons = Lens.uncons

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
fromName = fromList . Name.segments

fromName' :: Name -> Path'
fromName' n = case take 1 (Name.toString n) of
  "." -> Path' . Left . Absolute $ Path seq
  _ -> Path' . Right $ Relative path
  where
    path = fromName n
    seq = toSeq path

toName :: Path -> Name
toName = Name.unsafeFromText . toText

-- | Convert a Path' to a Name
toName' :: Path' -> Name
toName' = Name.unsafeFromText . toText'

-- Returns the nearest common ancestor, along with the
-- two inputs relativized to that ancestor.
relativeToAncestor :: Path -> Path -> (Path, Path, Path)
relativeToAncestor (Path a) (Path b) = case (a, b) of
  (ha :<| ta, hb :<| tb)
    | ha == hb ->
      let (ancestor, relA, relB) = relativeToAncestor (Path ta) (Path tb)
       in (ha `cons` ancestor, relA, relB)
  -- nothing in common
  _ -> (empty, Path a, Path b)

pattern Parent h t = Path (NameSegment h :<| t)

pattern Empty = Path Seq.Empty

empty :: Path
empty = Path mempty

instance Show Path where
  show = Text.unpack . toText

toText :: Path -> Text
toText (Path nss) = intercalateMap "." NameSegment.toText nss

fromText :: Text -> Path
fromText = \case
  "" -> empty
  t -> fromList $ NameSegment <$> Name.segments' t

toText' :: Path' -> Text
toText' = \case
  Path' (Left (Absolute path)) -> Text.cons '.' (toText path)
  Path' (Right (Relative path)) -> toText path

instance Cons Path Path NameSegment NameSegment where
  _Cons = prism (uncurry cons) uncons
    where
      cons :: NameSegment -> Path -> Path
      cons ns (Path p) = Path (ns :<| p)
      uncons :: Path -> Either Path (NameSegment, Path)
      uncons p = case p of
        Path (hd :<| tl) -> Right (hd, Path tl)
        _ -> Left p

instance Snoc Relative Relative NameSegment NameSegment where
  _Snoc = prism (uncurry snocRelative) $ \case
    Relative (Lens.unsnoc -> Just (s, a)) -> Right (Relative s, a)
    e -> Left e
    where
      snocRelative :: Relative -> NameSegment -> Relative
      snocRelative r n = Relative . (`Lens.snoc` n) $ unrelative r

instance Snoc Absolute Absolute NameSegment NameSegment where
  _Snoc = prism (uncurry snocAbsolute) $ \case
    Absolute (Lens.unsnoc -> Just (s, a)) -> Right (Absolute s, a)
    e -> Left e
    where
      snocAbsolute :: Absolute -> NameSegment -> Absolute
      snocAbsolute a n = Absolute . (`Lens.snoc` n) $ unabsolute a

instance Snoc Path Path NameSegment NameSegment where
  _Snoc = prism (uncurry snoc) unsnoc
    where
      unsnoc :: Path -> Either Path (Path, NameSegment)
      unsnoc = \case
        Path (s Seq.:|> a) -> Right (Path s, a)
        e -> Left e
      snoc :: Path -> NameSegment -> Path
      snoc (Path p) ns = Path (p <> pure ns)

instance Snoc Path' Path' NameSegment NameSegment where
  _Snoc = prism (uncurry snoc') $ \case
    Path' (Left (Lens.unsnoc -> Just (s, a))) -> Right (Path' (Left s), a)
    Path' (Right (Lens.unsnoc -> Just (s, a))) -> Right (Path' (Right s), a)
    e -> Left e
    where
      snoc' :: Path' -> NameSegment -> Path'
      snoc' (Path' e) n = case e of
        Left abs -> Path' (Left . Absolute $ Lens.snoc (unabsolute abs) n)
        Right rel -> Path' (Right . Relative $ Lens.snoc (unrelative rel) n)

instance Snoc Split' Split' NameSegment NameSegment where
  _Snoc = prism (uncurry snoc') $ \case
    -- unsnoc
    (Lens.unsnoc -> Just (s, a), ns) -> Right ((s, a), ns)
    e -> Left e
    where
      snoc' :: Split' -> NameSegment -> Split'
      snoc' (p, a) n = (Lens.snoc p a, n)

class Resolve l r o where
  resolve :: l -> r -> o

instance Resolve Path Path Path where
  resolve (Path l) (Path r) = Path (l <> r)

instance Resolve Relative Relative Relative where
  resolve (Relative (Path l)) (Relative (Path r)) = Relative (Path (l <> r))

instance Resolve Absolute Relative Absolute where
  resolve (Absolute l) (Relative r) = Absolute (resolve l r)

instance Resolve Path' Path' Path' where
  resolve _ a@(Path' Left {}) = a
  resolve (Path' (Left a)) (Path' (Right r)) = Path' (Left (resolve a r))
  resolve (Path' (Right r1)) (Path' (Right r2)) = Path' (Right (resolve r1 r2))

instance Resolve Path' Split' Path' where
  resolve l r = resolve l (unsplit' r)

instance Resolve Path' Split' Split' where
  resolve l (r, ns) = (resolve l r, ns)

instance Resolve Absolute HQSplit HQSplitAbsolute where
  resolve l (r, hq) = (resolve l (Relative r), hq)

instance Resolve Absolute Path' Absolute where
  resolve _ (Path' (Left a)) = a
  resolve a (Path' (Right r)) = resolve a r
