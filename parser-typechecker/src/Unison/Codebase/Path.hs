{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Path
  ( Path (..),
    Path' (..),
    Absolute (..),
    pattern AbsolutePath',
    Relative (..),
    pattern RelativePath',
    Resolve (..),
    pattern Empty,
    pattern (Lens.:<),
    pattern (Lens.:>),
    singleton,
    Unison.Codebase.Path.uncons,
    empty,
    absoluteEmpty,
    relativeEmpty',
    currentPath,
    prefix,
    unprefix,
    prefixName,
    unprefixName,
    HQSplit,
    Split,
    Split',
    HQSplit',
    ancestors,

    -- * tests
    isCurrentPath,
    isRoot,
    isRoot',

    -- * things that could be replaced with `Convert` instances
    absoluteToPath',
    fromAbsoluteSplit,
    fromList,
    fromName,
    fromName',
    fromPath',
    fromText,
    toAbsoluteSplit,
    toList,
    toName,
    toName',
    toPath',
    toText,
    toText',
    unsplit,
    unsplit',
    unsplitAbsolute,
    unsplitHQ,
    unsplitHQ',

    -- * things that could be replaced with `Parse` instances
    splitFromName,
    hqSplitFromName',

    -- * things that could be replaced with `Cons` instances
    cons,

    -- * things that could be replaced with `Snoc` instances
    snoc,
    unsnoc,
    -- This should be moved to a common util module, or we could use the 'witch' package.
    Convert (..),
  )
where

import Control.Lens hiding (cons, snoc, unsnoc, pattern Empty)
import qualified Control.Lens as Lens
import qualified Data.Foldable as Foldable
import Data.List.Extra (dropPrefix)
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.Sequence (Seq ((:<|), (:|>)))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified GHC.Exts as GHC
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Convert (..), Name, Parse)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment (NameSegment))
import qualified Unison.NameSegment as NameSegment
import Unison.Prelude hiding (empty, toList)
import Unison.Util.Monoid (intercalateMap)

-- `Foo.Bar.baz` becomes ["Foo", "Bar", "baz"]
newtype Path = Path {toSeq :: Seq NameSegment}
  deriving stock (Eq, Ord)
  deriving newtype (Semigroup, Monoid)

-- | Meant for use mostly in doc-tests where it's
-- sometimes convenient to specify paths as lists.
instance GHC.IsList Path where
  type Item Path = NameSegment
  toList (Path segs) = Foldable.toList segs
  fromList = Path . Seq.fromList

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

unsplitAbsolute :: (Absolute, NameSegment) -> Absolute
unsplitAbsolute =
  coerce unsplit

unsplitHQ :: HQSplit -> HQ'.HashQualified Path
unsplitHQ (p, a) = fmap (snoc p) a

unsplitHQ' :: HQSplit' -> HQ'.HashQualified Path'
unsplitHQ' (p, a) = fmap (snoc' p) a

type Split = (Path, NameSegment)

type HQSplit = (Path, HQ'.HQSegment)

type Split' = (Path', NameSegment)

type HQSplit' = (Path', HQ'.HQSegment)

type HQSplitAbsolute = (Absolute, HQ'.HQSegment)

-- | examples:
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

toAbsoluteSplit :: Absolute -> (Path', a) -> (Absolute, a)
toAbsoluteSplit a (p, s) = (resolve a p, s)

fromAbsoluteSplit :: (Absolute, a) -> (Path, a)
fromAbsoluteSplit (Absolute p, a) = (p, a)

absoluteEmpty :: Absolute
absoluteEmpty = Absolute empty

relativeEmpty' :: Path'
relativeEmpty' = Path' (Right (Relative empty))

toPath' :: Path -> Path'
toPath' = \case
  Path (NameSegment "" :<| tail) -> Path' . Left . Absolute . Path $ tail
  p -> Path' . Right . Relative $ p

-- Forget whether the path is absolute or relative
fromPath' :: Path' -> Path
fromPath' (Path' e) = case e of
  Left (Absolute p) -> p
  Right (Relative p) -> p

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

-- | what is this? —AI
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
fromName = fromList . List.NonEmpty.toList . Name.segments

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

pattern Empty = Path Seq.Empty

pattern AbsolutePath' :: Absolute -> Path'
pattern AbsolutePath' p = Path' (Left p)

pattern RelativePath' :: Relative -> Path'
pattern RelativePath' p = Path' (Right p)

{-# COMPLETE AbsolutePath', RelativePath' #-}

empty :: Path
empty = Path mempty

instance Show Path where
  show = Text.unpack . toText

-- | Note: This treats the path as relative.
toText :: Path -> Text
toText (Path nss) = intercalateMap "." NameSegment.toText nss

fromText :: Text -> Path
fromText = \case
  "" -> empty
  t -> fromList $ NameSegment <$> NameSegment.segments' t

toText' :: Path' -> Text
toText' = \case
  Path' (Left (Absolute path)) -> Text.cons '.' (toText path)
  Path' (Right (Relative path)) -> toText path

{-# COMPLETE Empty, (:<) #-}

{-# COMPLETE Empty, (:>) #-}

deriving anyclass instance AsEmpty Path

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

instance Convert Absolute Text where convert = toText' . absoluteToPath'

instance Convert Relative Text where convert = toText . unrelative

instance Convert Absolute String where convert = Text.unpack . convert

instance Convert Relative String where convert = Text.unpack . convert

instance Convert [NameSegment] Path where convert = fromList

instance Convert Path [NameSegment] where convert = toList

instance Convert HQSplit (HQ'.HashQualified Path) where convert = unsplitHQ

instance Convert Path Name where convert = toName

instance Convert Path' Name where convert = toName'

instance Convert HQSplit' (HQ'.HashQualified Path') where convert = unsplitHQ'

instance Parse Name HQSplit' where parse = hqSplitFromName'

instance Parse Name Split where parse = splitFromName
