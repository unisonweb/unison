{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Unison.Codebase.Path
  ( Path,
    pattern AbsolutePath,
    pattern RelativePath,
    absoluteFromSegments,
    relativeFromSegments,
    Position(..),
    singleton,
    unchecked,
    root,
    currentPath,
    Resolve(..),
    -- This seems semantically invalid
    stripPrefix,
    unprefixName,
    HQSplit,
    Split,
    ancestors,

    -- * tests
    isCurrentPath,
    isRoot,

    -- * things that could be replaced with `Convert` instances
    relativePathFromNameSegments,
    fromName,
    fromText,
    toName,
    toText,
    unsafeToRelative,
    unsafeToAbsolute,
    unsplit,
    unsplitHQ,

    -- * things that could be replaced with `Parse` instances
    splitFromName,
    hqSplitFromName,

    Lens.cons,
    Lens.uncons,
    Lens.snoc,
    Lens.unsnoc,
    segments_,

    pattern Empty,

  -- This should be moved to a common util module, or we could use the 'witch' package.
  Convert(..)
  )
where
import Unison.Prelude hiding (empty, toList)

import Control.Lens hiding (Empty, cons, snoc, unsnoc)
import qualified Control.Lens as Lens
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.Sequence (Seq ((:<|)))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Convert(..), Name, Parse)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment)
import qualified Unison.NameSegment as NameSegment
import Unison.Util.Monoid (intercalateMap)
import Data.Function (on)
import Unison.Codebase.Position (Position(..))
import qualified Data.List.Extra as List
import qualified Data.Foldable as Foldable

data Path (pos :: Position) where
  AbsoluteP :: Seq NameSegment -> Path 'Absolute
  RelativeP :: Seq NameSegment -> Path 'Relative
  UncheckedP :: Either (Path 'Absolute) (Path 'Relative) -> Path 'Unchecked

pattern AbsolutePath :: Path 'Absolute -> Path pos
pattern AbsolutePath p <- (match Just (const Nothing) -> Just p)
pattern RelativePath :: Path 'Relative -> Path pos
pattern RelativePath p <- (match (const Nothing) Just -> Just p)
{-# COMPLETE AbsolutePath, RelativePath #-}

absoluteFromSegments :: [NameSegment] -> Path 'Absolute
absoluteFromSegments = AbsoluteP . Seq.fromList
relativeFromSegments :: [NameSegment] -> Path 'Relative
relativeFromSegments = RelativeP . Seq.fromList

pattern Empty :: Path pos
pattern Empty <- (match (nullOf segments_) (nullOf segments_) -> True)

instance Eq (Path pos) where
  (==) = (==) `on` (view segments_)

instance Ord (Path pos) where
  compare = compare `on` (view segments_)

-- Relative paths are the only safe monoid
instance Semigroup (Path 'Relative) where
  RelativeP l <> RelativeP r = RelativeP (l <> r)

instance Monoid (Path 'Relative) where
  mempty = RelativeP mempty

unsafeToRelative :: Path pos -> Path 'Relative
unsafeToRelative p = RelativeP $ p ^. segments_
unsafeToAbsolute :: Path pos -> Path 'Absolute
unsafeToAbsolute p = AbsoluteP $ p ^. segments_

unchecked :: Path pos -> Path 'Unchecked
unchecked = match (UncheckedP . Left) (UncheckedP . Right)

segments_ :: Lens' (Path pos) (Seq NameSegment)
segments_ = lens getter setter
  where
    getter p = match (\(AbsoluteP p) -> p) (\(RelativeP p) -> p) p
    setter :: Path pos -> Seq NameSegment -> Path pos
    setter p segments = case p of
      AbsoluteP{} -> AbsoluteP segments
      RelativeP{} -> RelativeP segments
      UncheckedP (Left p) -> UncheckedP (Left $ setter p segments)
      UncheckedP (Right p) -> UncheckedP (Right $ setter p segments)

match :: (Path 'Absolute -> r) -> (Path 'Relative -> r) -> Path pos -> r
match onAbs onRel = \case
  p@AbsoluteP{} -> onAbs p
  p@RelativeP{} -> onRel p
  (UncheckedP (Left p)) -> onAbs p
  (UncheckedP (Right p)) -> onRel p

isCurrentPath :: Path 'Relative -> Bool
isCurrentPath = (== currentPath)

currentPath :: Path 'Relative
currentPath = RelativeP mempty

isRoot :: Path any -> Bool
isRoot = \case
  AbsolutePath p -> p == AbsoluteP mempty
  _ -> False

root :: Path 'Absolute
root = AbsoluteP mempty

toText :: Path pos -> Text
toText = \case
  AbsolutePath p -> ("." <> segmentsToText p)
  RelativePath p -> segmentsToText p
  where
    segmentsToText = (intercalateMap "." NameSegment.toText . view segments_)

instance Show (Path pos) where
  show = Text.unpack . toText

type Split pos = (Path pos, NameSegment)
type HQSplit pos = (Path pos, HQ'.HQSegment)

unsplit :: Split pos -> Path pos
unsplit (p, a) = Lens.snoc p a

unsplitHQ :: HQSplit pos -> HQ'.HashQualified (Path pos)
unsplitHQ (p, a) = fmap (Lens.snoc p) a

asList_ :: Iso (Seq a) (Seq b) [a] [b]
asList_ = iso Foldable.toList Seq.fromList

-- | examples:
--   stripPrefix .foo.bar .blah == .blah (absolute paths left alone)
--   stripPrefix .foo.bar id    == id    (relative paths starting w/ nonmatching prefix left alone)
--   stripPrefix .foo.bar foo.bar.baz == baz (relative paths w/ common prefix get stripped)
stripPrefix :: Path 'Absolute -> Path any -> Path 'Relative
stripPrefix p r =
  let prefixSegments = p ^. segments_ . asList_
      otherSegments = r ^. segments_ . asList_
   in RelativeP (Seq.fromList $ List.dropPrefix prefixSegments otherSegments)

relativePathFromNameSegments :: [NameSegment] -> Path 'Relative
relativePathFromNameSegments = RelativeP . Seq.fromList

ancestors :: Path 'Absolute -> Seq (Path 'Absolute)
ancestors p = AbsoluteP <$> Seq.inits (view segments_ p)

hqSplitFromName :: Name -> Maybe (HQSplit 'Unchecked)
hqSplitFromName = fmap (fmap HQ'.fromName) . Lens.unsnoc . fromName

splitFromName :: Name -> Maybe (Split 'Unchecked)
splitFromName = Lens.unsnoc . fromName

-- | what is this? —AI
unprefixName :: Path 'Absolute -> Name -> Name
unprefixName prefix = toName . stripPrefix prefix . fromName

singleton :: NameSegment -> Path 'Relative
singleton n = RelativeP (Seq.singleton n)

-- > Path.fromName . Name.unsafeFromText $ ".Foo.bar"
-- /Foo/bar
-- Int./  -> "Int"/"/"
-- pkg/Int.. -> "pkg"/"Int"/"."
-- Int./foo -> error because "/foo" is not a valid NameSegment
--                      and "Int." is not a valid NameSegment
--                      and "Int" / "" / "foo" is not a valid path (internal "")
-- todo: fromName needs to be a little more complicated if we want to allow
--       identifiers called Function.(.)
fromName :: Name -> Path 'Unchecked
fromName n =
  let segments = Seq.fromList . List.NonEmpty.toList . Name.segments $ n
   in if Name.isAbsolute n
         then unchecked $ AbsoluteP segments
         else unchecked $ RelativeP segments

toName :: Path pos -> Name
toName = Name.unsafeFromText . toText

fromText :: Text -> Path 'Unchecked
fromText t = case NameSegment.splitText t of
  (True, segments) -> unchecked . AbsoluteP $ Seq.fromList segments
  (False, segments)-> unchecked . RelativeP $ Seq.fromList segments

instance Cons (Path pos) (Path pos) NameSegment NameSegment where
  _Cons = prism (uncurry cons) uncons where
    cons :: NameSegment -> Path pos -> Path pos
    cons ns = over segments_ (Lens.cons ns)
    uncons :: Path pos -> Either (Path pos) (NameSegment, Path pos)
    uncons p = case p ^. segments_ of
      (hd :<| tl) -> Right (hd, p & segments_ .~ tl)
      _ -> Left p

instance Snoc (Path pos) (Path pos) NameSegment NameSegment where
  _Snoc = prism (uncurry snoc) unsnoc
    where
      snoc :: Path pos -> NameSegment -> Path pos
      snoc p ns = p & segments_ %~ (Lens.|> ns)
      unsnoc :: Path pos -> Either (Path pos) (Path pos, NameSegment)
      unsnoc p = case p ^. segments_ of
        (pref :> ns) -> Right (p & segments_ .~ pref, ns)
        _ -> Left p

instance AsEmpty (Path 'Relative) where
  _Empty = prism' (const $ RelativeP mempty) \case
    RelativeP Lens.Empty -> Just ()
    _ -> Nothing

instance AsEmpty (Path 'Absolute) where
  _Empty = prism' (const $ AbsoluteP mempty) \case
    AbsoluteP Lens.Empty -> Just ()
    _ -> Nothing

instance Snoc (Split pos) (Split pos) NameSegment NameSegment where
  _Snoc = prism (uncurry snoc') $ \case -- unsnoc
    (Lens.unsnoc -> Just (s, a), ns) -> Right ((s, a), ns)
    e -> Left e
    where
    snoc' :: Split pos -> NameSegment -> Split pos
    snoc' (p, a) n = (Lens.snoc p a, n)

class Resolve l r o | l r -> o where
  resolve :: l -> r -> o

instance Resolve (Path 'Absolute) (Path any) (Path 'Absolute) where
  resolve pref p = case p of
    RelativePath rel -> AbsoluteP $ (pref ^. segments_ <> rel ^. segments_)
    AbsolutePath a -> a

instance Resolve (Path 'Relative) (Path any) (Path any) where
  resolve pref p = case p of
    RelativePath _ -> p & segments_ %~ (pref ^. segments_ <>)
    AbsolutePath _ -> p

instance Resolve (Path 'Unchecked) (Path 'Relative) (Path 'Unchecked) where
  resolve pre p = case pre of
    AbsolutePath abs -> unchecked $ resolve abs p
    RelativePath rel -> unchecked $ resolve rel p

instance Resolve (Path 'Unchecked) (Path 'Unchecked) (Path 'Unchecked) where
  resolve pre p = case pre of
    AbsolutePath abs -> unchecked $ resolve abs p
    RelativePath rel -> unchecked $ resolve rel p

instance Resolve (Path 'Unchecked) (Path 'Absolute) (Path 'Absolute) where
  resolve pre p = case pre of
    AbsolutePath abs -> resolve abs p
    RelativePath rel -> resolve rel p

instance Resolve (Path pl) (Path sp) (Path r) 
  => Resolve (Path pl) (HQSplit sp) (HQSplit r) where
    resolve l (p, a) = (resolve l p, a)

instance Convert (Path pos) Text where convert = toText
instance Convert (Path pos) String where convert = show
instance Convert (Path pos) Name where convert = toName
instance Convert (HQSplit pos) (HQ'.HashQualified (Path pos)) where convert = unsplitHQ
instance Parse Name (HQSplit 'Unchecked) where parse = hqSplitFromName
instance Parse Name (Split 'Unchecked) where parse = splitFromName
