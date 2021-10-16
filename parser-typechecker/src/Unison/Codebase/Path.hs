{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Unison.Codebase.Path
  ( Path (..),
    PathType(..),
    UnknownPath,
    matchUnknown,
    -- Resolve (..),
    pattern Lens.Empty,
    isEmpty,
    singleton,
    emptyRelative,
    emptyAbsolute,
    currentPath,
    prefix,
    unprefix,
    -- prefixName,
    -- unprefixName,
    HQSplit,
    Split,
    UnknownHQSplit,
    UnknownSplit,
    -- ancestors,

    -- * tests
    isCurrentPath,
    isRoot,

    -- * things that could be replaced with `Convert` instances
    -- absoluteToPath',
    -- fromAbsoluteSplit,
    fromList,
    fromName,
    unknownPathFromText,
    -- fromText,
    -- toAbsoluteSplit,
    toList,
    -- toName,
    -- toName',
    toText,
    -- unsplit,
    -- unsplitHQ,

    -- * things that could be replaced with `Parse` instances
    -- splitFromName,
    -- hqSplitFromName,

    -- * things that could be replaced with `Cons` instances
    cons,
    Unison.Codebase.Path.uncons,

    -- * things that could be replaced with `Snoc` instances
    snoc,
    unsnoc,
  )
where
import Unison.Prelude hiding (empty, toList)

import Control.Lens hiding (Empty, cons, snoc, unsnoc)
import qualified Control.Lens as Lens
import qualified Data.Foldable as Foldable
import Data.List.Extra (dropPrefix)
import Data.Sequence (Seq ((:<|)))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Unison.HashQualified' as HQ'
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment (NameSegment))
import qualified Unison.NameSegment as NameSegment
import Unison.Util.Monoid (intercalateMap)
import qualified Unison.Util.Convert as Convert
import Unison.Util.Convert (Convert(..), Parse)

type UnknownPath = Either (Path 'Absolute) (Path 'Relative)

-- | Unwraps the Either of an UnknownPath to allow direct pattern matching.
matchUnknown :: UnknownPath -> (forall t. Path t -> r) -> r
matchUnknown (Left p) f = f p
matchUnknown (Right p) f = f p

data PathType = Absolute | Relative
-- `Foo.Bar.baz` becomes ["Foo", "Bar", "baz"]
data Path (t :: PathType) where
  AbsolutePath :: Seq NameSegment -> Path 'Absolute
  RelativePath :: Seq NameSegment -> Path 'Relative

deriving instance Eq (Path fixation)
deriving instance Ord (Path fixation)

-- | Any time a function requires a PathSelector, provide either 'AbsolutePath' or 'RelativePath'
type PathSelector t = (Seq NameSegment -> Path t)

-- | This could be an iso, but then we would need to carry around the Convert instances
-- everywhere, and we probably want people to explicitly state which Path type they want
-- anyways.
segments :: Lens' (Path t) (Seq NameSegment)
segments = lens getter setter
  where
    setter :: Path t -> Seq NameSegment -> Path t
    setter (AbsolutePath _) s = AbsolutePath s
    setter (RelativePath _) s = RelativePath s
    getter :: Path t -> Seq NameSegment
    getter (AbsolutePath s) = s
    getter (RelativePath s) = s

instance Semigroup (Path 'Relative) where
  RelativePath p <> RelativePath p' = RelativePath (p <> p')

instance Monoid (Path 'Relative) where
  mempty = RelativePath mempty

isCurrentPath :: Path 'Relative -> Bool
isCurrentPath p = p == currentPath

currentPath :: Path 'Relative
currentPath = mempty

isRoot :: Path 'Absolute -> Bool
isRoot p = p == AbsolutePath mempty

-- Use Cons for this instead.
-- unsplit' :: Split' -> Path'
-- unsplit' (Path' (Left (Absolute p)), seg) = Path' (Left (Absolute (unsplit (p, seg))))
-- unsplit' (Path' (Right (Relative p)), seg) = Path' (Right (Relative (unsplit (p, seg))))

-- unsplit :: Split -> Path
-- unsplit (Path p, a) = Path (p :|> a)

unsplitHQ :: HQSplit t -> HQ'.HashQualified (Path t)
unsplitHQ (p, a) = fmap (Lens.snoc p) a

type Split pathType = (Path pathType, NameSegment)
type HQSplit pathType = (Path pathType, HQ'.HQSegment)

type UnknownSplit = (UnknownPath, NameSegment)
type UnknownHQSplit = (UnknownPath, HQ'.HQSegment)

-- | TODO: Limit second arg to just Relative
--   examples:
--   unprefix .foo.bar .blah == .blah (absolute paths left alone)
--   unprefix .foo.bar id    == id    (relative paths starting w/ nonmatching prefix left alone)
--   unprefix .foo.bar foo.bar.baz == baz (relative paths w/ common prefix get stripped)
unprefix :: Path 'Absolute -> Path t -> Path t
unprefix (AbsolutePath prefix) = \case
  AbsolutePath abs -> AbsolutePath abs
  RelativePath rel ->
    RelativePath . Seq.fromList $ dropPrefix (Foldable.toList prefix) (Foldable.toList rel)

-- TODO: Limit second arg to just Relative
prefix :: Path 'Absolute -> Path t -> Path 'Absolute
prefix (AbsolutePath prefix) = \case
  AbsolutePath abs -> AbsolutePath abs
  RelativePath rel -> AbsolutePath $ prefix <> rel

-- toAbsoluteSplit :: Absolute -> (Path', a) -> (Absolute, a)
-- toAbsoluteSplit a (p, s) = (resolve a p, s)

-- fromAbsoluteSplit :: (Absolute, a) -> (Path, a)
-- fromAbsoluteSplit (Absolute p, a) = (p, a)

-- toPath' :: Path -> Path'
-- toPath' = \case
--   Path (NameSegment "" :<| tail) -> Path' . Left . Absolute . Path $ tail
--   p -> Path' . Right . Relative $ p

-- -- Forget whether the path is absolute or relative
-- fromPath' :: Path' -> Path
-- fromPath' (Path' e) = case e of
--   Left  (Absolute p) -> p
--   Right (Relative p) -> p

toList :: Path t -> [NameSegment]
toList (AbsolutePath p) = Foldable.toList p
toList (RelativePath p) = Foldable.toList p

-- | Prefer @@into @(Path Absolute)@@ or @@into @(Path Relative)@@ where possible.
fromList :: PathSelector t -> [NameSegment] -> Path t
fromList constr = constr . Seq.fromList

-- ancestors :: Absolute -> Seq Absolute
-- ancestors (Absolute (Path segments)) = Absolute . Path <$> Seq.inits segments

-- hqSplitFromName :: Name -> (Either
--                     (Maybe (HQ'.HashQualified (Path 'Absolute, a)))
--                     (Maybe (HQ'.HashQualified (Path 'Relative, a))))
-- hqSplitFromName = (over (both . _Just) HQ'.fromName) . over both Lens.unsnoc . fromName

-- -- | what is this? —AI
-- unprefixName :: Absolute -> Name -> Name
-- unprefixName prefix = toName . unprefix prefix . fromName'

-- prefixName :: Absolute -> Name -> Name
-- prefixName p = toName . prefix p . fromName'

singleton :: NameSegment -> Path 'Relative
singleton n = RelativePath [n]

cons :: NameSegment -> Path t -> Path t
cons = Lens.cons

snoc :: Path t -> NameSegment -> Path t
snoc = Lens.snoc

unsnoc :: Path t -> Maybe (Path t, NameSegment)
unsnoc = Lens.unsnoc

uncons :: Path t -> Maybe (NameSegment, Path t)
uncons = Lens.uncons

-- -- > Path.fromName . Name.unsafeFromText $ ".Foo.bar"
-- -- /Foo/bar
-- -- Int./  -> "Int"/"/"
-- -- pkg/Int.. -> "pkg"/"Int"/"."
-- -- Int./foo -> error because "/foo" is not a valid NameSegment
-- --                      and "Int." is not a valid NameSegment
-- --                      and "Int" / "" / "foo" is not a valid path (internal "")
-- -- todo: fromName needs to be a little more complicated if we want to allow
-- --       identifiers called Function.(.)
fromName :: Name -> UnknownPath
fromName n = case Name.toString n of
  ('.': path) -> Left $ AbsolutePath (Seq.fromList . Name.segments . Name.fromString $ path)
  _ -> Right $ RelativePath (Seq.fromList . Name.segments $ n)

-- toName :: Path -> Name
-- toName = Name.unsafeFromText . toText

-- -- | Convert a Path' to a Name
-- toName' :: Path' -> Name
-- toName' = Name.unsafeFromText . toText'

emptyRelative :: Path 'Relative
emptyRelative = RelativePath mempty

emptyAbsolute :: Path 'Absolute
emptyAbsolute = AbsolutePath mempty

isEmpty :: Path t -> Bool
isEmpty p = null $ view segments p

instance Show (Path t) where
  show = Text.unpack . toText

toText :: Path t -> Text
toText (RelativePath p) = intercalateMap "." NameSegment.toText p
toText (AbsolutePath p) = "." <> intercalateMap "." NameSegment.toText p

-- | TODO: Note, this is unsafe, since you might end up with the wrong type
-- Remove, or use something like 'fromName'
-- fromText :: PathSelector t -> Text -> Maybe (Path t)
-- fromText constr t = constr (fmap NameSegment . Seq.fromList . Name.segments' $ t)

unknownPathFromText :: Text -> UnknownPath
unknownPathFromText n = case (Text.unpack n) of
  ('.': path) -> Left $ AbsolutePath (Seq.fromList . Name.segments . Name.fromString $ path)
  _ -> Right $ RelativePath (Seq.fromList . fmap NameSegment . Name.segments' $ n)

-- toText' :: Path' -> Text
-- toText' = \case
--   Path' (Left (Absolute path)) -> Text.cons '.' (toText path)
--   Path' (Right (Relative path)) -> toText path

instance Lens.AsEmpty (Path 'Relative) where
  _Empty = prism (\() -> RelativePath mempty) matchEmpty
    where
      matchEmpty :: Path 'Relative -> Either (Path 'Relative) ()
      matchEmpty = \case
        RelativePath Lens.Empty -> Right ()
        r -> Left r

instance Lens.AsEmpty (Path 'Absolute) where
  _Empty = prism (\() -> AbsolutePath mempty) matchEmpty
    where
      matchEmpty :: Path 'Absolute -> Either (Path 'Absolute) ()
      matchEmpty = \case
        AbsolutePath Lens.Empty -> Right ()
        r -> Left r

instance Cons (Path t) (Path t) NameSegment NameSegment where
  _Cons = prism (uncurry cons) uncons where
    cons :: NameSegment -> Path t -> Path t
    cons ns = over segments (Lens.cons ns)
    uncons :: Path t -> Either (Path t) (NameSegment, (Path t))
    uncons p = case p of
      AbsolutePath (hd :<| tl) -> Right (hd, AbsolutePath tl)
      RelativePath (hd :<| tl) -> Right (hd, RelativePath tl)
      _ -> Left p

instance Snoc (Path t) (Path t) NameSegment NameSegment where
  _Snoc = prism (uncurry snoc') $ \case
    AbsolutePath (Lens.unsnoc -> Just (s,a)) -> Right (AbsolutePath s, a)
    RelativePath (Lens.unsnoc -> Just (s,a)) -> Right (RelativePath s, a)
    e -> Left e
    where
      snoc' :: Path t -> NameSegment -> Path t
      snoc' p ns = over segments (`Lens.snoc` ns) p

instance Cons UnknownPath UnknownPath NameSegment NameSegment where
  _Cons = prism (uncurry cons) uncons where
    cons :: NameSegment -> UnknownPath -> UnknownPath
    cons ns = over (beside segments segments) (Lens.cons ns)
    uncons :: UnknownPath -> Either (UnknownPath) (NameSegment, (UnknownPath))
    uncons p = matchUnknown p $ \case
      AbsolutePath (hd :<| tl) -> Right (hd, convert $ AbsolutePath tl)
      RelativePath (hd :<| tl) -> Right (hd, convert $ RelativePath tl)
      _ -> Left p

instance Snoc UnknownPath UnknownPath NameSegment NameSegment where
  _Snoc = prism (uncurry snoc') unsnoc
    where
      unsnoc :: (UnknownPath -> Either UnknownPath (UnknownPath, NameSegment))
      unsnoc p = matchUnknown p \case
        AbsolutePath (Lens.unsnoc -> Just (s,a)) -> Right (Convert.into @UnknownPath $ AbsolutePath s, a)
        RelativePath (Lens.unsnoc -> Just (s,a)) -> Right (Convert.into @UnknownPath $ RelativePath s, a)
        e -> Left (Convert.into @UnknownPath e)
      snoc' :: UnknownPath -> NameSegment -> UnknownPath
      snoc' p ns = matchUnknown p (\p -> Convert.into @UnknownPath $ over segments (`Lens.snoc` ns) p)

-- instance Snoc Split' Split' NameSegment NameSegment where
--   _Snoc = prism (uncurry snoc') $ \case -- unsnoc
--     (Lens.unsnoc -> Just (s, a), ns) -> Right ((s, a), ns)
--     e -> Left e
--     where
--     snoc' :: Split' -> NameSegment -> Split'
--     snoc' (p, a) n = (Lens.snoc p a, n)

-- class Resolve l r o where
--   resolve :: l -> r -> o

-- instance Resolve Path Path Path where
--   resolve (Path l) (Path r) = Path (l <> r)

-- instance Resolve Relative Relative Relative where
--   resolve (Relative (Path l)) (Relative (Path r)) = Relative (Path (l <> r))

-- instance Resolve Absolute Relative Absolute where
--   resolve (Absolute l) (Relative r) = Absolute (resolve l r)

-- instance Resolve Path' Path' Path' where
--   resolve _ a@(Path' Left{}) = a
--   resolve (Path' (Left a)) (Path' (Right r)) = Path' (Left (resolve a r))
--   resolve (Path' (Right r1)) (Path' (Right r2)) = Path' (Right (resolve r1 r2))

-- instance Resolve Path' Split' Path' where
--   resolve l r = resolve l (unsplit' r)

-- instance Resolve Path' Split' Split' where
--   resolve l (r, ns) = (resolve l r, ns)

-- instance Resolve Absolute HQSplit HQSplitAbsolute where
--   resolve l (r, hq) = (resolve l (Relative r), hq)

-- instance Resolve Absolute Path' Absolute where
--   resolve _ (Path' (Left a)) = a
--   resolve a (Path' (Right r)) = resolve a r

instance Convert [NameSegment] (Path 'Absolute) where convert = fromList AbsolutePath
instance Convert [NameSegment] (Path 'Relative) where convert = fromList RelativePath
instance Convert (Path 'Absolute) [NameSegment] where convert = toList
instance Convert (Path 'Relative) [NameSegment] where convert = toList
instance Convert (Path t) UnknownPath where
  convert p@AbsolutePath{} = Left p
  convert p@RelativePath{} = Right p

instance Convert Text UnknownPath where convert = unknownPathFromText
instance Parse UnknownPath (Path 'Absolute) where
  parse = either Just (const Nothing)
instance Parse UnknownPath (Path 'Relative) where
  parse = either (const Nothing) Just
instance Parse Text (Path 'Relative) where parse txt = Convert.parseFrom @UnknownPath (unknownPathFromText txt)
instance Parse Text (Path 'Absolute) where parse txt = Convert.parseFrom @UnknownPath (unknownPathFromText txt)

-- instance Convert (Path t) [NameSegment] where convert = toList
-- instance Convert (HQSplit t) (HQ'.HashQualified (Path t)) where convert = unsplitHQ'
-- instance Convert (Path t) Name where convert = toName
-- instance Parse Name (HQSplit t) where parse = hqSplitFromName'
-- instance Parse Name (Split t) where parse = splitFromName
