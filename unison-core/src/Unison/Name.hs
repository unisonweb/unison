module Unison.Name
  ( Name,
    Convert (..),
    Parse (..),
    cons,
    endsWithSegments,
    fromString,
    isPrefixOf,
    joinDot,
    makeAbsolute,
    isAbsolute,
    parent,
    module Unison.Util.Alphabetical,
    sortNames,
    sortNamed,
    sortByText,
    stripNamePrefix,
    segments,
    reverseSegments,
    countSegments,
    compareSuffix,
    segments',
    suffixes,
    searchBySuffix,
    suffixFrom,
    shortestUniqueSuffix,
    toString,
    toText,
    toVar,
    unqualified,
    unqualified',
    unsafeFromText,
    unsafeFromString,
    relativeFromSegment,
    relativeFromSegments,
    fromVar,
    splits,

    -- * Old name API (temporary), exported only for testing
    OldName,
    oldCompareSuffix,
    oldCountSegments,
    oldEndsWithSegments,
    oldFromSegment,
    oldFromString,
    oldIsAbsolute,
    oldIsPrefixOf,
    oldJoinDot,
    oldMakeAbsolute,
    oldParent,
    oldReverseSegments,
    oldSearchBySuffix,
    oldSegments,
    oldShortestUniqueSuffix,
    oldSortNames,
    oldStripNamePrefix,
    oldSuffixFrom,
    oldSuffixes,
    oldToString,
    oldToVar,
    oldUnqualified,
    oldUnsafeFromString,
    oldUnsafeFromText,
  )
where

import Control.Lens (mapped, over, unsnoc, _1, _2)
import qualified Control.Lens as Lens
import Data.List (find, inits, sortBy, tails)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as List (NonEmpty)
import qualified Data.List.NonEmpty as List.NonEmpty
import qualified Data.RFC5051 as RFC5051
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import qualified Unison.Hashable as H
import Unison.NameSegment
  ( NameSegment (NameSegment),
    segments',
  )
import qualified Unison.NameSegment as NameSegment
import Unison.Prelude
import Unison.Util.Alphabetical (Alphabetical, compareAlphabetical)
import qualified Unison.Util.Relation as R
import Unison.Var (Var)
import qualified Unison.Var as Var

data Name
  = Name Position (List.NonEmpty NameSegment)
  deriving stock (Eq)

instance Alphabetical Name where
  compareAlphabetical n1 n2 =
    compareAlphabetical (toText n1) (toText n2)

instance H.Hashable Name where
  tokens s = [H.Text (toText s)]

instance IsString Name where
  fromString =
    unsafeFromString

instance Ord Name where
  compare (Name p0 ss0) (Name p1 ss1) =
    compare ss0 ss1 <> compare p0 p1

instance Show Name where
  show =
    Text.unpack . toText

data Position
  = Absolute
  | Relative
  deriving stock (Eq, Ord, Show)

posToTextBuilder :: Position -> Text.Builder
posToTextBuilder = \case
  Absolute -> "."
  Relative -> ""

-- | Cons a name segment onto the head of a relative name. Monotonic with respect to ordering: it is safe to use
-- @cons s@ as the first argument to @Map.mapKeysMonotonic@.
--
-- /Precondition/: the name is relative
--
-- /O(n)/, where /n/ is the number of segments.
cons :: HasCallStack => NameSegment -> Name -> Name
cons x = \case
  Name Absolute _ -> error "cannot cons onto an absolute name"
  Name Relative (y :| ys) -> Name Relative (y :| ys ++ [x])

type OldName = Text

oldFromString :: String -> OldName
oldFromString =
  oldUnsafeFromText . Text.pack

toText :: Name -> Text
toText (Name pos (x0 :| xs)) =
  build (posToTextBuilder pos <> foldr step mempty xs <> NameSegment.toTextBuilder x0)
  where
    step :: NameSegment -> Text.Builder -> Text.Builder
    step x acc =
      acc <> NameSegment.toTextBuilder x <> "."

    build :: Text.Builder -> Text
    build =
      Text.Lazy.toStrict . Text.Builder.toLazyText

oldSortNames :: [OldName] -> [OldName]
oldSortNames = oldSortNamed id

sortNames :: [Name] -> [Name]
sortNames =
  sortNamed id

oldSortNamed :: (a -> OldName) -> [a] -> [a]
oldSortNamed = sortByText

sortNamed :: (a -> Name) -> [a] -> [a]
sortNamed f =
  sortByText (toText . f)

sortByText :: (a -> Text) -> [a] -> [a]
sortByText by as =
  let as' = [(a, by a) | a <- as]
      comp (_, s) (_, s2) = RFC5051.compareUnicode s s2
   in fst <$> sortBy comp as'

oldUnsafeFromText :: Text -> OldName
oldUnsafeFromText t =
  if Text.any (== '#') t then error $ "not a name: " <> show t else t

unsafeFromText :: Text -> Name
unsafeFromText = \case
  "." -> error "empty absolute name"
  ".." -> Name Absolute ("." :| [])
  t | Text.any (== '#') t -> error ("not a name: " <> show t)
  t ->
    case Text.split (== '.') t of
      [] -> error "empty name"
      "" : s : ss -> Name Absolute (List.NonEmpty.reverse (fmap NameSegment (s :| ss)))
      s : ss -> Name Relative (List.NonEmpty.reverse (fmap NameSegment (s :| ss)))

oldUnsafeFromString :: String -> OldName
oldUnsafeFromString = oldUnsafeFromText . Text.pack

unsafeFromString :: String -> Name
unsafeFromString =
  unsafeFromText . Text.pack

oldToVar :: Var v => OldName -> v
oldToVar = Var.named

toVar :: Var v => Name -> v
toVar =
  Var.named . toText

fromVar :: Var v => v -> Name
fromVar =
  unsafeFromText . Var.name

oldToString :: OldName -> String
oldToString = Text.unpack

toString :: Name -> String
toString =
  Text.unpack . toText

oldIsPrefixOf :: OldName -> OldName -> Bool
oldIsPrefixOf = Text.isPrefixOf

isPrefixOf :: Name -> Name -> Bool
isPrefixOf (Name p0 ss0) (Name p1 ss1) =
  p0 == p1 && List.isPrefixOf (reverse (toList ss0)) (reverse (toList ss1))

oldEndsWithSegments :: OldName -> OldName -> Bool
oldEndsWithSegments n ending = any (== ending) (oldSuffixes n)

-- foo.bar.baz `endsWithSegments` bar.baz == True
-- foo.bar.baz `endsWithSegments` baz == True
-- foo.bar.baz `endsWithSegments` az == False (not a full segment)
-- foo.bar.baz `endsWithSegments` zonk == False (doesn't match any segment)
-- foo.bar.baz `endsWithSegments` foo == False (matches a segment, but not at the end)
endsWithSegments :: Name -> Name -> Bool
endsWithSegments (Name _ ss0) (Name _ ss1) =
  List.NonEmpty.isPrefixOf (toList ss1) ss0

oldStripNamePrefix :: OldName -> OldName -> Maybe OldName
oldStripNamePrefix prefix name =
  Text.stripPrefix (prefix <> mid) name
  where
    mid = if prefix == "." then "" else "."

-- stripNamePrefix a.b  a.b.c = Just c
-- stripNamePrefix x.y  a.b.c = Nothing, x.y isn't a prefix of a.b.c
-- stripNamePrefix .a.b .a    = Just b
-- stripNamePrefix a.b  a.b   = Nothing, "" isn't a name
stripNamePrefix :: Name -> Name -> Maybe Name
stripNamePrefix (Name p0 ss0) (Name p1 ss1) = do
  guard (p0 == p1)
  s : ss <- List.stripPrefix (reverse (toList ss0)) (reverse (toList ss1))
  pure (Name Relative (List.NonEmpty.reverse (s :| ss)))

oldSuffixFrom :: OldName -> OldName -> Maybe OldName
oldSuffixFrom mid overall = case Text.breakOnAll mid overall of
  [] -> Nothing
  (_, rem) : _ -> Just rem

-- suffixFrom Int builtin.Int.+ ==> Int.+
-- suffixFrom Int Int.negate    ==> Int.negate
--
-- Currently used as an implementation detail of expanding wildcard
-- imports, (like `use Int` should catch `builtin.Int.+`)
-- but it may be generally useful elsewhere. See `expandWildcardImports`
-- for details.
suffixFrom :: Name -> Name -> Maybe Name
suffixFrom (Name p0 ss0) (Name _ ss1) = do
  -- it doesn't make sense to pass an absolute name as the first arg
  Relative <- Just p0
  s : ss <- align (toList ss0) (toList ss1)
  -- the returned name is always relative... right?
  pure (Name Relative (s :| ss))
  where
    -- Slide the first non-empty list along the second; if there's a match, return the prefix of the second that ends on
    -- that match.
    --
    -- align [a,b] [x,a,b,y] = Just [x,a,b]
    align :: forall a. Eq a => [a] -> [a] -> Maybe [a]
    align xs =
      go id
      where
        go :: ([a] -> [a]) -> [a] -> Maybe [a]
        go prepend = \case
          [] -> Nothing
          ys0@(y : ys) ->
            if List.isPrefixOf xs ys0
              then Just (prepend xs)
              else go (prepend . (y :)) ys

oldJoinDot :: OldName -> OldName -> OldName
oldJoinDot prefix suffix =
  if prefix == "."
    then prefix <> suffix
    else prefix <> "." <> suffix

joinDot :: HasCallStack => Name -> Name -> Name
joinDot n1@(Name p0 ss0) n2@(Name p1 ss1) =
  case p1 of
    Relative -> Name p0 (ss1 <> ss0)
    Absolute -> error ("joinDot " ++ show n1 ++ " " ++ show n2)

oldUnqualified :: OldName -> OldName
oldUnqualified = oldUnsafeFromText . unqualified'

unqualified :: Name -> Name
unqualified (Name _ (s :| _)) =
  Name Relative (s :| [])

oldParent :: OldName -> Maybe OldName
oldParent n = case unsnoc (NameSegment.toText <$> oldSegments n) of
  Nothing -> Nothing
  Just ([], _) -> Nothing
  Just (init, _) -> Just $ Text.intercalate "." init

-- parent . -> Nothing
-- parent + -> Nothing
-- parent foo -> Nothing
-- parent foo.bar -> foo
-- parent foo.bar.+ -> foo.bar
parent :: Name -> Maybe Name
parent = \case
  Name _ (_ :| []) -> Nothing
  Name _ (_ :| (s : ss)) -> Just (Name Relative (s :| ss))

oldSuffixes :: OldName -> [OldName]
oldSuffixes "" = []
oldSuffixes n = fmap up . filter (not . null) . tails $ segments' n
  where
    up = Text.intercalate "."

-- suffixes foo.bar.baz  -> [foo.bar.baz, bar.baz, baz]
-- suffixes .foo.bar.baz -> [foo.bar.baz, bar.baz, baz]
-- FIXME check if this is used
suffixes :: Name -> [Name]
suffixes (Name _ ss0) = do
  ss <- List.NonEmpty.tail (List.NonEmpty.inits ss0)
  -- fromList is safe here because all elements of `tail . inits` are non-empty
  pure (Name Relative (List.NonEmpty.fromList ss))

-- suffixes' foo.bar.baz  -> [baz, bar.baz, foo.bar.baz]
-- suffixes' .foo.bar.baz -> [baz, bar.baz, foo.bar.baz]
suffixes' :: Name -> [Name]
suffixes' =
  reverse . suffixes'

-- | Return all "splits" of a relative name, which pair a possibly-empty prefix of name segments with a suffix, such
-- that the original name is equivalent to @prefix + suffix@.
--
-- Note: always returns a non-empty list, but (currently) does not use @NonEmpty@ for convenience, as none of the 
-- call-sites care if the list is empty or not.
--
-- @
-- > splits foo.bar.baz
--
--   prefix    suffix
--   ------    ------
--   ∅         foo.bar.baz
--   foo       bar.baz
--   foo.bar   baz
-- @
--
-- /Precondition/: the name is relative.
splits :: HasCallStack => Name -> [([NameSegment], Name)]
splits (Name p ss0) =
  ss0
    & List.NonEmpty.toList
    & reverse
    & splits0
    & over (mapped . _2) (Name p . List.NonEmpty.reverse)
  where
    -- splits a.b.c
    -- ([], a.b.c) : over (mapped . _1) (a.) (splits b.c)
    -- ([], a.b.c) : over (mapped . _1) (a.) (([], b.c) : over (mapped . _1) (b.) (splits c))
    -- [([], a.b.c), ([a], b.c), ([a.b], c)]
    splits0 :: HasCallStack => [a] -> [([a], NonEmpty a)]
    splits0 = \case
      [] -> []
      [x] -> [([], x :| [])]
      x : xs -> ([], x :| xs) : over (mapped . _1) (x :) (splits0 xs)

unqualified' :: Text -> Text
unqualified' = fromMaybe "" . lastMay . segments'

oldMakeAbsolute :: OldName -> OldName
oldMakeAbsolute n
  | n == "." = ".."
  | Text.isPrefixOf "." n = n
  | otherwise = "." <> n

makeAbsolute :: Name -> Name
makeAbsolute (Name _ ss) =
  Name Absolute ss

relativeFromSegment :: NameSegment -> Name
relativeFromSegment s =
  Name Relative (s :| [])

relativeFromSegments :: NonEmpty NameSegment -> Name
relativeFromSegments =
  Name Relative

oldFromSegment :: NameSegment -> OldName
oldFromSegment = oldUnsafeFromText . NameSegment.toText

oldSegments :: OldName -> [NameSegment]
oldSegments n = NameSegment <$> segments' n

segments :: Name -> NonEmpty NameSegment
segments (Name _ ss) =
  List.NonEmpty.reverse ss

oldReverseSegments :: OldName -> [NameSegment]
oldReverseSegments n = NameSegment <$> NameSegment.reverseSegments' n

reverseSegments :: Name -> NonEmpty NameSegment
reverseSegments (Name _ ss) =
  ss

oldCountSegments :: OldName -> Int
oldCountSegments n = length (oldSegments n)

countSegments :: Name -> Int
countSegments (Name _ ss) =
  length ss

oldIsAbsolute :: OldName -> Bool
oldIsAbsolute = Text.isPrefixOf "."

isAbsolute :: Name -> Bool
isAbsolute = \case
  Name Absolute _ -> True
  Name Relative _ -> False

oldSearchBySuffix :: (Ord r) => OldName -> R.Relation OldName r -> Set r
oldSearchBySuffix suffix rel =
  R.lookupDom suffix rel `orElse` R.searchDom (oldCompareSuffix suffix) rel
  where
    orElse s1 s2 = if Set.null s1 then s2 else s1

-- If there's no exact matches for `suffix` in `rel`, find all
-- `r` in `rel` whose corresponding name `suffix` as a suffix.
-- For example, `searchBySuffix List.map {(base.List.map, r1)}`
-- will return `{r1}`.
--
-- NB: Implementation uses logarithmic time lookups, not a linear scan.
searchBySuffix :: (Ord r) => Name -> R.Relation Name r -> Set r
searchBySuffix suffix rel =
  R.lookupDom suffix rel `orElse` R.searchDom (compareSuffix suffix) rel
  where
    orElse s1 s2 = if Set.null s1 then s2 else s1

oldCompareSuffix :: OldName -> OldName -> Ordering
oldCompareSuffix suffix =
  let suffixSegs = oldReverseSegments suffix
      len = length suffixSegs
   in \n -> take len (oldReverseSegments n) `compare` suffixSegs

-- `compareSuffix suffix n` is equal to `compare n' suffix`, where
-- n' is `n` with only the last `countSegments suffix` segments.
--
-- Used for suffix-based lookup of a name. For instance, given a `r : Relation Name x`,
-- `Relation.searchDom (compareSuffix "foo.bar") r` will find all `r` whose name
-- has `foo.bar` as a suffix.
compareSuffix :: Name -> Name -> Ordering
compareSuffix (Name _ ss0) (Name _ ss1) =
  go (toList ss0) (toList ss1)
  where
    go [] _ = EQ -- only compare up to entire suffix (first arg)
    go _ [] = LT
    go (x : xs) (y : ys) = compare y x <> go xs ys

oldShortestUniqueSuffix :: Ord r => OldName -> r -> R.Relation OldName r -> OldName
oldShortestUniqueSuffix fqn r rel =
  maybe fqn (oldUnsafeFromText . Text.intercalate "." . map NameSegment.toText . reverse) (find isOk suffixes)
  where
    allowed = R.lookupDom fqn rel
    suffixes = drop 1 (inits (oldReverseSegments fqn))
    isOk suffix = (Set.size rs == 1 && Set.findMin rs == r) || rs == allowed
      where
        rs = R.searchDom compareEnd rel
        compareEnd n = compare (take len (oldReverseSegments n)) suffix
        len = length suffix

-- Tries to shorten `fqn` to the smallest suffix that still refers
-- to to `r`. Uses an efficient logarithmic lookup in the provided relation.
-- The returned `Name` may refer to multiple hashes if the original FQN
-- did as well.
--
-- NB: Only works if the `Ord` instance for `Name` orders based on
-- `Name.reverseSegments`.
shortestUniqueSuffix :: forall r. Ord r => Name -> r -> R.Relation Name r -> Name
shortestUniqueSuffix fqn r rel =
  fromMaybe fqn (find isOk (suffixes' fqn))
  where
    allowed :: Set r
    allowed =
      R.lookupDom fqn rel
    isOk :: Name -> Bool
    isOk suffix =
      (Set.size rs == 1 && Set.findMin rs == r) || rs == allowed
      where
        rs :: Set r
        rs =
          R.searchDom (compareSuffix suffix) rel

class Convert a b where
  convert :: a -> b

class Parse a b where
  parse :: a -> Maybe b

instance Parse Text NameSegment where
  parse txt = case NameSegment.segments' txt of
    [n] -> Just (NameSegment.NameSegment n)
    _ -> Nothing

instance (Parse a a2, Parse b b2) => Parse (a, b) (a2, b2) where
  parse (a, b) = (,) <$> parse a <*> parse b

instance Lens.Snoc Name Name NameSegment NameSegment where
  _Snoc =
    Lens.prism snoc unsnoc
    where
      snoc :: (Name, NameSegment) -> Name
      snoc (Name p (x :| xs), y) =
        Name p (y :| x : xs)
      unsnoc :: Name -> Either Name (Name, NameSegment)
      unsnoc name =
        case name of
          Name _ (_ :| []) -> Left name
          Name p (x :| y : ys) -> Right (Name p (y :| ys), x)
