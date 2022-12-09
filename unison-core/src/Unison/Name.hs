module Unison.Name
  ( Name,
    Convert (..),
    Parse (..),

    -- * Basic construction
    cons,
    joinDot,
    fromSegment,
    fromSegments,
    fromReverseSegments,

    -- * Basic queries
    countSegments,
    isAbsolute,
    isPrefixOf,
    beginsWithSegment,
    endsWithReverseSegments,
    endsWithSegments,
    stripReversedPrefix,
    reverseSegments,
    segments,
    suffixes,

    -- * Basic manipulation
    makeAbsolute,
    makeRelative,
    setPosition,
    parent,
    stripNamePrefix,
    unqualified,

    -- * To organize later
    sortNames,
    sortNamed,
    sortByText,
    searchBySuffix,
    searchByRankedSuffix,
    suffixFrom,
    shortestUniqueSuffix,
    commonPrefix,
    splits,

    -- * Re-exports
    module Unison.Util.Alphabetical,

    -- * Exported for testing
    compareSuffix,
  )
where

import Control.Lens (mapped, over, _1, _2)
import qualified Data.List as List
import qualified Data.List.Extra as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as List.NonEmpty
import qualified Data.Map as Map
import qualified Data.RFC5051 as RFC5051
import qualified Data.Set as Set
import Unison.Name.Internal (Name (..))
import Unison.NameSegment (NameSegment (NameSegment))
import qualified Unison.NameSegment as NameSegment
import Unison.Position (Position (..))
import Unison.Prelude
import Unison.Util.Alphabetical (Alphabetical, compareAlphabetical)
import qualified Unison.Util.List as List
import qualified Unison.Util.Relation as R

-- | @compareSuffix x y@ compares the suffix of @y@ (in reverse segment order) that is as long as @x@ to @x@ (in reverse
-- segment order).
--
-- >>> compareSuffix "b.c" "a.b.c"
-- EQ -- because [c,b] == [c,b]
--
-- >>> compareSuffix "b.c" "a.b.b"
-- LT -- because [b,b] < [c,b]
--
-- >>> compareSuffix "a.b.c" "b.c"
-- LT -- because [c,b] < [c,b,a]
--
-- >>> compareSuffix "b.b" "a.b.c"
-- GT -- because [c,b] > [b,b]
--
-- Used for suffix-based lookup of a name. For instance, given a @r : Relation Name x@,
-- @Relation.searchDom (compareSuffix "foo.bar") r@ will find all @r@ whose name has @foo.bar@ as a suffix.
--
-- This is only exported for testing; use 'searchBySuffix' or 'shortestUniqueSuffix' instead.
--
-- /O(n)/, where /n/ is the number of name segments.
compareSuffix :: Name -> Name -> Ordering
compareSuffix (Name _ ss0) =
  foldr f (const EQ) ss0 . List.NonEmpty.toList . reverseSegments
  where
    f :: NameSegment -> ([NameSegment] -> Ordering) -> ([NameSegment] -> Ordering)
    f x acc = \case
      [] -> LT
      y : ys -> compare y x <> acc ys

-- | Cons a name segment onto the head of a relative name. Not monotonic with respect to ordering! It is not safe to use
-- @cons s@ as the first argument to @Map.mapKeysMonotonic@!
--
-- /Precondition/: the name is relative
--
-- /O(n)/, where /n/ is the number of segments.
cons :: HasCallStack => NameSegment -> Name -> Name
cons x name =
  case name of
    Name Absolute _ ->
      error $
        reportBug
          "E495986"
          ("cannot cons " ++ show x ++ " onto absolute name" ++ show name)
    Name Relative (y :| ys) -> Name Relative (y :| ys ++ [x])

-- | Return the number of name segments in a name.
--
-- /O(n)/, where /n/ is the number of name segments.
countSegments :: Name -> Int
countSegments (Name _ ss) =
  length ss

-- | @beginsWithSegment name segment@ returns whether @name@'s first name segment is @segment@.
--
-- >>> beginsWithSegment "abc.def" "abc"
-- True
--
-- >>> beginsWithSegment "abc.def" "ab"
-- False
--
-- /O(n)/, where /n/ is the number of name segments.
beginsWithSegment :: Name -> NameSegment -> Bool
beginsWithSegment name segment =
  segment == List.NonEmpty.head (segments name)

-- | @endsWithSegments x y@ returns whether @x@ ends with @y@.
--
-- >>> endsWithSegments "a.b.c" ["b", "c"]
-- True
--
-- >>> endsWithSegments "a.b.c" ["d"]
-- False
--
-- >>> endsWithSegments "a.b.c" []
-- True
--
-- /O(n)/, where /n/ is the number of name segments.
endsWithSegments :: Name -> [NameSegment] -> Bool
endsWithSegments name ss =
  endsWithReverseSegments name (reverse ss)

-- | Like 'endsWithSegments', but accepts a list of name segments in reverse order.
--
-- Slightly more efficient than 'endsWithSegments'.
--
-- >>> endsWithReverseSegments "a.b.c" ["c", "b"]
-- True
endsWithReverseSegments :: Name -> [NameSegment] -> Bool
endsWithReverseSegments (Name _ ss0) ss1 =
  List.NonEmpty.isPrefixOf ss1 ss0

-- >>> stripReversedPrefix "a.b.c" ["b", "a"]
-- Just c
-- >>> stripReversedPrefix "x.y" ["b", "a"]
-- Nothing
-- >>> stripReversedPrefix "a.b" ["b", "a"]
-- Nothing
stripReversedPrefix :: Name -> [NameSegment] -> Maybe Name
stripReversedPrefix (Name p segs) suffix = do
  stripped <- List.stripSuffix suffix (toList segs)
  nonEmptyStripped <- List.NonEmpty.nonEmpty stripped
  pure $ Name p nonEmptyStripped

-- | Is this name absolute?
--
-- /O(1)/.
isAbsolute :: Name -> Bool
isAbsolute = \case
  Name Absolute _ -> True
  Name Relative _ -> False

-- | @isPrefixOf x y@ returns whether @x@ is a prefix of (or equivalent to) @y@, which is false if one name is relative
-- and the other is absolute.
--
-- >>> isPrefixOf "a.b" "a.b.c"
-- True
--
-- >>> isPrefixOf "a.b.c" "a.b.c"
-- True
--
-- >>> isPrefixOf ".a.b" "a.b.c"
-- False
--
-- /O(n)/, where /n/ is the number of name segments.
isPrefixOf :: Name -> Name -> Bool
isPrefixOf (Name p0 ss0) (Name p1 ss1) =
  p0 == p1 && List.isPrefixOf (reverse (toList ss0)) (reverse (toList ss1))

joinDot :: HasCallStack => Name -> Name -> Name
joinDot n1@(Name p0 ss0) n2@(Name p1 ss1) =
  case p1 of
    Relative -> Name p0 (ss1 <> ss0)
    Absolute ->
      error $
        reportBug
          "E261635"
          ( "joinDot: second name cannot be absolute. (name 1 = "
              ++ show n1
              ++ ", name 2 = "
              ++ show n2
              ++ ")"
          )

-- | Make a name absolute. No-op if the name is already absolute.
--
-- /O(1)/.
makeAbsolute :: Name -> Name
makeAbsolute = setPosition Absolute

-- | Make a name relative. No-op if the name is already relative.
--
-- /O(1)/.
makeRelative :: Name -> Name
makeRelative = setPosition Relative

-- | Overwrite a name's position.
-- This only changes the name's tag, it performs no manipulations to
-- the segments of the name.
--
-- /O(1)/.
setPosition :: Position -> Name -> Name
setPosition pos (Name _ ss) =
  Name pos ss

-- | Compute the "parent" of a name, unless the name is only a single segment, in which case it has no parent.
--
-- >>> parent "a.b.c"
-- Just "a.b"
--
-- >>> parent ".a.b.c"
-- Just ".a.b"
--
-- >>> parent "a"
-- Nothing
parent :: Name -> Maybe Name
parent (Name p ss0) =
  Name p <$> List.NonEmpty.nonEmpty (List.NonEmpty.tail ss0)

-- | Construct a relative name from a name segment.
--
-- /O(1)/.
fromSegment :: NameSegment -> Name
fromSegment s =
  Name Relative (s :| [])

-- | Construct a relative name from a list of name segments.
--
-- >>> fromSegments ("a" :| ["b", "c"])
-- "a.b.c"
--
-- /O(n)/, where /n/ is the number of name segments.
fromSegments :: NonEmpty NameSegment -> Name
fromSegments ss =
  Name Relative (List.NonEmpty.reverse ss)

-- | Construct a relative name from a list of name segments which are in reverse order
--
-- >>> fromReverseSegments ("c" :| ["b", "a"])
-- a.b.c
--
-- /O(1)/
fromReverseSegments :: NonEmpty NameSegment -> Name
fromReverseSegments rs =
  Name Relative rs

-- | Return the name segments of a name, in reverse order.
--
-- >>> reverseSegments "a.b.c"
-- "c" :| ["b", "a"]
--
-- /O(1)/.
reverseSegments :: Name -> NonEmpty NameSegment
reverseSegments (Name _ ss) =
  ss

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

-- Like `searchBySuffix`, but prefers names that have fewer
-- segments equal to "lib". This is used to prefer "local"
-- names rather than names coming from libraries, which
-- are traditionally placed under a "lib" subnamespace.
--
-- Example: foo.bar shadows lib.foo.bar
-- Example: lib.foo.bar shadows lib.blah.lib.foo.bar
searchByRankedSuffix :: (Ord r) => Name -> R.Relation Name r -> Set r
searchByRankedSuffix suffix rel = case searchBySuffix suffix rel of
  rs | Set.size rs <= 1 -> rs
  rs -> case Map.lookup 0 byDepth <|> Map.lookup 1 byDepth of
    -- anything with more than one lib in it is treated the same
    Nothing -> rs
    Just rs -> Set.fromList rs
    where
      byDepth =
        List.multimap
          [ (minLibs ns, r)
            | r <- toList rs,
              ns <- [filter ok (toList (R.lookupRan r rel))]
          ]
      lib = NameSegment "lib"
      libCount = length . filter (== lib) . toList . reverseSegments
      minLibs [] = 0
      minLibs ns = minimum (map libCount ns)
      ok name = compareSuffix suffix name == EQ

-- | Return the name segments of a name.
--
-- >>> segments "a.b.c"
-- "a" :| ["b", "c"]
--
-- /O(n)/, where /n/ is the number of name segments.
segments :: Name -> NonEmpty NameSegment
segments (Name _ ss) =
  List.NonEmpty.reverse ss

sortByText :: (a -> Text) -> [a] -> [a]
sortByText by as =
  let as' = [(a, by a) | a <- as]
      comp (_, s) (_, s2) = RFC5051.compareUnicode s s2
   in fst <$> List.sortBy comp as'

sortNamed :: (Name -> Text) -> (a -> Name) -> [a] -> [a]
sortNamed toText f =
  sortByText (toText . f)

sortNames :: (Name -> Text) -> [Name] -> [Name]
sortNames toText =
  sortNamed toText id

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

-- | @stripNamePrefix x y@ strips prefix @x@ from name @y@, and returns the resulting name. Returns @Nothing@ @x@ is not
-- a proper (meaning shorter-than) prefix of @y@.
--
-- >>> stripNamePrefix "a.b" "a.b.c"
-- Just "c"
--
-- >>> stripNamePrefix ".a.b" "a.b.c"
-- Nothing
--
-- >>> stripNamePrefix "a.b.c" "a.b.c"
-- Nothing
stripNamePrefix :: Name -> Name -> Maybe Name
stripNamePrefix (Name p0 ss0) (Name p1 ss1) = do
  guard (p0 == p1)
  s : ss <- List.stripPrefix (reverse (toList ss0)) (reverse (toList ss1))
  pure (Name Relative (List.NonEmpty.reverse (s :| ss)))

-- | Return all relative suffixes of a name, in descending-length order. The returned list will always be non-empty.
--
-- >>> suffixes "a.b.c"
-- ["a.b.c", "a.b", "c"]
--
-- >>> suffixes ".a.b.c"
-- ["a.b.c", "a.b", "c"]
suffixes :: Name -> [Name]
suffixes =
  reverse . suffixes'

-- Like `suffixes`, but returns names in ascending-length order. Currently unexported, as it's only used in the
-- implementation of `shortestUniqueSuffix`.
suffixes' :: Name -> [Name]
suffixes' (Name _ ss0) = do
  ss <- List.NonEmpty.tail (List.NonEmpty.inits ss0)
  -- fromList is safe here because all elements of `tail . inits` are non-empty
  pure (Name Relative (List.NonEmpty.fromList ss))

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

-- | Drop all leading segments from a name, retaining only the last segment as a relative name.
--
-- >>> unqualified "a.b.c"
-- "c"
--
-- >>> unqualified ".a.b.c"
-- "c"
unqualified :: Name -> Name
unqualified (Name _ (s :| _)) =
  Name Relative (s :| [])

-- Tries to shorten `fqn` to the smallest suffix that still refers
-- to to `r`. Uses an efficient logarithmic lookup in the provided relation.
-- The returned `Name` may refer to multiple hashes if the original FQN
-- did as well.
--
-- NB: Only works if the `Ord` instance for `Name` orders based on
-- `Name.reverseSegments`.
shortestUniqueSuffix :: forall r. Ord r => Name -> r -> R.Relation Name r -> Name
shortestUniqueSuffix fqn r rel =
  fromMaybe fqn (List.find isOk (suffixes' fqn))
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

-- | Returns the common prefix of two names as segments
--
-- Note: the returned segments are NOT reversed.
--
-- >>> commonPrefix "a.b.x" "a.b.y"
-- [a,b]
--
-- >>> commonPrefix "x.y.z" "a.b.c"
-- []
--
-- >>> commonPrefix "a.b.c" "a.b.c.d.e"
-- [a,b,c]
--
-- Must have equivalent positions or no there's no common prefix
-- >>> commonPrefix ".a.b.c" "a.b.c.d.e"
-- []
--
-- Prefix matches are performed at the *segment* level:
-- >>> commonPrefix "a.bears" "a.beats"
-- [a]
commonPrefix :: Name -> Name -> [NameSegment]
commonPrefix x@(Name p1 _) y@(Name p2 _)
  | p1 /= p2 = []
  | otherwise =
      commonPrefix' (toList $ segments x) (toList $ segments y)
  where
    commonPrefix' (a : as) (b : bs)
      | a == b = a : commonPrefix' as bs
    commonPrefix' _ _ = []

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
