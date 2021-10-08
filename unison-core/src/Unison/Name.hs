{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings   #-}

module Unison.Name
  ( Name(Name)
  , Convert(..)
  , Parse(..)
  , endsWithSegments
  , fromString
  , isPrefixOf
  , joinDot
  , makeAbsolute
  , isAbsolute
  , parent
  , module Unison.Util.Alphabetical
  , sortNames
  , sortNamed
  , sortNameds
  , sortByText
  , sortNamed'
  , stripNamePrefix
  , stripPrefixes
  , segments
  , reverseSegments
  , countSegments
  , compareSuffix
  , segments'
  , suffixes
  , searchBySuffix
  , suffixFrom
  , shortestUniqueSuffix
  , toString
  , toText
  , toVar
  , unqualified
  , unqualified'
  , unsafeFromText
  , unsafeFromString
  , fromSegment
  , fromVar
  )
where

import           Unison.Prelude
import qualified Unison.NameSegment            as NameSegment
import           Unison.NameSegment             ( NameSegment(NameSegment)
                                                , segments'
                                                )

import           Control.Lens                   ( unsnoc )
import qualified Control.Lens                  as Lens
import qualified Data.Text                     as Text
import qualified Data.Set                      as Set
import qualified Unison.Hashable               as H
import           Unison.Util.Alphabetical      (Alphabetical,compareAlphabetical)
import qualified Unison.Util.Relation          as R
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var
import qualified Data.RFC5051                  as RFC5051
import           Data.List                      ( sortBy, tails, inits, find )

newtype Name = Name { toText :: Text }
  deriving (Eq, Semigroup, Generic)

sortNames :: [Name] -> [Name]
sortNames = sortNamed id

sortNamed :: (a -> Name) -> [a] -> [a]
sortNamed by = sortByText (toText . by)

sortNameds :: (a -> [Name]) -> [a] -> [a]
sortNameds by = sortByText (Text.intercalate "." . map toText . by)

sortByText :: (a -> Text) -> [a] -> [a]
sortByText by as = let
  as' = [ (a, by a) | a <- as ]
  comp (_,s) (_,s2) = RFC5051.compareUnicode s s2
  in fst <$> sortBy comp as'

-- | Like sortNamed, but takes an additional backup comparison function if two
-- names are equal.
sortNamed' :: (a -> Name) -> (a -> a -> Ordering) -> [a] -> [a]
sortNamed' by by2 as = let
  as' = [ (a, toText (by a)) | a <- as ]
  comp (a,s) (a2,s2) = RFC5051.compareUnicode s s2 <> by2 a a2
  in fst <$> sortBy comp as'

unsafeFromText :: Text -> Name
unsafeFromText t =
  if Text.any (== '#') t then error $ "not a name: " <> show t else Name t

unsafeFromString :: String -> Name
unsafeFromString = unsafeFromText . Text.pack

toVar :: Var v => Name -> v
toVar (Name t) = Var.named t

fromVar :: Var v => v -> Name
fromVar = unsafeFromText . Var.name

toString :: Name -> String
toString = Text.unpack . toText

isPrefixOf :: Name -> Name -> Bool
a `isPrefixOf` b = toText a `Text.isPrefixOf` toText b

-- foo.bar.baz `endsWithSegments` bar.baz == True
-- foo.bar.baz `endsWithSegments` baz == True
-- foo.bar.baz `endsWithSegments` az == False (not a full segment)
-- foo.bar.baz `endsWithSegments` zonk == False (doesn't match any segment)
-- foo.bar.baz `endsWithSegments` foo == False (matches a segment, but not at the end)
endsWithSegments :: Name -> Name -> Bool
endsWithSegments n ending = any (== ending) (suffixes n)

-- stripTextPrefix a.b. a.b.c = Just c
-- stripTextPrefix a.b  a.b.c = Just .c;  you probably don't want to do this
-- stripTextPrefix x.y. a.b.c = Nothing
-- stripTextPrefix "" a.b.c = undefined
_stripTextPrefix :: Text -> Name -> Maybe Name
_stripTextPrefix prefix name =
  Name <$> Text.stripPrefix prefix (toText name)

-- stripNamePrefix a.b  a.b.c = Just c
-- stripNamePrefix a.b. a.b.c = undefined, "a.b." isn't a valid name IMO
-- stripNamePrefix x.y  a.b.c = Nothing, x.y isn't a prefix of a.b.c
-- stripNamePrefix "" a.b.c = undefined, "" isn't a valid name IMO
-- stripNamePrefix . .Nat = Just Nat
stripNamePrefix :: Name -> Name -> Maybe Name
stripNamePrefix prefix name =
  Name <$> Text.stripPrefix (toText prefix <> mid) (toText name)
  where
  mid = if toText prefix == "." then "" else "."

-- suffixFrom Int builtin.Int.+ ==> Int.+
-- suffixFrom Int Int.negate    ==> Int.negate
--
-- Currently used as an implementation detail of expanding wildcard
-- imports, (like `use Int` should catch `builtin.Int.+`)
-- but it may be generally useful elsewhere. See `expandWildcardImports`
-- for details.
suffixFrom :: Name -> Name -> Maybe Name
suffixFrom mid overall = case Text.breakOnAll (toText mid) (toText overall) of
  []         -> Nothing
  (_, rem):_ -> Just (Name rem)

-- a.b.c.d -> d
stripPrefixes :: Name -> Name
stripPrefixes = maybe "" fromSegment . lastMay . segments

joinDot :: Name -> Name -> Name
joinDot prefix suffix =
  if toText prefix == "." then Name (toText prefix <> toText suffix)
  else Name (toText prefix <> "." <> toText suffix)

unqualified :: Name -> Name
unqualified = unsafeFromText . unqualified' . toText

-- parent . -> Nothing
-- parent + -> Nothing
-- parent foo -> Nothing
-- parent foo.bar -> foo
-- parent foo.bar.+ -> foo.bar
parent :: Name -> Maybe Name
parent n = case unsnoc (NameSegment.toText <$> segments n) of
  Nothing        -> Nothing
  Just ([]  , _) -> Nothing
  Just (init, _) -> Just $ Name (Text.intercalate "." init)

-- suffixes "" -> []
-- suffixes bar -> [bar]
-- suffixes foo.bar -> [foo.bar, bar]
-- suffixes foo.bar.baz -> [foo.bar.baz, bar.baz, baz]
-- suffixes ".base.." -> [base.., .]
suffixes :: Name -> [Name]
suffixes (Name "") = []
suffixes (Name n ) = fmap up . filter (not . null) . tails $ segments' n
  where up ns = Name (Text.intercalate "." ns)

unqualified' :: Text -> Text
unqualified' = fromMaybe "" . lastMay . segments'

makeAbsolute :: Name -> Name
makeAbsolute n | toText n == "."                = Name ".."
               | Text.isPrefixOf "." (toText n) = n
               | otherwise                      = Name ("." <> toText n)

instance Show Name where
  show = toString

instance IsString Name where
  fromString = unsafeFromText . Text.pack

instance H.Hashable Name where
  tokens s = [H.Text (toText s)]

fromSegment :: NameSegment -> Name
fromSegment = unsafeFromText . NameSegment.toText

-- Smarter segmentation than `text.splitOn "."`
-- e.g. split `base..` into `[base,.]`
segments :: Name -> [NameSegment]
segments (Name n) = NameSegment <$> segments' n

reverseSegments :: Name -> [NameSegment]
reverseSegments (Name n) = NameSegment <$> NameSegment.reverseSegments' n

countSegments :: Name -> Int
countSegments n = length (segments n)

-- The `Ord` instance for `Name` considers the segments of the name
-- starting from the last, enabling efficient search by name suffix.
--
-- To order names alphabetically for purposes of display to a human,
-- `sortNamed` or one of its variants should be used, which provides a
-- Unicode and capitalization aware sorting (based on RFC5051).
instance Ord Name where
  compare n1 n2 =
       (reverseSegments n1 `compare` reverseSegments n2)
    <> (isAbsolute n1 `compare` isAbsolute n2)

instance Alphabetical Name where
  compareAlphabetical (Name n1) (Name n2) = compareAlphabetical n1 n2

isAbsolute :: Name -> Bool
isAbsolute (Name n) = Text.isPrefixOf "." n

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

-- `compareSuffix suffix n` is equal to `compare n' suffix`, where
-- n' is `n` with only the last `countSegments suffix` segments.
--
-- Used for suffix-based lookup of a name. For instance, given a `r : Relation Name x`,
-- `Relation.searchDom (compareSuffix "foo.bar") r` will find all `r` whose name
-- has `foo.bar` as a suffix.
compareSuffix :: Name -> Name -> Ordering
compareSuffix suffix =
  let
    suffixSegs = reverseSegments suffix
    len = length suffixSegs
  in
    \n -> take len (reverseSegments n) `compare` suffixSegs

-- Tries to shorten `fqn` to the smallest suffix that still refers
-- to to `r`. Uses an efficient logarithmic lookup in the provided relation.
-- The returned `Name` may refer to multiple hashes if the original FQN
-- did as well.
--
-- NB: Only works if the `Ord` instance for `Name` orders based on
-- `Name.reverseSegments`.
shortestUniqueSuffix :: Ord r => Name -> r -> R.Relation Name r -> Name
shortestUniqueSuffix fqn r rel =
  maybe fqn (convert . reverse) (find isOk suffixes)
  where
  allowed = R.lookupDom fqn rel
  suffixes = drop 1 (inits (reverseSegments fqn))
  isOk suffix = (Set.size rs == 1 && Set.findMin rs == r) || rs == allowed
    where rs = R.searchDom compareEnd rel
          compareEnd n = compare (take len (reverseSegments n)) suffix
          len = length suffix

class Convert a b where
  convert :: a -> b

class Parse a b where
  parse :: a -> Maybe b

instance Convert Name Text where convert = toText
instance Convert Name [NameSegment] where convert = segments
instance Convert NameSegment Name where convert = fromSegment
instance Convert [NameSegment] Name where
  convert sgs = unsafeFromText (Text.intercalate "." (map NameSegment.toText sgs))

instance Parse Text NameSegment where
  parse txt = case NameSegment.segments' txt of
    [n] -> Just (NameSegment.NameSegment n)
    _ -> Nothing

instance (Parse a a2, Parse b b2) => Parse (a,b) (a2,b2) where
  parse (a,b) = (,) <$> parse a <*> parse b

instance Lens.Snoc Name Name NameSegment NameSegment where
  _Snoc = Lens.prism snoc unsnoc
   where
    snoc :: (Name, NameSegment) -> Name
    snoc (n, s) = joinDot n (fromSegment s)
    unsnoc :: Name -> Either Name (Name, NameSegment)
    unsnoc n@(segments -> ns) = case Lens.unsnoc (NameSegment.toText <$> ns) of
      Nothing      -> Left n
      Just ([], _) -> Left n
      Just (init, last) ->
        Right (Name (Text.intercalate "." init), NameSegment last)
