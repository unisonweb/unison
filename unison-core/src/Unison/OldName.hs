module Unison.OldName
  ( -- * Old name API (temporary), exported only for testing
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

import Control.Lens (unsnoc)
import Data.List (find, inits, sortBy, tails)
import qualified Data.RFC5051 as RFC5051
import qualified Data.Set as Set
import qualified Data.Text as Text
import Unison.NameSegment
  ( NameSegment (NameSegment),
    segments',
  )
import qualified Unison.NameSegment as NameSegment
import Unison.Prelude
import qualified Unison.Util.Relation as R
import Unison.Var (Var)
import qualified Unison.Var as Var

type OldName = Text

oldFromString :: String -> OldName
oldFromString =
  oldUnsafeFromText . Text.pack

oldSortNames :: [OldName] -> [OldName]
oldSortNames = oldSortNamed id

oldSortNamed :: (a -> OldName) -> [a] -> [a]
oldSortNamed = sortByText

sortByText :: (a -> Text) -> [a] -> [a]
sortByText by as =
  let as' = [(a, by a) | a <- as]
      comp (_, s) (_, s2) = RFC5051.compareUnicode s s2
   in fst <$> sortBy comp as'

oldUnsafeFromText :: Text -> OldName
oldUnsafeFromText t =
  if Text.any (== '#') t then error $ "not a name: " <> show t else t

oldUnsafeFromString :: String -> OldName
oldUnsafeFromString = oldUnsafeFromText . Text.pack

oldToVar :: Var v => OldName -> v
oldToVar = Var.named

oldToString :: OldName -> String
oldToString = Text.unpack

oldIsPrefixOf :: OldName -> OldName -> Bool
oldIsPrefixOf = Text.isPrefixOf

oldEndsWithSegments :: OldName -> OldName -> Bool
oldEndsWithSegments n ending = any (== ending) (oldSuffixes n)

oldStripNamePrefix :: OldName -> OldName -> Maybe OldName
oldStripNamePrefix prefix name =
  Text.stripPrefix (prefix <> mid) name
  where
    mid = if prefix == "." then "" else "."

oldSuffixFrom :: OldName -> OldName -> Maybe OldName
oldSuffixFrom mid overall = case Text.breakOnAll mid overall of
  [] -> Nothing
  (_, rem) : _ -> Just rem

oldJoinDot :: OldName -> OldName -> OldName
oldJoinDot prefix suffix =
  if prefix == "."
    then prefix <> suffix
    else prefix <> "." <> suffix

oldUnqualified :: OldName -> OldName
oldUnqualified = oldUnsafeFromText . unqualified'

oldParent :: OldName -> Maybe OldName
oldParent n = case unsnoc (NameSegment.toText <$> oldSegments n) of
  Nothing -> Nothing
  Just ([], _) -> Nothing
  Just (init, _) -> Just $ Text.intercalate "." init

oldSuffixes :: OldName -> [OldName]
oldSuffixes "" = []
oldSuffixes n = fmap up . filter (not . null) . tails $ segments' n
  where
    up = Text.intercalate "."

unqualified' :: Text -> Text
unqualified' = fromMaybe "" . lastMay . segments'

oldMakeAbsolute :: OldName -> OldName
oldMakeAbsolute n
  | n == "." = ".."
  | Text.isPrefixOf "." n = n
  | otherwise = "." <> n

oldFromSegment :: NameSegment -> OldName
oldFromSegment = oldUnsafeFromText . NameSegment.toText

oldSegments :: OldName -> [NameSegment]
oldSegments n = NameSegment <$> segments' n

oldReverseSegments :: OldName -> [NameSegment]
oldReverseSegments n = NameSegment <$> NameSegment.reverseSegments' n

oldCountSegments :: OldName -> Int
oldCountSegments n = length (oldSegments n)

oldIsAbsolute :: OldName -> Bool
oldIsAbsolute = Text.isPrefixOf "."

oldSearchBySuffix :: (Ord r) => OldName -> R.Relation OldName r -> Set r
oldSearchBySuffix suffix rel =
  R.lookupDom suffix rel `orElse` R.searchDom (oldCompareSuffix suffix) rel
  where
    orElse s1 s2 = if Set.null s1 then s2 else s1

oldCompareSuffix :: OldName -> OldName -> Ordering
oldCompareSuffix suffix =
  let suffixSegs = oldReverseSegments suffix
      len = length suffixSegs
   in \n -> take len (oldReverseSegments n) `compare` suffixSegs

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
