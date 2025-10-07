-- | Utilities for displaying diffs between definitions.
module Unison.Server.Backend.DefinitionDiff
  ( diffDisplayObjects,
  )
where

import Data.Algorithm.Diff qualified as Diff
import Data.Foldable qualified as Foldable
import Data.Function
import Data.List qualified as List
import Data.List.Extra qualified as List
import Data.List.Split qualified as Split
import Data.Text qualified as Text
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Prelude
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Syntax qualified as Syntax
import Unison.Server.Types (DisplayObjectDiff (..), SemanticSyntaxDiff (..))
import Unison.Util.AnnotatedText (AnnotatedText (..), Segment (..))
import Unison.Util.AnnotatedText qualified as AT
import Unison.Util.List qualified as ListUtil

diffDisplayObjects :: (HasCallStack) => DisplayObject SyntaxText SyntaxText -> DisplayObject SyntaxText SyntaxText -> DisplayObjectDiff
diffDisplayObjects from to = case (from, to) of
  (BuiltinObject fromST, BuiltinObject toST) -> DisplayObjectDiff (BuiltinObject (diffSyntaxText fromST toST))
  (MissingObject fromSH, MissingObject toSH)
    | fromSH == toSH -> DisplayObjectDiff (MissingObject fromSH)
    | otherwise -> MismatchedDisplayObjects (MissingObject fromSH) (MissingObject toSH)
  (UserObject fromST, UserObject toST) -> DisplayObjectDiff (UserObject (diffSyntaxText fromST toST))
  (l, r) -> MismatchedDisplayObjects l r

diffSyntaxText :: SyntaxText -> SyntaxText -> [SemanticSyntaxDiff Syntax.Element]
diffSyntaxText (AnnotatedText fromST) (AnnotatedText toST) =
  diffSegments syntaxElementDiffEq fromST toST
    & expandSpecialCases specialCaseAnnotations

-- We special-case situations where the name of a definition changed but its hash didn't;
-- and cases where the name didn't change but the hash did.
--
-- The diff algorithm only understands whether items are equal or not, so in order to add this special behavior we
-- treat these special cases as equal, then we can detect and expand them in a post-processing step.
syntaxElementDiffEq :: AT.Segment Syntax.Element -> AT.Segment Syntax.Element -> Bool
syntaxElementDiffEq (AT.Segment {segment = fromSegment, annotation = fromAnnotation}) (AT.Segment {segment = toSegment, annotation = toAnnotation}) =
  fromSegment == toSegment
    || case (fromAnnotation, toAnnotation) of
      (Nothing, _) -> False
      (_, Nothing) -> False
      (Just a, Just b) ->
        case a of
          -- The set of annotations we want to special-case
          Syntax.TypeReference {} -> a == b
          Syntax.TermReference {} -> a == b
          Syntax.DataConstructorReference {} -> a == b
          Syntax.AbilityConstructorReference {} -> a == b
          Syntax.HashQualifier {} -> a == b
          _ -> False

specialCaseAnnotations :: AT.Segment Syntax.Element -> AT.Segment Syntax.Element -> Either (AT.Segment Syntax.Element) [SemanticSyntaxDiff Syntax.Element]
specialCaseAnnotations fromSegment toSegment
  | fromSegment == toSegment = Left fromSegment
  | AT.annotation fromSegment == AT.annotation toSegment = Right [SegmentChange (AT.segment fromSegment, AT.segment toSegment) (AT.annotation fromSegment)]
  -- We only emit an annotation change if it's a change in just the hash of the element (optionally the KIND of hash reference can change too).
  | AT.segment fromSegment == AT.segment toSegment,
    Just _fromHash <- AT.annotation fromSegment >>= elementHash,
    Just _toHash <- AT.annotation toSegment >>= elementHash =
      Right [AnnotationChange (AT.segment fromSegment) (AT.annotation fromSegment, AT.annotation toSegment)]
  | otherwise =
      -- the annotation changed, but it's not a recognized hash change.
      -- This can happen in certain special cases, e.g. a paren changed from being a syntax element into being part
      -- of a unit.
      -- We just emit both as old/new segments.
      Right [Old [fromSegment], New [toSegment]]
  where
    elementHash :: Syntax.Element -> Maybe Syntax.UnisonHash
    elementHash = \case
      Syntax.TypeReference hash -> Just hash
      Syntax.TermReference hash -> Just hash
      Syntax.DataConstructorReference hash -> Just hash
      Syntax.AbilityConstructorReference hash -> Just hash
      _ -> Nothing

diffSegments ::
  forall f a.
  (Foldable f) =>
  (a -> a -> Bool) ->
  f a ->
  f a ->
  [Diff.PolyDiff [a] [a]]
diffSegments diffEq left right =
  Diff.getGroupedDiffBy
    diffEq
    (Foldable.toList left)
    (Foldable.toList right)

expandSpecialCases ::
  (Segment a -> Segment a -> Either (Segment a) [SemanticSyntaxDiff a]) ->
  [Diff.Diff [AT.Segment a]] ->
  [SemanticSyntaxDiff a]
expandSpecialCases detectSpecialCase xs =
  xs
    & foldMap \case
      Diff.First ys -> [Old ys]
      Diff.Second ys -> [New ys]
      Diff.Both from to ->
        -- Each list should always be the same length.
        zipWith detectSpecialCase from to
          & (flip List.foldr [])
            ( \next acc -> case (acc, next) of
                (Both xs : rest, Left seg) -> Both (seg : xs) : rest
                (_, Left seg) -> Both [seg] : acc
                (_, Right diff) -> diff ++ acc
            )

data DiffOrSame = Different | Same
  deriving (Eq, Ord, Show)

-- | Compute a line-wise diff between two lists of segments.
--
-- >>> let s a = Segment a Nothing
-- >>> let left = [s "line1", s "\n", s "line2", s "\n", s "line3"]
-- >>> let right = [s "line1", s "\n", s "lineX", s "\n", s "line3"]
-- >>> linewiseDiff left right
-- ([Just [Segment {segment = "line1", annotation = Nothing}],Just [Segment {segment = "line2", annotation = Nothing}],Nothing,Just [Segment {segment = "line3", annotation = Nothing}]],[Just [Segment {segment = "line1", annotation = Nothing}],Nothing,Just [Segment {segment = "lineX", annotation = Nothing}],Just [Segment {segment = "line3", annotation = Nothing}]])
linewiseDiff ::
  forall f a.
  (Foldable f, Eq a) =>
  f (Segment a) ->
  f (Segment a) ->
  -- Returns a tuple of lists,
  -- Each list is the same length, when lines are present on both sides they're considered Equal.
  -- When lines are only present on one side, the other side has a Nothing in that position as padding.
  ([Maybe [Segment a]], [Maybe [Segment a]])
linewiseDiff left right =
  let leftLines = Split.splitWhen ((== "\n") . AT.segment) . toList $ left
      rightLines = Split.splitWhen ((== "\n") . AT.segment) . toList $ right
      groupedDiff = Diff.getGroupedDiff leftLines rightLines
      partitioned =
        groupedDiff
          & ListUtil.groupMap
            ( \d ->
                case d of
                  Diff.Both a b -> (Same, Left (a, b))
                  Diff.First a -> (Different, Right $ Left a)
                  Diff.Second b -> (Different, Right $ Right b)
            )
   in partitioned & foldMap \case
        (Same, ds) ->
          ds & foldMap \case
            Left (a, b) -> (Just <$> a, Just <$> b)
            Right _ -> error "impossible"
        (Different, ds) ->
          -- When left and right are different, We add padding to the end of the left and the beginning of the right
          -- so that the diffs are visually separate, but then line up again when we get to the next matching section.
          let (lefts :: [[Segment a]], rights :: [[Segment a]]) =
                ds
                  & foldMap \case
                    Left _ -> error "impossible"
                    Right (Left a) -> (a, mempty)
                    Right (Right b) -> (mempty, b)
              leftLineCount = length lefts
              rightLineCount = length rights
           in ( (Just <$> lefts) <> replicate rightLineCount Nothing,
                replicate leftLineCount Nothing <> (Just <$> rights)
              )

-- Diff data can be one-sided or have a counter-part on the other side of the diff.
-- We can use this to represent things like name-changes for the same hash, or hash-changes for the same name.
data Paired a
  = OneSided a
  | Paired a a

swapPair :: Paired a -> Paired a
swapPair (OneSided a) = OneSided a
swapPair (Paired a b) = Paired b a

-- Takes the left and right sides of a diff which are part of the same contiguous chunk, then
-- diffs them and returns padded left/right line diffs
diffChangeChunk ::
  forall a.
  (Eq a) =>
  (Segment a -> Segment a -> Bool) ->
  [[Segment a]] ->
  [[Segment a]] ->
  -- Lists of lines, where each line is a list of segments.
  -- 'Nothing' lines are just padding
  ([Maybe [Paired (Segment a)]], [Maybe [Paired (Segment a)]])
diffChangeChunk diffEq leftLines rightLines =
  -- Represent newlines with 'Nothing' so we can do a flat diff.
  let flattenedL :: [Maybe (Segment a)]
      flattenedL = List.intercalate [Nothing] (fmap Just <$> leftLines)
      flattenedR :: [Maybe (Segment a)]
      flattenedR = List.intercalate [Nothing] (fmap Just <$> rightLines)
      diff :: [Diff.PolyDiff [Maybe (Segment a)] [Maybe (Segment a)]]
      diff = diffSegments mayDiffEq flattenedL flattenedR
      (leftResults :: [[Paired (Segment a)]], rightResults) =
        diff
          & foldMap \case
            Diff.First ys ->
              let reLined = List.splitOn [Nothing] ys
               in ((fmap) OneSided . catMaybes <$> reLined, mempty)
            Diff.Second ys ->
              let reLined = List.splitOn [Nothing] ys
               in (mempty, (fmap) OneSided . catMaybes <$> reLined)
            Diff.Both from to ->
              let reLinedL = List.splitOn [Nothing] from
                  reLinedR = List.splitOn [Nothing] to
                  zipped = zipWith (zipWith Paired) (catMaybes <$> reLinedL) (catMaybes <$> reLinedR)
               in (zipped, fmap swapPair <$> zipped)
      -- Now only padding newlines are represented by Nothing.
      padding = repeat Nothing
      leftLength = length leftResults
      rightLength = length rightResults
      maxLines = max leftLength rightLength
   in ( (Just <$> leftResults) <> take (maxLines - leftLength) padding,
        take (maxLines - rightLength) padding <> (Just <$> rightResults)
      )
  where
    mayDiffEq :: Maybe (Segment a) -> Maybe (Segment a) -> Bool
    mayDiffEq = \cases
      Nothing Nothing -> True
      (Just l) (Just r) -> diffEq l r
      _ _ -> False
