-- | Utilities for displaying diffs between definitions.
module Unison.Server.Backend.DefinitionDiff
  ( diffDisplayObjects,
    linewiseDiff,
    Paired (..),
    Changed (..),
  )
where

import Data.Algorithm.Diff qualified as Diff
import Data.Foldable qualified as Foldable
import Data.Function
import Data.List qualified as List
import Data.List.Extra qualified as List
import Data.List.NonEmpty qualified as NEL
import Data.List.Split qualified as Split
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Prelude
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Syntax qualified as Syntax
import Unison.Server.Types (Changed (..), DisplayObjectDiff (..), LinewiseDiff (..), Paired (..), SemanticSyntaxDiff (..), swapPair)
import Unison.Util.AnnotatedText (AnnotatedText (..), Segment (..))
import Unison.Util.AnnotatedText qualified as AT
import Unison.Util.List qualified as ListUtil
import Unison.Util.Recursion qualified as Rec

diffDisplayObjects :: (HasCallStack) => DisplayObject SyntaxText SyntaxText -> DisplayObject SyntaxText SyntaxText -> DisplayObjectDiff
diffDisplayObjects from to = case (from, to) of
  (BuiltinObject fromST, BuiltinObject toST) -> DisplayObjectDiff (BuiltinObject (semanticLinewiseDiff fromST toST))
  (MissingObject fromSH, MissingObject toSH)
    | fromSH == toSH -> DisplayObjectDiff (MissingObject fromSH)
    | otherwise -> MismatchedDisplayObjects (MissingObject fromSH) (MissingObject toSH)
  (UserObject fromST, UserObject toST) -> DisplayObjectDiff (UserObject (semanticLinewiseDiff fromST toST))
  (l, r) -> MismatchedDisplayObjects l r

-- diffSyntaxText :: SyntaxText -> SyntaxText -> [SemanticSyntaxDiff Syntax.Element]
-- diffSyntaxText (AnnotatedText fromST) (AnnotatedText toST) =
--   diffSegments syntaxElementDiffEq fromST toST
--     & expandSpecialCases specialCaseAnnotations

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

data DiffOrSame = Different | Same
  deriving (Eq, Ord, Show)

-- | Compute a line-wise diff between two lists of segments.
--
-- >>> let s a = Segment a Nothing
-- >>> let left = [s "line1", s "\n", s "line2", s "\n", s "line3", s "\n", s "line3"]
-- >>> let right = [s "line1", s "\n", s "lineX", s "\n", s "line3"]
-- >>> linewiseDiff (==) left right
-- LinewiseDiff {lhsLines = [Unchanged [Paired (Segment {segment = "line1", annotation = Nothing}) (Segment {segment = "line1", annotation = Nothing})],Changed [OneSided (Segment {segment = "line2", annotation = Nothing})],Changed [OneSided (Segment {segment = "line3", annotation = Nothing})],Unchanged [Paired (Segment {segment = "line3", annotation = Nothing}) (Segment {segment = "line3", annotation = Nothing})]], rhsLines = [Unchanged [Paired (Segment {segment = "line1", annotation = Nothing}) (Segment {segment = "line1", annotation = Nothing})],Changed [OneSided (Segment {segment = "lineX", annotation = Nothing})],Spacer,Unchanged [Paired (Segment {segment = "line3", annotation = Nothing}) (Segment {segment = "line3", annotation = Nothing})]]}
linewiseDiff ::
  forall f a.
  (Foldable f, Eq a, Show a) =>
  (Segment a -> Segment a -> Bool) ->
  f (Segment a) ->
  f (Segment a) ->
  -- Returns a tuple of lists,
  -- Each list is the same length, when lines are present on both sides they're considered Equal.
  -- When lines are only present on one side, the other side has a Nothing in that position as padding.
  LinewiseDiff (Paired (Segment a))
linewiseDiff diffEq left right =
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
   in partitioned
        & foldMap \case
          (Same, ds) ->
            ds & foldMap \case
              Left (a, b) -> do
                let (l, r) = pairLines a b
                 in (Unchanged <$> l, Unchanged <$> r)
              Right _ -> error "impossible"
          (Different, ds) ->
            -- When left and right are different, We do a subdiff on the chunk
            let (lefts :: [[Segment a]], rights :: [[Segment a]]) =
                  ds
                    & foldMap \case
                      Left _ -> error "impossible"
                      Right (Left a) -> (a, mempty)
                      Right (Right b) -> (mempty, b)
             in diffChangeChunk diffEq lefts rights
        & \(lhsLines, rhsLines) ->
          LinewiseDiff {lhsLines, rhsLines}
  where
    pairLines :: forall x. [[x]] -> [[x]] -> ([[Paired x]], [[Paired x]])
    pairLines left right =
      let paired = zipWith (zipWith Paired) left right
       in ( paired,
            fmap swapPair <$> paired
          )

-- | Compute a semantic line-wise diff between two SyntaxText values, with special-casing
-- to detect tokens whose name stayed the same but whose hash changed, or whose hash stayed the same but whose name changed.
semanticLinewiseDiff :: SyntaxText -> SyntaxText -> LinewiseDiff (SemanticSyntaxDiff Syntax.Element)
semanticLinewiseDiff (AnnotatedText lhs) (AnnotatedText rhs) =
  linewiseDiff syntaxElementDiffEq lhs rhs
    <&> specialCasePairs
    & \(LinewiseDiff {lhsLines, rhsLines}) ->
      LinewiseDiff {lhsLines = (fmap . fmap) aggregateChunks lhsLines, rhsLines = (fmap . fmap) aggregateChunks rhsLines}
  where
    specialCasePairs :: Paired (Segment Syntax.Element) -> SemanticSyntaxDiff Syntax.Element
    specialCasePairs = \case
      OneSided a -> OnlyThisSide (NEL.singleton a)
      Paired fromSegment toSegment
        | fromSegment == toSegment -> Both (NEL.singleton fromSegment)
        | AT.annotation fromSegment == AT.annotation toSegment -> SegmentChange (AT.segment fromSegment, AT.segment toSegment) (AT.annotation fromSegment)
        -- We only emit an annotation change if it's a change in just the hash of the element (optionally the KIND of hash reference can change too).
        | AT.segment fromSegment == AT.segment toSegment,
          Just _fromHash <- AT.annotation fromSegment >>= elementHash,
          Just _toHash <- AT.annotation toSegment >>= elementHash ->
            AnnotationChange (AT.segment fromSegment) (AT.annotation fromSegment, AT.annotation toSegment)
        | otherwise ->
            -- the annotation changed, but it's not a recognized hash change.
            -- This can happen in certain special cases, e.g. a paren changed from being a syntax element into being part
            -- of a unit.
            -- We just emit both as old/new segments.
            OnlyThisSide (NEL.singleton fromSegment)
        where
          elementHash :: Syntax.Element -> Maybe Syntax.UnisonHash
          elementHash = \case
            Syntax.TypeReference hash -> Just hash
            Syntax.TermReference hash -> Just hash
            Syntax.DataConstructorReference hash -> Just hash
            Syntax.AbilityConstructorReference hash -> Just hash
            _ -> Nothing

    -- Collapse subsequent chunks of the same kind of diff into one chunk.
    aggregateChunks :: [SemanticSyntaxDiff Syntax.Element] -> [SemanticSyntaxDiff Syntax.Element]
    aggregateChunks =
      Rec.cata \case
        Rec.Neither -> []
        Rec.Both x [] -> [x]
        Rec.Both (OnlyThisSide xs) (OnlyThisSide ys : rest) ->
          OnlyThisSide (xs <> ys) : rest
        Rec.Both (Both xs) (Both ys : rest) ->
          Both (xs <> ys) : rest
        Rec.Both x xs -> x : xs

-- | Takes the left and right sides of a diff which are part of the same contiguous chunk, then
-- diffs them and returns padded left/right line diffs
diffChangeChunk ::
  forall a.
  (Eq a) =>
  (Segment a -> Segment a -> Bool) ->
  [[Segment a]] ->
  [[Segment a]] ->
  -- Lists of lines, where each line is a list of segments.
  -- 'Nothing' lines are just padding
  ([Changed [Paired (Segment a)]], [Changed [Paired (Segment a)]])
diffChangeChunk diffEq leftLines rightLines =
  -- Represent newlines with 'Nothing' so we can do a flat diff.
  let flattenedL :: [Maybe (Segment a)]
      flattenedL = List.intercalate [Nothing] (fmap Just <$> leftLines)
      flattenedR :: [Maybe (Segment a)]
      flattenedR = List.intercalate [Nothing] (fmap Just <$> rightLines)
      diff :: [Diff.PolyDiff [Maybe (Segment a)] [Maybe (Segment a)]]
      diff = diffSegments mayDiffEq flattenedL flattenedR
      (leftResults :: [Maybe (Paired (Segment a))], rightResults) =
        diff
          & foldMap \case
            Diff.First ys ->
              (fmap OneSided <$> ys, mempty)
            Diff.Second ys ->
              (mempty, fmap OneSided <$> ys)
            Diff.Both from to ->
              let zipper = \cases
                    Nothing Nothing -> Nothing
                    (Just l) (Just r) -> Just (Paired l r)
                    _ _ -> error "impossible"
               in (zipWith zipper from to, zipWith zipper to from)

      -- Now only padding newlines are represented by Nothing.
      padding = repeat Spacer
      relinedLeft = catMaybes <$> List.splitOn [Nothing] leftResults
      relinedRight = catMaybes <$> List.splitOn [Nothing] rightResults
      leftLength = length relinedLeft
      rightLength = length relinedRight
      maxLines = max leftLength rightLength
   in ( (Changed <$> relinedLeft) <> take (maxLines - leftLength) padding,
        (Changed <$> relinedRight) <> take (maxLines - rightLength) padding
      )
  where
    mayDiffEq :: Maybe (Segment a) -> Maybe (Segment a) -> Bool
    mayDiffEq = \cases
      Nothing Nothing -> True
      (Just l) (Just r) -> diffEq l r
      _ _ -> False
