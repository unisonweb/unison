-- | Utilities for displaying diffs between definitions.
module Unison.Server.Backend.DefinitionDiff
  ( diffDisplayObjects,
    partitionDiff,
  )
where

import Data.Algorithm.Diff qualified as Diff
import Data.Foldable qualified as Foldable
import Data.Function
import Data.List qualified as List
import Data.Monoid (Sum (..))
import Data.Text qualified as Text
import Unison.Codebase.Editor.DisplayObject (DisplayObject (..))
import Unison.Prelude
import Unison.Server.Syntax (SyntaxText)
import Unison.Server.Syntax qualified as Syntax
import Unison.Server.Types
import Unison.Util.AnnotatedText (AnnotatedText (..))
import Unison.Util.AnnotatedText qualified as AT

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
  Diff.getGroupedDiffBy
    diffEq
    (Foldable.toList @Seq fromST)
    (Foldable.toList @Seq toST)
    & expandSpecialCases
  where
    -- We special-case situations where the name of a definition changed but its hash didn't;
    -- and cases where the name didn't change but the hash did.
    --
    -- The diff algorithm only understands whether items are equal or not, so in order to add this special behavior we
    -- treat these special cases as equal, then we can detect and expand them in a post-processing step.
    diffEq :: AT.Segment Syntax.Element -> AT.Segment Syntax.Element -> Bool
    diffEq (AT.Segment {segment = fromSegment, annotation = fromAnnotation}) (AT.Segment {segment = toSegment, annotation = toAnnotation}) =
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

    expandSpecialCases :: [Diff.Diff [AT.Segment (Syntax.Element)]] -> [SemanticSyntaxDiff Syntax.Element]
    expandSpecialCases xs =
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
    detectSpecialCase :: AT.Segment Syntax.Element -> AT.Segment Syntax.Element -> Either (AT.Segment Syntax.Element) [(SemanticSyntaxDiff Syntax.Element)]
    detectSpecialCase fromSegment toSegment
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

data Lined a
  = Element a
  | Newline
  | Space
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Helper type for accumulating lines.
-- The newlines are represented as the _gaps_ between elements.
data Lines line
  = Lines [Lined line]
  deriving (Show, Eq, Functor, Foldable, Traversable)

newlineCount :: Lines a -> Int
newlineCount (Lines xs) =
  xs
    & filter \case
      Newline -> True
      _ -> False
    & length

instance Semigroup (Lines a) where
  (Lines l1) <> (Lines l2) = Lines (l1 <> l2)

instance Monoid (Lines a) where
  mempty = Lines []

data DiffState ann = DiffState
  { leftLinesSinceSync :: Sum Int,
    rightLinesSinceSync :: Sum Int,
    leftLinesAcc :: Lines (AT.Segment ann),
    rightLinesAcc :: Lines (AT.Segment ann)
  }
  deriving (Generic)

instance Semigroup (DiffState ann) where
  (DiffState l1 r1 la1 ra1) <> (DiffState l2 r2 la2 ra2) =
    DiffState (l1 <> l2) (r1 <> r2) (la1 <> la2) (ra1 <> ra2)

instance Monoid (DiffState ann) where
  mempty = DiffState mempty mempty mempty mempty

partitionDiff :: (Eq ann) => [SemanticSyntaxDiff ann] -> PartitionedDiff [AT.Segment ann]
partitionDiff tokens =
  List.foldl' go (DiffState mempty mempty mempty mempty) tokens
    & diffStateToPartitionedDiff
  where
    go ds token =
      case token of
        Old oldTokens ->
          let oldLines = splitLines oldTokens
           in ds
                <> DiffState
                  { leftLinesSinceSync = Sum (newlineCount oldLines),
                    rightLinesSinceSync = mempty,
                    leftLinesAcc = oldLines,
                    rightLinesAcc = mempty
                  }
        New newTokens ->
          let newLines = splitLines newTokens
           in ds
                <> DiffState
                  { leftLinesSinceSync = mempty,
                    rightLinesSinceSync = Sum (newlineCount newLines),
                    leftLinesAcc = mempty,
                    rightLinesAcc = newLines
                  }
        Both bothTokens ->
          let bothLines = splitLines bothTokens
              (leftPadding, rightPadding) =
                let offset = getSum (leftLinesSinceSync ds) - getSum (rightLinesSinceSync ds)
                 in if offset > 0 then (0, offset) else (abs offset, 0)
           in ds
                <> DiffState
                  { leftLinesSinceSync = mempty,
                    rightLinesSinceSync = mempty,
                    leftLinesAcc = addSpacers leftPadding bothLines,
                    rightLinesAcc = addSpacers rightPadding bothLines
                  }
        -- TODO: maybe can just remove the distinction here.
        SegmentChange (left, right) (maybeSyntaxElement) ->
          let leftLines = splitLines [AT.Segment left maybeSyntaxElement]
              rightLines = splitLines [AT.Segment right maybeSyntaxElement]
           in ds
                <> DiffState
                  { leftLinesSinceSync = Sum (newlineCount leftLines),
                    rightLinesSinceSync = Sum (newlineCount rightLines),
                    leftLinesAcc = leftLines,
                    rightLinesAcc = rightLines
                  }
        AnnotationChange text (mayLeftElem, mayRightElem) ->
          -- Since the count will be the same on both sides, no need to actually count it.
          let lineCount = mempty
           in ds
                <> DiffState
                  { leftLinesSinceSync = lineCount,
                    rightLinesSinceSync = lineCount,
                    leftLinesAcc = splitLines [AT.Segment text mayLeftElem],
                    rightLinesAcc = splitLines [AT.Segment text mayRightElem]
                  }

diffStateToPartitionedDiff :: (Eq ann) => DiffState ann -> PartitionedDiff [AT.Segment ann]
diffStateToPartitionedDiff DiffState {leftLinesAcc = Lines leftLines, rightLinesAcc = Lines rightLines} =
  let (left, right) = unzip (zipWith go (linesToList leftLines) (linesToList rightLines))
   in PartitionedDiff left right
  where
    linesToList :: [Lined x] -> [Maybe [x]]
    linesToList xs =
      -- TODO: inefficient
      List.foldl
        ( \(acc, next) x ->
            case x of
              Newline -> (acc <> [Just next], [])
              -- TODO: This may introduce a duplicate newline :|
              Space -> (acc <> [Just next, Nothing], [])
              Element e -> (acc, next <> [e])
        )
        ([], [])
        xs
        & \(acc, next) -> acc <> [Just next]

    go :: forall a. (Eq a) => Maybe [a] -> Maybe [a] -> (DiffTagged [a], DiffTagged [a])
    go = \cases
      Nothing Nothing -> error "diffing: Encountered adjacent spacers"
      (Just l) Nothing -> (Changed l, Spacer)
      Nothing (Just r) -> (Spacer, Changed r)
      (Just l) (Just r)
        | l == r -> (Unchanged l, Unchanged r)
        | otherwise -> (Changed l, Changed r)

-- >>> splitLines [AT.Segment "a\nb\n" Nothing , AT.Segment "c\nd" Nothing, AT.Segment "ef" Nothing, AT.Segment "\n\n\n" Nothing] :: Lines (AT.Segment ())
-- Lines [Element (Segment {segment = "a", annotation = Nothing}),Newline,Element (Segment {segment = "b", annotation = Nothing}),Newline,Element (Segment {segment = "c", annotation = Nothing}),Newline,Element (Segment {segment = "d", annotation = Nothing}),Element (Segment {segment = "ef", annotation = Nothing}),Newline,Newline,Newline]
splitLines :: [AT.Segment a] -> Lines (AT.Segment a)
splitLines = foldMap \case
  AT.Segment txt ann ->
    -- TODO: should probably change underlying String to Text
    Text.splitOn "\n" (Text.pack txt)
      & map
        \case
          "" -> Nothing
          txt -> Just (Element $ AT.Segment (Text.unpack txt) ann)
      & List.intersperse (Just Newline)
      & catMaybes
      & Lines

addSpacers :: Int -> Lines a -> Lines a
addSpacers 0 ls = ls
addSpacers n (Lines xs) = Lines $ go xs
  where
    go = \case
      (Newline : rest) -> replicate n Space <> rest
      (x : rest) -> x : go rest
      [] -> replicate n Space
