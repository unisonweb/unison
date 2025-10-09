module Unison.Test.Server.Backend.DefinitionDiff (test) where

import Data.Function
import Data.List qualified as List
import Data.Text qualified as Text
import EasyTest qualified as EasyTest
import Hedgehog hiding (Test, test)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Text.RawString.QQ (r)
import Unison.Prelude
import Unison.Server.Backend.DefinitionDiff
import Unison.Server.Orphans ()
import Unison.Server.Types (LinewiseDiff(..))
import Unison.Util.AnnotatedText (Segment (..))

test :: EasyTest.Test ()
test =
  void . EasyTest.scope "definitiondiffs" $ do
    success <-
      EasyTest.io
        $ checkParallel
        $ Group
          "linewise-diffs"
        $ [ ("sides-always-equal-lengths", sidesAlwaysEqualLengths)
          ]
          <> explicitTestCases
    EasyTest.expect success

genDiffs :: Gen (Text, Text)
genDiffs = do
  initial <- diffText
  -- Choose a diffee that's either a few modifications of the initial text or a completely new random text
  diffee <- Gen.choice [modifiedText initial, diffText]
  pure (initial, diffee)
  where
    modifiedText txt = do
      numModifications <- Gen.int (Range.linear 1 5)
      result <- modifications txt numModifications
      pure result

    whitespace = Gen.element [" ", "\n"]
    -- Get a random char, including '\n' explicitly improves the chance of newlines, which are especially important for diffs. Choice shrinks towards earlier elements
    diffText = do
      numWords <- Gen.int (Range.linear 10 30)
      let wordsM = List.replicate numWords diffWord
      Text.concat <$> (sequenceA $ List.intersperse whitespace wordsM)

    diffWord = Gen.text (Range.linear 1 5) Gen.alpha
    -- Get a random location in the text, preference for the beginning and end of the text, since those are
    -- interesting.
    genTextLoc txt = Gen.choice [pure (Text.length txt), pure 0, Gen.int (Range.linear 0 (Text.length txt))]
    changeSomething txt = do
      loc <- genTextLoc txt
      let (before, after) = Text.splitAt loc txt
      newTxt <- diffWord
      dropAmt <- Gen.int (Range.linear 1 10)
      pure $ before <> newTxt <> Text.drop dropAmt after
    insertSomething txt = do
      loc <- genTextLoc txt
      let (before, after) = Text.splitAt loc txt
      newTxt <- diffWord
      pure $ before <> newTxt <> after
    deleteSomething txt = do
      loc <- genTextLoc txt
      let (before, after) = Text.splitAt loc txt
      dropAmt <- Gen.int (Range.linear 1 10)
      pure $ before <> Text.drop dropAmt after
    -- Apply n random modifications to the text
    modifications txt 0 = pure txt
    modifications txt n = do
      updated <- Gen.choice [changeSomething txt, insertSomething txt, deleteSomething txt]
      modifications updated (n - 1)

-- Both sides of a line-wise diff should be padded to match lengths.
sidesAlwaysEqualLengths :: Property
sidesAlwaysEqualLengths =
  property $ do
    (left, right) <- forAll $ genDiffs
    let leftSegments = textToSegment left
        rightSegments = textToSegment right
        LinewiseDiff{lhsLines, rhsLines} = linewiseDiff (==) leftSegments rightSegments
    diff lhsLines ((==) `on` length) rhsLines

textToSegment :: Text -> [Segment ()]
textToSegment txt =
  txt
    & Text.lines
    & fmap
      ( \t ->
          t
            & Text.splitOn " "
            & fmap (Text.unpack)
            & fmap \case
              "" -> Nothing
              w -> Just (Segment w Nothing)
            & List.intersperse (Just $ (Segment " " Nothing))
            & catMaybes
      )
    & List.intercalate [Segment "\n" Nothing]

data TestCase = TestCase
  { name :: PropertyName,
    left :: Text,
    right :: Text,
    expected :: Text
  }

explicitTestCases :: [(PropertyName, Property)]
explicitTestCases =
  [ TestCase
      { name = "simple-modification",
        left =
          [r|
one word
two words
three words
|],
        right =
          [r|
one word
different words
three words
|],
        expected =
          [r|
one word     | one word
*{two} words | *{different} words
three words  | three words
|]
      },
    TestCase
      { name = "unrelated-strings",
        left =
          [r|
Completely different string
shouldn't
have much in common
|],
        right =
          [r|
Lorem ipsum dolor sit amet
consectetur adipisicing elit
sed do eiusmod tempor incididunt
ut labore et dolore magna aliqua
Ut enim ad minim veniam
|],
        expected =
          [r|
*{Completely} {different} {string} | *{Lorem} {ipsum} {dolor}{ }{sit}{ }{amet}
*{shouldn't}                       | *{consectetur}{ }{adipisicing}{ }{elit}
*{have} {much} {in} {common}       | *{sed} {do} {eiusmod} {tempor}{ }{incididunt}
////                               | *{ut}{ }{labore}{ }{et}{ }{dolore}{ }{magna}{ }{aliqua}
////                               | *{Ut}{ }{enim}{ }{ad}{ }{minim}{ }{veniam}
|]
      },
    TestCase
      { name = "complex-diff",
        left =
          [r|
unchangedDefinition = 1
myDefinition : Nat -> Text -> Text
myDefinition n txt =
  -- Check for non-positive n
  if n == 0 then txt
  else Text.drop n txt
|],
        right =
          [r|
unchangedDefinition = 1
myNewDefinition : Int -> Text -> Text
myNewDefinition k input =
  -- Check for non-positive n
  if n <= 0
    then txt
  else
    Text.take n input
|],
        expected =
          [r|
unchangedDefinition = 1                 | unchangedDefinition = 1
*{myDefinition} : {Nat} -> Text -> Text | *{myNewDefinition} : {Int} -> Text -> Text
*{myDefinition} {n} {txt} =             | *{myNewDefinition} {k} {input} =
  -- Check for non-positive n           |   -- Check for non-positive n
*  if n {==} 0 then txt                 | *  if n {<=} 0
*  else {Text.drop} n {txt}             | * { }{ }{ }then txt
////                                    | *  else
////                                    | *  { }{ }{Text.take}{ }n {input}
|]
      }
  ]
    <&> \(TestCase {name, left, right, expected}) ->
      ( name,
        property $ do
          let actual = (Text.strip (testDiff (Text.strip left) (Text.strip right)))
          Hedgehog.footnote ("Actual:\n\n" <> Text.unpack actual <> "\n\nExpected:\n\n" <> Text.unpack (Text.strip expected))
          actual === (Text.strip expected)
      )

-- | Renders a structured diff side as text, annotating changed lines with `*` and changed segments within changed lines by wrapping them with `{}`.
-- Spacers are represented by `////`.
renderDiffText :: [Changed [Paired (Segment a)]] -> Text
renderDiffText diffs =
  let renderSegment :: Segment a -> Text
      renderSegment (Segment {segment}) = Text.pack segment
      renderPaired :: Paired (Segment a) -> Text
      renderPaired (OneSided s) = "{" <> renderSegment s <> "}"
      renderPaired (Paired s1 _) = renderSegment s1
      renderChanged :: Changed [Paired (Segment a)] -> Text
      renderChanged Spacer = "////"
      renderChanged (Unchanged segs) = Text.concat (renderPaired <$> segs)
      renderChanged (Changed segs) = "*" <> Text.concat (renderPaired <$> segs)
   in Text.unlines $ fmap renderChanged diffs

testDiff :: Text -> Text -> Text
testDiff l r =
  let left = textToSegment l
      right = textToSegment r
      LinewiseDiff{lhsLines, rhsLines} = linewiseDiff (==) left right
   in align (renderDiffText lhsLines) (renderDiffText rhsLines)

align :: Text -> Text -> Text
align left right = Text.unlines $ zipWith formatRow leftLines rightLines
  where
    leftLines = Text.lines left
    rightLines = Text.lines right

    -- Find the maximum length in the left column
    maxLeftWidth = maximum $ map Text.length leftLines

    -- Pad the left text and combine with right text
    formatRow l r = Text.justifyLeft maxLeftWidth ' ' l <> " | " <> r

-- Helpers for testing semantic diffs on plaintext

-- simpleLeft :: Text
-- simpleLeft = "one word\ntwo words\nthree words"

-- simpleRight :: Text
-- simpleRight = "one word\ndifferent words\nthree words"

-- complexLeft :: Text
-- complexLeft = "one word\ntwo words\nthree words"

-- complexRight :: Text
-- complexRight = "one word\nmulti-line\ndifference\nshould add spacers\nthree words"
