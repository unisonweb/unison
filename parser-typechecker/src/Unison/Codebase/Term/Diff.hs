{-# LANGUAGE QuasiQuotes #-}

module Unison.Codebase.Term.Diff where

import Data.Algorithm.Diff qualified as Diff
import Data.Text qualified as Text
import Text.RawString.QQ (r)
import Unison.Prelude

data DiffTag = Both | LeftOnly | RightOnly
  deriving stock (Show, Eq)

-- -- | Pretty print each term using their own pretty-printers, then do a simple text diff between them.
-- diffTerms :: (PPED.PrettyPrintEnvDecl, (HQ.HashQualified Name, Reference, DisplayObject (Type Symbol Ann) (Term Symbol Ann))) -> (HQ.HashQualified Name, Reference, DisplayObject (Type Symbol Ann) (Term Symbol Ann)) -> [(DiffTag, Text)]
-- diffTerms (beforePPED, beforeTerm) (afterPPED, afterTerm) =
--   let beforeTxt = Text.pack $ Pretty.toPlain 80 . Pretty.syntaxToColor $ P.prettyTerm beforePPED False False beforeTerm
--       afterTxt = Text.pack $ Pretty.toPlain 80 . Pretty.syntaxToColor $ P.prettyTerm afterPPED False False afterTerm
--    in diffText beforeTxt afterTxt

testStr1 :: Text
testStr1 =
  [r|This is a test string.
It has multiple lines.
Like this one.
  |]

testStr2 :: Text
testStr2 =
  [r|This is a test string.
New Line!
has some modified lines.
|]

-- | Perform a simple text diff between two strings.
--
-- >>> diffText "one" "o1ne"
-- [(Both,"o"),(RightOnly,""),(Both,"ne")]
--
-- >>> Diff.diffTexts "one" "o123ne"
-- fromList [EditInsert {insertPos = 1, insertFrom = 1, insertTo = 1},EditInsert {insertPos = 1, insertFrom = 2, insertTo = 2},EditInsert {insertPos = 1, insertFrom = 3, insertTo = 3}]
diffText :: Text -> Text -> [Diff.Diff Text]
diffText before after =
  let groups = Diff.getGroupedDiff (Text.unpack before) (Text.unpack after)
   in bimap Text.pack Text.pack <$> groups
