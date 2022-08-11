{-# LANGUAGE RecordWildCards #-}

module Unison.Test.CommandLine where

import EasyTest
import qualified System.Console.Haskeline as Line
import Unison.CommandLine.Completion

data CompletionTest = CT {query :: String, expected :: [(String, Bool)], options :: [String]}

testCompletion :: (String -> [String] -> [Line.Completion]) -> CompletionTest -> Test ()
testCompletion compl CT {..} =
  expectEqual expected ((\(Line.Completion {..}) -> (replacement, isFinished)) <$> compl query options)

test :: Test ()
test = scope "commandline" $ do
  scope "completion" $ do
    scope "prefixCompletionFilter" $ do
      testCompletion prefixCompletionFilter $
        CT
          { query = ".ba",
            expected = [(".base", False), (".base.List", False), (".bar", False)],
            options = [".base", ".base.List", ".bar", ".other", ".bx"]
          }
    scope "fuzzySuffixSegmentCompletionFilter" $ do
      testCompletion fuzzySuffixSegmentCompletionFilter $
        CT
          { query = ".base.map",
            expected = [(".base.map", False), (".base.filterMap", False), (".mapMaybe", False)],
            options = [".base.filter", ".base.map", ".base.filterMap", ".base.mapMaybe", ".map", ".other.base.map"]
          }
