{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE RecordWildCards #-}
module Unison.Test.CommandLine where

import EasyTest
import qualified System.Console.Haskeline        as Line
import Unison.CommandLine (completeWithinQueryNamespace)

data CompletionTest = CT {query :: String, expected :: [(String, Bool)], options :: [String]}
testCompletion :: (String -> [String] -> [Line.Completion]) -> CompletionTest -> Test ()
testCompletion compl CT{..}=
  expectEqual expected ((\(Line.Completion{..}) -> (replacement, isFinished)) <$> compl query options)

test :: Test ()
test = scope "commandline" $ do
  scope "completion" $ do
    scope "completeWithinQueryNamespace" $ do
      scope "only completes up to a single namespace boundary" $ do
        testCompletion completeWithinQueryNamespace $ CT { query=".ba"
                                                       , expected=[(".base", False)]
                                                       , options=[".base", ".base.List", ".base.Map"]
                                                       }
      scope "completes into the next namespace if query is a complete namespace" $ do
        testCompletion completeWithinQueryNamespace $ CT { query=".base"
                                                       , expected=[(".base", True), (".base.List", False), (".base.Map", False)]
                                                       , options=[".base", ".base.List", ".base.Map"]
                                                       }

      scope "completes " $ do
        testCompletion completeWithinQueryNamespace $ CT { query=".f"
                                                       , expected=[(".function", False), (".facade", False), (".fellows", False)]
                                                       , options=[".function", ".facade", ".fellows"]}
