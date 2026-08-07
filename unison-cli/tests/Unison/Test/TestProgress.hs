module Unison.Test.TestProgress
  ( test,
  )
where

import Control.Monad.Writer.Strict (runWriter, tell)
import EasyTest
import Unison.Codebase.Editor.HandleInput.Tests qualified as Tests
import Unison.CommandLine.OutputMessages qualified as OutputMessages

test :: Test ()
test =
  (scope "test-progress" . tests)
    [ scope "reports progress in order with correct counters" do
        let testRefs :: [String]
            testRefs = ["a", "b", "c"]
            runTest _ = pure ([], ["ok"])
            (_, events) =
              runWriter $
                Tests.runIOTestsWithProgress
                  (\progress ref -> tell [Started progress ref])
                  runTest
                  (\progress ref isOk -> tell [Finished progress ref isOk])
                  testRefs
        expectEqual
          [ Started (0, 3) "a",
            Finished (0, 3) "a" True,
            Started (1, 3) "b",
            Finished (1, 3) "b" True,
            Started (2, 3) "c",
            Finished (2, 3) "c" True
          ]
          events,
      scope "reports tests remaining after the current test" do
        expectEqual
          [3, 2, 1]
          (OutputMessages.testsRemainingAfterCurrent <$> [(0, 3), (1, 3), (2, 3)])
    ]

data ProgressEvent r
  = Started (Int, Int) r
  | Finished (Int, Int) r Bool
  deriving (Eq, Show)
