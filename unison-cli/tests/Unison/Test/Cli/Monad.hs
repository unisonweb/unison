module Unison.Test.Cli.Monad
  ( test,
  )
where

import Control.Lens
import EasyTest
import Unison.Cli.Monad qualified as Cli
import Unison.Codebase.Editor.StructuredArgument qualified as SA
import Unison.Syntax.Name qualified as Name

test :: Test ()
test =
  (scope "Unison.Cli.Monad" . tests)
    [ scope "label" do
        (r, state) <-
          io do
            Cli.runCli dummyEnv dummyLoopState do
              Cli.label \goto -> do
                Cli.label \_ -> do
                  Cli.setNumberedArgs [SA.Name $ Name.unsafeParseText "foo"]
                  goto (1 :: Int)
                pure 2
        -- test that 'goto' short-circuits, as expected
        expectEqual' (Cli.Success 1) r
        -- test that calling 'goto' doesn't lose state changes made along the way
        expectEqual'
          [SA.Name $ Name.unsafeParseText "foo"]
          (state ^. #numberedArgs)
        ok
    ]

dummyEnv :: Cli.Env
dummyEnv = undefined

dummyLoopState :: Cli.LoopState
dummyLoopState =
  Cli.LoopState
    { projectPathStack = undefined,
      latestFile = Nothing,
      latestTypecheckedFile = Nothing,
      lastInput = Nothing,
      numberedArgs = [],
      lastRunResult = Nothing
    }
