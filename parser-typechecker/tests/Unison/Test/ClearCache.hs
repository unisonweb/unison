{-# Language OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Test.ClearCache where

import Data.Foldable (for_)
import Data.String.Here (i)
import EasyTest
import qualified Unison.Test.Ucm as Ucm
import qualified Unison.Codebase as Codebase
import qualified Unison.Var as WatchKind

test :: Test ()
test = scope "clearWatchCache" $
  for_ [minBound .. maxBound] \fmt -> scope (show fmt) do
    c <- io $ Ucm.initCodebase fmt
    let listWatches = io $ Ucm.lowLevel c \c ->
          Codebase.watches c WatchKind.RegularWatch

    io $ Ucm.runTranscript c [i|
      ```ucm
      .> alias.term ##Nat.+ +
      ```
      ```unison
      > 1 + 1
      ```
    |]

    beforeClear <- listWatches
    expectNotEqual beforeClear []

    io $ Ucm.runTranscript c [i|
      ```ucm
      .> debug.clear-cache
      ```
    |]

    afterClear <- listWatches
    expectEqual afterClear []