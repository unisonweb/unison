{-# Language OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Unison.Test.ClearCache where

import Data.Foldable (for_)
import Data.List.Extra (enumerate)
import Data.String.Here (i)
import EasyTest
import qualified Unison.Codebase as Codebase
import qualified Unison.Test.Ucm as Ucm
import qualified Unison.WatchKind as WatchKind

test :: Test ()
test = scope "clearWatchCache" $
  for_ enumerate \codebaseFormat -> scope (show codebaseFormat) do
    c <- io $ Ucm.initCodebase codebaseFormat
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