{-# LANGUAGE TemplateHaskell #-}
{-# Language QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}


module Unison.Test.Codebase.Upgrade12 (test) where

import Data.Functor (void)
import Data.String.Here.Interpolated (i)
import EasyTest (Test, expectJust, io, ok, scope, tests)
import Shellmet ()
import qualified Unison.Codebase as Codebase
import qualified Unison.Test.Ucm as Ucm
import Unison.UnisonFile (pattern TestWatch)
import Debug.Trace (traceShowM)

test :: Test ()
test = scope "codebase.upgrade12" $ tests [
  scope "typeAlias" do
    void $ io do
      c1 <- Ucm.initCodebase Ucm.CodebaseFormat1
      Ucm.runTranscript c1 [i|
          ```ucm
          .> alias.type ##Nat builtin.Nat
          .> history
          .> history builtin
          ```
        |]
      c2 <- Ucm.upgradeCodebase c1
      Ucm.runTranscript c2 [i|
          ```unison
          x : Nat
          x = 3
          ```
        |]
    ok,

  scope "topLevelTerm" do
    void $ io do
      c1 <- Ucm.initCodebase Ucm.CodebaseFormat1
      Ucm.runTranscript c1 [i|
          ```unison:hide
          y = 3
          ```
          ```ucm
          .> add
          ```
        |]
      c2 <- Ucm.upgradeCodebase c1
      Ucm.runTranscript c2 [i|
        ```ucm
        .> find
        ```
        ```unison
        > y
        ```
      |]
    ok,

  scope "metadataForTerm" do
    void $ io do
      c1 <- Ucm.initCodebase Ucm.CodebaseFormat1
      Ucm.runTranscript c1 ""
      Ucm.runTranscript c1 [i|
          ```unison:hide
          doc = "y is the number 3"
          y = 3
          ```
          ```ucm
          .> debug.file
          .> add
          .> link doc y
          .> links y
          .> history
          ```
        |]
        -- 8bbb doc
        -- mps7 y
        -- ttjf post-link
        -- 988m pre-link
        -- 7asf empty
      Ucm.runTranscript c1 [i|
        ```ucm
        .> links y
        ```
      |]
      c2 <- Ucm.upgradeCodebase c1
      Ucm.runTranscript c2 [i|
        ```ucm
        .> links y
        ```
      |]
    ok,

  scope "metadataForType" do
    void $ io do
      c1 <- Ucm.initCodebase Ucm.CodebaseFormat1
      Ucm.runTranscript c1 [i|
          ```unison:hide
          doc = "Nat means natural number"
          ```
          ```ucm
          .> add
          .> alias.type ##Nat Nat
          .> link doc Nat
          ```
        |]
      c2 <- Ucm.upgradeCodebase c1
      Ucm.runTranscript c2 [i|
        ```ucm
        .> links Nat
        ```
      |]
    ok,

  scope "subNamespace" do
    void $ io do
      c1 <- Ucm.initCodebase Ucm.CodebaseFormat1
      Ucm.runTranscript c1 [i|
          ```ucm
          .> alias.type ##Nat builtin.Nat
          ```
          ```unison
          unique type a.b.C = C Nat
          ```
          ```ucm
          .> add
          ```
        |]
      c2 <- Ucm.upgradeCodebase c1
      Ucm.runTranscript c2 [i|
        ```ucm
        .> find
        ```
        ```unison
        > a.b.C.C 3
        ```
      |]
    ok,

  scope "accessPatch" do
    void $ io do
      c1 <- Ucm.initCodebase Ucm.CodebaseFormat1
      Ucm.runTranscript c1 [i|
          ```ucm
          .> alias.type ##Nat builtin.Nat
          ```
          ```unison:hide
          unique type A = A Nat
          foo = A.A 3
          ```
          ```ucm
          .> debug.file
          .> add
          ```
          ```unison:hide
          unique type A = A Nat Nat
          foo = A.A 3 3
          ```
          ```ucm
          .> debug.file
          .> update
          ```
          ```ucm
          .> view.patch patch
          ```
        |]
      c2 <- Ucm.upgradeCodebase c1
      Ucm.runTranscript c2 [i|
        ```ucm
        .> view.patch patch
        ```
      |]
    ok,

-- #00k3c9bp6m A
-- #6v94dtbfk1 foo
-- #d3bn4dqp1a A'
-- #p3a21bjjl4 foo'

  scope "history" do
    void $ io do
      c1 <- Ucm.initCodebase Ucm.CodebaseFormat1
      Ucm.runTranscript c1 [i|
          ```unison
          foo = 3
          ```
          ```ucm
          .> add
          ```
          ```unison
          foo = 4
          ```
          ```ucm
          .> update
          .> history
          ```
        |]
      c2 <- Ucm.upgradeCodebase c1
      Ucm.runTranscript c2 [i|
          ```ucm
          .> history
          .> reset-root #ls8
          .> history
          ```
        |]
    ok,

  scope "test-watches" do
    (watchTerms1, watchTerms2) <- io do
      c1 <- Ucm.initCodebase Ucm.CodebaseFormat1
      Ucm.runTranscript c1 [i|
        ```ucm
        .> builtins.merge
        ```
        ```unison
        test> pass = [Ok "Passed"]
        ```
        ```ucm
        .> add
        ```
      |]
      (watches1, watchTerms1) <- Ucm.lowLevel c1 \c1' -> do
        watches1@(_:_) <- Codebase.watches c1' TestWatch
        watchTerms1 <- traverse (Codebase.getWatch c1' TestWatch) watches1
        pure (watches1, watchTerms1)
      Ucm.runTranscript c1 [i|
        ```unison
        test> pass = [Ok "Passed"]
        ```
      |]
      c2 <- Ucm.upgradeCodebase c1
      watchTerms2 <- Ucm.lowLevel c2 \c2' ->
        traverse (Codebase.getWatch c2' TestWatch) watches1
      traceShowM watches1
      traceShowM watchTerms1
      traceShowM watchTerms2
      pure (watchTerms1, watchTerms2)
    expectJust (sequence watchTerms1)
    expectJust (sequence watchTerms2)
    ok
  ]
