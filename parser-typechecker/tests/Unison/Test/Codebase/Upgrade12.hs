{-# LANGUAGE TemplateHaskell #-}
{-# Language QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Codebase.Upgrade12 (test) where

import Data.Functor (void)
import Data.String.Here.Interpolated (i)
import EasyTest (Test, io, ok, scope, tests)
import Shellmet ()
import qualified Unison.Test.Ucm as Ucm

test :: Test ()
test = scope "codebase.upgrade12" $ tests [
  scope "typeAlias" do
    void $ io do
      c1 <- Ucm.initCodebase Ucm.CodebaseFormat1
      Ucm.runTranscript c1 Ucm.Runtime1 [i|
          ```ucm
          .> alias.type ##Nat builtin.Nat
          .> history
          .> history builtin
          ```
        |]
      c2 <- Ucm.upgradeCodebase c1
      Ucm.runTranscript c2 Ucm.Runtime1 [i|
          ```unison
          x : Nat
          x = 3
          ```
        |]
    ok,

  scope "topLevelTerm" do
    void $ io do
      c1 <- Ucm.initCodebase Ucm.CodebaseFormat1
      Ucm.runTranscript c1 Ucm.Runtime1 [i|
          ```unison:hide
          y = 3
          ```
          ```ucm
          .> add
          ```
        |]
      c2 <- Ucm.upgradeCodebase c1
      Ucm.runTranscript c2 Ucm.Runtime1 [i|
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
      Ucm.runTranscript c1 Ucm.Runtime1 ""
      Ucm.runTranscript c1 Ucm.Runtime1 [i|
          ```unison:hide
          doc = "y is the number 3"
          y = 3
          ```
          ```ucm
          .> add
          .> link doc y
          ```
        |]
      Ucm.runTranscript c1 Ucm.Runtime1 [i|
        ```ucm
        .> links y
        ```
      |]
      c2 <- Ucm.upgradeCodebase c1
      Ucm.runTranscript c2 Ucm.Runtime1 [i|
        ```ucm
        .> links y
        ```
      |]
    ok,

  scope "metadataForType" do
    void $ io do
      c1 <- Ucm.initCodebase Ucm.CodebaseFormat1
      Ucm.runTranscript c1 Ucm.Runtime1 [i|
          ```unison:hide
          doc = "Nat means natural number"
          ```
          ```ucm
          .> alias.type ##Nat Nat
          .> link doc Nat
          ```
        |]
      c2 <- Ucm.upgradeCodebase c1
      Ucm.runTranscript c2 Ucm.Runtime1 [i|
        ```ucm
        .> docs y
        ```
      |]
    ok,

  scope "subNamespace" do
    void $ io do
      c1 <- Ucm.initCodebase Ucm.CodebaseFormat1
      Ucm.runTranscript c1 Ucm.Runtime1 [i|
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
      Ucm.runTranscript c2 Ucm.Runtime1 [i|
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
      Ucm.runTranscript c1 Ucm.Runtime1 [i|
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
      Ucm.runTranscript c2 Ucm.Runtime1 [i|
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
      Ucm.runTranscript c1 Ucm.Runtime1 [i|
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
      Ucm.runTranscript c2 Ucm.Runtime1 [i|
          ```ucm
          .> history
          .> reset-root #ls8
          .> history
          ```
        |]
    ok
  ]