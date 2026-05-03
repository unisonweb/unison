{-# LANGUAGE OverloadedStrings #-}

-- | Round-trip tests that exercise marking a definition as a given via
-- 'Unison.Codebase.Givens', then saving and reloading the namespace
-- through a SQLite codebase.
module Unison.Test.Givens
  ( test,
  )
where

import Data.Function ((&))
import Data.Set qualified as Set
import EasyTest
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Givens qualified as Givens
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Test.Ucm qualified as Ucm

-- A builtin referent that does not require a separate term entry in
-- the codebase. We are exercising the namespace metadata pipeline
-- here, not term storage.
sampleReferent :: Referent
sampleReferent = Referent.Ref (Reference.Builtin "Nat.+")

sampleName :: NameSegment
sampleName = NameSegment "myGivenAlias"

mkMarkedBranch :: Branch m
mkMarkedBranch =
  let b0 =
        Branch.empty0
          & Branch.addTermName sampleReferent sampleName
          & Givens.markGivenAt sampleReferent sampleName
   in Branch.one b0

mkUnmarkedBranch :: Branch m
mkUnmarkedBranch =
  let b0 =
        Branch.empty0
          & Branch.addTermName sampleReferent sampleName
   in Branch.one b0

data RoundTripResult = RoundTripResult
  { reloadedMarkedIsGiven :: !Bool,
    reloadedMarkedIsGivenAt :: !Bool,
    reloadedUnmarkedIsGiven :: !Bool,
    reloadedMarkedSentinelInMd :: !Bool,
    -- | Whether the namespace hashes of the marked and unmarked
    -- branches differ (they should: the sentinel is in 'MdValues' which
    -- is part of the namespace hash).
    namespaceHashesDiffer :: !Bool
  }

doRoundTrip :: Ucm.Codebase -> IO RoundTripResult
doRoundTrip c = Ucm.lowLevel c \codebase -> do
  let marked = mkMarkedBranch
      unmarked = mkUnmarkedBranch
      markedH = Branch.headHash marked
      unmarkedH = Branch.headHash unmarked
  Codebase.putBranch codebase marked
  Codebase.putBranch codebase unmarked
  Just reloadedMarked <- Codebase.getBranchForHash codebase markedH
  Just reloadedUnmarked <- Codebase.getBranchForHash codebase unmarkedH
  let m0 = Branch.head reloadedMarked
      u0 = Branch.head reloadedUnmarked
  pure
    RoundTripResult
      { reloadedMarkedIsGiven = Givens.isGiven sampleReferent m0,
        reloadedMarkedIsGivenAt = Givens.isGivenAt sampleReferent sampleName m0,
        reloadedUnmarkedIsGiven = Givens.isGiven sampleReferent u0,
        reloadedMarkedSentinelInMd =
          Set.member Givens.givenSentinel (Givens.metadataValuesFor sampleReferent m0),
        namespaceHashesDiffer = markedH /= unmarkedH
      }

test :: Test ()
test = scope "givens.roundtrip" $ do
  c <- io $ Ucm.initCodebase Ucm.CodebaseFormat2
  result <- io $ doRoundTrip c
  io $ Ucm.deleteCodebase c
  scope "given sentinel survives save/load" do
    expect (reloadedMarkedIsGiven result)
    expect (reloadedMarkedIsGivenAt result)
  scope "unmarked branch reloads as not-given" do
    expect (not (reloadedUnmarkedIsGiven result))
  scope "sentinel survives metadata round-trip" do
    expect (reloadedMarkedSentinelInMd result)
  scope "namespace hash changes when sentinel is added (ADR-013)" do
    expect (namespaceHashesDiffer result)
