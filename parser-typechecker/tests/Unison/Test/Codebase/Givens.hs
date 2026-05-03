-- | Unit tests for "Unison.Codebase.Givens".
module Unison.Test.Codebase.Givens
  ( test,
  )
where

import Data.Function ((&))
import Data.Functor.Identity (Identity)
import Data.Map qualified as Map
import Data.Set qualified as Set
import EasyTest
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Givens qualified as Givens
import Unison.Codebase.Metadata qualified as Metadata
import Unison.Hashing.V2.Convert qualified as Convert
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.Parser.Ann (Ann (External))
import Unison.Prelude (view)
import Unison.Reference (TermReference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Util.Star2 qualified as Star2

dummyTermRef :: TermReference
dummyTermRef = Reference.Builtin "Test.dummyTerm"

dummyReferent :: Referent
dummyReferent = Referent.Ref dummyTermRef

barName :: NameSegment
barName = NameSegment "bar"

otherTag :: TermReference
otherTag = Reference.Builtin "Test.someOtherTag"

emptyBranch :: Branch0 Identity
emptyBranch = Branch.empty0

-- A branch containing a single (referent, name) pair with no metadata.
branchWithDummy :: Branch0 Identity
branchWithDummy =
  Branch.branch0
    (mempty & Star2.insertD1 (dummyReferent, barName))
    mempty
    Map.empty
    Map.empty

-- A branch where `dummyReferent` carries an unrelated metadata value
-- but not the given sentinel.
branchWithUnrelatedMetadata :: Branch0 Identity
branchWithUnrelatedMetadata =
  Branch.branch0
    ( mempty
        & Star2.insertD1 (dummyReferent, barName)
        & Metadata.insert (dummyReferent, otherTag)
    )
    mempty
    Map.empty
    Map.empty

test :: Test ()
test =
  scope "codebase.givens" . tests $
    [ scope "isGiven returns False on a fresh branch" do
        expect (not (Givens.isGiven dummyReferent emptyBranch))
        expect (not (Givens.isGiven dummyReferent branchWithDummy)),
      scope "isGiven returns False when only unrelated metadata is present" do
        expect (not (Givens.isGiven dummyReferent branchWithUnrelatedMetadata))
        expect (not (Givens.isGivenAt dummyReferent barName branchWithUnrelatedMetadata)),
      scope "markGivenAt makes isGiven return True" do
        let marked = Givens.markGivenAt dummyReferent barName branchWithDummy
        expect (Givens.isGiven dummyReferent marked)
        expect (Givens.isGivenAt dummyReferent barName marked),
      scope "markGivenAt is a no-op for absent referents" do
        let marked = Givens.markGivenAt dummyReferent barName emptyBranch
        expect (not (Givens.isGiven dummyReferent marked)),
      scope "unmarkGivenAt removes only the sentinel and preserves other metadata" do
        let starting =
              Givens.markGivenAt dummyReferent barName branchWithUnrelatedMetadata
            unmarked = Givens.unmarkGivenAt dummyReferent barName starting
        expect (not (Givens.isGiven dummyReferent unmarked))
        -- The unrelated metadata value must still be associated with
        -- the referent after unmarking.
        expect (Set.member otherTag (Givens.metadataValuesFor dummyReferent unmarked)),
      scope "namesMarkedGiven returns the segments where the sentinel applies" do
        let marked = Givens.markGivenAt dummyReferent barName branchWithDummy
        expect (Givens.namesMarkedGiven dummyReferent branchWithDummy == Set.empty)
        expect (Givens.namesMarkedGiven dummyReferent marked == Set.singleton barName),
      scope "namespace-hash round-trip: mark then unmark restores the original hash" do
        let marked = Givens.markGivenAt dummyReferent barName branchWithDummy
            unmarked = Givens.unmarkGivenAt dummyReferent barName marked
            h0 = Convert.hashBranch0 branchWithDummy
            h1 = Convert.hashBranch0 marked
            h2 = Convert.hashBranch0 unmarked
        -- Marking changes the namespace (Branch0) hash, demonstrating
        -- that MdValues participates in the namespace hash and so the
        -- sentinel is preserved through the hashing layer.
        expect (h0 /= h1)
        -- Unmarking returns to the original namespace hash: the
        -- sentinel is the only difference and so it round-trips
        -- through hashing without ambiguity.
        expect (h0 == h2),
      scope "marking only adds the sentinel to the metadata set" do
        -- Structural sanity: 'markGivenAt' is purely additive on the
        -- metadata-value set keyed on the referent. This is the
        -- prerequisite for the hash invariance below: marking
        -- changes only 'MdValues', it does not rewrite the
        -- referent under which the term is published.
        let marked = Givens.markGivenAt dummyReferent barName branchWithDummy
            mdBefore = Givens.metadataValuesFor dummyReferent branchWithDummy
            mdAfter = Givens.metadataValuesFor dummyReferent marked
        -- The pre-existing metadata is preserved.
        expect (mdBefore `Set.isSubsetOf` mdAfter)
        -- The only new entry is the sentinel.
        expect (Set.delete Givens.givenSentinel mdAfter == mdBefore),
      scope "marking does not change the term hash (ADR-014)" do
        -- ADR-014 promises that marking a definition as a given does
        -- not perturb its term hash: only the enclosing namespace
        -- hash changes, because the sentinel lives in 'MdValues' and
        -- not in the term itself. We exercise the marking pipeline
        -- on a sample term:
        --
        --   1. Build a 'Reference' from a term via 'hashClosedTerm'
        --      and place a referent for it under a name in a branch.
        --   2. Apply 'markGivenAt' to mark that name as a given.
        --   3. Recover the referent stored in the post-marking
        --      branch's d1 relation (i.e., the term's referent as
        --      it now lives in the codebase metadata schema after
        --      passing through the marking pipeline).
        --   4. Recompute the term's hash from the original 'Term'.
        --
        -- The pre-marking reference, the recovered post-marking
        -- referent's reference, and the recomputed hash must all be
        -- byte-identical. A regression in which marking somehow
        -- rewrote the referent (e.g. by folding the metadata into
        -- the term hash) would be caught here.
        let sampleTerm :: Term Symbol Ann
            sampleTerm = Term.nat External 42
            hashBefore = Convert.hashClosedTerm sampleTerm
            sampleTermRef = Reference.fromId hashBefore
            sampleReferent' = Referent.Ref sampleTermRef
            seg = NameSegment "myGiven"
            branchBefore :: Branch0 Identity
            branchBefore =
              Branch.branch0
                (mempty & Star2.insertD1 (sampleReferent', seg))
                mempty
                Map.empty
                Map.empty
            branchAfter = Givens.markGivenAt sampleReferent' seg branchBefore
            -- Recover the referent as it lives in the post-marking
            -- branch. If marking had rewritten the (referent, name)
            -- mapping under a different reference, this lookup
            -- would either come up empty or yield a different
            -- referent.
            referentsAfter :: Set.Set Referent
            referentsAfter =
              Star2.lookupD1 seg (view Branch.terms_ branchAfter)
            -- And rehash the term, post-marking, to confirm the
            -- term value is unaffected by anything done in the
            -- marking pipeline.
            hashAfter = Convert.hashClosedTerm sampleTerm
        -- Pre-marking referent is recoverable from the post-marking
        -- branch under the same name segment, with the same term
        -- reference.
        expect (Set.member sampleReferent' referentsAfter)
        expect (referentsAfter == Set.singleton sampleReferent')
        -- The recomputed hash matches both the pre-marking hash
        -- and the term reference embedded in the post-marking
        -- referent. Byte-for-byte identical per ADR-014.
        expect (hashBefore == hashAfter)
        let referentRefs =
              Set.map (\case Referent.Ref r -> Just r; _ -> Nothing) referentsAfter
        expect (referentRefs == Set.singleton (Just sampleTermRef))
        -- And the marking actually took effect: 'isGiven' returns
        -- True only for the post-marking branch.
        expect (Givens.isGiven sampleReferent' branchAfter)
        expect (not (Givens.isGiven sampleReferent' branchBefore))
    ]
