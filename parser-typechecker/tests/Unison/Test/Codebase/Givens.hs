module Unison.Test.Codebase.Givens (test) where

import Data.Function ((&))
import Data.Functor.Identity (Identity)
import Data.Set qualified as Set
import EasyTest
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Givens qualified as Givens
import Unison.Codebase.Metadata qualified as Metadata
import Unison.Hashing.V2.Convert qualified as Hashing
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.Prelude (view)
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Util.Star2 qualified as Star2

test :: Test ()
test =
  scope "codebase.givens" $
    let r = Referent.Ref (Reference.Builtin "Test.dictionary")
        other = Referent.Ref (Reference.Builtin "Test.other")
        tag = Reference.Builtin "Test.unrelatedMetadata"
        a = NameSegment "a"
        b = NameSegment "b"
        missing = NameSegment "missing"
        original :: Branch.Branch0 Identity
        original =
          Branch.branch0
            (mempty & Star2.insertD1 (r, a) & Star2.insertD1 (r, b) & Star2.insertD1 (other, a) & Metadata.insert (r, tag))
            mempty
            mempty
            mempty
        marked = Givens.markGivenAt r a original
        unmarked = Givens.unmarkGivenAt r b marked
        hash = Hashing.hashBranch0
     in tests
          [ scope "unrelated-metadata" $ expect (not (Givens.isGiven r original)),
            scope "aliases-share-mark" $ do
              expect (Givens.isGivenAt r a marked)
              expect (Givens.isGivenAt r b marked)
              expectEqual (Givens.namesMarkedGiven r marked) (Set.fromList [a, b]),
            scope "conflicted-name-keeps-other-referent-independent" $
              expect (not (Givens.isGivenAt other a marked)),
            scope "missing-pairs-are-unchanged" $ do
              expectEqual (hash (Givens.markGivenAt r missing original)) (hash original)
              expectEqual (hash (Givens.markGivenAt other b original)) (hash original)
              expectEqual (hash (Givens.unmarkGivenAt r missing marked)) (hash marked),
            scope "term-identities-unchanged" $
              expectEqual (Star2.d1 (view Branch.terms_ marked)) (Star2.d1 (view Branch.terms_ original)),
            scope "mark-only-adds-tag" $
              expectEqual (Givens.metadataValuesFor r marked) (Set.fromList [tag, Givens.givenSentinel]),
            scope "namespace-hash-records-mark" $ do
              expect (hash marked /= hash original)
              expectEqual (hash unmarked) (hash original),
            scope "idempotence" $ do
              expectEqual (hash (Givens.markGivenAt r a marked)) (hash marked)
              expectEqual (hash (Givens.unmarkGivenAt r a unmarked)) (hash unmarked),
            scope "unmark-through-alias" $ do
              expect (not (Givens.isGivenAt r a unmarked))
              expect (not (Givens.isGivenAt r b unmarked))
              expectEqual (Givens.metadataValuesFor r unmarked) (Set.singleton tag)
          ]
