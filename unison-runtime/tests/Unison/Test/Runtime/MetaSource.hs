-- | Smoke tests for "Unison.Runtime.MetaSource".
--
-- The embedded Unison source string in 'MetaSource' is parsed + typechecked
-- lazily, so a clean Haskell build only verifies that the wrapper module
-- compiles — not that the source actually parses, typechecks, and resolves
-- to the GUID-pinned hashes we expect. These tests force evaluation so any
-- regression there fails the test suite instead of slipping into a runtime
-- error at first use.
module Unison.Test.Runtime.MetaSource (test) where

import Control.Exception (evaluate, try, SomeException)
import EasyTest
import Unison.Runtime.MetaSource qualified as M

test :: Test ()
test =
  scope "MetaSource" do
    scope "embedded source parses and typechecks" do
      -- Force the typechecked file; any parse or typecheck failure throws
      -- via the `error` calls inside MetaSource.
      r <- io . try @SomeException . evaluate $ M.typecheckedFile `seq` ()
      case r of
        Left e -> crash ("typecheckedFile threw: " <> show e)
        Right () -> ok

    scope "all 14 type references resolve" do
      let refs =
            [ M.nameRef,
              M.hashRef,
              M.referenceRef,
              M.constructorReferenceRef,
              M.referentRef,
              M.literalRef,
              M.seqOpRef,
              M.patternRef,
              M.kindRef,
              M.matchCaseRef,
              M.abtRef,
              M.termRef,
              M.termFRef,
              M.typeFRef
            ]
      r <- io . try @SomeException . evaluate $ foldr seq () refs
      case r of
        Left e -> crash ("forcing type refs threw: " <> show e)
        Right () -> ok

    scope "constructor IDs resolve by name" do
      -- Spot-check the constructor IDs we'll use from the decompile builtin.
      -- If any of these names doesn't exist on its parent type, the
      -- `constructorNamed` call inside MetaSource calls `error`.
      let cids =
            [ M.nameNameId,
              M.hashHashId,
              M.referenceBuiltinId,
              M.referenceDerivedId,
              M.constructorReferenceCtorId,
              M.referentRefRefId,
              M.referentRefConId,
              M.litNatId,
              M.litIntId,
              M.litTextId,
              M.litCharId,
              M.litFloatId,
              M.litBooleanId,
              M.litBytesId,
              M.seqOpPConsId,
              M.seqOpPSnocId,
              M.seqOpPConcatId,
              M.kindKStarId,
              M.kindKArrowId,
              M.matchCaseCtorId,
              M.abtVarId,
              M.abtAbsId,
              M.abtCycleId,
              M.abtTmId,
              M.termTermId,
              M.termFAppId,
              M.termFLamId,
              M.termFLetId,
              M.termFLetRecId,
              M.termFIfId,
              M.termFMatchId,
              M.termFHandleId,
              M.termFAnnId,
              M.termFRefId,
              M.termFConstructorId,
              M.termFRequestId,
              M.termFLitId,
              M.termFListId,
              M.termFTermLinkId,
              M.termFTypeLinkId,
              M.typeFArrowId,
              M.typeFImplicitArrowId,
              M.typeFAppId,
              M.typeFEffectId,
              M.typeFEffectsId,
              M.typeFForallId,
              M.typeFIntroOuterId,
              M.typeFRefId,
              M.typeFAnnId,
              M.patternPUnboundId,
              M.patternPVarId,
              M.patternPBooleanId,
              M.patternPIntId,
              M.patternPNatId,
              M.patternPFloatId,
              M.patternPTextId,
              M.patternPCharId,
              M.patternPBytesId,
              M.patternPConstructorId,
              M.patternPAsId,
              M.patternPEffectPureId,
              M.patternPEffectBindId,
              M.patternPSequenceLiteralId,
              M.patternPSequenceOpId
            ]
      r <- io . try @SomeException . evaluate $ foldr seq () cids
      case r of
        Left e -> crash ("forcing constructor IDs threw: " <> show e)
        Right () -> ok
