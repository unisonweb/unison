module U.Codebase.Sqlite.V2.HashHandle
  ( v2HashHandle,
  )
where

import Data.Function ((&))
import Data.Set qualified as Set
import U.Codebase.Branch.Hashing qualified as H2
import U.Codebase.Causal.Hashing qualified as H2
import U.Codebase.Term.Hashing qualified as H2
import U.Codebase.HashTags (BranchHash (..))
import U.Codebase.Sqlite.Branch.Format qualified as BranchFormat
import U.Codebase.Sqlite.HashHandle
import U.Util.Type (removeAllEffectVars)
import Unison.Hashing.V2 qualified as H2
import Unison.Hashing.V2.Convert2 (h2ToV2Reference, hashBranchFormatToH2Branch, v2ToH2Type, v2ToH2TypeD)

v2HashHandle :: HashHandle
v2HashHandle =
  HashHandle
    { toReference = h2ToV2Reference . H2.typeToReference . v2ToH2Type . removeAllEffectVars,
      toReferenceMentions = Set.map h2ToV2Reference . H2.typeToReferenceMentions . v2ToH2Type . removeAllEffectVars,
      toReferenceDecl = \h -> h2ToV2Reference . H2.typeToReference . v2ToH2TypeD h . removeAllEffectVars,
      toReferenceDeclMentions = \h -> Set.map h2ToV2Reference . H2.typeToReferenceMentions . v2ToH2TypeD h . removeAllEffectVars,
      hashBranch = H2.hashBranch,
      hashCausal = H2.hashCausal,
      hashBranchFormatFull = \localIds localBranch ->
        BranchFormat.localToHashBranch localIds localBranch
          & hashBranchFormatToH2Branch
          & H2.contentHash
          & BranchHash,
      verifyTermFormatHash = H2.verifyTermFormatHash
    }
