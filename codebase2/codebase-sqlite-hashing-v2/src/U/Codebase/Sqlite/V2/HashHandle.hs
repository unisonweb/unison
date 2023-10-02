module U.Codebase.Sqlite.V2.HashHandle
  ( v2HashHandle,
  )
where

import Data.Set qualified as Set
import U.Codebase.Branch (Branch)
import U.Codebase.Branch.Hashing qualified as H2
import U.Codebase.BranchV3 (BranchV3)
import U.Codebase.Causal.Hashing qualified as H2
import U.Codebase.HashTags (BranchHash (..), CausalHash)
import U.Codebase.Reference (Reference)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.Branch.Format (HashBranchLocalIds)
import U.Codebase.Sqlite.Branch.Format qualified as BranchFormat
import U.Codebase.Sqlite.Branch.Full (LocalBranch)
import U.Codebase.Sqlite.HashHandle (HashHandle (HashHandle))
import U.Codebase.Sqlite.HashHandle qualified
import U.Codebase.Sqlite.Symbol (Symbol)
import U.Codebase.Term (ClosedTerm)
import U.Codebase.Type (TypeD, TypeT)
import U.Core.ABT.Var (Var)
import U.Util.Type (removeAllEffectVars)
import Unison.Hash (Hash)
import Unison.Hashing.V2 qualified as H2
import Unison.Hashing.V2.Convert2 qualified as Convert
import Unison.Prelude

v2HashHandle :: HashHandle
v2HashHandle =
  HashHandle
    { hashBranch,
      hashBranchV3,
      hashBranchFormatFull,
      hashCausal,
      hashClosedTerm,
      toReference,
      toReferenceDecl,
      toReferenceDeclMentions,
      toReferenceMentions
    }

hashBranch :: Monad m => Branch m -> m BranchHash
hashBranch =
  H2.hashBranch

hashBranchV3 :: BranchV3 m -> BranchHash
hashBranchV3 =
  H2.hashBranchV3

hashBranchFormatFull :: HashBranchLocalIds -> LocalBranch -> BranchHash
hashBranchFormatFull = \localIds localBranch ->
  BranchFormat.localToHashBranch localIds localBranch
    & Convert.hashBranchFormatToH2Branch
    & H2.contentHash
    & BranchHash

hashCausal :: BranchHash -> Set CausalHash -> CausalHash
hashCausal =
  H2.hashCausal

hashClosedTerm :: ClosedTerm Symbol -> Reference.Id
hashClosedTerm =
  Convert.h2ToV2ReferenceId . H2.hashClosedTerm . Convert.v2ToH2Term

toReference :: (Show v, Var v) => TypeT v -> Reference
toReference =
  Convert.h2ToV2Reference . H2.typeToReference . Convert.v2ToH2Type . removeAllEffectVars

toReferenceDecl :: (Show v, Var v) => Hash -> TypeD v -> Reference
toReferenceDecl h =
  Convert.h2ToV2Reference . H2.typeToReference . Convert.v2ToH2TypeD h . removeAllEffectVars

toReferenceMentions :: TypeT Symbol -> Set Reference
toReferenceMentions =
  Set.map Convert.h2ToV2Reference . H2.typeToReferenceMentions . Convert.v2ToH2Type . removeAllEffectVars

toReferenceDeclMentions :: (Show v, Var v) => Hash -> TypeD v -> Set Reference
toReferenceDeclMentions h =
  Set.map Convert.h2ToV2Reference . H2.typeToReferenceMentions . Convert.v2ToH2TypeD h . removeAllEffectVars
