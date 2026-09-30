module U.Codebase.Sqlite.HashHandle
  ( HashHandle (..),
    HashMismatch (..),
    HashValidationError (..),
    DeclHashingError (..),
    TypeAliasHashingError (..),
    HashingFailure (..),
    crashOnHashingFailure,
  )
where

import Control.Exception
import U.Codebase.Branch.Type (Branch)
import U.Codebase.BranchV3 (BranchV3)
import U.Codebase.HashTags
import U.Codebase.Reference qualified as C
import U.Codebase.Sqlite.Branch.Format (HashBranchLocalIds)
import U.Codebase.Sqlite.Branch.Full (LocalBranch)
import U.Codebase.Sqlite.Decl.Format qualified as DeclFormat
import U.Codebase.Sqlite.Patch.Format (HashPatchLocalIds)
import U.Codebase.Sqlite.Patch.Full (LocalPatch)
import U.Codebase.Sqlite.Symbol (Symbol)
import U.Codebase.Sqlite.Term.Format qualified as TermFormat
import U.Codebase.Sqlite.TypeAlias.Format qualified as TypeAliasFormat
import U.Codebase.Term qualified as C.Term
import U.Codebase.Type qualified as C.Type
import Unison.Hash (Hash)
import Unison.Prelude

data HashMismatch = HashMismatch
  { expectedHash :: Hash,
    actualHash :: Hash
  }

data HashingFailure
  = -- | two or more component elements can not be completely ordered with respect to one another
    -- https://github.com/unisonweb/unison/issues/2787
    IncompleteElementOrderingError ComponentHash
  deriving (Eq, Ord)
  deriving anyclass (Exception)

instance Show HashingFailure where
  show hf = reportBug "E253299" (renderHashingFailure hf)
    where
      renderHashingFailure :: HashingFailure -> String
      renderHashingFailure = \case
        IncompleteElementOrderingError h ->
          unlines
            [ "Failed to hash the component: " <> show h,
              "Hashing failed because cyclic definitions because the definitions could not be completely ordered.",
              "This happens when multiple definitions in a mutually recursive cycle are identical except",
              "for references to other elements in the same cycle.",
              "If all elements are identical, consider simple recursion instead of mutual recursion,",
              "If mutual recursion is required, you may disambiguate identical definitions by",
              "adding a dummy comment like:",
              "_ = \"this is the foo definition\""
            ]

-- | We don't expect to encounter these, but if we do we should print a nice message.
--
-- In the future we will hopefully prevent this error entirely.
crashOnHashingFailure :: (HasCallStack) => Either HashingFailure a -> a
crashOnHashingFailure = \case
  Left hf -> throw hf
  Right a -> a

data HashValidationError
  = HashValidationMismatch HashMismatch
  | HashingFailure HashingFailure

data DeclHashingError
  = DeclHashMismatch HashMismatch
  | DeclHashResolutionFailure

data TypeAliasHashingError
  = TypeAliasHashMismatch HashMismatch
  | TypeAliasHashResolutionFailure

data HashHandle = HashHandle
  { -- | Hash type
    toReference :: C.Term.Type Symbol -> C.Reference,
    -- | Hash type's mentions
    toReferenceMentions :: C.Term.Type Symbol -> Set C.Reference,
    -- | Hash the type of a single constructor in a decl component. The provided hash argument is the hash of the decl component.
    toReferenceDecl :: Hash -> C.Type.TypeD Symbol -> C.Reference,
    -- | Hash decl's mentions
    toReferenceDeclMentions :: Hash -> C.Type.TypeD Symbol -> Set C.Reference,
    hashBranch :: forall m. (Monad m) => Branch m -> m BranchHash,
    hashBranchV3 :: forall m. BranchV3 m -> BranchHash,
    hashCausal ::
      -- The causal's namespace hash
      BranchHash ->
      -- The causal's parents
      Set CausalHash ->
      CausalHash,
    hashBranchFormatFull ::
      HashBranchLocalIds ->
      LocalBranch ->
      BranchHash,
    hashPatchFormatFull ::
      HashPatchLocalIds ->
      LocalPatch ->
      PatchHash,
    verifyTermFormatHash ::
      ComponentHash ->
      TermFormat.HashTermFormat ->
      Maybe HashValidationError,
    verifyDeclFormatHash ::
      ComponentHash ->
      DeclFormat.HashDeclFormat ->
      Maybe DeclHashingError,
    verifyTypeAliasFormatHash ::
      ComponentHash ->
      TypeAliasFormat.HashTypeAliasFormat ->
      Maybe TypeAliasHashingError
  }
