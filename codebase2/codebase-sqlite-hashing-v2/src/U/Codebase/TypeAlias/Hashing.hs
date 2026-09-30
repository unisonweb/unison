module U.Codebase.TypeAlias.Hashing
  ( verifyTypeAliasFormatHash,
  )
where

import U.Codebase.HashTags
import U.Codebase.Sqlite.HashHandle (HashMismatch (..))
import U.Codebase.Sqlite.HashHandle qualified as HH
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.Symbol qualified as S
import U.Codebase.Sqlite.TypeAlias.Format qualified as TypeAliasFormat
import U.Codebase.Type qualified as C.Type
import U.Codebase.TypeAlias qualified as C.TypeAlias
import Unison.Hash32 qualified as Hash32
import Unison.Hashing.V2 qualified as H2
import Unison.Hashing.V2.Convert2 qualified as H2
import Unison.Prelude
import Unison.Symbol qualified as Unison
import Unison.Var qualified as Var

-- | Verify that a stored type alias hashes to its expected component hash.
--
-- Aliases are single-element components (no recursion), so this is structurally
-- simpler than 'verifyDeclFormatHash': we resolve the local refs against the
-- lookup vectors, convert to the v1 hashing representation, hash, and compare.
verifyTypeAliasFormatHash ::
  ComponentHash ->
  TypeAliasFormat.HashTypeAliasFormat ->
  Maybe HH.TypeAliasHashingError
verifyTypeAliasFormatHash (ComponentHash expected) hashFmt =
  let resolved = resolveRefs hashFmt
      h2alias = H2.v2ToH2TypeAlias (C.TypeAlias.vmap symbol2to1 resolved)
      H2.ReferenceId actual _ = H2.hashTypeAlias h2alias
   in if expected == actual
        then Nothing
        else Just (HH.TypeAliasHashMismatch (HashMismatch expected actual))
  where
    symbol2to1 :: S.Symbol -> Unison.Symbol
    symbol2to1 (S.Symbol i t) = Unison.Symbol i (Var.User t)

-- | Resolve the local-id references in the body against the alias's lookup
-- vectors, producing a body with fully-qualified ('Reference') refs.
resolveRefs :: TypeAliasFormat.HashTypeAliasFormat -> C.TypeAlias.TypeAlias S.Symbol
resolveRefs (TypeAliasFormat.TypeAlias ids (C.TypeAlias.TypeAliasR params body)) =
  let Identity (substText, substHash) =
        Q.localIdsToLookups Identity pure (bimap id Hash32.toHash ids)
   in C.TypeAlias.TypeAliasR params (C.Type.rmap (bimap substText substHash) body)
