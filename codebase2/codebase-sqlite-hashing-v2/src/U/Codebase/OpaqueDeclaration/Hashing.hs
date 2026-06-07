module U.Codebase.OpaqueDeclaration.Hashing
  ( verifyOpaqueDeclarationFormatHash,
  )
where

import U.Codebase.HashTags
import U.Codebase.OpaqueDeclaration qualified as C.OpaqueDeclaration
import U.Codebase.Sqlite.HashHandle (HashMismatch (..))
import U.Codebase.Sqlite.HashHandle qualified as HH
import U.Codebase.Sqlite.OpaqueDeclaration.Format qualified as OpaqueDeclarationFormat
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.Symbol qualified as S
import U.Codebase.Type qualified as C.Type
import Unison.Hash32 qualified as Hash32
import Unison.Hashing.V2 qualified as H2
import Unison.Hashing.V2.Convert2 qualified as H2
import Unison.Prelude
import Unison.Symbol qualified as Unison
import Unison.Var qualified as Var

-- | Verify that a stored opaque declaration hashes to its expected component
-- hash.
--
-- Opaque declarations are single-element components (no recursion), so this is
-- structurally simpler than 'verifyDeclFormatHash': we resolve the local refs
-- against the lookup vectors, convert to the v1 hashing representation, hash,
-- and compare.
verifyOpaqueDeclarationFormatHash ::
  ComponentHash ->
  OpaqueDeclarationFormat.HashOpaqueDeclarationFormat ->
  Maybe HH.OpaqueDeclarationHashingError
verifyOpaqueDeclarationFormatHash (ComponentHash expected) hashFmt =
  let resolved = resolveRefs hashFmt
      h2od = H2.v2ToH2OpaqueDeclaration (C.OpaqueDeclaration.vmap symbol2to1 resolved)
      H2.ReferenceId actual _ = H2.hashOpaqueDeclaration h2od
   in if expected == actual
        then Nothing
        else Just (HH.OpaqueDeclarationHashMismatch (HashMismatch expected actual))
  where
    symbol2to1 :: S.Symbol -> Unison.Symbol
    symbol2to1 (S.Symbol i t) = Unison.Symbol i (Var.User t)

-- | Resolve the local-id references in the RHS against the opaque
-- declaration's lookup vectors, producing a body with fully-qualified
-- ('Reference') refs.
resolveRefs :: OpaqueDeclarationFormat.HashOpaqueDeclarationFormat -> C.OpaqueDeclaration.OpaqueDeclaration S.Symbol
resolveRefs (OpaqueDeclarationFormat.OpaqueDeclaration ids (C.OpaqueDeclaration.OpaqueDeclarationR modifier params rhs)) =
  let Identity (substText, substHash) =
        Q.localIdsToLookups Identity pure (bimap id Hash32.toHash ids)
   in C.OpaqueDeclaration.OpaqueDeclarationR modifier params (C.Type.rmap (bimap substText substHash) rhs)
