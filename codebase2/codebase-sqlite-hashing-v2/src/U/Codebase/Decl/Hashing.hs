module U.Codebase.Decl.Hashing where

import Control.Lens
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import U.Codebase.Decl qualified as C
import U.Codebase.Decl qualified as C.Decl
import U.Codebase.HashTags
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.Decl.Format qualified as DeclFormat
import U.Codebase.Sqlite.HashHandle (HashMismatch (..))
import U.Codebase.Sqlite.HashHandle qualified as HH
import U.Codebase.Sqlite.LocalIds qualified as LocalIds
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.Symbol qualified as S
import U.Codebase.Sqlite.Symbol qualified as Sqlite
import Unison.Hash (Hash)
import Unison.Hash32
import Unison.Hash32 qualified as Hash32
import Unison.Hashing.V2 qualified as H2
import Unison.Hashing.V2.Convert2 qualified as H2
import Unison.Prelude
import Unison.Symbol qualified as Unison
import Unison.Syntax.Name qualified as Name
import Unison.Var qualified as Var

verifyDeclFormatHash :: ComponentHash -> DeclFormat.HashDeclFormat -> Maybe HH.DeclHashingError
verifyDeclFormatHash (ComponentHash hash32) (DeclFormat.Decl (DeclFormat.LocallyIndexedComponent elements)) =
  Foldable.toList elements
    & fmap s2cDecl
    & Reference.component hash
    & fmap (\(decl, refId) -> (refId, (C.Decl.vmap symbol2to1 decl, ())))
    & Map.fromList
    & C.Decl.unhashComponent hash Var.unnamedRef
    & Map.toList
    & fmap (\(_refId, (v, decl, ())) -> (v, either H2.toDataDecl id $ H2.v2ToH2Decl decl))
    & Map.fromList
    & H2.hashDecls Name.unsafeFromVar
    & \case
      Left _err -> Just HH.DeclHashResolutionFailure
      Right m ->
        m
          & altMap \(_, H2.ReferenceId hash' _, _) ->
            if hash == hash'
              then Nothing
              else Just (HH.DeclHashMismatch $ HashMismatch hash hash')
  where
    hash :: Hash
    hash = Hash32.toHash hash32
    symbol2to1 :: S.Symbol -> Unison.Symbol
    symbol2to1 (S.Symbol i t) = Unison.Symbol i (Var.User t)

s2cDecl :: (LocalIds.LocalIds' Text Hash32, DeclFormat.Decl Sqlite.Symbol) -> C.Decl Sqlite.Symbol
s2cDecl (ids, decl) =
  let Identity (substText, substHash) = Q.localIdsToLookups Identity pure (bimap id Hash32.toHash ids)
      refmap = (bimap substText (fmap substHash))
   in Q.x2cDecl refmap decl
