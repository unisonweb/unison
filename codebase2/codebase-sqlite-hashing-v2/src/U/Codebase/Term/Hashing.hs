module U.Codebase.Term.Hashing where

import Control.Lens
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import U.Codebase.HashTags
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.HashHandle (HashMismatch (..))
import U.Codebase.Sqlite.LocalIds qualified as LocalIds
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.Symbol qualified as S
import U.Codebase.Sqlite.Term.Format qualified as S.Term
import U.Codebase.Sqlite.Term.Format qualified as TermFormat
import U.Codebase.Term qualified as C
import U.Codebase.Term qualified as C.Term
import U.Codebase.Type qualified as C.Type
import U.Core.ABT qualified as ABT
import Unison.Hash32
import Unison.Hash32 qualified as Hash32
import Unison.Hashing.V2 qualified as H2
import Unison.Hashing.V2.Convert2 qualified as H2
import Unison.Prelude
import Unison.Symbol qualified as Unison
import Unison.Var qualified as Var

verifyTermFormatHash :: ComponentHash -> TermFormat.HashTermFormat -> Maybe (HashMismatch)
verifyTermFormatHash (ComponentHash hash) (TermFormat.Term (TermFormat.LocallyIndexedComponent elements)) =
  Foldable.toList elements
    & fmap s2cTermWithType
    & Reference.component hash
    & fmap (\((tm, typ), refId) -> (refId, ((mapTermV tm), (mapTypeV typ))))
    & Map.fromList
    & C.Term.unhashComponent hash Var.unnamedRef
    & Map.toList
    & fmap (\(_refId, (v, trm, typ)) -> (v, (H2.v2ToH2Term trm, H2.v2ToH2Type typ, ())))
    & Map.fromList
    & H2.hashTermComponents
    & altMap \(H2.ReferenceId hash' _, _trm, _typ, _extra) ->
      if hash == hash'
        then Nothing
        else Just (HashMismatch hash hash')
  where
    mapTermV ::
      ABT.Term (C.Term.F' text' termRef' typeRef' termLink' typeLink' S.Symbol) S.Symbol a ->
      ABT.Term (C.Term.F' text' termRef' typeRef' termLink' typeLink' Unison.Symbol) Unison.Symbol a
    mapTermV =
      C.Term.extraMap id id id id id symbol2to1
        >>> ABT.vmap symbol2to1
    mapTypeV :: ABT.Term (C.Type.F' r) S.Symbol () -> ABT.Term (C.Type.F' r) Unison.Symbol ()
    mapTypeV = ABT.vmap symbol2to1
    symbol2to1 :: S.Symbol -> Unison.Symbol
    symbol2to1 (S.Symbol i t) = Unison.Symbol i (Var.User t)

s2cTermWithType :: (LocalIds.LocalIds' Text Hash32, S.Term.Term, S.Term.Type) -> (C.Term S.Symbol, C.Term.Type S.Symbol)
s2cTermWithType (ids, tm, tp) =
  let Identity (substText, substHash) = Q.localIdsToLookups Identity pure (bimap id Hash32.toHash ids)
   in (Q.x2cTerm substText substHash tm, Q.x2cTType substText substHash tp)
