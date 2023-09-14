module U.Codebase.Term.Hashing (hashTermFormat) where

import Control.Lens
import Data.Foldable qualified as Foldable
import Data.Map qualified as Map
import U.Codebase.HashTags
import U.Codebase.Sqlite.LocalIds qualified as LocalIds
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.Symbol qualified as S
import U.Codebase.Sqlite.Term.Format qualified as S.Term
import U.Codebase.Sqlite.Term.Format qualified as TermFormat
import U.Codebase.Term qualified as C
import U.Codebase.Term qualified as C.Term
import U.Core.ABT qualified as ABT
import Unison.Hash32
import Unison.Hash32 qualified as Hash32
import Unison.Hashing.V2 (hashTermComponents, hashTermComponentsSimple)
import Unison.Prelude

hashTermFormat :: TermFormat.HashTermFormat -> ComponentHash
hashTermFormat (TermFormat.Term hashLocalComp) =
  let termsInComp = somethingsomethingLocallyIndexedTermComponent hashLocalComp
      -- The values don't matter, just need to be distinct.
      vars = S.Symbol 0 . tShow <$> [(1 :: Int) ..]
      componentMap = Map.fromList (zip vars termsInComp)
   in componentMap
        & fmap (bimap _ _)
        & hashTermComponentsSimple
        & _

somethingsomethingLocallyIndexedTermComponent ::
  TermFormat.HashLocallyIndexedComponent ->
  [(C.Term S.Symbol, C.Term.Type S.Symbol)]
somethingsomethingLocallyIndexedTermComponent (TermFormat.LocallyIndexedComponent elements) = do
  s2cTermWithType <$> (Foldable.toList elements)

s2cTermWithType :: (LocalIds.LocalIds' Text Hash32, S.Term.Term, S.Term.Type) -> (C.Term S.Symbol, C.Term.Type S.Symbol)
s2cTermWithType (ids, tm, tp) =
  let Identity (substText, substHash) = Q.localIdsToLookups Identity pure (bimap id Hash32.toHash ids)
   in (Q.x2cTerm substText substHash tm, Q.x2cTType substText substHash tp)

-- Q.x2cTerm subsText subsHash tm
-- let lookupText = LocalIds.textLookup ids
-- let tm' = C.Term.extraMap id substTermRef substTypeRef substTermLink substTypeLink id tm
-- let tp' = C.Term.extraMap id substTermRef substTypeRef substTermLink substTypeLink id tp
-- pure (tm', tp')

-- | implementation detail of {s,w}2c*Term* & s2cDecl
-- localIdsToLookups :: (Monad m) => (t -> m Text) -> (d -> m Hash) -> LocalIds' t d -> m (LocalTextId -> Text, LocalDefnId -> Hash)
-- localIdsToLookups loadText loadHash localIds = do
--   texts <- traverse loadText $ LocalIds.textLookup localIds
--   hashes <- traverse loadHash $ LocalIds.defnLookup localIds
--   let substText (LocalTextId w) = texts Vector.! fromIntegral w
--       substHash (LocalDefnId w) = hashes Vector.! fromIntegral w
--   pure (substText, substHash)

-- -- pure (x2cTerm substText substHash tm, x2cTType substText substHash tp)
