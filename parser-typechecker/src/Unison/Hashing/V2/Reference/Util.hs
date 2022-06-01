module Unison.Hashing.V2.Reference.Util (hashComponents, hashForComponent) where

import qualified Data.Map as Map
import Unison.ABT (Var)
import Unison.Hash (Hash)
import qualified Unison.Hashing.V2.ABT as ABT
import qualified Unison.Hashing.V2.Reference as Reference
import Unison.Hashing.V2.Tokenizable (Hashable1)
import Unison.Prelude

-- | Given a map of definitions, determines component groupings, hashes those components,
-- and rewrites internal references using Reference IDs.
hashComponents ::
  (Functor f, Hashable1 f, Foldable f, Show v, Var v) =>
  (Reference.Id -> ABT.Term f v ()) ->
  Map v (ABT.Term f v a) ->
  Map v (Reference.Id, ABT.Term f v a)
hashComponents embedRef tms =
  Map.fromList [(v, (r, e)) | ((v, e), r) <- cs]
  where
    cs = Reference.components $ ABT.hashComponents ref tms
    ref h i = embedRef (Reference.Id h i)

-- | A simplified version of 'hashComponents' which is strictly for verifying component
-- hashes.
--
-- Expects a single component, fails with 'Left' if provided multiple.
hashForComponent ::
  (Functor f, Hashable1 f, Foldable f, Show v, Var v) =>
  (Reference.Id -> ABT.Term f v ()) ->
  Map v (ABT.Term f v a) ->
  Either Text Hash
hashForComponent embedRef tms =
  case ABT.hashComponents ref tms of
    [(h, _definitions)] -> Right h
    _ -> Left "Expected a single component but detected multiple."
  where
    ref h i = embedRef (Reference.Id h i)
