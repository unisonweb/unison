module Unison.Hashing.V1.Reference.Util where

import Unison.Prelude

import qualified Data.Map as Map
import Unison.Hashable (Hashable1)
import qualified Unison.Hashing.V1.ABT as ABT
import qualified Unison.ABT as ABT
import qualified Unison.Hashing.V1.Reference as Reference
import Unison.ABT (Var)

hashComponents ::
     (Functor f, Hashable1 f, Foldable f, Eq v, Show v, Var v)
  => (Reference.Id -> ABT.Term f v ())
  -> Map v (ABT.Term f v a)
  -> Map v (Reference.Id, ABT.Term f v a)
hashComponents embedRef tms =
  Map.fromList [(v, (r, e)) | ((v, e), r) <- cs]
  where
    cs = Reference.components $ ABT.hashComponents ref tms
    ref h i n = embedRef (Reference.Id h i n)
