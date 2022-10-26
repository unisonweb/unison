module Unison.Hashing.V2.Reference.Util where

import qualified Data.Map as Map
import Unison.ABT (Var)
import qualified Unison.Hashing.V2.ABT as ABT
import qualified Unison.Hashing.V2.Reference as Reference
import Unison.Hashing.V2.Tokenizable (Hashable1)
import Unison.Prelude

hashComponents ::
  (Functor f, Hashable1 f, Foldable f, Eq v, Show v, Var v) =>
  (Reference.Id -> ABT.Term f v ()) ->
  Map v (ABT.Term f v a) ->
  Map v (Reference.Id, ABT.Term f v a)
hashComponents embedRef tms =
  Map.fromList [(v, (r, e)) | ((v, e), r) <- cs]
  where
    cs = Reference.components $ ABT.hashComponents ref tms
    ref h i = embedRef (Reference.Id h i)
