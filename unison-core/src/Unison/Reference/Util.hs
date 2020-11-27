module Unison.Reference.Util where

import qualified Data.Map as Map
import Unison.ABT (Var)
import qualified Unison.ABT as ABT
import Unison.Hashable (Hashable1)
import Unison.Prelude
import Unison.Reference
import qualified Unison.Reference as Reference

hashComponents ::
  (Functor f, Hashable1 f, Foldable f, Eq v, Show v, Var v) =>
  (Reference.Id -> ABT.Term f v ()) ->
  Map v (ABT.Term f v a) ->
  Map v (Reference.Id, ABT.Term f v a)
hashComponents embedRef tms =
  Map.fromList [(v, (r, e)) | ((v, e), r) <- cs]
  where
    cs = components $ ABT.hashComponents ref tms
    ref h i n = embedRef (Id h i n)
