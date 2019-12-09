module Unison.Reference.Util where

import Unison.Prelude

import Unison.Reference
import Unison.Hashable (Hashable1)
import Unison.ABT (Var)
import qualified Unison.ABT as ABT
import qualified Data.Map as Map

hashComponents ::
     (Functor f, Hashable1 f, Foldable f, Eq v, Show v, Var v)
  => (Reference -> ABT.Term f v ())
  -> Map v (ABT.Term f v a)
  -> Map v (Reference, ABT.Term f v a)
hashComponents embedRef tms =
  Map.fromList [ (v, (r,e)) | ((v,e), r) <- cs ]
  where cs = components $ ABT.hashComponents ref tms
        ref h i n = embedRef (DerivedId (Id h i n))


