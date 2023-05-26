module Unison.Hashing.V2.Reference.Util
  ( hashComponents,
  )
where

import Data.Map qualified as Map
import Unison.ABT (Var)
import Unison.Hashing.V2.ABT qualified as ABT
import Unison.Hashing.V2.Reference (ReferenceId (..))
import Unison.Hashing.V2.Reference qualified as Reference
import Unison.Hashing.V2.Tokenizable (Hashable1)
import Unison.Prelude

hashComponents ::
  (Functor f, Hashable1 f, Foldable f, Eq v, Show v, Var v) =>
  (ReferenceId -> ABT.Term f v ()) ->
  Map v (ABT.Term f v a) ->
  Map v (ReferenceId, ABT.Term f v a)
hashComponents embedRef tms =
  Map.fromList [(v, (r, e)) | ((v, e), r) <- cs]
  where
    cs = Reference.components $ ABT.hashComponents ref tms
    ref h i = embedRef (ReferenceId h i)
