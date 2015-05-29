{-# OPTIONS_GHC -fno-warn-orphans #-}
module Unison.Type.Extra where

import Data.Bytes.Serial
import qualified Unison.Digest as Digest
import qualified Data.Bytes.Put as Put
import Unison.Kind.Extra ()
import Unison.Reference.Extra ()
import Unison.Type

instance Serial Literal
instance Serial1 F
instance Serial1 Prop

instance Digest.Digestable1 F where
  -- NB: Initial 0 avoids hash collisions with terms, which have different leading byte
  -- See `Digestable Term.F` in `Unison.Term`.
  digest1 _ hash e = Digest.run $ Put.putWord8 0 *> case e of
    Lit l -> Put.putWord8 0 *> serialize l
    Arrow a b -> Put.putWord8 1 *> serialize (hash a) *> serialize (hash b)
    App a b -> Put.putWord8 2 *> serialize (hash a) *> serialize (hash b)
    Ann a k -> Put.putWord8 3 *> serialize (hash a) *> serialize k
    Constrain a u -> Put.putWord8 4 *> serialize (hash a) *> serialize u
    Forall a -> Put.putWord8 5 *> serialize (hash a)
    Existential v -> Put.putWord8 6 *> serialize (hash v)
    Universal v -> Put.putWord8 7 *> serialize (hash v)

