{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.A_Type where

import Control.Applicative
import Data.Functor.Classes
import Data.Foldable (Foldable)
import Data.Aeson (toJSON, parseJSON)
import Data.Aeson.TH
import Data.Bytes.Serial
import GHC.Generics
import qualified Data.Bytes.Put as Put
import qualified Unison.Digest as Digest
import qualified Unison.JSON as J
import qualified Unison.Kind as K
import qualified Unison.Reference as R
import qualified Unison.ABT as ABT

-- | Type literals
data Literal
  = Number
  | Text
  | Vector
  | Distance
  | Ref R.Reference -- ^ A type literal uniquely defined by some nameless Hash
  deriving (Eq,Ord,Show,Generic)

deriveJSON defaultOptions ''Literal
instance Serial Literal

-- | Base functor for types in the Unison language
data F a
  = Lit Literal
  | Arrow a a
  | Ann a K.Kind
  | App a a
  | Constrain a () -- todo: constraint language
  | Forall a
  deriving (Eq,Foldable,Functor,Generic1)

deriveJSON defaultOptions ''F
instance Serial1 F
instance Eq1 F where eq1 = (==)

-- | Terms are represented as ABTs over the base functor F.
type Type = ABT.Term F

-- some smart constructors

lit :: Literal -> Type
lit l = ABT.tm (Lit l)

app :: Type -> Type -> Type
app f arg = ABT.tm (App f arg)

ann :: Type -> K.Kind -> Type
ann e t = ABT.tm (Ann e t)

forall :: ABT.V -> Type -> Type
forall v body = ABT.tm (Forall (ABT.abs v body))

instance Digest.Digestable1 F where
  digest1 _ hash e = case e of
    Lit l -> Digest.run $ Put.putWord8 0 *> serialize l
    Arrow a b -> Digest.run $ Put.putWord8 1 *> serialize (hash a) *> serialize (hash b)
    App a b -> Digest.run $ Put.putWord8 2 *> serialize (hash a) *> serialize (hash b)
    Ann a k -> Digest.run $ Put.putWord8 3 *> serialize (hash a) *> serialize k
    Constrain a u -> Digest.run $ Put.putWord8 4 *> serialize (hash a) *> serialize u
    Forall a -> Digest.run $ Put.putWord8 5 *> serialize (hash a)

instance J.ToJSON1 F where
  toJSON1 f = toJSON f

instance J.FromJSON1 F where
  parseJSON1 j = parseJSON j
