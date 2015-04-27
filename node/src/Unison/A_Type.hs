{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.A_Type where

import Control.Applicative
import Data.Aeson (toJSON, parseJSON)
import Data.Aeson.TH
import Data.Bytes.Serial
import Data.Foldable (Foldable)
import Data.Functor.Classes (Eq1(..),Show1(..))
import Data.Set (Set)
import Data.Traversable (Traversable)
import GHC.Generics
import Unison.Note (Noted)
import qualified Data.Bytes.Put as Put
import qualified Unison.ABT as ABT
import qualified Unison.Digest as Digest
import qualified Unison.JSON as J
import qualified Unison.Kind as K
import qualified Unison.A_Reference as R

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
  | Existential a
  | Universal a
  deriving (Eq,Foldable,Functor,Generic1,Show,Traversable)

deriveJSON defaultOptions ''F
instance Serial1 F
instance Eq1 F where eq1 = (==)
instance Show1 F where showsPrec1 = showsPrec

-- | Terms are represented as ABTs over the base functor F.
type Type = ABT.Term F

-- An environment for looking up type references
type Env f = R.Reference -> Noted f Type

freeVars :: Type -> Set ABT.V
freeVars = ABT.freeVars

data Monotype = Monotype { getPolytype :: Type } deriving (Eq,Show)

-- Smart constructor which checks if a `Type` has no `Forall` quantifiers.
monotype :: Type -> Maybe Monotype
monotype t = Monotype <$> ABT.visit isMono t where
  isMono (Forall' _ _) = Just Nothing
  isMono _ = Nothing

-- some smart patterns
pattern Lit' l <- ABT.Tm' (Lit l)
pattern Arrow' i o <- ABT.Tm' (Arrow i o)
pattern Ann' t k <- ABT.Tm' (Ann t k)
pattern App' f x <- ABT.Tm' (App f x)
pattern Constrain' t u <- ABT.Tm' (Constrain t u)
pattern Forall' v body <- ABT.Tm' (Forall (ABT.Abs' v body))
pattern Existential' v <- ABT.Tm' (Existential (ABT.Var' v))
pattern Universal' v <- ABT.Tm' (Universal (ABT.Var' v))

matchExistential :: ABT.V -> Type -> Bool
matchExistential v (Existential' x) = x == v
matchExistential _ _ = False

matchUniversal :: ABT.V -> Type -> Bool
matchUniversal v (Universal' x) = x == v
matchUniversal _ _ = False

-- some smart constructors

lit :: Literal -> Type
lit l = ABT.tm (Lit l)

app :: Type -> Type -> Type
app f arg = ABT.tm (App f arg)

arrow :: Type -> Type -> Type
arrow i o = ABT.tm (Arrow i o)

ann :: Type -> K.Kind -> Type
ann e t = ABT.tm (Ann e t)

forall :: ABT.V -> Type -> Type
forall v body = ABT.tm (Forall (ABT.abs v body))

existential :: ABT.V -> Type
existential v = ABT.tm (Existential (ABT.var v))

universal :: ABT.V -> Type
universal v = ABT.tm (Universal (ABT.var v))

constrain :: Type -> () -> Type
constrain t u = ABT.tm (Constrain t u)

instance Digest.Digestable1 F where
  digest1 _ hash e = case e of
    Lit l -> Put.putWord8 0 *> serialize l
    Arrow a b -> Put.putWord8 1 *> serialize (hash a) *> serialize (hash b)
    App a b -> Put.putWord8 2 *> serialize (hash a) *> serialize (hash b)
    Ann a k -> Put.putWord8 3 *> serialize (hash a) *> serialize k
    Constrain a u -> Put.putWord8 4 *> serialize (hash a) *> serialize u
    Forall a -> Put.putWord8 5 *> serialize (hash a)
    Existential v -> Put.putWord8 6 *> serialize (hash v)
    Universal v -> Put.putWord8 7 *> serialize (hash v)

instance J.ToJSON1 F where
  toJSON1 f = toJSON f

instance J.FromJSON1 F where
  parseJSON1 j = parseJSON j

--instance Show a => Show (F a) where
--  show (Lit' l) = show l
--  show (Arrow' (Arrow' i i2) o) = "(" ++ show i ++ " -> " ++ show i2 ++ ") -> " ++ show o
--  show (Arrow' i o) = show i ++ " -> " ++ show o
--  show (ABT.Var' n) = show n
--  show (Ann' t k) = show t ++ ":" ++ show k
--  show (App' f arg) = "(" ++ show f ++ " " ++ show arg ++ ")"
--  show (Constrain' t _) = show t
--  show (Forall' x (Forall' y (Forall' z t))) =
--    "(∀ " ++ (intercalate " " . map show) [x,y,z] ++ ". " ++ show t ++ ")"
--  show (Forall' x (Forall' y t)) = "(∀ " ++ (intercalate " " . map show) [x,y] ++ ". " ++ show t++")"
--  show (Forall' x t) = "(∀ " ++ show x ++ ". " ++ show t++")"
