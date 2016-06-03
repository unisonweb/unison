{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Type where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson.TH
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics
import Prelude.Extras (Eq1(..),Show1(..))
import Unison.Hashable (Hashable, Hashable1)
import Unison.Note (Noted)
import Unison.Reference (Reference)
import Unison.TypeVar (TypeVar)
import Unison.Var (Var)
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.Hash as Hash
import qualified Unison.Hashable as Hashable
import qualified Unison.JSON as J
import qualified Unison.Kind as K
import qualified Unison.Reference as Reference
import qualified Unison.TypeVar as TypeVar

-- | Type literals
data Literal
  = Number
  | Text
  | Vector
  | Ref Reference -- ^ A type literal uniquely defined by some nameless Hash
  deriving (Eq,Ord,Generic)

deriveJSON defaultOptions ''Literal

-- | Base functor for types in the Unison language
data F a
  = Lit Literal
  | Arrow a a
  | Ann a K.Kind
  | App a a
  | Constrain a () -- todo: constraint language
  | Forall a
  deriving (Eq,Foldable,Functor,Generic1,Traversable)

deriveJSON defaultOptions ''F
instance Eq1 F where (==#) = (==)
instance Show1 F where showsPrec1 = showsPrec

-- | Types are represented as ABTs over the base functor F, with variables in `v`
type Type v = AnnotatedType v ()

-- | Like `Type v`, but with an annotation of type `a` at every level in the tree
type AnnotatedType v a = ABT.Term F v a

-- An environment for looking up type references
type Env f v = Reference -> Noted f (Type v)

wrapV :: Ord v => AnnotatedType v a -> AnnotatedType (ABT.V v) a
wrapV = ABT.vmap ABT.Bound

freeVars :: Type v -> Set v
freeVars = ABT.freeVars

data Monotype v = Monotype { getPolytype :: Type v } deriving (Eq)

instance Var v => Show (Monotype v) where
  show = show . getPolytype

-- Smart constructor which checks if a `Type` has no `Forall` quantifiers.
monotype :: Var v => Type v -> Maybe (Monotype v)
monotype t = Monotype <$> ABT.visit isMono t where
  isMono (Forall' _) = Just Nothing
  isMono _ = Nothing

-- some smart patterns
pattern Lit' l <- ABT.Tm' (Lit l)
pattern Arrow' i o <- ABT.Tm' (Arrow i o)
pattern Arrows' spine <- (unArrows -> Just spine)
pattern Ann' t k <- ABT.Tm' (Ann t k)
pattern App' f x <- ABT.Tm' (App f x)
pattern Apps' f args <- (unApps -> Just (f, args))
pattern Constrain' t u <- ABT.Tm' (Constrain t u)
pattern Forall' subst <- ABT.Tm' (Forall (ABT.Abs' subst))
pattern ForallNamed' v body <- ABT.Tm' (Forall (ABT.out -> ABT.Abs v body))
pattern Var' v <- ABT.Var' v
pattern Existential' v <- ABT.Var' (TypeVar.Existential v)
pattern Universal' v <- ABT.Var' (TypeVar.Universal v)

unArrows :: Type v -> Maybe [Type v]
unArrows t =
  case go t of [] -> Nothing; l -> Just l
  where
    go (Arrow' i o) = i : go o
    go _ = []

unApps :: Type v -> Maybe (Type v, [Type v])
unApps t = case go t [] of [] -> Nothing; f:args -> Just (f,args)
  where
  go (App' i o) acc = go i (o:acc)
  go fn args = fn:args

matchExistential :: Eq v => v -> Type (TypeVar v) -> Bool
matchExistential v (Existential' x) = x == v
matchExistential _ _ = False

matchUniversal :: Eq v => v -> Type (TypeVar v) -> Bool
matchUniversal v (Universal' x) = x == v
matchUniversal _ _ = False

-- | True if the given type is a function, possibly quantified
isArrow :: Var v => Type v -> Bool
isArrow (ForallNamed' _ t) = isArrow t
isArrow (Constrain' t _) = isArrow t
isArrow (Arrow' _ _) = True
isArrow _ = False

-- some smart constructors

lit :: Ord v => Literal -> Type v
lit l = ABT.tm (Lit l)

vector :: Ord v => Type v
vector = lit Vector

vectorOf :: Ord v => Type v -> Type v
vectorOf t = vector `app` t

ref :: Ord v => Reference -> Type v
ref = lit . Ref

builtin :: Ord v => Text -> Type v
builtin = ref . Reference.Builtin

app :: Ord v => Type v -> Type v -> Type v
app f arg = ABT.tm (App f arg)

arrow :: Ord v => Type v -> Type v -> Type v
arrow i o = ABT.tm (Arrow i o)

ann :: Ord v => Type v -> K.Kind -> Type v
ann e t = ABT.tm (Ann e t)

forall :: Ord v => v -> Type v -> Type v
forall v body = ABT.tm (Forall (ABT.abs v body))

existential :: Ord v => v -> Type (TypeVar v)
existential v = ABT.var (TypeVar.Existential v)

universal :: Ord v => v -> Type (TypeVar v)
universal v = ABT.var (TypeVar.Universal v)

v' :: Var v => Text -> Type v
v' s = ABT.var (ABT.v' s)

forall' :: Var v => [Text] -> Type v -> Type v
forall' vs body = foldr forall body (map ABT.v' vs)

constrain :: Ord v => Type v -> () -> Type v
constrain t u = ABT.tm (Constrain t u)

-- | Bind all free variables with an outer `forall`.
generalize :: Ord v => Type v -> Type v
generalize t = foldr forall t $ Set.toList (ABT.freeVars t)

instance Hashable Literal where
  tokens l = case l of
    Number -> [Hashable.Tag 0]
    Text -> [Hashable.Tag 1]
    Vector -> [Hashable.Tag 2]
    Ref (Reference.Builtin name) -> Hashable.Tag 3 : Hashable.tokens name
    Ref (Reference.Derived h) -> [Hashable.Tag 4, Hashable.Hashed (Hashable.fromBytes (Hash.toBytes h))]

instance Hashable1 F where
  hash1 _ hash e =
    let
      (tag, hashed) = (Hashable.Tag, Hashable.Hashed)
      -- Note: start each layer with leading `0` byte, to avoid collisions with
      -- terms, which start each layer with leading `1`. See `Hashable1 Term.F`
    in Hashable.accumulate $ tag 0 : case e of
      Lit l -> [tag 0, Hashable.accumulateToken l]
      Arrow a b -> [tag 1, hashed (hash a), hashed (hash b) ]
      App a b -> [tag 2, hashed (hash a), hashed (hash b) ]
      Ann a k -> [tag 3, hashed (hash a), Hashable.accumulateToken k ]
      Constrain a u -> [tag 4, hashed (hash a), Hashable.accumulateToken u]
      Forall a -> [tag 5, hashed (hash a)]

instance J.ToJSON1 F where
  toJSON1 f = toJSON f

instance J.FromJSON1 F where
  parseJSON1 j = parseJSON j

instance Show Literal where
  show Number = "Number"
  show Text = "Text"
  show Vector = "Vector"
  show (Ref r) = show r

instance Show a => Show (F a) where
  showsPrec p fa = go p fa where
    go _ (Lit l) = showsPrec 0 l
    go p (Arrow i o) =
      showParen (p > 0) $ showsPrec (p+1) i <> s" -> " <> showsPrec p o
    go p (Ann t k) =
      showParen (p > 1) $ showsPrec 0 t <> s":" <> showsPrec 0 k
    go p (App f x) =
      showParen (p > 9) $ showsPrec 9 f <> s" " <> showsPrec 10 x
    go p (Constrain t _) = showsPrec p t
    go p (Forall body) = case p of
      0 -> showsPrec p body
      _ -> showParen True $ s"∀ " <> showsPrec 0 body
    (<>) = (.)
    s = showString
