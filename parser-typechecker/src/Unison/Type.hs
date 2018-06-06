{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Type where

import Data.List
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics
import Prelude.Extras (Eq1(..),Show1(..))
import Unison.Hashable (Hashable1)
import Unison.Note (Noted)
import Unison.Reference (Reference)
import Unison.TypeVar (TypeVar)
import Unison.Var (Var)
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.Hashable as Hashable
import qualified Unison.Kind as K
import qualified Unison.Reference as Reference
import qualified Unison.TypeVar as TypeVar

-- | Base functor for types in the Unison language
data F a
  = Ref Reference
  | Arrow a a
  | Ann a K.Kind
  | App a a
  | Effect [a] a
  | Forall a
  deriving (Eq,Foldable,Functor,Generic,Generic1,Traversable)

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

arity :: Type v -> Int
arity (ForallNamed' _ body) = arity body
arity (Arrow' _ o) = 1 + arity o
arity _ = 0

-- some smart patterns
pattern Ref' r <- ABT.Tm' (Ref r)
pattern Arrow' i o <- ABT.Tm' (Arrow i o)
pattern Arrows' spine <- (unArrows -> Just spine)
pattern Ann' t k <- ABT.Tm' (Ann t k)
pattern App' f x <- ABT.Tm' (App f x)
pattern Apps' f args <- (unApps -> Just (f, args))
pattern Effect' es t <- ABT.Tm' (Effect es t)
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
isArrow (Arrow' _ _) = True
isArrow _ = False

-- some smart constructors

vector :: Ord v => Type v
vector = builtin "Sequence"

vectorOf :: Ord v => Type v -> Type v
vectorOf t = vector `app` t

ref :: Ord v => Reference -> Type v
ref = ABT.tm . Ref

builtin :: Ord v => Text -> Type v
builtin = ref . Reference.Builtin

int64 :: Ord v => Type v
int64 = builtin "Int64"

uint64 :: Ord v => Type v
uint64 = builtin "UInt64"

float :: Ord v => Type v
float = builtin "Float"

boolean :: Ord v => Type v
boolean = builtin "Boolean"

text :: Ord v => Type v
text = builtin "Text"

iff :: Var v => Type v
iff = forall aa $ arrows [boolean, a, a] a
  where aa = ABT.v' "a"
        a = var aa

andor :: Ord v => Type v
andor = arrows [boolean, boolean] boolean

app :: Ord v => Type v -> Type v -> Type v
app f arg = ABT.tm (App f arg)

apps :: Ord v => Type v -> [Type v] -> Type v
apps f = foldl' app f

arrow :: Ord v => Type v -> Type v -> Type v
arrow i o = ABT.tm (Arrow i o)

ann :: Ord v => Type v -> K.Kind -> Type v
ann e t = ABT.tm (Ann e t)

forall :: Ord v => v -> Type v -> Type v
forall v body = ABT.tm (Forall (ABT.abs v body))

var :: Ord v => v -> Type v
var = ABT.var

existential :: Ord v => v -> Type (TypeVar v)
existential v = ABT.var (TypeVar.Existential v)

universal :: Ord v => v -> Type (TypeVar v)
universal v = ABT.var (TypeVar.Universal v)

v' :: Var v => Text -> Type v
v' s = ABT.var (ABT.v' s)

forall' :: Var v => [Text] -> Type v -> Type v
forall' vs body = foldr forall body (map ABT.v' vs)

foralls :: Var v => [v] -> Type v -> Type v
foralls vs body = foldr forall body vs

arrows :: Ord v => [Type v] -> Type v -> Type v
arrows ts result = foldr arrow result ts

effect :: Ord v => [Type v] -> Type v -> Type v
effect es t = ABT.tm (Effect es t)

-- | Bind all free variables with an outer `forall`.
generalize :: Ord v => Type v -> Type v
generalize t = foldr forall t $ Set.toList (ABT.freeVars t)

instance Hashable1 F where
  hash1 hashCycle hash e =
    let
      (tag, hashed) = (Hashable.Tag, Hashable.Hashed)
      -- Note: start each layer with leading `0` byte, to avoid collisions with
      -- terms, which start each layer with leading `1`. See `Hashable1 Term.F`
    in Hashable.accumulate $ tag 0 : case e of
      Ref r -> [tag 0, Hashable.accumulateToken r]
      Arrow a b -> [tag 1, hashed (hash a), hashed (hash b) ]
      App a b -> [tag 2, hashed (hash a), hashed (hash b) ]
      Ann a k -> [tag 3, hashed (hash a), Hashable.accumulateToken k ]
      -- Example:
      --   a) {Remote, Abort} (() -> {Remote} ()) should hash the same as
      --   b) {Abort, Remote} (() -> {Remote} ()) but should hash differently from
      --   c) {Remote, Abort} (() -> {Abort} ())
      Effect es t -> let
        (hs, hrem) = hashCycle es
        -- if we used `hash` here instead of `hrem`, then a) and c) would have
        -- the same hash!
        in [tag 4] ++ map hashed hs ++ [hashed (hrem t)]
      Forall a -> [tag 5, hashed (hash a)]

instance Show a => Show (F a) where
  showsPrec p fa = go p fa where
    go _ (Ref r) = showsPrec 0 r
    go p (Arrow i o) =
      showParen (p > 0) $ showsPrec (p+1) i <> s" -> " <> showsPrec p o
    go p (Ann t k) =
      showParen (p > 1) $ showsPrec 0 t <> s":" <> showsPrec 0 k
    go p (App f x) =
      showParen (p > 9) $ showsPrec 9 f <> s" " <> showsPrec 10 x
    go p (Effect es t) = showParen (p > 0) $
      s"{" <> showsPrec 0 es <> s"} " <> showsPrec p t
    go p (Forall body) = case p of
      0 -> showsPrec p body
      _ -> showParen True $ s"∀ " <> showsPrec 0 body
    (<>) = (.)
    s = showString
