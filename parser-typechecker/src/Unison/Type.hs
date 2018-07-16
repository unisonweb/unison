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
import qualified Unison.Var as Var

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

freeVars :: AnnotatedType v a -> Set v
freeVars = ABT.freeVars

bindBuiltins :: Var v => [(v, AnnotatedType v a)] -> AnnotatedType v a -> AnnotatedType v a
bindBuiltins bs = ABT.substs bs

data Monotype v a = Monotype { getPolytype :: AnnotatedType v a } deriving (Eq)

instance (Show a, Var v) => Show (Monotype v a) where
  show = show . getPolytype

-- Smart constructor which checks if a `Type` has no `Forall` quantifiers.
monotype :: Var v => AnnotatedType v a -> Maybe (Monotype v a)
monotype t = Monotype <$> ABT.visit isMono t where
  isMono (Forall' _) = Just Nothing
  isMono _ = Nothing

arity :: AnnotatedType v a -> Int
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
pattern Effect'' es t <- (stripEffect -> (es, t))
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

vector :: Ord v => a -> AnnotatedType v a
vector a = builtin a "Sequence"

--vectorOf :: Ord v => a -> AnnotatedType v a -> Type v
--vectorOf a t = vector `app` t

ref :: Ord v => a -> Reference -> AnnotatedType v a
ref a = ABT.tm' a . Ref

builtin :: Ord v => a -> Text -> AnnotatedType v a
builtin a = ref a . Reference.Builtin

int64 :: Ord v => a -> AnnotatedType v a
int64 a = builtin a "Int64"

uint64 :: Ord v => a -> AnnotatedType v a
uint64 a = builtin a "UInt64"

float :: Ord v => a -> AnnotatedType v a
float a = builtin a "Float"

boolean :: Ord v => a -> AnnotatedType v a
boolean a = builtin a "Boolean"

text :: Ord v => a -> AnnotatedType v a
text a = builtin a "Text"

stream :: Ord v => a -> AnnotatedType v a
stream a = builtin a "Stream"

app :: Ord v => a -> AnnotatedType v a -> AnnotatedType v a -> AnnotatedType v a
app a f arg = ABT.tm' a (App f arg)

-- `f x y z` means `((f x) y) z` and the annotation paired with `y` is the one
-- meant for `app (f x) y`
apps :: Ord v => AnnotatedType v a -> [(a, AnnotatedType v a)] -> AnnotatedType v a
apps f params = foldl' go f params where
  go f (a,t) = app a f t

arrow :: Ord v => a -> AnnotatedType v a -> AnnotatedType v a -> AnnotatedType v a
arrow a i o = ABT.tm' a (Arrow i o)

ann :: Ord v => a -> AnnotatedType v a -> K.Kind -> AnnotatedType v a
ann a e t = ABT.tm' a (Ann e t)

forall :: Ord v => a -> v -> AnnotatedType v a -> AnnotatedType v a
forall a v body = ABT.tm' a (Forall (ABT.abs' a v body))

iff :: Var v => Type v
iff = forall () aa $ arrows (f <$> [boolean(), a, a]) a
  where aa = ABT.v' "a"
        a = var () aa
        f x = ((), x)

andor :: Ord v => Type v
andor = arrows (f <$> [boolean(), boolean()]) $ boolean()
  where f x = ((), x)

var :: Ord v => a -> v -> AnnotatedType v a
var = ABT.annotatedVar

existential :: Ord v => v -> Type (TypeVar v)
existential v = ABT.var (TypeVar.Existential v)

universal :: Ord v => v -> Type (TypeVar v)
universal v = ABT.var (TypeVar.Universal v)

existential' :: Ord v => a -> v -> AnnotatedType (TypeVar v) a
existential' a v = ABT.annotatedVar a (TypeVar.Existential v)

universal' :: Ord v => a -> v -> AnnotatedType (TypeVar v) a
universal' a v = ABT.annotatedVar a (TypeVar.Universal v)

v' :: Var v => Text -> Type v
v' s = ABT.var (ABT.v' s)

forall' :: Var v => a -> [Text] -> AnnotatedType v a -> AnnotatedType v a
forall' a vs body = foldr (forall a) body (Var.named <$> vs)

foralls :: Var v => a -> [v] -> AnnotatedType v a -> AnnotatedType v a
foralls a vs body = foldr (forall a) body vs

-- Note: `a -> b -> c` parses as `a -> (b -> c)`
-- the annotation associated with `b` will be the annotation for the `b -> c`
-- node
arrows :: Ord v => [(a, AnnotatedType v a)] -> AnnotatedType v a -> AnnotatedType v a
arrows ts result = foldr go result ts where
  go (a,t) result = arrow a t result

-- The types of effectful computations
effect :: Ord v => a -> [AnnotatedType v a] -> AnnotatedType v a -> AnnotatedType v a
effect a es (Effect' fs t) = ABT.tm' a (Effect (es ++ fs) t)
effect a es t = ABT.tm' a (Effect es t)

-- The types of first-class effect values
-- which get deconstructed in effect handlers.
effectV :: Ord v => a -> (a, AnnotatedType v a) -> (a, AnnotatedType v a) -> AnnotatedType v a
effectV builtinA e t = apps (builtin builtinA "Effect") [e, t]

-- Strips effects from a type. E.g. `{e} a` becomes `a`.
stripEffect :: AnnotatedType v a -> ([AnnotatedType v a], AnnotatedType v a)
stripEffect (Effect' e t) = case stripEffect t of (ei, t) -> (e ++ ei, t)
stripEffect t = ([], t)

-- The type of the flipped function application operator:
-- `(a -> (a -> b) -> b)`
flipApply :: Var v => Type v -> Type v
flipApply t = forall() b $ arrow() (arrow() t (var() b)) (var() b)
  where b = ABT.fresh t (ABT.v' "b")

-- | Bind all free variables with an outer `forall`.
generalize :: Ord v => AnnotatedType v a -> AnnotatedType v a
generalize t = foldr (forall (ABT.annotation t)) t $ Set.toList (ABT.freeVars t)

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
