{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Type where

import Debug.Trace
import qualified Control.Monad.Writer.Strict as Writer
import Control.Monad (join)
import Data.Functor.Identity (runIdentity)
import Data.Functor.Const (Const(..), getConst)
import Data.Monoid (Any(..))
import qualified Data.Char as Char
import           Data.List
import           Data.List.Extra (nubOrd)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Generics
import           Prelude.Extras (Eq1(..),Show1(..),Ord1(..))
import qualified Unison.ABT as ABT
import           Unison.Blank
import           Unison.Hashable (Hashable1)
import qualified Unison.Hashable as Hashable
import qualified Unison.Kind as K
import           Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import           Unison.TypeVar (TypeVar)
import qualified Unison.TypeVar as TypeVar
import           Unison.Var (Var)
import qualified Unison.Var as Var
import qualified Unison.Settings as Settings

-- | Base functor for types in the Unison language
data F a
  = Ref Reference
  | Arrow a a
  | Ann a K.Kind
  | App a a
  | Effect a a
  | Effects [a]
  | Forall a
  deriving (Foldable,Functor,Generic,Generic1,Eq,Ord,Traversable)

instance Eq1 F where (==#) = (==)
instance Ord1 F where compare1 = compare
instance Show1 F where showsPrec1 = showsPrec

-- | Types are represented as ABTs over the base functor F, with variables in `v`
type Type v = AnnotatedType v ()

-- | Like `Type v`, but with an annotation of type `a` at every level in the tree
type AnnotatedType v a = ABT.Term F v a

wrapV :: Ord v => AnnotatedType v a -> AnnotatedType (ABT.V v) a
wrapV = ABT.vmap ABT.Bound

freeVars :: AnnotatedType v a -> Set v
freeVars = ABT.freeVars

bindBuiltins :: Var v => [(v, Reference)] -> AnnotatedType v a -> AnnotatedType v a
bindBuiltins bs = ABT.substsInheritAnnotation [ (v, ref() r) | (v,r) <- bs ]

data Monotype v a = Monotype { getPolytype :: AnnotatedType v a } deriving Eq

instance (Var v) => Show (Monotype v a) where
  show = show . getPolytype

-- Smart constructor which checks if a `Type` has no `Forall` quantifiers.
monotype :: Var v => AnnotatedType v a -> Maybe (Monotype v a)
monotype t = Monotype <$> ABT.visit isMono t where
  isMono (Forall' _) = Just Nothing
  isMono _ = Nothing

arity :: AnnotatedType v a -> Int
arity (ForallNamed' _ body) = arity body
arity (Arrow' _ o) = 1 + arity o
arity (Ann' a _) = arity a
arity _ = 0

-- some smart patterns
pattern Ref' r <- ABT.Tm' (Ref r)
pattern Arrow' i o <- ABT.Tm' (Arrow i o)
pattern Arrows' spine <- (unArrows -> Just spine)
pattern EffectfulArrows' fst rest <- (unEffectfulArrows -> Just (fst, rest))
pattern Ann' t k <- ABT.Tm' (Ann t k)
pattern App' f x <- ABT.Tm' (App f x)
pattern Apps' f args <- (unApps -> Just (f, args))
pattern Pure' t <- (unPure -> Just t)
pattern Effects' es <- ABT.Tm' (Effects es)
-- Effect1' must match at least one effect
pattern Effect1' e t <- ABT.Tm' (Effect e t)
pattern Effect' es t <- (unEffects1 -> Just (es, t))
pattern Effect'' es t <- (unEffect0 -> (es, t))
-- Effect0' may match zero effects
pattern Effect0' es t <- (unEffect0 -> (es, t))
pattern Forall' subst <- ABT.Tm' (Forall (ABT.Abs' subst))
pattern ForallsNamed' vs body <- (unForalls -> Just (vs, body))
pattern ForallNamed' v body <- ABT.Tm' (Forall (ABT.out -> ABT.Abs v body))
pattern Var' v <- ABT.Var' v
pattern Cycle' xs t <- ABT.Cycle' xs t
pattern Abs' subst <- ABT.Abs' subst
pattern Existential' b v <- ABT.Var' (TypeVar.Existential b v)
pattern Universal' v <- ABT.Var' (TypeVar.Universal v)

unPure :: Ord v => AnnotatedType v a -> Maybe (AnnotatedType v a)
unPure (Effect'' [] t) = Just t
unPure (Effect'' _ _) = Nothing
unPure t = Just t

unArrows :: AnnotatedType v a -> Maybe [AnnotatedType v a]
unArrows t =
  case go t of [_] -> Nothing; l -> Just l
  where go (Arrow' i o) = i : go o
        go o = [o]

unEffectfulArrows :: AnnotatedType v a ->
     Maybe (AnnotatedType v a, [(Maybe [AnnotatedType v a], AnnotatedType v a)])
unEffectfulArrows t = case t of Arrow' i o -> Just (i, go o); _ -> Nothing
  where go (Effect1' (Effects' es) (Arrow' i o)) = (Just $ es >>= flattenEffects, i) : go o
        go (Effect1' (Effects' es) t) = [(Just $ es >>= flattenEffects, t)]
        go (Arrow' i o) = (Nothing, i) : go o
        go t = [(Nothing, t)]

unApps :: AnnotatedType v a -> Maybe (AnnotatedType v a, [AnnotatedType v a])
unApps t = case go t [] of [] -> Nothing; [_] -> Nothing; f:args -> Just (f,args)
  where go (App' i o) acc = go i (o:acc)
        go fn args = fn:args

unForalls :: AnnotatedType v a -> Maybe ([v], AnnotatedType v a)
unForalls t = go t []
  where go (ForallNamed' v body) vs = go body (v:vs)
        go _body [] = Nothing
        go body vs = Just(reverse vs, body)

unEffect0 :: Ord v => AnnotatedType v a -> ([AnnotatedType v a], AnnotatedType v a)
unEffect0 (Effect1' e a) = (flattenEffects e, a)
unEffect0 t = ([], t)

unEffects1 :: Ord v => AnnotatedType v a -> Maybe ([AnnotatedType v a], AnnotatedType v a)
unEffects1 (Effect1' (Effects' es) a) = Just (es, a)
unEffects1 _ = Nothing

matchExistential :: Eq v => v -> Type (TypeVar b v) -> Bool
matchExistential v (Existential' _ x) = x == v
matchExistential _ _ = False

matchUniversal :: Eq v => v -> Type (TypeVar b v) -> Bool
matchUniversal v (Universal' x) = x == v
matchUniversal _ _ = False

-- | True if the given type is a function, possibly quantified
isArrow :: Var v => AnnotatedType v a -> Bool
isArrow (ForallNamed' _ t) = isArrow t
isArrow (Arrow' _ _) = True
isArrow _ = False

-- some smart constructors

--vectorOf :: Ord v => a -> AnnotatedType v a -> Type v
--vectorOf a t = vector `app` t

ref :: Ord v => a -> Reference -> AnnotatedType v a
ref a = ABT.tm' a . Ref

derivedBase58 :: Ord v => Reference -> a -> AnnotatedType v a
derivedBase58 r a = ref a r

derivedBase58' :: Text -> Reference
derivedBase58' base58 = Reference.derivedBase58 base58 0 1

intRef, natRef, floatRef, booleanRef, textRef, streamRef, vectorRef :: Reference
intRef = Reference.Builtin "Int"
natRef = Reference.Builtin "Nat"
floatRef = Reference.Builtin "Float"
booleanRef = Reference.Builtin "Boolean"
textRef = Reference.Builtin "Text"
streamRef = Reference.Builtin "Stream"
vectorRef = Reference.Builtin "Sequence"

builtin :: Ord v => a -> Text -> AnnotatedType v a
builtin a = ref a . Reference.Builtin

int :: Ord v => a -> AnnotatedType v a
int a = ref a $ intRef

nat :: Ord v => a -> AnnotatedType v a
nat a = ref a $ natRef

float :: Ord v => a -> AnnotatedType v a
float a = ref a $ floatRef

boolean :: Ord v => a -> AnnotatedType v a
boolean a = ref a $ booleanRef

text :: Ord v => a -> AnnotatedType v a
text a = ref a $ textRef

stream :: Ord v => a -> AnnotatedType v a
stream a = ref a $ streamRef

vector :: Ord v => a -> AnnotatedType v a
vector a = ref a $ vectorRef

app :: Ord v => a -> AnnotatedType v a -> AnnotatedType v a -> AnnotatedType v a
app a f arg = ABT.tm' a (App f arg)

-- `f x y z` means `((f x) y) z` and the annotation paired with `y` is the one
-- meant for `app (f x) y`
apps :: Ord v => AnnotatedType v a -> [(a, AnnotatedType v a)] -> AnnotatedType v a
apps f params = foldl' go f params where
  go f (a,t) = app a f t

app' :: (Ord v, Semigroup a) => AnnotatedType v a -> AnnotatedType v a -> AnnotatedType v a
app' f arg = app (ABT.annotation f <> ABT.annotation arg) f arg

apps' :: (Semigroup a, Ord v) => AnnotatedType v a -> [AnnotatedType v a] -> AnnotatedType v a
apps' f args = foldl app' f args

arrow :: Ord v => a -> AnnotatedType v a -> AnnotatedType v a -> AnnotatedType v a
arrow a i o = ABT.tm' a (Arrow i o)

arrow' :: (Semigroup a, Ord v) => AnnotatedType v a -> AnnotatedType v a -> AnnotatedType v a
arrow' i o = arrow (ABT.annotation i <> ABT.annotation o) i o

ann :: Ord v => a -> AnnotatedType v a -> K.Kind -> AnnotatedType v a
ann a e t = ABT.tm' a (Ann e t)

forall :: Ord v => a -> v -> AnnotatedType v a -> AnnotatedType v a
forall a v body = ABT.tm' a (Forall (ABT.abs' a v body))

iff :: Var v => Type v
iff = forall () aa $ arrows (f <$> [boolean(), a, a]) a
  where aa = ABT.v' "a"
        a = var () aa
        f x = ((), x)

iff' :: Var v => a -> AnnotatedType v a
iff' loc = forall loc aa $ arrows (f <$> [boolean loc, a, a]) a
  where aa = ABT.v' "a"
        a = var loc aa
        f x = (loc, x)

iff2 :: Var v => a -> AnnotatedType v a
iff2 loc = forall loc aa $ arrows (f <$> [a, a]) a
  where aa = ABT.v' "a"
        a = var loc aa
        f x = (loc, x)

andor :: Ord v => Type v
andor = arrows (f <$> [boolean(), boolean()]) $ boolean()
  where f x = ((), x)

andor' :: Ord v => a -> AnnotatedType v a
andor' a = arrows (f <$> [boolean a, boolean a]) $ boolean a
  where f x = (a, x)

var :: Ord v => a -> v -> AnnotatedType v a
var = ABT.annotatedVar

existential :: Ord v => Blank loc -> v -> Type (TypeVar (Blank loc) v)
existential blank v = ABT.var (TypeVar.Existential blank v)

universal :: Ord v => v -> Type (TypeVar b v)
universal v = ABT.var (TypeVar.Universal v)

existentialp :: Ord v => a -> v -> AnnotatedType (TypeVar (Blank x) v) a
existentialp a v = existential' a Blank v

existential' :: Ord v => a -> Blank x -> v -> AnnotatedType (TypeVar (Blank x) v) a
existential' a blank v = ABT.annotatedVar a (TypeVar.Existential blank v)

universal' :: Ord v => a -> v -> AnnotatedType (TypeVar b v) a
universal' a v = ABT.annotatedVar a (TypeVar.Universal v)

v' :: Var v => Text -> Type v
v' s = ABT.var (ABT.v' s)

-- Like `v'`, but creates an annotated variable given an annotation
av' :: Var v => a -> Text -> AnnotatedType v a
av' a s = ABT.annotatedVar a (ABT.v' s)

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
effect a es (Effect1' fs t) =
  let es' = (es >>= flattenEffects) ++ flattenEffects fs
  in ABT.tm' a (Effect (ABT.tm' a (Effects es')) t)
effect a es t = ABT.tm' a (Effect (ABT.tm' a (Effects es)) t)

effects :: Ord v => a -> [AnnotatedType v a] -> AnnotatedType v a
effects a es = ABT.tm' a (Effects $ es >>= flattenEffects)

effect1 :: Ord v => a -> AnnotatedType v a -> AnnotatedType v a -> AnnotatedType v a
effect1 a es (Effect1' fs t) =
  let es' = flattenEffects es ++ flattenEffects fs
  in ABT.tm' a (Effect (ABT.tm' a (Effects es')) t)
effect1 a es t = ABT.tm' a (Effect es t)

flattenEffects :: AnnotatedType v a -> [AnnotatedType v a]
flattenEffects (Effects' es) = es >>= flattenEffects
flattenEffects es = [es]

-- The types of first-class effect values
-- which get deconstructed in effect handlers.
effectV :: Ord v => a -> (a, AnnotatedType v a) -> (a, AnnotatedType v a) -> AnnotatedType v a
effectV builtinA e t = apps (builtin builtinA "Effect") [e, t]

-- Strips effects from a type. E.g. `{e} a` becomes `a`.
stripEffect :: Ord v => AnnotatedType v a -> ([AnnotatedType v a], AnnotatedType v a)
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

unforall :: AnnotatedType v a -> AnnotatedType v a
unforall (ForallsNamed' _ t) = t
unforall t = t

generalizeAndUnTypeVar :: Var v => AnnotatedType (TypeVar b v) a -> AnnotatedType v a
generalizeAndUnTypeVar = cleanup . ABT.vmap TypeVar.underlying . generalize

toTypeVar :: Ord v => AnnotatedType v a -> AnnotatedType (TypeVar b v) a
toTypeVar = ABT.vmap TypeVar.Universal

dependencies :: Ord v => AnnotatedType v a -> Set Reference
dependencies t = Set.fromList . Writer.execWriter $ ABT.visit' f t
  where f t@(Ref r) = Writer.tell [r] *> pure t
        f t = pure t

usesEffects :: Var v => AnnotatedType v a -> Bool
usesEffects t = getAny . getConst $ ABT.visit go t where
  go (Effect1' _ _) = Just (Const (Any True))
  go _ = Nothing

-- Returns free effect variables in the given type, for instance, in:
--
--   ∀ e3 . a ->{e,e2} b ->{e3} c
--
-- This function would return the set {e, e2}, but not `e3` since `e3`
-- is bound by the enclosing forall.
freeEffectVars :: Var v => AnnotatedType v a -> Set v
freeEffectVars t =
  Set.fromList . join . runIdentity $
    ABT.foreachSubterm go (snd <$> ABT.annotateBound t)
  where
    go t@(Effects' es) =
      let frees = Set.fromList [ v | Var' v <- es >>= flattenEffects ]
      in pure . Set.toList $ frees `Set.difference` ABT.annotation t
    go t@(Effect1' e _) =
      let frees = Set.fromList [ v | Var' v <- flattenEffects e ]
      in pure . Set.toList $ frees `Set.difference` ABT.annotation t
    go _ = pure []

existentializeArrows :: (Var v, Monad m)
                     => m v
                     -> AnnotatedType v a
                     -> m (AnnotatedType v a)
existentializeArrows freshVar t = ABT.visit go t
  where
  go t@(Arrow' a b) = case b of
    Effect1' _ _ -> Nothing
    _ -> Just $ do
      e <- freshVar
      a <- existentializeArrows freshVar a
      b <- existentializeArrows freshVar b
      let ann = ABT.annotation t
      pure $ arrow ann a (effect ann [var ann e] b)
  go _ = Nothing

-- Remove free effect variables from the type that are in the set
removeEffectVars :: Var v => Set v -> AnnotatedType v a -> AnnotatedType v a
removeEffectVars removals t =
  let z = effects () []
      t'0 = ABT.substsInheritAnnotation ((,z) <$> Set.toList removals) t
      t' = trace ("t' = " <> show t'0) t'0
      -- leave explicitly empty `{}` alone
      removeEmpty (Effect1' (Effects' []) _) = Nothing
      removeEmpty t@(Effect1' e v) =
        let es = flattenEffects (trace ("BEFORE:   " <> show e) e)
        in case trace ("AFTER:   " <> show es) es of
             [] -> Just (ABT.visitPure removeEmpty v)
             _ -> Just (effect (ABT.annotation t) es $ ABT.visitPure removeEmpty v)
      removeEmpty t@(Forall' _) = Just t
      removeEmpty _ = Nothing
  in ABT.visitPure removeEmpty t'

removePureEffects :: Var v => AnnotatedType v a -> AnnotatedType v a
removePureEffects t | not Settings.removePureEffects = t
                    | otherwise =
  generalize $ removeEffectVars (Set.filter isPure fvs) tu
  where
    tu0 = unforall t
    tu = trace ("tu = \n" <> show tu0) tu0
    fvs0 = freeEffectVars tu `Set.difference` ABT.freeVars t
    fvs = trace ("fvs = " <> show fvs0) fvs0
    -- debug = TP.pretty' (Just 90) mempty
    -- If an effect variable is mentioned only once, it is on
    -- an arrow `a ->{e} b`. Generalizing this to
    -- `∀ e . a ->{e} b` gives us the pure arrow `a -> b`.
    isPure v = ABT.occurrences v tu <= 1

functionResult :: AnnotatedType v a -> Maybe (AnnotatedType v a)
functionResult t = go False t where
  go inArr (ForallNamed' _ body) = go inArr body
  go _inArr (Arrow' _i o) = go True o
  go inArr t = if inArr then Just t else Nothing


-- | Bind all free variables that start with a lowercase letter with an outer `forall`.
generalizeLowercase :: Var v => AnnotatedType v a -> AnnotatedType v a
generalizeLowercase t = foldr (forall (ABT.annotation t)) t vars
  where vars = [ v | v <- Set.toList (ABT.freeVars t), isLow v]
        isLow v = all Char.isLower . take 1 . Text.unpack . Var.name $ v

-- | This function removes all variable shadowing from the type and reduces
-- fresh ids to the minimum possible to avoid ambiguity.
cleanupVars1 :: Var v => AnnotatedType v a -> AnnotatedType v a
cleanupVars1 t | not Settings.cleanupTypes = t
cleanupVars1 t = let
  varsByName = foldl' step Map.empty (ABT.allVars t)
  step m v = Map.insertWith (++) (Var.name $ Var.reset v) [v] m
  changedVars = Map.fromList [ (v, Var.freshenId i v)
                             | (_, vs) <- Map.toList varsByName
                             , (v,i) <- nubOrd vs `zip` [0..]]

  in ABT.changeVars changedVars t

-- This removes duplicates and normalizes the order of ability lists
cleanupAbilityLists :: Var v => AnnotatedType v a -> AnnotatedType v a
cleanupAbilityLists t = ABT.visitPure go t where
  -- leave explicitly empty `{}` alone
  go (Effect1' (Effects' []) _v) = Nothing
  go t@(Effect1' e v) =
    let es = Set.toList . Set.fromList $ flattenEffects e
    in case es of
         [] -> Just (ABT.visitPure go v)
         _ -> Just (effect (ABT.annotation t) es $ ABT.visitPure go v)
  go _ = Nothing

cleanup :: Var v => AnnotatedType v a -> AnnotatedType v a
cleanup t | not Settings.cleanupTypes = t
cleanup t = cleanupVars1 . cleanupAbilityLists $ t

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
      Effects es -> let
        (hs, _) = hashCycle es
        in [tag 4] ++ map hashed hs
      Effect e t -> [tag 5, hashed (hash e), hashed (hash t)]
      Forall a -> [tag 6, hashed (hash a)]

instance Show a => Show (F a) where
  showsPrec p fa = go p fa where
    go _ (Ref r) = showsPrec 0 r
    go p (Arrow i o) =
      showParen (p > 0) $ showsPrec (p+1) i <> s" -> " <> showsPrec p o
    go p (Ann t k) =
      showParen (p > 1) $ showsPrec 0 t <> s":" <> showsPrec 0 k
    go p (App f x) =
      showParen (p > 9) $ showsPrec 9 f <> s" " <> showsPrec 10 x
    go p (Effects es) = showParen (p > 0) $
      s"{" <> showsPrec 0 es <> s"}"
    go p (Effect e t) = showParen (p > 0) $
     showParen True $ showsPrec 0 e <> s" " <> showsPrec p t
    go p (Forall body) = case p of
      0 -> showsPrec p body
      _ -> showParen True $ s"∀ " <> showsPrec 0 body
    (<>) = (.)
    s = showString
