{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module U.Util.Type where

import Data.Set (Set)
import qualified Data.Set as Set
import U.Codebase.Type (F' (..), TypeR)
import U.Core.ABT (pattern Var')
import qualified U.Core.ABT as ABT
import qualified U.Core.ABT.Var as ABT

-- * Constructors

effect :: Ord v => [TypeR r v] -> TypeR r v -> TypeR r v
effect es (Effect1' fs t) =
  let es' = (es >>= flattenEffects) ++ flattenEffects fs
   in ABT.tm () (Effect (ABT.tm () (Effects es')) t)
effect es t = ABT.tm () (Effect (ABT.tm () (Effects es)) t)

effects :: Ord v => [TypeR r v] -> TypeR r v
effects es = ABT.tm () (Effects $ es >>= flattenEffects)

-- * Modification

-- Remove all effect variables from the type.
-- Used for type-based search, we apply this transformation to both the
-- indexed type and the query type, so the user can supply `a -> b` that will
-- match `a ->{e} b` (but not `a ->{IO} b`).
removeAllEffectVars :: ABT.Var v => TypeR r v -> TypeR r v
removeAllEffectVars t =
  let allEffectVars = foldMap go (ABT.subterms t)
      go (Effects' vs) = Set.fromList [v | Var' v <- vs]
      go (Effect1' (Var' v) _) = Set.singleton v
      go _ = mempty
      (vs, tu) = unforall' t
   in generalize vs (removeEffectVars allEffectVars tu)

-- Remove free effect variables from the type that are in the set
removeEffectVars :: ABT.Var v => Set v -> TypeR r v -> TypeR r v
removeEffectVars removals t =
  let z = effects []
      t' = ABT.substsInheritAnnotation ((,z) <$> Set.toList removals) t
      -- leave explicitly empty `{}` alone
      removeEmpty (Effect1' (Effects' []) v) = Just (ABT.visitPure removeEmpty v)
      removeEmpty (Effect1' e v) =
        case flattenEffects e of
          [] -> Just (ABT.visitPure removeEmpty v)
          es -> Just (effect es $ ABT.visitPure removeEmpty v)
      removeEmpty (Effects' es) =
        Just $ effects (es >>= flattenEffects)
      removeEmpty _ = Nothing
   in ABT.visitPure removeEmpty t'

flattenEffects :: TypeR r v -> [TypeR r v]
flattenEffects (Effects' es) = es >>= flattenEffects
flattenEffects es = [es]

-- | Bind the given variables with an outer `forall`, if they are used in `t`.
generalize :: Ord v => [v] -> TypeR r v -> TypeR r v
generalize vs t = foldr f t vs
  where
    f v t = if Set.member v (ABT.freeVars t) then forall v t else t

-- * Patterns

pattern ForallsNamed' :: [v] -> TypeR r v -> TypeR r v
pattern ForallsNamed' vs body <- (unForalls -> Just (vs, body))

pattern ForallNamed' :: v -> TypeR r v -> TypeR r v
pattern ForallNamed' v body <- ABT.Tm' (Forall (ABT.out -> ABT.Abs v body))

pattern Effects' :: [TypeR r v] -> TypeR r v
pattern Effects' es <- ABT.Tm' (Effects es)

pattern Effect1' :: TypeR r v -> TypeR r v -> TypeR r v
pattern Effect1' e t <- ABT.Tm' (Effect e t)

pattern Ref' :: r -> TypeR r v
pattern Ref' r <- ABT.Tm' (Ref r)

forall :: Ord v => v -> TypeR r v -> TypeR r v
forall v body = ABT.tm () (Forall (ABT.abs () v body))

unforall' :: TypeR r v -> ([v], TypeR r v)
unforall' (ForallsNamed' vs t) = (vs, t)
unforall' t = ([], t)

unForalls :: TypeR r v -> Maybe ([v], TypeR r v)
unForalls t = go t []
  where
    go (ForallNamed' v body) vs = go body (v : vs)
    go _body [] = Nothing
    go body vs = Just (reverse vs, body)
