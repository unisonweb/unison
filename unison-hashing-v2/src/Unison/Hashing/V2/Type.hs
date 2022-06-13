{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Hashing.V2.Type
  ( Type,
    F (..),
    bindExternal,
    bindReferences,

    -- * find by type index stuff
    toReference,
    toReferenceMentions,

    -- * builtin term references
    booleanRef,
    charRef,
    effectRef,
    floatRef,
    intRef,
    listRef,
    natRef,
    textRef,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.Hashing.V2.ABT as ABT
import qualified Unison.Hashing.V2.Kind as K
import Unison.Hashing.V2.Reference (Reference)
import qualified Unison.Hashing.V2.Reference as Reference
import Unison.Hashing.V2.Tokenizable (Hashable1)
import qualified Unison.Hashing.V2.Tokenizable as Hashable
import qualified Unison.Name as Name
import qualified Unison.Names.ResolutionResult as Names
import Unison.Prelude
import qualified Unison.Util.List as List
import Unison.Var (Var)

-- | Base functor for types in the Unison language
data F a
  = Ref Reference
  | Arrow a a
  | Ann a K.Kind
  | App a a
  | Effect a a
  | Effects [a]
  | Forall a
  | IntroOuter a -- binder like ∀, used to introduce variables that are
  -- bound by outer type signatures, to support scoped type
  -- variables
  deriving (Foldable, Functor, Traversable)

-- | Types are represented as ABTs over the base functor F, with variables in `v`
type Type v a = ABT.Term F v a

freeVars :: Type v a -> Set v
freeVars = ABT.freeVars

bindExternal ::
  ABT.Var v => [(v, Reference)] -> Type v a -> Type v a
bindExternal bs = ABT.substsInheritAnnotation [(v, ref () r) | (v, r) <- bs]

bindReferences ::
  Var v =>
  Set v ->
  Map Name.Name Reference ->
  Type v a ->
  Names.ResolutionResult v a (Type v a)
bindReferences keepFree ns t =
  let fvs = ABT.freeVarOccurrences keepFree t
      rs = [(v, a, Map.lookup (Name.unsafeFromVar v) ns) | (v, a) <- fvs]
      ok (v, _a, Just r) = pure (v, r)
      ok (v, a, Nothing) = Left (pure (Names.TypeResolutionFailure v a Names.NotFound))
   in List.validate ok rs <&> \es -> bindExternal es t

-- some smart patterns
pattern Ref' r <- ABT.Tm' (Ref r)

pattern ForallsNamed' vs body <- (unForalls -> Just (vs, body))

pattern ForallNamed' v body <- ABT.Tm' (Forall (ABT.out -> ABT.Abs v body))

unForalls :: Type v a -> Maybe ([v], Type v a)
unForalls t = go t []
  where
    go (ForallNamed' v body) vs = go body (v : vs)
    go _body [] = Nothing
    go body vs = Just (reverse vs, body)

-- some smart constructors
ref :: Ord v => a -> Reference -> Type v a
ref a = ABT.tm' a . Ref

intRef, natRef, floatRef, booleanRef, textRef, charRef, listRef, effectRef :: Reference
intRef = Reference.Builtin "Int"
natRef = Reference.Builtin "Nat"
floatRef = Reference.Builtin "Float"
booleanRef = Reference.Builtin "Boolean"
textRef = Reference.Builtin "Text"
charRef = Reference.Builtin "Char"
listRef = Reference.Builtin "Sequence"
effectRef = Reference.Builtin "Effect"

forall :: Ord v => a -> v -> Type v a -> Type v a
forall a v body = ABT.tm' a (Forall (ABT.abs' a v body))

-- | Bind the given variables with an outer `forall`, if they are used in `t`.
generalize :: Ord v => [v] -> Type v a -> Type v a
generalize vs t = foldr f t vs
  where
    f v t =
      if Set.member v (ABT.freeVars t) then forall (ABT.annotation t) v t else t

unforall' :: Type v a -> ([v], Type v a)
unforall' (ForallsNamed' vs t) = (vs, t)
unforall' t = ([], t)

toReference :: (Ord v, Show v) => Type v a -> Reference
toReference (Ref' r) = r
-- a bit of normalization - any unused type parameters aren't part of the hash
toReference (ForallNamed' v body) | not (Set.member v (ABT.freeVars body)) = toReference body
toReference t = Reference.Derived (ABT.hash t) 0

toReferenceMentions :: (Ord v, Show v) => Type v a -> Set Reference
toReferenceMentions ty =
  let (vs, _) = unforall' ty
      gen ty = generalize (Set.toList (freeVars ty)) $ generalize vs ty
   in Set.fromList $ toReference . gen <$> ABT.subterms ty

instance Hashable1 F where
  hash1 hashCycle hash e =
    let (tag, hashed) = (Hashable.Tag, Hashable.Hashed)
     in -- Note: start each layer with leading `0` byte, to avoid collisions with
        -- terms, which start each layer with leading `1`. See `Hashable1 Term.F`
        Hashable.accumulate $
          tag 0 : case e of
            Ref r -> [tag 0, Hashable.accumulateToken r]
            Arrow a b -> [tag 1, hashed (hash a), hashed (hash b)]
            App a b -> [tag 2, hashed (hash a), hashed (hash b)]
            Ann a k -> [tag 3, hashed (hash a), Hashable.accumulateToken k]
            -- Example:
            --   a) {Remote, Abort} (() -> {Remote} ()) should hash the same as
            --   b) {Abort, Remote} (() -> {Remote} ()) but should hash differently from
            --   c) {Remote, Abort} (() -> {Abort} ())
            Effects es ->
              let (hs, _) = hashCycle es
               in tag 4 : map hashed hs
            Effect e t -> [tag 5, hashed (hash e), hashed (hash t)]
            Forall a -> [tag 6, hashed (hash a)]
            IntroOuter a -> [tag 7, hashed (hash a)]
