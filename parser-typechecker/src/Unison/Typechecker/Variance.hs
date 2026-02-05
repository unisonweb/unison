module Unison.Typechecker.Variance where

import Control.Monad.State.Strict
import Data.Foldable (traverse_)
import Data.Graph (flattenSCC, stronglyConnComp)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.DataDeclaration
import Unison.Reference
import Unison.Type
import Unison.Typechecker.TypeLookup (TypeLookup (..))
import Unison.Var (Var, freshIn)

-- Polarity for variable occurrences during checking. This is used both
-- for tracking the ambient polarity as we walk down the type, and
-- recording information about the occurrences so that we can later solve
-- the overall variance of a parameter from its occurrences.
data Polarity v = Positive | Negative | Exact | As v | Op v
  deriving (Eq, Ord, Show)

-- Reverse polarity, for moving into negative positions.
inv :: Polarity v -> Polarity v
inv Positive = Negative
inv Negative = Positive
inv (As v) = Op v
inv (Op v) = As v
-- Reverse of invariant is invariant
inv Exact = Exact

act :: Polarity v -> Polarity v -> Polarity v
act Positive p = p
act Negative p = inv p
act Exact _ = Exact
act _ _ = Exact -- TODO: revisit

-- Concrete variance information for a parameter.
data Variance = Any | Pos | Neg | Inv
  deriving (Eq, Ord, Show)

defaultVariances :: Map Reference [Variance]
defaultVariances =
  Map.fromList
    [ (listRef, [Pos]),
      (iarrayRef, [Pos])
    ]

lookupVariance :: Map Reference [Variance] -> Type v a -> Maybe [Variance]
lookupVariance vs (Ref' r) = Map.lookup r vs
lookupVariance _ _ = Nothing

combine :: (Ord v) => [Map v [Polarity v]] -> Map v [Polarity v]
combine [] = Map.empty
combine (m : ms) = foldl' (Map.unionWith (++)) m ms

collectVariance ::
  (Var v) =>
  Map Reference [Variance] ->
  Map Reference [v] ->
  Type v a ->
  Map v [Polarity v]
collectVariance prev group = descend Positive
  where
    descend pol = \case
      Arrow' i o ->
        Map.unionWith (++) (descend (inv pol) i) (descend pol o)
      Effect1' e r ->
        Map.unionWith (++) (descend pol e) (descend pol r)
      Apps' f xs
        | Ref' r <- f,
          Just bnd <- Map.lookup r group ->
            combine $ zipWith (descend . act pol . As) bnd xs
        | Just vs <- lookupVariance prev f ->
            combine $ descend pol f : zipWith h vs xs
        -- if it's not in the info we have, assume invariant
        | otherwise -> combine $ descend pol f : map (descend Exact) xs
        where
          -- for 'any variant' positions, we don't need to keep inferring
          h Any _ = Map.empty
          h Neg t = descend (inv pol) t
          h Pos t = descend pol t
          h Inv t = descend Exact t
      Ann' t _ -> descend pol t
      Effects' ts -> combine $ map (descend pol) ts
      ForallsNamed' _ t -> descend pol t
      IntroOuterNamed' _ t -> descend pol t
      Var' v -> Map.singleton v [pol]
      _ -> Map.empty

collectDeclVariance ::
  (Var v, Show a) =>
  Map Reference [Variance] ->
  Map Reference [v] ->
  DataDeclaration v a ->
  Map v [Polarity v]
collectDeclVariance vars group decl =
  combine $
    fmap (collectVariance vars group)
      . split
      =<< constructors decl
  where
    split (_, ForallsNamedOpt' _vs (Arrows' ts)) = ts
    split (_, t) = [t]

-- Simplifies some polarities
simplify :: (Var v) => v -> [Polarity v] -> [Polarity v]
simplify v = reduce . Set.delete (As v) . Set.fromList
  where
    reduce s
      -- invariant overrides everything
      | Exact `Set.member` s = [Exact]
      -- both positive and negative is invariant
      | Positive `Set.member` s,
        Negative `Set.member` s =
          [Exact]
      -- a variable that must be its own opposite is invariant
      | Op v `Set.member` s = [Exact]
      | otherwise = Set.toList s

chain :: (Var v) => Map v [Polarity v] -> [Polarity v] -> [Polarity v]
chain m = foldMap f
  where
    -- If an `As` or `Op` is not in the map, we will never be able to
    -- find it. All variables should have been initialized to at least
    -- `x -> As x` by the result types of constructors, so if a
    -- variable isn't in the map, assume the worst and use invariant.
    f (As v) = Map.findWithDefault [Exact] v m
    f (Op v) = inv <$> Map.findWithDefault [Exact] v m
    f p = [p]

checkFinished :: Map v [Polarity v] -> Maybe (Map v Variance)
checkFinished = traverse f
  where
    f [] = Just Any
    f [Exact] = Just Inv
    f [Positive] = Just Pos
    f [Negative] = Just Neg
    f _ = Nothing

solve :: (Var v) => Map v [Polarity v] -> Map v Variance
solve map0
  | Just m <- checkFinished map0 = m
  | otherwise = solve . Map.mapWithKey simplify $ chain map0 <$> map0

inferDeclGroupVariance ::
  (Var v, Show a) =>
  Map Reference [Variance] ->
  Map Reference (DataDeclaration v a) ->
  Map Reference [Variance]
inferDeclGroupVariance vars (freshenGroup -> group) =
  resolveGroup . solve $
    foldMap (collectDeclVariance vars groupVars . snd) group
  where
    groupVars = fst <$> group
    resolveGroup m = Map.mapMaybe (resolve m . fst) group
    resolve m bound = traverse (\v -> Map.lookup v m) bound

freshenGroup ::
  (Var v) =>
  Map Reference (DataDeclaration v a) ->
  Map Reference ([v], DataDeclaration v a)
freshenGroup group = evalState (traverse freshDecl group) Set.empty

freshDecl ::
  (Var v) =>
  DataDeclaration v a ->
  State (Set.Set v) ([v], DataDeclaration v a)
freshDecl dd = do
  vs <- traverse fv (bound dd)
  let frvs = Map.fromList $ zip (bound dd) vs
      f v = Map.findWithDefault v v frvs
  pure (vs, vmap' f dd)
  where
    fv u = state \avoid ->
      let v = freshIn avoid u
       in (v, Set.insert v avoid)

inferDeclVariances ::
  (Var v, Show a) =>
  Map Reference [Variance] ->
  Map Reference (DataDeclaration v a) ->
  Map Reference [Variance]
inferDeclVariances boot (Map.toList -> rdds) =
  execState (traverse_ inf sccs) boot
  where
    inf (Map.fromList . flattenSCC -> ddm) = do
      vs <- get
      put . Map.union vs $ inferDeclGroupVariance vs ddm

    trc p@(r, dd) = (p, r, Set.toList $ typeDependencies dd)
    sccs = stronglyConnComp $ fmap trc rdds

fromTypeLookup ::
  (Var v, Show a) => TypeLookup v a -> Map Reference [Variance]
fromTypeLookup = inferDeclVariances defaultVariances . dataDecls
