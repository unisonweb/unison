
module Unison.Typechecker.Variance where

import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Unison.DataDeclaration
import Unison.Reference
import Unison.Type
import Unison.Var (Var)

-- Polarity for variable occurrences during checking. This is used both
-- for tracking the ambient polarity as we walk down the type, and
-- recording information about the occurrences so that we can later solve
-- the overall variance of a parameter from its occurrences.
data Polarity v = Positive | Negative | Exact | As v | Op v
  deriving (Eq, Ord)

-- Reverse polarity, for moving into negative positions.
inv :: Polarity v -> Polarity v
inv Positive = Negative
inv Negative = Positive
inv (As v) = Op v
inv (Op v) = As v
-- Reverse of invariant is invariant
inv Exact = Exact

-- Concrete variance information for a parameter.
data Variance = Any | Pos | Neg | Inv
  deriving (Eq, Ord)

both :: Variance -> Variance -> Variance
both Any   v = v
both   v Any = v
both Pos Pos = Pos
both Neg Neg = Neg
both   _   _ = Inv

defaultVariances :: Map Reference [Variance]
defaultVariances =
  Map.fromList
    [ (listRef, [Pos])
    , (iarrayRef, [Pos])
    ]

lookupVariance :: Map Reference [Variance] -> Type v a -> Maybe [Variance]
lookupVariance vs (Ref' r) = Map.lookup r vs
lookupVariance _ _ = Nothing

combine :: Ord v => [Map v [Polarity v]] -> Map v [Polarity v]
combine [] = Map.empty
combine (m:ms) = foldl' (Map.unionWith (++)) m ms

collectVariance ::
  Var v => Map Reference [Variance] -> Type v a -> Map v [Polarity v]
collectVariance prev = descend Positive
  where
    descend pol = \case
      Arrow' i o ->
        Map.unionWith (++) (descend (inv pol) i) (descend pol o)
      Effect1' e r ->
        Map.unionWith (++) (descend pol e) (descend pol r)
      Apps' f xs
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
  (Var v, Show a) => DataDeclaration v a -> Map v [Polarity v]
collectDeclVariance decl =
  combine $ collectVariance defaultVariances . snd <$> constructors decl

-- Simplifies some polarities
simplify :: Var v => v -> [Polarity v] -> [Polarity v]
simplify v = reduce . Set.delete (As v) . Set.fromList
  where
    reduce s
      -- invariant overrides everything
      | Exact `Set.member` s = [Exact]
      -- both positive and negative is invariant
      | Positive `Set.member` s,
        Negative `Set.member` s = [Exact]
      -- a variable that must be its own opposite is invariant
      | Op v `Set.member` s = [Exact]
      | otherwise = Set.toList s

chain :: Var v => Map v [Polarity v] -> [Polarity v] -> [Polarity v]
chain m = foldMap f
  where
    f (As v) = Map.findWithDefault [] v m
    f (Op v) = inv <$> Map.findWithDefault [] v m
    f p = [p]

checkFinished :: Map v [Polarity v] -> Maybe (Map v Variance)
checkFinished = traverse f
  where
    f [] = Just Any
    f [Exact] = Just Inv
    f [Positive] = Just Pos
    f [Negative] = Just Neg
    f _ = Nothing

solve :: Var v => Map v [Polarity v] -> Map v Variance
solve map0
  | Just m <- checkFinished map0 = m
  | otherwise = solve . Map.mapWithKey simplify $ chain map0 <$> map0

