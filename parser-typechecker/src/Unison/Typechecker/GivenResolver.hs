-- | Dictionary selection for already determined, monomorphic goals.
module Unison.Typechecker.GivenResolver
  ( GivenName (..),
    Scope (..),
    Given (..),
    ResolveError (..),
    resolve,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.Prelude
import Unison.Reference (TermReference)
import Unison.Type (Type)
import Unison.Var (Var)
import Unison.Var qualified as Var

-- | Local binding identities must be freshened variables, never printed names.
data GivenName v = Local v | Global TermReference
  deriving stock (Eq, Ord, Show)

-- | Larger depths are more local. Namespace candidates have the lowest priority.
data Scope = Ambient | Lexical Int
  deriving stock (Eq, Ord, Show)

data Given v loc = Given
  { givenName :: GivenName v,
    givenTyVars :: [v],
    givenConclusion :: Type v loc,
    givenScope :: Scope
  }
  deriving stock (Show)

data ResolveError v loc
  = NoGiven (Type v loc)
  | Ambiguous (Type v loc) (NonEmpty (GivenName v))
  deriving stock (Show)

-- | Only matching candidates participate in shadowing. Repeated namespace
-- aliases identify the same dictionary and do not create ambiguity.
resolve :: (Var v) => [Given v loc] -> Type v loc -> Either (ResolveError v loc) (GivenName v)
resolve pool goal =
  case Map.toAscList matches of
    [] -> Left (NoGiven goal)
    candidates@((_, firstScope) : rest) ->
      let depth = foldl' max firstScope (map snd rest)
       in case [name | (name, scope) <- candidates, scope == depth] of
            [name] -> Right name
            name : names -> Left (Ambiguous goal (name :| names))
            [] -> Left (NoGiven goal)
  where
    matches =
      Map.fromListWith
        max
        [ (givenName, givenScope)
        | Given {givenName, givenTyVars, givenConclusion, givenScope} <- pool,
          isJust (matchType (Set.fromList givenTyVars) givenConclusion goal)
        ]

-- | Instantiate only candidate variables. Goal variables remain rigid, even
-- when they have the same names as variables quantified by the candidate.
matchType :: forall v loc. (Var v) => Set v -> Type v loc -> Type v loc -> Maybe (Map v (Type v loc))
matchType flexible = go Set.empty Map.empty
  where
    go :: Set v -> Map v (Type v loc) -> Type v loc -> Type v loc -> Maybe (Map v (Type v loc))
    go bound substitution patternType target = case (ABT.out patternType, ABT.out target) of
      (ABT.Var v, _)
        | Set.member v flexible,
          Set.disjoint bound (ABT.freeVars target) ->
            case Map.lookup v substitution of
              Nothing -> Just (Map.insert v target substitution)
              Just previous -> substitution <$ guard (previous == target)
      (ABT.Var v, ABT.Var w) -> substitution <$ guard (v == w)
      (ABT.Tm f, ABT.Tm g)
        | (() <$ f) == (() <$ g) ->
            foldM (\s (p, t) -> go bound s p t) substitution (zip (toList f) (toList g))
      (ABT.Abs v p, ABT.Abs w t) ->
        let used =
              flexible
                <> Set.fromList (ABT.allVars patternType <> ABT.allVars target)
                <> Set.unions (map ABT.freeVars (Map.elems substitution))
            fresh = Var.freshIn used v
         in go (Set.insert fresh bound) substitution (ABT.rename v fresh p) (ABT.rename w fresh t)
      _ -> Nothing
