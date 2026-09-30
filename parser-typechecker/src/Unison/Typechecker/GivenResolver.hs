-- | Bounded dictionary search for already determined goals.
module Unison.Typechecker.GivenResolver
  ( GivenName (..),
    Scope (..),
    Given (..),
    ResolveError (..),
    ResolutionTree (..),
    Limits (..),
    givenFromType,
    resolve,
    resolveWith,
  )
where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.State.Strict (State, StateT, evalState, execStateT, get, put)
import Data.List (mapAccumL)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.Prelude
import Unison.Reference (TermReference)
import Unison.Type (Type)
import Unison.Type qualified as Type
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
    givenPremises :: [Type v loc],
    givenConclusion :: Type v loc,
    givenScope :: Scope
  }
  deriving stock (Show)

data ResolveError v loc
  = NoGiven (Type v loc)
  | Ambiguous (Type v loc) (NonEmpty (GivenName v))
  | SearchLimit [Type v loc]
  deriving stock (Show)

data ResolutionTree v loc = ResolutionTree
  { resolvedGiven :: GivenName v,
    resolvedType :: Type v loc,
    resolvedPremises :: [ResolutionTree v loc]
  }
  deriving stock (Show)

data Limits = Limits {depthLimit :: Int, workLimit :: Int}

-- | Extract a candidate's quantified variables and dictionary premises.
-- Freshening preserves the identity of shadowed type variables, including
-- quantifiers between premises. Free variables remain rigid.
givenFromType :: (Var v) => GivenName v -> Scope -> Type v loc -> Given v loc
givenFromType name scope typ = go (Set.fromList (ABT.allVars typ)) [] [] typ
  where
    go used vars premises typ = case typ of
      Type.ForallNamed' v body ->
        let fresh = Var.freshIn used v
         in go (Set.insert fresh used) (fresh : vars) premises (ABT.rename v fresh body)
      Type.ImplicitArrow' premise body -> go used vars (premise : premises) body
      _ -> Given name (reverse vars) (reverse premises) typ scope

-- | Only matching candidates participate in shadowing. Repeated namespace
-- aliases identify the same dictionary and do not create ambiguity.
resolve :: (Var v) => [Given v loc] -> Type v loc -> Either (ResolveError v loc) (ResolutionTree v loc)
resolve = resolveWith (Limits 50 10000)

-- | A search limit is inconclusive, so it cannot be treated as a failed
-- candidate when deciding that another candidate is the unique winner.
resolveWith :: forall v loc. (Var v) => Limits -> [Given v loc] -> Type v loc -> Either (ResolveError v loc) (ResolutionTree v loc)
resolveWith limits pool goal = evalState (search [] goal) 0
  where
    candidates = Map.elems (Map.fromListWith deeperGiven [(givenName g, g) | g <- pool])
    deeperGiven g h = if givenScope g >= givenScope h then g else h
    scopes = map (reverse . snd) (Map.toDescList (Map.fromListWith (<>) [(givenScope g, [g]) | g <- candidates]))
    search :: [Type v loc] -> Type v loc -> State Int (Either (ResolveError v loc) (ResolutionTree v loc))
    search ancestors goal
      | not (withinGoalSize 4096 goal) = pure (Left (SearchLimit (reverse ancestors)))
      | goal `elem` ancestors = pure (Left (NoGiven goal))
      | length ancestors > depthLimit limits = pure (Left (SearchLimit (reverse (goal : ancestors))))
      | otherwise = searchScopes ancestors goal scopes

    searchScopes _ goal [] = pure (Left (NoGiven goal))
    searchScopes ancestors goal (scope : rest) = do
      attempts <- traverse (attempt (goal : ancestors) goal) scope
      case [chain | Left (SearchLimit chain) <- attempts] of
        chain : _ -> pure (Left (SearchLimit chain))
        [] -> case [(given, tree) | Right (given, tree) <- attempts] of
          [] -> searchScopes ancestors goal rest
          successes -> pure (select goal successes)

    attempt ancestors goal given = do
      work <- get
      put (work + 1)
      if work >= workLimit limits
        then pure (Left (SearchLimit (reverse ancestors)))
        else case instantiate goal given of
          Nothing -> pure (Left (NoGiven goal))
          Just premises -> do
            children <- runExceptT (traverse (ExceptT . search ancestors) premises)
            pure ((\trees -> (given, ResolutionTree (givenName given) goal trees)) <$> children)

    select goal successes = case filter (\(g, _) -> not (any (\(h, _) -> moreSpecific h g) successes)) successes of
      [] -> Left (NoGiven goal)
      [(_, tree)] -> Right tree
      (g, _) : gs -> Left (Ambiguous goal (givenName g :| map (givenName . fst) gs))

-- | Compare declared conclusions, before specializing either to the goal.
-- Equivalent or incomparable patterns remain ambiguous.
moreSpecific :: (Var v) => Given v loc -> Given v loc -> Bool
moreSpecific a b = matches b a && not (matches a b)
  where
    matches candidate target =
      isJust (matchType (Set.fromList (givenTyVars candidate)) (givenConclusion candidate) (givenConclusion target))

-- | A depth bound alone does not bound goals such as C a -> C (Pair a a).
-- Stop after a bounded traversal, before matching or reporting the growing goal.
withinGoalSize :: forall v loc. Int -> Type v loc -> Bool
withinGoalSize budget goal = isJust (execStateT (walk goal) budget)
  where
    walk :: Type v loc -> StateT Int Maybe ()
    walk term = do
      remaining <- get
      guard (remaining > 0)
      put (remaining - 1)
      traverse_ walk (ABT.out term)

-- | Freshen candidate variables away from the goal before substitution.
-- Every variable needed by a premise must be determined by head matching.
instantiate :: (Var v) => Type v loc -> Given v loc -> Maybe [Type v loc]
instantiate goal Given {givenTyVars, givenPremises, givenConclusion} = do
  let used = Set.fromList (givenTyVars <> concatMap ABT.allVars (goal : givenConclusion : givenPremises))
      freshen seen v = let v' = Var.freshIn seen v in (Set.insert v' seen, v')
      (_, fresh) = mapAccumL freshen used givenTyVars
      rename = ABT.substsInheritAnnotation (zip givenTyVars (map ABT.var fresh))
      conclusion = rename givenConclusion
      premises = map rename givenPremises
  substitution <- matchType (Set.fromList fresh) conclusion goal
  let needed = Set.fromList fresh `Set.intersection` Set.unions (map ABT.freeVars premises)
  guard (needed `Set.isSubsetOf` Map.keysSet substitution)
  pure (map (ABT.substsInheritAnnotation (Map.toList substitution)) premises)

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
