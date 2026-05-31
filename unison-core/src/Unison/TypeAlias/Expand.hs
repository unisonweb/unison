module Unison.TypeAlias.Expand
  ( -- * Expansion (Var-based, used during in-file parsing)
    expand,
    expandAll,
    expandInTerm,
    ExpansionError (..),

    -- * Expansion (Reference-based, used after name resolution)
    expandRefs,
    expandRefsInTerm,
    RefExpansionError (..),

    -- * Normalization
    normalize,
    NormalizationError (..),
  )
where

import Data.List (nubBy)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Term (Term)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.TypeAlias (TypeAlias (..))
import Unison.TypeAlias qualified as TypeAlias
import Unison.Var (Var)

-- | Errors produced during alias expansion.
data ExpansionError v a
  = -- | Alias used without enough arguments. Carries the alias name, the
    -- expected arity, the actual number of arguments at the use site, and
    -- the annotation pointing at the use site.
    UnsaturatedAliasUse v Int Int a
  deriving stock (Show, Eq)

-- | Errors produced while normalizing a set of aliases.
data NormalizationError v
  = -- | Aliases form a dependency cycle. The set names the alias members
    -- involved in some cycle.
    AliasCycle (Set v)
  deriving stock (Show, Eq)

-- | Walk a type and replace saturated alias applications with their bodies.
--
-- Aliases referenced in the type must be present in the given map. Unknown
-- alias names are ignored — they pass through as ordinary variables (which
-- will then be handled by name resolution or implicit forall generalization
-- downstream).
--
-- Aliases must be fully saturated at every use site; partial application is
-- rejected because Unison's 'Type.F' has no lambda constructor to represent
-- the result. See @docs/type-aliases.markdown@.
--
-- The map is expected to contain aliases whose bodies do not themselves
-- reference other aliases (i.e. produced by 'normalize'). Passing
-- un-normalized aliases is safe but produces under-expanded types.
expand ::
  forall v a.
  (Var v, Semigroup a) =>
  Map v (TypeAlias v a) ->
  Type v a ->
  Either (ExpansionError v a) (Type v a)
expand aliases = go
  where
    go :: Type v a -> Either (ExpansionError v a) (Type v a)
    go t = case Type.unApps t of
      Just (Type.Var' v, args)
        | Just alias <- Map.lookup v aliases ->
            applyAlias v alias args (ABT.annotation t)
      _ -> case ABT.out t of
        -- A bare reference to an alias (no application) is an unsaturated
        -- use unless the alias is nullary.
        ABT.Var v
          | Just alias <- Map.lookup v aliases ->
              applyAlias v alias [] (ABT.annotation t)
        _ -> goABT t

    applyAlias ::
      v ->
      TypeAlias v a ->
      [Type v a] ->
      a ->
      Either (ExpansionError v a) (Type v a)
    applyAlias v alias args useSite = do
      let arity = TypeAlias.arity alias
          nArgs = length args
      if nArgs < arity
        then Left (UnsaturatedAliasUse v arity nArgs useSite)
        else do
          let (sat, extra) = splitAt arity args
          sat' <- traverse go sat
          extra' <- traverse go extra
          let body' =
                ABT.substsInheritAnnotation (zip alias.paramNames sat') alias.body
          pure (Type.apps' body' extra')

    -- Recurse into the structure when the head isn't an alias application.
    -- 'Type.Effects' is handled specially: an ability-row alias used as an
    -- element of an Effects list splices its body's elements into the
    -- surrounding row, with set semantics applied afterwards (flatten +
    -- dedupe).
    goABT :: Type v a -> Either (ExpansionError v a) (Type v a)
    goABT t = case ABT.out t of
      ABT.Var v -> Right (ABT.annotatedVar (ABT.annotation t) v)
      ABT.Cycle body -> ABT.cycle' (ABT.annotation t) <$> goABT body
      ABT.Abs v body -> ABT.abs' (ABT.annotation t) v <$> goABT body
      ABT.Tm (Type.Effects es) -> do
        expandedElems <- traverse go es
        let flattened = concatMap spliceRowElement expandedElems
            canonical = dedupRow flattened
        Right (ABT.tm' (ABT.annotation t) (Type.Effects canonical))
      ABT.Tm f -> ABT.tm' (ABT.annotation t) <$> traverse go f

-- | If a type is itself an Effects list, extract its elements; otherwise
-- wrap the type as a singleton. Used to splice ability-row aliases into
-- their surrounding row after expansion.
spliceRowElement :: Type v a -> [Type v a]
spliceRowElement t = case ABT.out t of
  ABT.Tm (Type.Effects es) -> es
  _ -> [t]

-- | Stable dedupe of ability-row elements by structural equality (ignoring
-- annotations). Order is determined by first appearance.
dedupRow :: (Var v) => [Type v a] -> [Type v a]
dedupRow = nubBy (\x y -> stripAnns x == stripAnns y)
  where
    stripAnns = ABT.amap (const ())

-- | Apply 'expand' to a foldable of types, accumulating errors.
expandAll ::
  (Var v, Semigroup a, Traversable t) =>
  Map v (TypeAlias v a) ->
  t (Type v a) ->
  Either (ExpansionError v a) (t (Type v a))
expandAll aliases = traverse (expand aliases)

-- * Reference-based expansion (used after name resolution)

-- | Errors produced during ref-based expansion. Same shape as
-- 'ExpansionError' but keyed by 'Reference' rather than 'Var'.
data RefExpansionError a
  = UnsaturatedAliasRefUse Reference Int Int a
  deriving stock (Show, Eq)

-- | Walk a type and expand alias 'Reference's against their bodies. Used
-- after name resolution, when alias names have been bound to refs but the
-- in-file expansion (which is keyed by 'Var') no longer matches.
--
-- The map keys are 'Reference.Id' values for derived alias refs (builtin
-- type refs are never aliases). Saturation rules are the same as 'expand':
-- under-application is an error.
expandRefs ::
  forall v a.
  (Var v, Semigroup a) =>
  Map Reference.Id (TypeAlias v a) ->
  Type v a ->
  Either (RefExpansionError a) (Type v a)
expandRefs aliases = go
  where
    go :: Type v a -> Either (RefExpansionError a) (Type v a)
    go t = case Type.unApps t of
      Just (Type.Ref' r, args)
        | Reference.DerivedId rid <- r,
          Just alias <- Map.lookup rid aliases ->
            applyAlias r alias args (ABT.annotation t)
      _ -> case ABT.out t of
        ABT.Tm (Type.Ref r)
          | Reference.DerivedId rid <- r,
            Just alias <- Map.lookup rid aliases ->
              applyAlias r alias [] (ABT.annotation t)
        _ -> goABT t

    applyAlias :: Reference -> TypeAlias v a -> [Type v a] -> a -> Either (RefExpansionError a) (Type v a)
    applyAlias r alias args useSite = do
      let arity = TypeAlias.arity alias
          nArgs = length args
      if nArgs < arity
        then Left (UnsaturatedAliasRefUse r arity nArgs useSite)
        else do
          let (sat, extra) = splitAt arity args
          sat' <- traverse go sat
          extra' <- traverse go extra
          let body' =
                ABT.substsInheritAnnotation (zip alias.paramNames sat') alias.body
          pure (Type.apps' body' extra')

    goABT :: Type v a -> Either (RefExpansionError a) (Type v a)
    goABT t = case ABT.out t of
      ABT.Var v -> Right (ABT.annotatedVar (ABT.annotation t) v)
      ABT.Cycle body -> ABT.cycle' (ABT.annotation t) <$> goABT body
      ABT.Abs v body -> ABT.abs' (ABT.annotation t) v <$> goABT body
      ABT.Tm f -> ABT.tm' (ABT.annotation t) <$> traverse go f

-- | Walk a term and apply 'expandRefs' to every type carried by a
-- 'Term.Ann' node.
expandRefsInTerm ::
  forall v a.
  (Var v, Semigroup a) =>
  Map Reference.Id (TypeAlias v a) ->
  Term v a ->
  Either (RefExpansionError a) (Term v a)
expandRefsInTerm aliases = ABT.transformM go
  where
    go :: forall x. Term.F v a a x -> Either (RefExpansionError a) (Term.F v a a x)
    go = \case
      Term.Ann body typ -> Term.Ann body <$> expandRefs aliases typ
      other -> Right other

-- * In-file expansion (Var-based)

-- | Walk a term and apply 'expand' to every type appearing in a type
-- annotation (the 'Term.Ann' node).
--
-- 'Term.F' carries inline 'Type' expressions only in @Ann a Type@ nodes;
-- @Constructor@, @Request@, and @TypeLink@ use 'Reference's and don't carry
-- inline types. So this walk is exhaustive for in-line type usage.
expandInTerm ::
  forall v a.
  (Var v, Semigroup a) =>
  Map v (TypeAlias v a) ->
  Term v a ->
  Either (ExpansionError v a) (Term v a)
expandInTerm aliases = ABT.transformM go
  where
    go :: forall x. Term.F v a a x -> Either (ExpansionError v a) (Term.F v a a x)
    go = \case
      Term.Ann body typ -> Term.Ann body <$> expand aliases typ
      other -> Right other

-- | Normalize a set of aliases so each body no longer mentions any other
-- alias from the input map. Detects dependency cycles.
--
-- After normalization, calling 'expand' with the result map fully expands
-- a single use site in one pass — no need to iterate to a fixpoint.
normalize ::
  forall v a.
  (Var v, Semigroup a) =>
  Map v (TypeAlias v a) ->
  Either (NormalizationError v) (Map v (TypeAlias v a))
normalize aliases = do
  order <- topoSort aliases
  pure (foldl' step Map.empty order)
  where
    step :: Map v (TypeAlias v a) -> v -> Map v (TypeAlias v a)
    step acc v =
      let alias = aliases Map.! v
          body' = case expand acc alias.body of
            -- 'expand' only fails on UnsaturatedAliasUse; if it happens here
            -- it means an alias body uses another alias unsaturated, which is
            -- a real user error that downstream typechecking will report (we
            -- leave the body as-is so the original error site is preserved).
            Left _ -> alias.body
            Right b -> b
       in Map.insert v (alias {body = body'}) acc

-- | Topologically sort aliases by dependency. The body of each alias is
-- inspected for free variables that match other alias names; those are
-- treated as edges.
topoSort ::
  forall v a.
  (Var v) =>
  Map v (TypeAlias v a) ->
  Either (NormalizationError v) [v]
topoSort aliases = reverse <$> dfsAll Set.empty Set.empty (Map.keys aliases) []
  where
    -- Build alias -> set of alias names mentioned in its body, excluding the
    -- alias's own params (which are bound inside the body).
    edges :: Map v (Set v)
    edges =
      aliases & Map.map \alias ->
        ABT.freeVars alias.body
          & Set.intersection (Map.keysSet aliases)
          & flip Set.difference (Set.fromList alias.paramNames)

    -- DFS with two-color marking: 'inProgress' detects back-edges (cycles);
    -- 'done' suppresses revisits.
    dfsAll ::
      Set v -> -- done
      Set v -> -- in progress (path)
      [v] ->
      [v] -> -- accumulator (post-order, reversed at the end)
      Either (NormalizationError v) [v]
    dfsAll _ _ [] acc = pure acc
    dfsAll done inProgress (v : vs) acc
      | Set.member v done = dfsAll done inProgress vs acc
      | otherwise = do
          (done', acc') <- dfs done inProgress v acc
          dfsAll done' inProgress vs acc'

    dfs ::
      Set v ->
      Set v ->
      v ->
      [v] ->
      Either (NormalizationError v) (Set v, [v])
    dfs done inProgress v acc
      | Set.member v done = pure (done, acc)
      | Set.member v inProgress = Left (AliasCycle (Set.insert v inProgress))
      | otherwise = do
          let inProgress' = Set.insert v inProgress
              deps = Map.findWithDefault Set.empty v edges
          (done', acc') <- foldM (\(d, a) u -> dfs d inProgress' u a) (done, acc) (Set.toList deps)
          pure (Set.insert v done', v : acc')
