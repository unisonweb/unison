-- | Mock AST and supporting types for the implicit-resolution spike.
--
-- See @docs/implicits-plan.md@ §1.3 and §4. None of this is intended to
-- match Unison's real 'Unison.Type.F' — it is just rich enough to exercise
-- unification with higher-kinded constructors.
module Implicits.Types
  ( -- * Mock type AST
    Ty (..),
    TyVar (..),
    ConName (..),

    -- * Givens
    Given (..),
    Scope (..),
    Pool (..),
    poolFromList,

    -- * Resolution result
    ResolutionTree (..),
    ResolveError (..),
    NearMiss (..),

    -- * Substitutions (used by Unify and Resolve)
    Subst,
    emptySubst,

    -- * Helpers
    freeTyVars,
    applySubst,
    showTy,
  )
where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Set (Set)
import qualified Data.Set as Set

------------------------------------------------------------------------------
-- Mock type AST
------------------------------------------------------------------------------

-- | A Hindley-Milner-ish type AST, curried so HKTs fall out naturally.
--
-- Examples:
--
-- @
--   Show (List Nat)    = TApp (TCon \"Show\")
--                             (TApp (TCon \"List\") (TCon \"Nat\"))
--   Functor f          = TApp (TCon \"Functor\") (TVar f)
--   Map (List a) Nat   = TApp (TApp (TCon \"Map\") (TApp (TCon \"List\") (TVar a)))
--                             (TCon \"Nat\")
-- @
data Ty
  = TVar !TyVar
  | TCon !ConName
  | TApp !Ty !Ty
  deriving stock (Eq, Ord, Show)

-- | Type variable. Both unification metavariables and rigid quantified
-- variables share this representation; the resolver distinguishes them by
-- whether they live in the candidate's own 'givenTyVars' (rigid until
-- freshened) or the unifier's metavariable supply.
newtype TyVar = TyVar Int
  deriving stock (Show)
  deriving newtype (Eq, Ord)

newtype ConName = ConName String
  deriving stock (Show)
  deriving newtype (Eq, Ord)

------------------------------------------------------------------------------
-- Givens
------------------------------------------------------------------------------

-- | A single available given. Universally quantified over 'givenTyVars'.
--
-- A given like @Show a => Show (List a)@ becomes
--
-- @
--   Given { givenName       = \"Show.list\"
--         , givenTyVars     = [a]
--         , givenPremises   = [TApp Show (TVar a)]
--         , givenConclusion = TApp Show (TApp List (TVar a))
--         , givenScope      = Ambient
--         }
-- @
data Given = Given
  { givenName :: !String,
    givenTyVars :: ![TyVar],
    givenPremises :: ![Ty],
    givenConclusion :: !Ty,
    givenScope :: !Scope
  }
  deriving stock (Eq, Show)

-- | Lexical depth or ambient scope. Smaller 'Lexical' values are *more*
-- inner (e.g. @Lexical 0@ is the innermost binder); 'Ambient' is the
-- outermost.
data Scope
  = Lexical !Int
  | Ambient
  deriving stock (Eq, Ord, Show)

-- | The candidate pool. Order is irrelevant; we keep it as a list because
-- we always traverse it linearly.
newtype Pool = Pool {poolGivens :: [Given]}
  deriving stock (Eq, Show)

poolFromList :: [Given] -> Pool
poolFromList = Pool

------------------------------------------------------------------------------
-- Resolution result
------------------------------------------------------------------------------

-- | A successful resolution: which given was chosen, what its conclusion
-- was specialised to, and how each premise was satisfied.
data ResolutionTree = ResolutionTree
  { rtChosen :: !Given,
    rtSpecialisedConclusion :: !Ty,
    rtPremises :: ![ResolutionTree]
  }
  deriving stock (Eq, Show)

-- | Resolution errors, with enough information for the eventual diagnostic
-- formatter.
data ResolveError
  = -- | No matching given for the goal; near-misses are candidates whose
    -- conclusions unified with the goal but whose premises failed.
    NoGiven !Ty ![NearMiss]
  | -- | Multiple givens of incomparable specificity match.
    Ambiguous !Ty ![Given]
  | -- | Resolution chain exceeded the depth limit; the chain (outermost
    -- first) is included for diagnosis.
    DepthExceeded ![Ty]
  deriving stock (Eq, Show)

-- | A candidate that unified at the head but failed somewhere in its
-- premises. Carries the sub-error so we can show the user *why* it didn't
-- pan out.
data NearMiss = NearMiss
  { nmGiven :: !Given,
    nmReason :: !ResolveError
  }
  deriving stock (Eq, Show)

------------------------------------------------------------------------------
-- Substitutions
------------------------------------------------------------------------------

-- | Substitution from 'TyVar' identifiers (the inner 'Int') to types.
type Subst = IntMap Ty

emptySubst :: Subst
emptySubst = IntMap.empty

-- | Free variables of a type.
freeTyVars :: Ty -> Set TyVar
freeTyVars = go Set.empty
  where
    go !acc = \case
      TVar v -> Set.insert v acc
      TCon _ -> acc
      TApp a b -> go (go acc a) b

-- | Apply a substitution to a type. Substitutions must be idempotent
-- (which the unifier maintains), so a single pass is sufficient.
applySubst :: Subst -> Ty -> Ty
applySubst s = go
  where
    go t = case t of
      TVar (TyVar i) -> case IntMap.lookup i s of
        Just t' -> t'
        Nothing -> t
      TCon _ -> t
      TApp a b -> TApp (go a) (go b)

------------------------------------------------------------------------------
-- Pretty-printing helpers
------------------------------------------------------------------------------

-- | Render a 'Ty' as a Unison-ish surface string. Used in error messages
-- and tests.
showTy :: Ty -> String
showTy = top
  where
    top = \case
      TApp f a -> top f ++ " " ++ atom a
      t -> atom t
    atom = \case
      TVar (TyVar i) -> "v" ++ show i
      TCon (ConName n) -> n
      t@TApp {} -> "(" ++ top t ++ ")"
