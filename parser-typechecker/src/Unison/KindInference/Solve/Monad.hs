module Unison.KindInference.Solve.Monad
  ( Solve (..),
    Env (..),
    SolveState (..),
    Descriptor (..),
    ConstraintMap,
    run,
    emptyState,
    find,
    genStateL,
    runGen,
    addUnconstrainedVar,
  )
where

import Control.Lens (Lens', (%%~))
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.Reader qualified as M
import Control.Monad.State.Strict qualified as M
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Unison.KindInference.Constraint.Solved (Constraint (..))
import Unison.KindInference.Generate.Monad (Gen (..))
import Unison.KindInference.Generate.Monad qualified as Gen
import Unison.KindInference.UVar (UVar (..))
import Unison.PatternMatchCoverage.UFMap qualified as U
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.Symbol
import Unison.Type qualified as T
import Unison.Var

data Env = Env {prettyPrintEnv :: PrettyPrintEnv}

type ConstraintMap v loc = U.UFMap (UVar v loc) (Descriptor v loc)

-- | The @SolveState@ holds all kind constraints gathered for each
-- type. For example, after processing data and effect decls the
-- @typeMap@ will hold entries for every decl, and looking up the
-- corresponding @UVar@ in @constraints@ will return its kind.
--
-- The other fields, @unifVars@ and @newUnifVars@, are relevant when
-- interleaving constraint generation with solving. Constraint
-- generation needs to create fresh unification variables, so it needs
-- the set of bound unification variables from
-- @unifVars@. @newUnifVars@ holds the uvars that are candidates for
-- kind defaulting (see
-- 'Unison.KindInference.Solve.defaultUnconstrainedVars').
data SolveState v loc = SolveState
  { unifVars :: !(Set Symbol),
    newUnifVars :: [UVar v loc],
    constraints :: !(U.UFMap (UVar v loc) (Descriptor v loc)),
    typeMap :: !(Map (T.Type v loc) (NonEmpty (UVar v loc)))
  }

-- | Constraints associated with a unification variable
data Descriptor v loc = Descriptor
  { descriptorConstraint :: Maybe (Constraint (UVar v loc) v loc)
  }

newtype Solve v loc a = Solve {unSolve :: Env -> SolveState v loc -> (a, SolveState v loc)}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadFix,
      M.MonadReader Env,
      M.MonadState (SolveState v loc)
    )
    via M.ReaderT Env (M.State (SolveState v loc))

-- | Helper for inteleaving constraint generation and solving
genStateL :: Lens' (SolveState v loc) (Gen.GenState v loc)
genStateL f st =
  ( \genState ->
      st
        { unifVars = Gen.unifVars genState,
          typeMap = Gen.typeMap genState
        }
  )
    <$> f
      Gen.GenState
        { unifVars = unifVars st,
          typeMap = typeMap st,
          newVars = []
        }

-- | Interleave constraint generation into constraint solving
runGen :: (Var v) => Gen v loc a -> Solve v loc a
runGen gena = do
  st <- M.get
  let gena' = do
        res <- gena
        st <- M.get
        pure (res, Gen.newVars st)
  let ((cs, vs), st') = st & genStateL %%~ Gen.run gena'
  M.put st'
  traverse_ addUnconstrainedVar vs
  M.modify \st -> st {newUnifVars = vs ++ newUnifVars st}
  pure cs

-- | Add a unification variable to the constarint mapping with no
-- constraints. This is done on uvars created during constraint
-- generation to initialize the new uvars (see 'runGen').
addUnconstrainedVar :: (Var v) => UVar v loc -> Solve v loc ()
addUnconstrainedVar uvar = do
  st@SolveState {constraints} <- M.get
  let constraints' = U.insert uvar Descriptor {descriptorConstraint = Nothing} constraints
  M.put st {constraints = constraints'}

-- | Runner for the @Solve@ monad
run :: Env -> SolveState v loc -> Solve v loc a -> (a, SolveState v loc)
run e st action = unSolve action e st

-- | Initial solve state
emptyState :: SolveState v loc
emptyState =
  SolveState
    { unifVars = Set.empty,
      newUnifVars = [],
      constraints = U.empty,
      typeMap = M.empty
    }

-- | Lookup the constraints associated with a unification variable
find :: (Var v) => UVar v loc -> Solve v loc (Maybe (Constraint (UVar v loc) v loc))
find k = do
  st@SolveState {constraints} <- M.get
  case U.lookupCanon k constraints of
    Just (_canon, _size, Descriptor {descriptorConstraint}, constraints') -> do
      M.put st {constraints = constraints'}
      pure descriptorConstraint
    Nothing -> error "find: Nothing"
