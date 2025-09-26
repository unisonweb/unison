module Unison.KindInference.Generate.Monad
  ( Gen (..),
    GenState (..),
    GenError (..),
    GeneratedConstraint,
    run,
    freshVar,
    pushType,
    popType,
    scopedType,
    lookupType,
  )
where

import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Functor.Compose
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.ABT (ABT (Tm), Term (Term))
import Unison.KindInference.Constraint.Provenance (Provenance)
import Unison.KindInference.Constraint.Unsolved (Constraint (..))
import Unison.KindInference.UVar (UVar (..))
import Unison.Prelude
import Unison.Symbol
import Unison.Type qualified as T
import Unison.Var

-- | A generated constraint
type GeneratedConstraint v loc = Constraint (UVar v loc) v loc Provenance

-- | The @Gen@ monad state
data GenState v loc = GenState
  { unifVars :: !(Set Symbol),
    typeMap :: !(Map (T.Type v loc) (NonEmpty (UVar v loc))),
    newVars :: [UVar v loc]
  }
  deriving stock (Generic)

data GenError = MissingBuiltin Text

newtype Gen v loc a = Gen
  { unGen :: StateT (GenState v loc) (Except GenError) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState (GenState v loc),
      MonadError GenError
    )

-- | @Gen@ monad runner
run :: Gen v loc a -> GenState v loc -> Either GenError (a, GenState v loc)
run (Gen ma) st0 =
  runStateT ma st0
    & runExcept

-- | Create a unique @UVar@ associated with @typ@
freshVar :: (Var v) => T.Type v loc -> Gen v loc (UVar v loc)
freshVar typ = do
  st@GenState {unifVars, newVars} <- get
  let var :: Symbol
      var = freshIn unifVars (typed (Inference Other))
      uvar = UVar var typ
      unifVars' = Set.insert var unifVars
  put st {unifVars = unifVars', newVars = uvar : newVars}
  pure uvar

-- | Associate a fresh @UVar@ with @t@, push onto context
pushType :: (Var v) => T.Type v loc -> Gen v loc (UVar v loc)
pushType t = do
  GenState {typeMap} <- get
  (var, newTypeMap) <-
    let f = \case
          Nothing -> Compose $ (\v -> (v, Just (v :| []))) <$> freshVar t
          Just xs -> Compose $ (\v -> (v, Just (NonEmpty.cons v xs))) <$> freshVar t
     in getCompose $ Map.alterF f t typeMap
  modify \st -> st {typeMap = newTypeMap}
  pure var

-- | Lookup the @UVar@ associated with a @Type@
lookupType :: (Var v) => T.Type v loc -> Gen v loc (Maybe (UVar v loc))
lookupType t = do
  GenState {typeMap} <- get
  pure (NonEmpty.head <$> lookups t typeMap)
  where
    lookups t typeMap
      | r@(Just _) <- Map.lookup t typeMap = r
    lookups (Term _ _ (Tm (T.Effects [v]))) typeMap = lookups v typeMap
    lookups _ _ = Nothing

-- | Remove a @Type@ from the context
popType :: (Var v) => T.Type v loc -> Gen v loc ()
popType t = do
  modify \st -> st {typeMap = del (typeMap st)}
  where
    del m =
      let f = \case
            Nothing -> Nothing
            Just (_ :| ys) -> case ys of
              [] -> Nothing
              x : xs -> Just (x :| xs)
       in Map.alter f t m

-- | Helper to run an action with the given @Type@ in the context
scopedType :: (Var v) => T.Type v loc -> (UVar v loc -> Gen v loc r) -> Gen v loc r
scopedType t m = do
  s <- pushType t
  r <- m s
  popType t
  pure r
