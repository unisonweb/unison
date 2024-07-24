module Unison.KindInference.Generate.Monad
  ( Gen (..),
    GenState (..),
    GeneratedConstraint,
    run,
    freshVar,
    pushType,
    popType,
    scopedType,
    lookupType,
  )
where

import Control.Monad.State.Strict
import Data.Functor.Compose
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
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

newtype Gen v loc a = Gen
  { unGen :: GenState v loc -> (a, GenState v loc)
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState (GenState v loc)
    )
    via State (GenState v loc)

-- | @Gen@ monad runner
run :: Gen v loc a -> GenState v loc -> (a, GenState v loc)
run (Gen ma) st0 = ma st0

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
  pure (NonEmpty.head <$> Map.lookup t typeMap)

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
