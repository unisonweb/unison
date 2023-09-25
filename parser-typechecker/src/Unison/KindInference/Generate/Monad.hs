module Unison.KindInference.Generate.Monad
  ( Gen (..),
    GenState (..),
    GeneratedConstraint,
    run,
    freshVar,
    insertType,
    deleteType,
    scopedType,
    lookupType,
  )
where

import Data.Set qualified as Set
import Control.Monad.State.Strict
import Data.Functor.Compose
import Data.Map.Strict qualified as Map
import Unison.KindInference.Constraint.Unsolved (Constraint (..))
import Unison.KindInference.Constraint.Provenance (Provenance)
import Unison.KindInference.UVar (UVar(..))
import Unison.Prelude
import Unison.Symbol
import Unison.Type qualified as T
import Unison.Var

type GeneratedConstraint v loc = Constraint (UVar v loc) v loc Provenance

data GenState v loc = GenState
  { unifVars :: !(Set Symbol),
    typeMap :: !(Map (T.Type v loc) (UVar v loc)),
    newVars :: [UVar v loc]
  }
  deriving stock (Generic)

newtype Gen v loc a = Gen
  { unGen :: GenState v loc -> (a, GenState v loc)
  }
  deriving
    ( Functor
    , Applicative
    , Monad,
      MonadState (GenState v loc)
    ) via State (GenState v loc)


run :: Gen v loc a -> GenState v loc -> (a, GenState v loc)
run (Gen ma) st0 = ma st0

-- | Create a unique @UVar@ associated with @typ@
freshVar :: Var v => T.Type v loc -> Gen v loc (UVar v loc)
freshVar typ = do
  st@GenState{unifVars, newVars} <- get
  let var :: Symbol
      var = freshIn unifVars (typed (Inference Other))
      uvar = UVar var typ
      unifVars' = Set.insert var unifVars
  put st { unifVars = unifVars', newVars = uvar : newVars }
  pure uvar

-- | Lookup the @UVar@ associated with @t@, or create one if it
-- doesn't exist
insertType :: Var v => T.Type v loc -> Gen v loc (UVar v loc)
insertType t = do
  GenState {typeMap} <- get
  (var, newTypeMap) <-
    let f = \case
          Nothing -> Compose $ (\v -> (v, Just v)) <$> freshVar t
          Just v -> Compose (pure (v, Just v))
     in getCompose $ Map.alterF f t typeMap
  modify \st -> st {typeMap = newTypeMap}
  pure var

lookupType :: Var v => T.Type v loc -> Gen v loc (Maybe (UVar v loc))
lookupType t = do
  GenState {typeMap} <- get
  pure (Map.lookup t typeMap)

deleteType :: Var v => T.Type v loc -> Gen v loc ()
deleteType t = do
  modify \st -> st {typeMap = Map.delete t (typeMap st)}

scopedType :: Var v => T.Type v loc -> (UVar v loc -> Gen v loc r) -> Gen v loc r
scopedType t m = do
  s <- insertType t
  r <- m s
  deleteType t
  pure r
