module Unison.KindInference.Generate.Monad
  ( Gen (..),
    GenState (..),
    GeneratedConstraint,
    runList,
    addConstraint,
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
    typeMap :: !(Map (T.Type v loc) (UVar v loc))
  }
  deriving stock (Generic)

newtype Gen v loc a = Gen
  { unGen ::
      forall r.
      -- Add constraint handler
      ( GeneratedConstraint v loc ->
        -- The handler may need the state
        GenState v loc ->
        -- the handler may modify the state
        (GenState v loc -> r) ->
        r
      ) ->
      (UVar v loc -> r -> r) ->
      GenState v loc ->
      (a -> GenState v loc -> r) ->
      r
  }

instance Functor (Gen v loc) where
  fmap f (Gen m) = Gen \cons initVar st k ->
    m cons initVar st (k . f)

instance Applicative (Gen v loc) where
  pure a = Gen \_ _ st k -> k a st
  Gen mf <*> Gen ma = Gen \cons initVar st k ->
    mf cons initVar st (\f st -> ma cons initVar st (\a st -> k (f a) st))

instance Monad (Gen v loc) where
  Gen ma >>= f = Gen \cons initVar st k ->
    ma cons initVar st (\a st -> unGen (f a) cons initVar st k)

instance MonadState (GenState v loc) (Gen v loc) where
  get = Gen \_ _ st k -> k st st
  put st = Gen \_ _ _ k -> k () st

-- | Accumulate the constraints in a list and return the final state
runList :: Gen v loc a -> GenState v loc -> ([GeneratedConstraint v loc], [UVar v loc], GenState v loc)
runList (Gen ma) st0 =
  ma handleConstraint handleVar st0 finalK
  where
    handleConstraint c st k =
      let (cs, vs, st') = k st
       in (c : cs, vs, st')

    handleVar v (cs, vs, st) = (cs, v : vs, st)
    finalK _ finalState = ([], [], finalState)

-- | Add a constraint
addConstraint :: GeneratedConstraint v loc -> Gen v loc ()
addConstraint c = Gen \cons _ st k ->
  cons c st (k ())


initializeUVar :: UVar v loc -> Gen v loc ()
initializeUVar u = Gen \_ initVar st k ->
  initVar u (k () st)

-- | Create a unique @UVar@ associated with @typ@
freshVar :: Var v => T.Type v loc -> Gen v loc (UVar v loc)
freshVar typ = do
  st@GenState{unifVars} <- get
  let var :: Symbol
      var = freshIn unifVars (typed (Inference Other))
      uvar = UVar var typ
      unifVars' = Set.insert var unifVars
  put st { unifVars = unifVars' }
  initializeUVar uvar
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
