{-# LANGUAGE ConstraintKinds #-}

module Unison.PrettyPrintEnv.MonadPretty where

import Control.Lens (over, set, view, views, _1, _2)
import Control.Monad.Reader (MonadReader, Reader, local, runReader)
import qualified Data.Set as Set
import Unison.Prelude (Set)
import Unison.PrettyPrintEnv (PrettyPrintEnv)
import Unison.Var (Var)

type MonadPretty v m = (Var v, MonadReader (PrettyPrintEnv, Set v) m)

getPPE :: (MonadPretty v m) => m PrettyPrintEnv
getPPE = view _1

-- | Run a computation with a modified PrettyPrintEnv, restoring the original
withPPE :: (MonadPretty v m) => PrettyPrintEnv -> m a -> m a
withPPE p = local (set _1 p)

applyPPE :: (MonadPretty v m) => (PrettyPrintEnv -> a) -> m a
applyPPE = views _1

applyPPE2 :: (MonadPretty v m) => (PrettyPrintEnv -> a -> b) -> a -> m b
applyPPE2 f a = views _1 (`f` a)

applyPPE3 :: (MonadPretty v m) => (PrettyPrintEnv -> a -> b -> c) -> a -> b -> m c
applyPPE3 f a b = views _1 (\ppe -> f ppe a b)

-- | Run a computation with a modified PrettyPrintEnv, restoring the original
modifyPPE :: (MonadPretty v m) => (PrettyPrintEnv -> PrettyPrintEnv) -> m a -> m a
modifyPPE = local . over _1

modifyTypeVars :: (MonadPretty v m) => (Set v -> Set v) -> m a -> m a
modifyTypeVars = local . over _2

-- | Add type variables to the set of variables that need to be avoided
addTypeVars :: (MonadPretty v m) => [v] -> m a -> m a
addTypeVars = modifyTypeVars . Set.union . Set.fromList

-- | Check if a list of type variables contains any variables that need to be
-- avoided
willCapture :: (MonadPretty v m) => [v] -> m Bool
willCapture vs = views _2 (not . Set.null . Set.intersection (Set.fromList vs))

runPretty :: (Var v) => PrettyPrintEnv -> Reader (PrettyPrintEnv, Set v) a -> a
runPretty ppe m = runReader m (ppe, mempty)
