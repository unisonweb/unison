{-# LANGUAGE ConstraintKinds #-}

module Unison.PrettyPrintEnv.MonadPretty where

import Control.Monad.Reader
import qualified Data.Set as Set
import Unison.Prelude (Set)
import Unison.PrettyPrintEnv (PrettyPrint (..))
import qualified Unison.PrettyPrintEnv as PPE
import Unison.Var (Var)

type MonadPretty v m = (Var v, MonadReader (Set v) m, PrettyPrint m)

modifyTypeVars :: MonadPretty v m => (Set v -> Set v) -> m a -> m a
modifyTypeVars = local

-- | Add type variables to the set of variables that need to be avoided
addTypeVars :: MonadPretty v m => [v] -> m a -> m a
addTypeVars = modifyTypeVars . Set.union . Set.fromList

-- | Check if a list of type variables contains any variables that need to be
-- avoided
willCapture :: MonadPretty v m => [v] -> m Bool
willCapture vs = asks (not . Set.null . Set.intersection (Set.fromList vs))

newtype PrettyM v m a = PrettyM {unPrettyM :: ReaderT (Set v) m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader (Set v))

instance PrettyPrint m => PrettyPrint (PrettyM v m) where
  termNames = PrettyM . lift . PPE.termNames
  typeNames = PrettyM . lift . PPE.typeNames

runPretty :: Var v => PrettyM v m a -> m a
runPretty (PrettyM m) = runReaderT m (Set.empty)
