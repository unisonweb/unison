{-# LANGUAGE GADTs #-}

module Unison.PrettyPrintEnvDecl.Scanner where

import Control.Applicative
import Control.Applicative.Free.Fast
import Unison.LabeledDependency (LabeledDependency)
import Unison.Name (Name)
import Unison.Prelude hiding (liftM)
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))
import qualified Unison.PrettyPrintEnvDecl as PPED

-- | An applicative which allows us to most efficiently construct a PPE,
-- the idea is that computations which require a PPE specify their full set of dependencies,
-- we can statically analyze them using to the applicative, then construct a PPE which
-- contains names for all of those dependencies, and interpret the computation using the
-- resulting PPE.
--
-- It's okay for computations to use a monad on the inside, but we restrict the PPE
-- dependencies to an applicative so we can do the static analysis.
type PrettyPrintGrouper m = Ap (PrettyPrintRequest m)

data PrettyPrintRequest m a where
  WithPPEForDeps :: Set LabeledDependency -> (PrettyPrintEnvDecl -> m a) -> PrettyPrintRequest m a
  WithBiases :: [Name] -> PrettyPrintGrouper m a -> PrettyPrintRequest m a
  LiftM :: m a -> PrettyPrintRequest m a

collectDeps :: forall m a. PrettyPrintGrouper m a -> Set LabeledDependency
collectDeps group = getConst $ runAp go group
  where
    go :: forall x. PrettyPrintRequest m x -> Const (Set LabeledDependency) x
    go = \case
      WithPPEForDeps deps _action -> Const deps
      LiftM _ -> Const mempty
      WithBiases _names group -> Const $ collectDeps group

runWithPPE :: forall m a. Applicative m => PrettyPrintEnvDecl -> PrettyPrintGrouper m a -> m a
runWithPPE pped group = runAp go $ group
  where
    go :: forall x. PrettyPrintRequest m x -> m x
    go = \case
      WithPPEForDeps _ action -> action pped
      LiftM m -> m
      WithBiases names group -> runWithPPE (PPED.biasTo names pped) group

withBiases :: [Name] -> PrettyPrintGrouper m a -> PrettyPrintGrouper m a
withBiases names action = liftAp $ WithBiases names action

liftM :: Monad m => m a -> PrettyPrintGrouper m a
liftM = liftAp . LiftM

withPPED :: Set LabeledDependency -> (PrettyPrintEnvDecl -> m a) -> PrettyPrintGrouper m a
withPPED deps f = liftAp $ WithPPEForDeps deps f

withUnsuffixifiedPPE :: Set LabeledDependency -> (PrettyPrintEnv -> m a) -> PrettyPrintGrouper m a
withUnsuffixifiedPPE deps f = withPPED deps $ \ppe -> f (PPED.unsuffixifiedPPE ppe)

withSuffixifiedPPE :: Set LabeledDependency -> (PrettyPrintEnv -> m a) -> PrettyPrintGrouper m a
withSuffixifiedPPE deps f = withPPED deps $ \ppe -> f (PPED.suffixifiedPPE ppe)
