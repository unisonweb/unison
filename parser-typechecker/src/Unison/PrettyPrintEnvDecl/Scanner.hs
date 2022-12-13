{-# LANGUAGE GADTs #-}

module Unison.PrettyPrintEnvDecl.Scanner where

import Control.Applicative
import Control.Applicative.Free.Fast
import Unison.LabeledDependency (LabeledDependency)
import Unison.Prelude
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))

-- | An applicative which allows us to most efficiently construct a PPE,
-- the idea is that computations which require a PPE specify their full set of dependencies,
-- we can statically analyze them using to the applicative, then construct a PPE which
-- contains names for all of those dependencies, and interpret the computation using the
-- resulting PPE.
type PrettyPrintGroup = Ap PrettyPrintRequest

data PrettyPrintRequest a where
  PPEForDeps :: Set LabeledDependency -> PrettyPrintRequest PrettyPrintEnvDecl

ppeForDeps :: Set LabeledDependency -> PrettyPrintGroup PrettyPrintEnvDecl
ppeForDeps = liftAp . PPEForDeps

collectDeps :: PrettyPrintGroup a -> Set LabeledDependency
collectDeps group = getConst $ runAp go group
  where
    go :: forall x. PrettyPrintRequest x -> Const (Set LabeledDependency) x
    go = \case
      PPEForDeps deps -> Const deps

runWithPPE :: PrettyPrintEnvDecl -> PrettyPrintGroup a -> a
runWithPPE ppe group = runIdentity . runAp go $ group
  where
    go :: forall x. PrettyPrintRequest x -> Identity x
    go = \case
      PPEForDeps _ -> Identity ppe
