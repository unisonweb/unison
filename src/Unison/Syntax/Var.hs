{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}

module Unison.Syntax.Var where

import Data.Foldable
import Data.Traversable
import Unison.Syntax.DeBruijn

type Var = DeBruijn

succ :: Var -> Var
succ (DeBruijn i) = DeBruijn (i + 1)
succ v = v

decr :: Var -> Var
decr (DeBruijn i) = DeBruijn (i - 1)
decr v = v

minv :: Var -> Var -> Var
minv (DeBruijn i) (DeBruijn j) = DeBruijn (min i j)
minv b@(DeBruijn i) _ = b
minv _ b@(DeBruijn i) = b
minv b _ = b

bound1 :: Var
bound1 = DeBruijn 1
