{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}

module Unison.Syntax.Var where

import Unison.Syntax.DeBruijn

type Var = DeBruijn

succ :: Var -> Var
succ (DeBruijn i) = DeBruijn (i + 1)

decr :: Var -> Var
decr (DeBruijn i) = DeBruijn (i - 1)

minv :: Var -> Var -> Var
minv (DeBruijn i) (DeBruijn j) = DeBruijn (min i j)

bound1 :: Var
bound1 = DeBruijn 1
