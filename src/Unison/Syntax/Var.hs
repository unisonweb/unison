{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}

module Unison.Syntax.Var where

import Unison.Syntax.Index

type Var = Index

succ :: Var -> Var
succ (I i) = I (i + 1)

decr :: Var -> Var
decr (I i) = I (i - 1)

minv :: Var -> Var -> Var
minv (I i) (I j) = I (min i j)

bound1 :: Var
bound1 = I 1
