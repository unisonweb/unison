{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}

module Unison.Syntax.Var where

import Data.Foldable
import Data.Traversable
import Unison.Syntax.DeBruijn

data Var v = Bound DeBruijn
           | Free v
           deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

succ :: Var v -> Var v
succ (Bound (DeBruijn i)) = Bound (DeBruijn (i + 1))
succ v = v

bound1 :: Var v
bound1 = Bound (DeBruijn 1)

bound0 :: Var v
bound0 = Bound (DeBruijn 0)
