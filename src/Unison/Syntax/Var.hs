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

decr :: Var v -> Var v
decr (Bound (DeBruijn i)) = Bound (DeBruijn (i - 1))
decr v = v

minv :: Var v -> Var v -> Var v
minv (Bound (DeBruijn i)) (Bound (DeBruijn j)) = Bound (DeBruijn (min i j))
minv b@(Bound (DeBruijn i)) _ = b
minv _ b@(Bound (DeBruijn i)) = b
minv b _ = b

bound1 :: Var v
bound1 = Bound (DeBruijn 1)
