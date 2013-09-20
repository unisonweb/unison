module Unison.Syntax.DeBruijn where

-- how many scopes out to go to reach the lambda where this is bound
data DeBruijn = DeBruijn Int
  deriving (Eq,Ord,Show,Read)

succ :: DeBruijn -> DeBruijn
succ (DeBruijn i) = DeBruijn (i + 1)

