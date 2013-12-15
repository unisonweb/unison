module Unison.Syntax.Index where

-- how many scopes out to go to reach the lambda where this is bound
newtype Index = I Int deriving (Eq,Ord,Show,Read)

succ :: Index -> Index
succ (I i) = I (i + 1)

bound1 :: Index
bound1 = I 1

