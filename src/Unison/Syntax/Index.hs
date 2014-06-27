module Unison.Syntax.Index where

newtype Index = I Int deriving (Eq,Ord)

instance Show Index where
  show (I i) | i <= 0    = "t" ++ show (abs i)
  show (I i) | otherwise = "x" ++ show i

succ :: Index -> Index
succ (I i) = I (i + 1)

bound1 :: Index
bound1 = I 1

