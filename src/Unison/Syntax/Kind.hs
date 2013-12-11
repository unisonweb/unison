module Unison.Syntax.Kind where

data Kind = Star | Constraint | Arrow Kind Kind deriving (Eq,Ord,Read,Show)

