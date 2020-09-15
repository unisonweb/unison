module Unison.Codebase.V1.Type.Kind where

data Kind = Star | Arrow Kind Kind deriving (Eq,Ord,Show)