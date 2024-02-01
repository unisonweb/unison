module Unison.Codebase.Editor.Output.PushPull where

data PushPull = Push | Pull deriving (Eq, Ord, Show)

fold :: a -> a -> PushPull -> a
fold push pull p = case p of
  Push -> push
  Pull -> pull
