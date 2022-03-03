module Unison.Codebase.Verbosity where

data Verbosity = Default | Silent deriving (Eq, Show)

isSilent :: Verbosity -> Bool
isSilent v = case v of
  Default -> False
  Silent -> True
