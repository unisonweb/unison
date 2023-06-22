module Unison.Codebase.Verbosity where

data Verbosity = Verbose | Silent deriving (Eq, Show)

isSilent :: Verbosity -> Bool
isSilent v = case v of
  Verbose -> False
  Silent -> True
