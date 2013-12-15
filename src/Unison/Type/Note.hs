module Unison.Type.Note where

import Data.List

newtype Note = Note [String]

note :: String -> Note
note s = Note [s]

scope :: String -> Either Note a -> Either Note a
scope s (Left (Note stack)) = Left (Note (s : stack))
scope _ e = e

instance Show Note where
  show (Note stack) = intercalate "\n" stack

