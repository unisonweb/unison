module Unison.Type.Note where

type Note = [String]

note :: String -> Note
note s = [s]

scope :: String -> Either Note a -> Either Note a
scope s (Left stack) = Left (s : stack)
scope _ e = e

