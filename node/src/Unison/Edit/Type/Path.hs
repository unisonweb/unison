module Unison.Edit.Type.Path where

-- | Represents a path into an ADT. We choose which numbered constructor,
-- and then which argument of that constructor we are addressing, where
-- @0@ denotes the overall constructor itself.
data Path = Path { constructor :: !Int, arg :: !Int }
