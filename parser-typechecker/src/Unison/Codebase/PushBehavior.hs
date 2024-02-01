-- | This module defines the 'PushBehavior' type.
module Unison.Codebase.PushBehavior
  ( PushBehavior (..),
  )
where

-- | How a `push` behaves.
data PushBehavior
  = -- Force-push over what's there.
    ForcePush
  | -- | The namespace being pushed to is required to be empty.
    RequireEmpty
  | -- | The namespace being pushed to is required to be non-empty
    RequireNonEmpty
  deriving stock (Eq, Show)
