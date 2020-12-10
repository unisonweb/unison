module Unison.Codebase.Editor.DisplayObject where

import Unison.Reference as Reference

data DisplayObject a = BuiltinObject | MissingObject Reference.Id | UserObject a
  deriving (Eq, Ord, Show, Functor)

toMaybe :: DisplayObject a -> Maybe a
toMaybe = \case
  UserObject a -> Just a
  _ -> Nothing

