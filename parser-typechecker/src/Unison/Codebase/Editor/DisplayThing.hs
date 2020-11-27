module Unison.Codebase.Editor.DisplayThing where

import Unison.Reference as Reference

data DisplayThing a = BuiltinThing | MissingThing Reference.Id | RegularThing a
  deriving (Eq, Ord, Show)

toMaybe :: DisplayThing a -> Maybe a
toMaybe = \case
  RegularThing a -> Just a
  _ -> Nothing
