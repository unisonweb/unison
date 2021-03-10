{-# LANGUAGE DeriveGeneric #-}

module Unison.Codebase.Editor.DisplayObject where

import Unison.Prelude
import Unison.ShortHash

data DisplayObject a = BuiltinObject | MissingObject ShortHash | UserObject a
  deriving (Eq, Ord, Show, Functor, Generic)

toMaybe :: DisplayObject a -> Maybe a
toMaybe = \case
  UserObject a -> Just a
  _ -> Nothing

