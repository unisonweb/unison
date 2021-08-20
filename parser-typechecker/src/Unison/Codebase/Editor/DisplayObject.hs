{-# LANGUAGE DeriveGeneric #-}

module Unison.Codebase.Editor.DisplayObject where

import Unison.Prelude
import Unison.ShortHash
import Data.Bifunctor

data DisplayObject b a = BuiltinObject b | MissingObject ShortHash | UserObject a
  deriving (Eq, Ord, Show, Functor, Generic)

instance Bifunctor DisplayObject where
  bimap _ _ (MissingObject sh) = MissingObject sh
  bimap f _ (BuiltinObject b) = BuiltinObject (f b)
  bimap _ f (UserObject a) = UserObject (f a)

toMaybe :: DisplayObject b a -> Maybe a
toMaybe = \case
  UserObject a -> Just a
  _ -> Nothing

