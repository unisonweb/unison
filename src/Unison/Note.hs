module Unison.Note where

import Data.List
import Control.Monad

-- | Hierarchical error message type used throughout Unison
newtype Note = Note [String]

note :: String -> Note
note s = Note [s]

note' :: String -> Maybe a -> Either Note a
note' s Nothing = Left (note s)
note' _ (Just a) = Right a

scope :: String -> Either Note a -> Either Note a
scope s (Left (Note stack)) = Left (Note (s : stack))
scope _ e = e

scopeM :: Monad m => String -> m (Either Note a) -> m (Either Note a)
scopeM s = liftM (scope s)

scopeF :: Functor f => String -> f (Either Note a) -> f (Either Note a)
scopeF s = fmap (scope s)

instance Show Note where
  show (Note stack) = intercalate "\n" stack

