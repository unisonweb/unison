module Unison.OrBuiltin
  ( OrBuiltin (..),
    builtin_,
    notBuiltin_,
  )
where

import Control.Lens (Traversal)

data OrBuiltin a b
  = Builtin a
  | NotBuiltin b
  deriving stock (Eq, Ord, Show)

builtin_ :: Traversal (OrBuiltin a b) (OrBuiltin a' b) a a'
builtin_ f = \case
  Builtin x -> Builtin <$> f x
  NotBuiltin x -> pure (NotBuiltin x)

notBuiltin_ :: Traversal (OrBuiltin a b) (OrBuiltin a b') b b'
notBuiltin_ f = \case
  Builtin x -> pure (Builtin x)
  NotBuiltin x -> NotBuiltin <$> f x
