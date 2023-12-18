-- | A "watch kind" is the slug that comes before a ">" in a Unison file:
--
-- >
-- > foo> bar = baz
-- >
--
-- In this example, the watch kind is "foo".
module Unison.WatchKind
  ( WatchKind,
    pattern RegularWatch,
    pattern TestWatch,
    watchKindShouldBeStoredInDatabase,
  )
where

import Data.String (IsString)

type WatchKind =
  String

-- | A non-test watch, such as
-- @
-- > 3 + 4
-- @
pattern RegularWatch :: (Eq a, IsString a) => a
pattern RegularWatch = ""

-- | A named test watch, such as
--
-- @
-- test> x = expect (1 == 1)
-- @
--
-- Note: currently test watches don't need to be named by the user, but that "feature" will be removed soon.
pattern TestWatch :: (Eq a, IsString a) => a
pattern TestWatch = "test"

-- Haha terrible name. Regular terms (no ">" in sight) and test watches ("test>") should be stored in the database
-- (the latter, if nameless, get a random name), but other watches (like regular ">" or even weird "oink>") shouldn't.
watchKindShouldBeStoredInDatabase :: Maybe WatchKind -> Bool
watchKindShouldBeStoredInDatabase = \case
  Nothing -> True
  Just TestWatch -> True
  _ -> False
