{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.WatchKind where

type WatchKind = String

-- | A non-test watch, such as
-- @
-- > 3 + 4
-- @
pattern RegularWatch = ""

-- | A named test watch, such as
--
-- @
-- test> x = expect (1 == 1)
-- @
--
-- Note: currently test watches don't need to be named by the user, but that "feature" will be removed soon.
pattern TestWatch = "test"
