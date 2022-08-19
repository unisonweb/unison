{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.WatchKind where

import Data.String (IsString)

type WatchKind = String

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
