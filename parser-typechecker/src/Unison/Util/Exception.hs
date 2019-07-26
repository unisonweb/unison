module Unison.Util.Exception where

import Control.Concurrent.Async (withAsync, waitCatch)
import Control.Exception (SomeException)

-- These are adapted from: https://github.com/snoyberg/classy-prelude/blob/ccd19f2c62882c69d5dcdd3da5c0df1031334c5a/classy-prelude/ClassyPrelude.hs#L320
-- License is MIT: https://github.com/snoyberg/classy-prelude/blob/ccd19f2c62882c69d5dcdd3da5c0df1031334c5a/classy-prelude/LICENSE 

-- Catch all exceptions except asynchronous exceptions.
tryAny :: IO a -> IO (Either SomeException a)
tryAny action = withAsync action waitCatch

-- Catch all exceptions except asynchronous exceptions.
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny action onE = tryAny action >>= either onE return
