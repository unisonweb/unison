module Unison.Util.Alternative
  ( whenM,
  )
where

import Control.Applicative (Alternative (empty))

whenM :: (Monad m, Alternative m) => m Bool -> a -> m a
whenM m a = do
  b <- m
  if b then pure a else empty
