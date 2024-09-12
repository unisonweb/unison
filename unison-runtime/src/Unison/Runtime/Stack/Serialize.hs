module Unison.Runtime.Stack.Serialize (putClosure, getClosure) where

import Data.Bytes.Get
import Data.Bytes.Put
import Unison.Runtime.Stack (Closure)

putClosure :: (MonadPut m) => Closure -> m ()
putClosure = error "putClosure not implemented"

getClosure :: (MonadGet m) => m Closure
getClosure = error "getClosure not implemented"
