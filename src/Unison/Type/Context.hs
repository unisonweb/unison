{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Unison.Type.Context where

import Unison.Syntax.Type as T
import Unison.Type.Context.Element as E

-- | An ordered algorithmic context
data Context (t :: E.T) sa a v = Context [Element t sa a v]
