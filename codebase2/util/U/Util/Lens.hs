{-# LANGUAGE ConstraintKinds #-}

module U.Util.Lens where

import qualified Control.Lens as Lens

type Field1' s a = Lens.Field1 s s a a
type Field2' s a = Lens.Field2 s s a a
