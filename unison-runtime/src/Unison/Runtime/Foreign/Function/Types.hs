{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.Foreign.Function.Types
  ( GForeignFunc (..),
    ffRef,
  )
where

import Unison.Runtime.ANF (Mem (..))
import Unison.Runtime.MCode

-- Foreign functions operating on stacks
data GForeignFunc ref stack where
  FF ::
    -- The foreign function number/ref
    !ref ->
    !(stack 'UN -> stack 'BX -> Args -> IO a) ->
    !(stack 'UN -> stack 'BX -> r -> IO (stack 'UN, stack 'BX)) ->
    !(a -> IO r) ->
    GForeignFunc ref stack

ffRef :: GForeignFunc ref stack -> ref
ffRef (FF ref _ _ _) = ref

instance (Show ref) => Show (GForeignFunc ref s) where
  show ff = "ForeignFunc<" <> show (ffRef ff) <> ">"

instance (Eq ref) => Eq (GForeignFunc ref s) where
  l == r = ffRef l == ffRef r

instance (Ord ref) => Ord (GForeignFunc ref s) where
  compare l r = compare (ffRef l) (ffRef r)
