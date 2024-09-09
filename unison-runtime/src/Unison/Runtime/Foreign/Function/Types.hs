{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.Foreign.Function.Types
  ( GForeignFunc (..),
  )
where

import Unison.Runtime.ANF (Mem (..), internalBug)
import Unison.Runtime.MCode

-- Foreign functions operating on stacks
data GForeignFunc stack where
  FF ::
    (stack 'UN -> stack 'BX -> Args -> IO a) ->
    (stack 'UN -> stack 'BX -> r -> IO (stack 'UN, stack 'BX)) ->
    (a -> IO r) ->
    GForeignFunc stack

instance Show (GForeignFunc s) where
  show _ = "ForeignFunc"

instance Eq (GForeignFunc s) where
  _ == _ = internalBug "Eq ForeignFunc"

instance Ord (GForeignFunc s) where
  compare _ _ = internalBug "Ord ForeignFunc"
