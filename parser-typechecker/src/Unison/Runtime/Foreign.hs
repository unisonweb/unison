{-# language GADTs #-}
{-# language BangPatterns #-}

module Unison.Runtime.Foreign
  ( Foreign(..)
  , ForeignArgs
  , ForeignRslt
  , ForeignFunc(..)
  , unwrapForeign
  , foreign0
  , foreign1
  , foreign2
  , foreign3
  ) where

import Unsafe.Coerce

data Foreign where
  Wrap :: e -> Foreign

instance Show Foreign where
  showsPrec p !_ = showParen (p>9) $ showString "Foreign _"

type ForeignArgs = [Foreign]
type ForeignRslt = [Either Int Foreign]

newtype ForeignFunc = FF (ForeignArgs -> IO ForeignRslt)

instance Show ForeignFunc where
  show _ = "ForeignFunc"

unwrapForeign :: Foreign -> a
unwrapForeign (Wrap e) = unsafeCoerce e

foreign0 :: IO [Either Int Foreign] -> ForeignFunc
foreign0 e = FF $ \[] -> e

foreign1 :: (a -> IO [Either Int Foreign]) -> ForeignFunc
foreign1 f = FF $ \[x] -> f $ unwrapForeign x

foreign2 :: (a ->b -> IO [Either Int Foreign]) -> ForeignFunc
foreign2 f = FF $ \[x,y] -> f (unwrapForeign x) (unwrapForeign y)

foreign3 :: (a -> b -> c -> IO [Either Int Foreign]) -> ForeignFunc
foreign3 f
   = FF $ \[x,y,z]
  -> f (unwrapForeign x) (unwrapForeign y) (unwrapForeign z)

