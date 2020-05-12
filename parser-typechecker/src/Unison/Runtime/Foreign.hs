{-# language GADTs #-}
{-# language BangPatterns #-}
{-# language PatternGuards #-}

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
  , foreign1m1
  , foreign1m2
  , foreign1m3
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

foreign2 :: (a -> b -> IO [Either Int Foreign]) -> ForeignFunc
foreign2 f = FF $ \[x,y] -> f (unwrapForeign x) (unwrapForeign y)

foreign3 :: (a -> b -> c -> IO [Either Int Foreign]) -> ForeignFunc
foreign3 f
   = FF $ \[x,y,z]
  -> f (unwrapForeign x) (unwrapForeign y) (unwrapForeign z)

foreign1m1 :: (Maybe a -> IO [Either Int Foreign]) -> ForeignFunc
foreign1m1 f = FF $ \case
  t:xs
    | 0 <- unwrapForeign t
    -> f Nothing
    | 1 <- unwrapForeign t
    , [x] <- xs
    -> f (Just $ unwrapForeign x)
  _ -> error "mismatched foreign calling convention"

foreign1m2 :: (Maybe a -> b -> IO [Either Int Foreign]) -> ForeignFunc
foreign1m2 f = FF $ \case
  t:xs
    | 0 <- unwrapForeign t
    , [y] <- xs
    -> f Nothing (unwrapForeign y)
    | 1 <- unwrapForeign t
    , [x,y] <- xs
    -> f (Just $ unwrapForeign x) (unwrapForeign y)
  _ -> error "mismatched foreign calling convention"

foreign1m3
  :: (Maybe a -> b -> c -> IO [Either Int Foreign]) -> ForeignFunc
foreign1m3 f = FF $ \case
  t:xs
    | 0 <- unwrapForeign t
    , [y,z] <- xs
    -> f Nothing (unwrapForeign y) (unwrapForeign z)
    | 1 <- unwrapForeign t
    , [x,y,z] <- xs
    -> f (Just $ unwrapForeign x) (unwrapForeign y) (unwrapForeign z)
  _ -> error "mismatched foreign calling convention"
