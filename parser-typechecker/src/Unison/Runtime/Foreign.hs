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
  , wrapText
  , unwrapText
  ) where

import GHC.Stack (HasCallStack)

import Data.Bifunctor

import Control.Concurrent (ThreadId)
import Data.Text (Text,unpack)
import Network.Socket (Socket)
import System.FilePath (FilePath)
import System.IO (BufferMode(..), SeekMode, Handle, IOMode)
import Unison.Util.Bytes (Bytes)
import Unison.Reference (Reference)
import qualified Unison.Type as Ty

import Unsafe.Coerce

data Foreign where
  Wrap :: Reference -> e -> Foreign

wrapText :: Text -> Foreign
wrapText = Wrap Ty.textRef

unwrapText :: Foreign -> Maybe Text
unwrapText (Wrap r v)
  | r == Ty.textRef = Just $ unsafeCoerce v
  | otherwise = Nothing

instance Eq Foreign where
  Wrap rl t0 == Wrap rr u0
    | rl == Ty.textRef, rr == Ty.textRef = t == u
    where
    t, u :: Text
    t = unsafeCoerce t0
    u = unsafeCoerce u0
  _ == _ = error "Eq Foreign"
instance Ord Foreign where compare __ = error "Ord Foreign"

instance Show Foreign where
  showsPrec p !_ = showParen (p>9) $ showString "Foreign _"

type ForeignArgs = [Foreign]
type ForeignRslt = [Either Int Foreign]

newtype ForeignFunc = FF (ForeignArgs -> IO ForeignRslt)

instance Show ForeignFunc where
  show _ = "ForeignFunc"
instance Eq ForeignFunc where
  _ == _ = error "Eq ForeignFunc"
instance Ord ForeignFunc where
  compare _ _ = error "Ord ForeignFunc"

decodeForeignEnum :: Enum a => [Foreign] -> (a,[Foreign])
decodeForeignEnum = first toEnum . decodeForeign

class ForeignConvention a where
  decodeForeign :: [Foreign] -> (a, [Foreign])
  decodeForeign (f:fs) = (unwrapForeign f, fs)
  decodeForeign _ = foreignCCError

instance ForeignConvention Int
instance ForeignConvention Text
instance ForeignConvention Bytes
instance ForeignConvention Handle
instance ForeignConvention Socket
instance ForeignConvention ThreadId

instance ForeignConvention FilePath where
  decodeForeign = first unpack . decodeForeign
instance ForeignConvention SeekMode where
  decodeForeign = decodeForeignEnum
instance ForeignConvention IOMode where
  decodeForeign = decodeForeignEnum

instance ForeignConvention a => ForeignConvention (Maybe a) where
  decodeForeign (f:fs)
    | 0 <- unwrapForeign f = (Nothing, fs)
    | 1 <- unwrapForeign f
    , (x, fs) <- decodeForeign fs = (Just x, fs)
  decodeForeign _ = foreignCCError

instance (ForeignConvention a, ForeignConvention b)
      => ForeignConvention (a,b)
  where
  decodeForeign fs
    | (x,fs) <- decodeForeign fs
    , (y,fs) <- decodeForeign fs
    = ((x,y), fs)

instance ( ForeignConvention a
         , ForeignConvention b
         , ForeignConvention c
         )
      => ForeignConvention (a,b,c)
  where
  decodeForeign fs
    | (x, fs) <- decodeForeign fs
    , (y, fs) <- decodeForeign fs
    , (z, fs) <- decodeForeign fs
    = ((x,y,z), fs)

instance ForeignConvention BufferMode where
  decodeForeign (f:fs)
    | 0 <- unwrapForeign f = (NoBuffering,fs)
    | 1 <- unwrapForeign f = (LineBuffering,fs)
    | 2 <- unwrapForeign f = (BlockBuffering Nothing, fs)
    | 3 <- unwrapForeign f
    , (n,fs) <- decodeForeign fs
    = (BlockBuffering $ Just n, fs)
  decodeForeign _ = foreignCCError

foreignCCError :: HasCallStack => a
foreignCCError = error "mismatched foreign calling convention"

unwrapForeign :: Foreign -> a
unwrapForeign (Wrap _ e) = unsafeCoerce e

foreign0 :: IO [Either Int Foreign] -> ForeignFunc
foreign0 e = FF $ \[] -> e

foreign1
  :: ForeignConvention a
  => (a -> IO [Either Int Foreign])
  -> ForeignFunc
foreign1 f = FF $ \case
  fs | (x,[]) <- decodeForeign fs
    -> f x
     | otherwise -> foreignCCError

foreign2
  :: ForeignConvention a
  => ForeignConvention b
  => (a -> b -> IO [Either Int Foreign])
  -> ForeignFunc
foreign2 f = FF $ \case
  fs | (x,fs) <- decodeForeign fs
     , (y,[]) <- decodeForeign fs
    -> f x y
     | otherwise -> foreignCCError

foreign3
  :: ForeignConvention a
  => ForeignConvention b
  => ForeignConvention c
  => (a -> b -> c -> IO [Either Int Foreign])
  -> ForeignFunc
foreign3 f = FF $ \case
  fs | (x,fs) <- decodeForeign fs
     , (y,fs) <- decodeForeign fs
     , (z,[]) <- decodeForeign fs
    -> f x y z
     | otherwise -> foreignCCError

