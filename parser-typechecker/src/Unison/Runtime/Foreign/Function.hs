{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# language GADTs #-}
{-# language DataKinds #-}
{-# language ViewPatterns #-}
{-# language RecordWildCards #-}
{-# language UndecidableInstances #-}

module Unison.Runtime.Foreign.Function
  ( ForeignFunc(..)
  , ForeignConvention(..)
  , mkForeign
  )
  where

import GHC.IO.Exception (IOException(..), IOErrorType(..))

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (TVar)
import Control.Exception (evaluate)
import qualified Data.Char as Char
import Data.IORef (IORef)
import Data.Foldable (toList)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Sequence as Sq
import Data.Word (Word64)
import Network.Socket (Socket)
import System.IO (BufferMode(..), SeekMode, Handle, IOMode)
import Unison.Util.Bytes (Bytes)
import Unison.Util.Text (Text,unpack,pack)

import Unison.Reference (Reference)
import Unison.Type (mvarRef, tvarRef, typeLinkRef, refRef)
import Unison.Symbol (Symbol)

import Unison.Runtime.ANF (SuperGroup, Mem(..), Value, internalBug)
import Unison.Runtime.MCode
import Unison.Runtime.Exception
import Unison.Runtime.Foreign
import Unison.Runtime.Stack
import qualified Unison.Builtin.Decls as Ty

-- Foreign functions operating on stacks
data ForeignFunc where
  FF :: (Stack 'UN -> Stack 'BX -> Args -> IO a)
     -> (Stack 'UN -> Stack 'BX -> r -> IO (Stack 'UN, Stack 'BX))
     -> (a -> IO r)
     -> ForeignFunc

instance Show ForeignFunc where
  show _ = "ForeignFunc"
instance Eq ForeignFunc where
  _ == _ = internalBug "Eq ForeignFunc"
instance Ord ForeignFunc where
  compare _ _ = internalBug "Ord ForeignFunc"

class ForeignConvention a where
  readForeign
    :: [Int] -> [Int] -> Stack 'UN -> Stack 'BX -> IO ([Int], [Int], a)
  writeForeign
    :: Stack 'UN -> Stack 'BX -> a -> IO (Stack 'UN, Stack 'BX)

mkForeign
  :: (ForeignConvention a, ForeignConvention r)
  => (a -> IO r)
  -> ForeignFunc
mkForeign ev = FF readArgs writeForeign ev
  where
  readArgs ustk bstk (argsToLists -> (us,bs))
    = readForeign us bs ustk bstk >>= \case
        ([], [], a) -> pure a
        _ -> internalBug
               "mkForeign: too many arguments for foreign function"

instance ForeignConvention Int where
  readForeign (i:us) bs ustk _ = (us,bs,) <$> peekOff ustk i
  readForeign [    ] _  _    _ = foreignCCError "Int"
  writeForeign ustk bstk i = do
    ustk <- bump ustk
    (ustk, bstk) <$ poke ustk i

instance ForeignConvention Word64 where
  readForeign (i:us) bs ustk _ = (us,bs,) <$> peekOffN ustk i
  readForeign [] _ _ _ = foreignCCError "Word64"
  writeForeign ustk bstk n = do
    ustk <- bump ustk
    (ustk, bstk) <$ pokeN ustk n

instance ForeignConvention Char where
  readForeign (i:us) bs ustk _ = (us,bs,) . Char.chr <$> peekOff ustk i
  readForeign [] _ _ _ = foreignCCError "Char"
  writeForeign ustk bstk ch = do
    ustk <- bump ustk
    (ustk, bstk) <$ poke ustk (Char.ord ch)

instance ForeignConvention Closure where
  readForeign us (i:bs) _ bstk = (us,bs,) <$> peekOff bstk i
  readForeign _  [    ] _ _    = foreignCCError "Closure"
  writeForeign ustk bstk c = do
    bstk <- bump bstk
    (ustk, bstk) <$ (poke bstk =<< evaluate c)

instance ForeignConvention Text where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

instance ForeignConvention Bytes where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

instance ForeignConvention Socket where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

instance ForeignConvention ThreadId where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

instance ForeignConvention Handle where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

instance ForeignConvention POSIXTime where
  readForeign = readForeignAs (fromIntegral :: Int -> POSIXTime)
  writeForeign = writeForeignAs (round :: POSIXTime -> Int)

instance ForeignConvention a => ForeignConvention (Maybe a) where
  readForeign (i:us) bs ustk bstk
    = peekOff ustk i >>= \case
        0 -> pure (us, bs, Nothing)
        1 -> fmap Just <$> readForeign us bs ustk bstk
        _ -> foreignCCError "Maybe"
  readForeign [] _ _ _ = foreignCCError "Maybe"

  writeForeign ustk bstk Nothing = do
    ustk <- bump ustk
    (ustk,bstk) <$ poke ustk 0
  writeForeign ustk bstk (Just x) = do
    (ustk,bstk) <- writeForeign ustk bstk x
    ustk <- bump ustk
    (ustk,bstk) <$ poke ustk 1

instance (ForeignConvention a, ForeignConvention b)
      => ForeignConvention (Either a b)
  where
  readForeign (i:us) bs ustk bstk
    = peekOff ustk i >>= \case
        0 -> readForeignAs Left us bs ustk bstk
        1 -> readForeignAs Right us bs ustk bstk
        _ -> foreignCCError "Either"
  readForeign _ _ _ _ = foreignCCError "Either"

  writeForeign ustk bstk (Left a) = do
    (ustk,bstk) <- writeForeign ustk bstk a
    ustk <- bump ustk
    (ustk,bstk) <$ poke ustk 0
  writeForeign ustk bstk (Right b) = do
    (ustk,bstk) <- writeForeign ustk bstk b
    ustk <- bump ustk
    (ustk,bstk) <$ poke ustk 1

ioeDecode :: Int -> IOErrorType
ioeDecode 0 = AlreadyExists
ioeDecode 1 = NoSuchThing
ioeDecode 2 = ResourceBusy
ioeDecode 3 = ResourceExhausted
ioeDecode 4 = EOF
ioeDecode 5 = IllegalOperation
ioeDecode 6 = PermissionDenied
ioeDecode 7 = UserError
ioeDecode _ = internalBug "ioeDecode"

ioeEncode :: IOErrorType -> Int
ioeEncode AlreadyExists = 0
ioeEncode NoSuchThing = 1
ioeEncode ResourceBusy = 2
ioeEncode ResourceExhausted = 3
ioeEncode EOF = 4
ioeEncode IllegalOperation = 5
ioeEncode PermissionDenied = 6
ioeEncode UserError = 7
ioeEncode _ = internalBug "ioeDecode"

instance ForeignConvention IOException where
  readForeign = readForeignAs (bld . ioeDecode)
    where
    bld t = IOError Nothing t "" "" Nothing Nothing

  writeForeign = writeForeignAs (ioeEncode . ioe_type)

readForeignAs
  :: ForeignConvention a
  => (a -> b)
  -> [Int] -> [Int]
  -> Stack 'UN -> Stack 'BX
  -> IO ([Int], [Int], b)
readForeignAs f us bs ustk bstk = fmap f <$> readForeign us bs ustk bstk

writeForeignAs
  :: ForeignConvention b
  => (a -> b)
  -> Stack 'UN -> Stack 'BX
  -> a -> IO (Stack 'UN, Stack 'BX)
writeForeignAs f ustk bstk x = writeForeign ustk bstk (f x)

readForeignEnum
  :: Enum a
  => [Int] -> [Int] -> Stack 'UN -> Stack 'BX
  -> IO ([Int], [Int], a)
readForeignEnum = readForeignAs toEnum

writeForeignEnum
  :: Enum a
  => Stack 'UN -> Stack 'BX -> a
  -> IO (Stack 'UN, Stack 'BX)
writeForeignEnum = writeForeignAs fromEnum

readForeignBuiltin
  :: BuiltinForeign b
  => [Int] -> [Int] -> Stack 'UN -> Stack 'BX
  -> IO ([Int], [Int], b)
readForeignBuiltin = readForeignAs (unwrapBuiltin . marshalToForeign)

writeForeignBuiltin
  :: BuiltinForeign b
  => Stack 'UN -> Stack 'BX -> b
  -> IO (Stack 'UN, Stack 'BX)
writeForeignBuiltin = writeForeignAs (Foreign . wrapBuiltin)

writeTypeLink :: Stack 'UN -> Stack 'BX -> Reference
  -> IO (Stack 'UN, Stack 'BX)
writeTypeLink = writeForeignAs (Foreign . Wrap typeLinkRef)

readTypelink :: [Int] -> [Int] -> Stack 'UN -> Stack 'BX
  -> IO ([Int], [Int], Reference)
readTypelink = readForeignAs (unwrapForeign . marshalToForeign)

instance ForeignConvention Double where
  readForeign (i:us) bs ustk _ = (us,bs,) <$> peekOffD ustk i
  readForeign _ _ _ _ = foreignCCError "Double"
  writeForeign ustk bstk d = bump ustk >>= \ustk ->
    (ustk,bstk) <$ pokeD ustk d

instance ForeignConvention Bool where
  readForeign = readForeignEnum
  writeForeign = writeForeignEnum
instance ForeignConvention String where
  readForeign = readForeignAs unpack
  writeForeign = writeForeignAs pack

instance ForeignConvention SeekMode where
  readForeign = readForeignEnum
  writeForeign = writeForeignEnum
instance ForeignConvention IOMode where
  readForeign = readForeignEnum
  writeForeign = writeForeignEnum

instance ForeignConvention () where
  readForeign us bs _ _ = pure (us, bs, ())
  writeForeign ustk bstk _ = pure (ustk, bstk)

instance (ForeignConvention a, ForeignConvention b)
      => ForeignConvention (a,b)
  where
  readForeign us bs ustk bstk = do
    (us,bs,a) <- readForeign us bs ustk bstk
    (us,bs,b) <- readForeign us bs ustk bstk
    pure (us, bs, (a,b))

  writeForeign ustk bstk (x, y) = do
    (ustk, bstk) <- writeForeign ustk bstk y
    writeForeign ustk bstk x

instance ForeignConvention a => ForeignConvention (Failure a) where
  readForeign us bs ustk bstk = do
    (us,bs,typeref) <- readTypelink us bs ustk bstk
    (us,bs,message) <- readForeign us bs ustk bstk
    (us,bs,any) <- readForeign us bs ustk bstk
    pure (us, bs, Failure typeref message any)

  writeForeign ustk bstk (Failure typeref message any) = do
    (ustk, bstk) <- writeForeign ustk bstk any
    (ustk, bstk) <- writeForeign ustk bstk message
    writeTypeLink ustk bstk typeref

instance ( ForeignConvention a
         , ForeignConvention b
         , ForeignConvention c
         )
      => ForeignConvention (a,b,c)
  where
  readForeign us bs ustk bstk = do
    (us,bs,a) <- readForeign us bs ustk bstk
    (us,bs,b) <- readForeign us bs ustk bstk
    (us,bs,c) <- readForeign us bs ustk bstk
    pure (us, bs, (a,b,c))

  writeForeign ustk bstk (a,b,c) = do
    (ustk,bstk) <- writeForeign ustk bstk c
    (ustk,bstk) <- writeForeign ustk bstk b
    writeForeign ustk bstk a

no'buf, line'buf, block'buf, sblock'buf :: Int
no'buf = Ty.bufferModeNoBufferingId
line'buf = Ty.bufferModeLineBufferingId
block'buf = Ty.bufferModeBlockBufferingId
sblock'buf = Ty.bufferModeSizedBlockBufferingId

instance ForeignConvention BufferMode where
  readForeign (i:us) bs ustk bstk
    = peekOff ustk i >>= \case
        t | t == no'buf -> pure (us, bs, NoBuffering)
          | t == line'buf -> pure (us, bs, LineBuffering)
          | t == block'buf -> pure (us, bs, BlockBuffering Nothing)
          | t == sblock'buf
            -> fmap (BlockBuffering . Just)
           <$> readForeign us bs ustk bstk
          | otherwise
            -> foreignCCError
             $ "BufferMode (unknown tag: " <> show t <> ")"
  readForeign _ _ _ _ = foreignCCError $ "BufferMode (empty stack)"

  writeForeign ustk bstk bm = bump ustk >>= \ustk ->
    case bm of
      NoBuffering -> (ustk,bstk) <$ poke ustk no'buf
      LineBuffering -> (ustk,bstk) <$ poke ustk line'buf
      BlockBuffering Nothing -> (ustk,bstk) <$ poke ustk block'buf
      BlockBuffering (Just n) -> do
        poke ustk n
        ustk <- bump ustk
        (ustk,bstk) <$ poke ustk sblock'buf

instance ForeignConvention [Closure] where
  readForeign us (i:bs) _ bstk
    = (us,bs,) . toList <$> peekOffS bstk i
  readForeign _ _ _ _ = foreignCCError "[Closure]"
  writeForeign ustk bstk l = do
    bstk <- bump bstk
    (ustk,bstk) <$ pokeS bstk (Sq.fromList l)

instance ForeignConvention [Foreign] where
  readForeign = readForeignAs (fmap marshalToForeign)
  writeForeign = writeForeignAs (fmap Foreign)

instance ForeignConvention (MVar Closure) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap mvarRef)

instance ForeignConvention (TVar Closure) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap tvarRef)

instance ForeignConvention (IORef Closure) where
  readForeign = readForeignAs (unwrapForeign . marshalToForeign)
  writeForeign = writeForeignAs (Foreign . Wrap refRef)

instance ForeignConvention (SuperGroup Symbol) where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

instance ForeignConvention Value where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

instance ForeignConvention Foreign where
  readForeign = readForeignAs marshalToForeign
  writeForeign = writeForeignAs Foreign

instance {-# overlappable #-} BuiltinForeign b => ForeignConvention b where
  readForeign = readForeignBuiltin
  writeForeign = writeForeignBuiltin

instance {-# overlappable #-} BuiltinForeign b => ForeignConvention [b]
  where
  readForeign us (i:bs) _ bstk
    = (us,bs,) . fmap (unwrapForeign . marshalToForeign)
    . toList <$> peekOffS bstk i
  readForeign _ _ _ _ = foreignCCError "[b]"
  writeForeign ustk bstk l = do
    bstk <- bump bstk
    (ustk,bstk) <$ pokeS bstk (Foreign . wrapBuiltin <$> Sq.fromList l)

foreignCCError :: String -> IO a
foreignCCError nm
  = die $ "mismatched foreign calling convention for `" ++ nm ++ "`"

