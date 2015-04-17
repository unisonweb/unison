{-# LANGUAGE DeriveFunctor #-}

module Unison.Digest
  ( Digest
  , DigestM -- ctor not exported, so easy to change hash fn later
  , Digestable1(..)
  , Hash
  , run
  ) where

import Control.Applicative
import Control.Monad
import Data.Bytes.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Crypto.Hash as H
import qualified Data.Serialize.Put as S

data PairS a = PairS a !(H.Context H.SHA3_512)

type Digest = DigestM ()
newtype DigestM a = DigestM { digest :: H.Context H.SHA3_512 -> PairS a }

type Hash = B.ByteString

class Functor f => Digestable1 f where
  -- | Produce a hash for an `f a`, given a hashing function for `a`.
  -- The first argument, @s@ can be used by the instance to produce
  -- a canonical permutation of any sequence of @a@ values, useful
  -- if the instance contains @a@ values whose order should not affect
  -- hash results. We can think of @s@ as a sort function using some
  -- ordering that the instance doesn't have to be aware of.
  --
  -- More precisely, @s@ will have the property that for any
  -- @xs = [x1, x2, .. xN]@, @s@ will produce the same permutation of
  -- @xs@ for any permutation of @xs@ as input.
  digest1 :: ([a] -> [a]) -> (a -> Hash) -> f a -> Hash

run :: Digest -> B.ByteString
run d = case digest d H.hashInit of
  PairS _ ctx -> H.digestToByteString (H.hashFinalize ctx)

write :: (H.Context H.SHA3_512 -> H.Context H.SHA3_512) -> DigestM ()
write f = DigestM (\ctx -> PairS () (f ctx))

instance MonadPut DigestM where
  putWord8 b = write (\ctx -> H.hashUpdate ctx (B.singleton b))
  putByteString bs = write (\ctx -> H.hashUpdate ctx bs)
  putLazyByteString bs = write (\ctx -> H.hashUpdates ctx (LB.toChunks bs))
  flush = return () -- noop
  -- probably inefficient, but delegate to cereal for these
  putWord16le = putByteString . S.runPut . S.putWord16le
  putWord16be = putByteString . S.runPut . S.putWord16be
  putWord16host = putByteString . S.runPut . S.putWord16host
  putWord32le = putByteString . S.runPut . S.putWord32le
  putWord32be = putByteString . S.runPut . S.putWord32be
  putWord32host = putByteString . S.runPut . S.putWord32host
  putWord64le = putByteString . S.runPut . S.putWord64le
  putWord64be = putByteString . S.runPut . S.putWord64be
  putWord64host = putByteString . S.runPut . S.putWord64host
  putWordhost = putByteString . S.runPut . S.putWordhost

instance Monad DigestM where
  return a = DigestM (\d -> PairS a d)
  DigestM st >>= f = DigestM (\d -> let PairS a d' = st d in d' `seq` digest (f a) d')

instance Applicative DigestM where
  pure = return
  (<*>) = ap

instance Functor DigestM where
  fmap = liftM
