{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.Hash.Extra where

import Data.Bytes.Serial
import Data.List
import Data.Word (Word8)
import System.Random
import Unison.Hash
import qualified Crypto.Hash as CH
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.Bytes.Put as Put
import qualified Data.Bytes.VarInt as VarInt
import qualified Unison.Hashable as H

instance H.Accumulate Hash where
  accumulate = fromBytes . BA.convert . CH.hashFinalize . foldl' step CH.hashInit where
    step :: CH.Context CH.SHA3_512 -> H.Token Hash -> CH.Context CH.SHA3_512
    step acc (H.Tag b) = CH.hashUpdate acc (B.singleton b)
    step acc (H.Bytes bs) = CH.hashUpdate acc bs
    step acc (H.VarInt i) = CH.hashUpdate acc (Put.runPutS $ serialize (VarInt.VarInt i))
    step acc (H.Text txt) = CH.hashUpdate acc (Put.runPutS $ serialize txt)
    step acc (H.Double d) = CH.hashUpdate acc (Put.runPutS $ serialize d)
    step acc (H.Hashed h) = CH.hashUpdate acc (toBytes h)
  fromBytes = Unison.Hash.fromBytes
  toBytes = Unison.Hash.toBytes

instance Serial Hash where
  serialize h = serialize (toBytes h)
  deserialize = fromBytes <$> deserialize

instance Random Hash where
  -- bounds are ignored
  randomR (lo, hi) gen =
    let rs = iterate (random . snd) (0, gen)
        rPairs = take 64 $ tail rs
        newGen = snd . head $ reverse rPairs
        bstring = B.pack $ map fst rPairs
    in (fromBytes bstring, newGen)
  random = randomR (undefined, undefined)
