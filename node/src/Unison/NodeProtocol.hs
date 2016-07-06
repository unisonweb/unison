{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.NodeProtocol where

import Control.Monad.Trans.Reader (ask)
import Data.Bytes.Serial (Serial)
import GHC.Generics
import Unison.BlockStore (BlockStore(..), Series(..))
import Unison.Hash.Extra ()
import qualified Data.ByteString as B
import qualified Unison.Remote as Remote
import qualified Unison.Runtime.Multiplex as Mux

instance Serial Series

-- | Channel which is returned by `Done` is assumed to remain subscribed for
-- t seconds of inactivity on the sender side, and t + delta of inactivity on the
-- recipient side. Thus sender can always just send if the channel hasn't
-- expired, assuming clock skew is less than delta.
--
-- The sender keeps an expiring cache of connections returned by handshaking.
data Handshake
  = Done (Mux.Channel (Maybe B.ByteString) ()) -- an encrypted channel
  | More (Mux.Channel B.ByteString Handshake) deriving Generic

instance Serial Handshake

data Protocol term signature hash =
  Protocol
    -- | Shut down and destroy this node; requires proof of knowledge of private key
    { _destroyIn :: Mux.Channel signature ()
    -- | Destroy another node
    , _destroyOut :: Mux.Channel (Remote.Node, signature) ()
    -- | Initiate handshake with another node, eventually terminates in a channel that
    -- may be used to send encrypted messages to that node
    , _handshake :: Mux.Channel (Remote.Node, B.ByteString) Handshake
    -- | Various `BlockStore` methods
    , _insert :: Mux.Channel B.ByteString hash
    , _lookup :: Mux.Channel hash (Maybe B.ByteString)
    , _declare :: Mux.Channel Series hash
    , _update :: Mux.Channel (Series,hash,B.ByteString) (Maybe hash)
    , _append :: Mux.Channel (Series,hash,B.ByteString) (Maybe hash)
    , _resolve :: Mux.Channel Series (Maybe hash)
    , _resolves :: Mux.Channel Series [hash] }

blockStoreProxy :: (Serial hash) => Protocol term signature hash -> Mux.Multiplex (BlockStore hash)
blockStoreProxy p = go <$> Mux.Multiplex ask
  where
  timeout = 5000000 :: Mux.Microseconds
  go env =
    let
      mt :: (Serial a, Serial b) => Mux.Channel a b -> a -> IO b
      mt chan a = Mux.run env $ Mux.requestTimed' timeout chan a
      insert bytes = mt (_insert p) bytes
      lookup h = mt (_lookup p) h
      declare series = mt (_declare p) series
      update series h bytes = mt (_update p) (series,h,bytes)
      append series h bytes = mt (_append p) (series,h,bytes)
      resolve series = mt (_resolve p) series
      resolves series = mt (_resolves p) series
    in BlockStore insert lookup declare update append resolve resolves
