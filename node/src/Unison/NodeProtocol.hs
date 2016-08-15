{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
{-# Language OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.NodeProtocol where

import Data.ByteString (ByteString)
import Control.Monad
import Data.Bytes.Serial (Serial)
import GHC.Generics
import Unison.BlockStore (BlockStore(..), Series(..))
import Unison.Remote (Remote)
import Unison.Runtime.Multiplex (EncryptedChannel,Channel,Request)
import qualified Data.ByteString as B
import qualified Unison.Remote as Remote
import qualified Unison.Runtime.Multiplex as Mux

instance Serial Series

data Ack = Ack ByteString deriving Generic
instance Serial Ack

destroyedMessage :: ByteString
destroyedMessage = "destroyed"

data Protocol term signature hash thash =
  Protocol
    -- | Shut down and destroy this node; requires proof of knowledge of private key
    { _destroyIn :: Channel signature
    -- | Destroy another node
    , _destroyOut :: Channel signature
    -- | Create a new node (TODO - pass in parameters here)
    , _spawn :: Request B.ByteString Remote.Node
    -- | Channel used to initiate handshaking to establish an encrypted pipe of `Maybe (Remote term)`
    , _eval :: EncryptedChannel (Remote.Node, Remote.Universe)
                                (Remote term, Channel Ack)
                                (Maybe ([thash], Channel (Maybe [(thash,term)])))
    -- | Evaluate an expression, with no handshaking. Used only by container.
    , _localEval :: Channel term
    -- | Channel used for syncing hashes
    , _sync :: EncryptedChannel Remote.Node ([thash], Channel Ack) (Maybe [(thash,term)])
    -- | Various `BlockStore` methods
    , _insert :: Request B.ByteString hash
    , _lookup :: Request hash (Maybe B.ByteString)
    , _declare :: Request Series hash
    , _delete :: Request Series ()
    , _update :: Request (Series,hash,B.ByteString) (Maybe hash)
    , _append :: Request (Series,hash,B.ByteString) (Maybe hash)
    , _resolve :: Request Series (Maybe hash)
    , _resolves :: Request Series [hash] }

blockStoreProxy :: (Serial hash) => Protocol term signature hash thash -> Mux.Multiplex (BlockStore hash)
blockStoreProxy p = go <$> Mux.ask
  where
  timeout = 5000000 :: Mux.Microseconds
  go env =
    let
      mt :: (Serial a, Serial b) => Request a b -> a -> IO b
      mt chan a = Mux.run env . join $ Mux.requestTimed timeout chan a
      insert bytes = mt (_insert p) bytes
      lookup h = mt (_lookup p) h
      declare series = mt (_declare p) series
      delete series = mt (_delete p) series
      update series h bytes = mt (_update p) (series,h,bytes)
      append series h bytes = mt (_append p) (series,h,bytes)
      resolve series = mt (_resolve p) series
      resolves series = mt (_resolves p) series
    in BlockStore insert lookup declare delete update append resolve resolves
