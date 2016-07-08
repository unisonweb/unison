{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Unison.NodeProtocol where

import Data.Bytes.Serial (Serial)
import Unison.BlockStore (BlockStore(..), Series(..))
import Unison.Hash (Hash)
import Unison.Hash.Extra ()
import Unison.Remote (Remote)
import Unison.Runtime.Multiplex (EncryptedChannel,Channel,Request)
import qualified Data.ByteString as B
import qualified Unison.Runtime.Multiplex as Mux
import qualified Unison.Remote as Remote

instance Serial Series

data Protocol term signature hash =
  Protocol
    -- | Shut down and destroy this node; requires proof of knowledge of private key
    { _destroyIn :: Channel signature
    -- | Destroy another node
    , _destroyOut :: Channel signature
    -- | Channel used to initiate handshaking to establish an encrypted pipe of `Maybe (Remote term)`
    , _eval :: EncryptedChannel (Remote.Node, Remote.Universe) (Maybe (Remote term))
    -- generalize over Hash
    , _needs :: EncryptedChannel Remote.Node (Maybe ([Hash], EncryptedChannel Remote.Node [(Hash,term)]))
    -- | Various `BlockStore` methods
    , _insert :: Request B.ByteString hash
    , _lookup :: Request hash (Maybe B.ByteString)
    , _declare :: Request Series hash
    , _update :: Request (Series,hash,B.ByteString) (Maybe hash)
    , _append :: Request (Series,hash,B.ByteString) (Maybe hash)
    , _resolve :: Request Series (Maybe hash)
    , _resolves :: Request Series [hash] }

blockStoreProxy :: (Serial hash) => Protocol term signature hash -> Mux.Multiplex (BlockStore hash)
blockStoreProxy p = go <$> Mux.ask
  where
  timeout = 5000000 :: Mux.Microseconds
  go env =
    let
      mt :: (Serial a, Serial b) => Request a b -> a -> IO b
      mt chan a = Mux.run env $ Mux.requestTimed timeout chan a
      insert bytes = mt (_insert p) bytes
      lookup h = mt (_lookup p) h
      declare series = mt (_declare p) series
      update series h bytes = mt (_update p) (series,h,bytes)
      append series h bytes = mt (_append p) (series,h,bytes)
      resolve series = mt (_resolve p) series
      resolves series = mt (_resolves p) series
    in BlockStore insert lookup declare update append resolve resolves
