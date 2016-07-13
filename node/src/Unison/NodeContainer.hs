module Unison.NodeContainer where

import Unison.BlockStore as BS
import Unison.Cryptography as C
import qualified Unison.Remote as Remote
import qualified Unison.Runtime.Remote as Remote

make :: BS.BlockStore h
     -> C.Cryptography key symmetricKey signKey signature hash Remote.Cleartext
     -> IO ()
make bs crypto = do
  undefined
  -- persistent state: a `Map Node BS.Series`, representing nodes in the
  -- container and the `Series` in the `BlockStore` where the node persistent
  -- state can be found
