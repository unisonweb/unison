{-# LANGUAGE CPP #-}
module Main where

import EasyTest
-- import Unison.Test.Util
import qualified Unison.Test.BlockStore.FileBlockStore as FBS
#ifdef leveldb
import qualified Unison.Test.BlockStore.LevelDbStore as LBS
#endif
import qualified Unison.Test.BlockStore.MemBlockStore as MBS
import qualified Unison.Test.Cryptography as Crypto
import qualified Unison.Test.Journal as J
import qualified Unison.Test.Index as Index
import qualified Unison.Test.Html as Html
import qualified Unison.Test.Http as Http
import qualified Unison.Test.ResourcePool as ResourcePool
import qualified Unison.Test.SerializationAndHashing as SAH


test :: Test ()
test = scope "unison-node" . tests $
  [ Index.test
  , MBS.test
  , J.test
  , Crypto.test
  , Html.test
  , Http.test
  , FBS.test
  , ResourcePool.test
#ifdef leveldb
  , LBS.test
#endif
  , SAH.test ]

main :: IO ()
main = run test
