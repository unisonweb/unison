{-# Language BangPatterns #-}
{-# Language OverloadedStrings #-}
{-# Language CPP #-}

module Main where

import Control.Monad.IO.Class
import Crypto.Hash (hash, Digest, Blake2b_512)
import Data.Bytes.Serial (serialize)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types.Method (StdMethod(OPTIONS))
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.IO (hSetBinaryMode, hFlush, stdin)
import System.Process as P
import Unison.NodeProtocol.V0 (protocol)
import Unison.NodeServer as NS
import Unison.Parsers (unsafeParseTerm)
import Unison.Runtime.Lock (Lock(..),Lease(..))
import Web.Scotty as S
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Lazy as LB
import qualified Data.Bytes.Put as Put
import qualified Data.Text as Text
#ifdef leveldb
import qualified Unison.BlockStore.LevelDbStore as LDBS
#else
import qualified Unison.BlockStore.FileBlockStore as FBS
#endif
import qualified Unison.NodeContainer as C
import qualified Unison.NodeProtocol as NP
import qualified Unison.Remote as R
import qualified Unison.Runtime.Multiplex as Mux
import qualified Unison.Typechecker.Components as Components

main :: IO ()
main = Mux.uniqueChannel >>= \rand ->
  let
    h bytes = BA.convert (hash bytes :: Digest Blake2b_512)
#ifdef leveldb
    blockstore = LDBS.make rand h "blockstore.leveldb"
#else
    blockstore = FBS.make' rand h "blockstore"
#endif
    locker _ = pure held
    held = Lock (pure (Just (Lease (pure True) (pure ()))))
    mkNode _ = do -- todo: actually use node params
      publicKey <- Put.runPutS . serialize <$> rand
      pure $ R.Node "localhost" publicKey
    launchNode node = do
      (Just stdin, Just stdout, Just stderr, handle) <- P.createProcess_ "node-worker" cmd
      hSetBinaryMode stdin True
      B.hPut stdin . Put.runPutS $ do
        serialize ("ignored-private-key" :: B.ByteString)
        serialize node
        serialize (R.Universe "local-universe")
        serialize B.empty -- no sandbox specification
      hFlush stdin
      let proof = "not-real-delete-proof"
      pure (stdin, stdout, stderr, handle, proof)
    cmd = (P.shell "stack exec worker") {
        P.std_out = P.CreatePipe,
        P.std_in = P.CreatePipe,
        P.std_err = P.CreatePipe }
  in do
#ifdef leveldb
    putStrLn "using leveldb-based block store"
#else
    putStrLn "using file-based block store"
#endif
    blockstore <- blockstore
    send <- C.make blockstore locker protocol mkNode launchNode
    S.scotty 8081 $ do
      S.middleware logStdoutDev
      S.addroute OPTIONS (S.regex ".*") $ NS.originOptions
      NS.postRoute "/compute/:nodepk" $ do
        nodepk <- S.param "nodepk"
        let node = R.Node "localhost" (Put.runPutS . serialize . Base64.decodeLenient $ nodepk)
        programtxt <- S.body
        let programstr = Text.unpack (decodeUtf8 (LB.toStrict programtxt))
        let !prog = unsafeParseTerm programstr
        let !prog' = Components.minimize' prog
        liftIO . putStrLn $ "parsed " ++ show prog
        liftIO . putStrLn $ "parsed' " ++ show prog'
        let destination = Put.runPutS (serialize node)
        let pk = Mux.Packet (Mux.channelId $ NP._localEval protocol) (Put.runPutS (serialize prog'))
        liftIO $ send (Mux.Packet destination (Put.runPutS (serialize pk)))
