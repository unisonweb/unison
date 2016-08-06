{-# Language OverloadedStrings #-}

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
import qualified Unison.BlockStore.FileBlockStore as FBS
import qualified Unison.NodeContainer as C
import qualified Unison.Remote as R
import qualified Unison.Runtime.Multiplex as Mux

main :: IO ()
main = Mux.uniqueChannel >>= \rand ->
  let
    fileBS = FBS.make' rand h "blockstore"
    h bytes = BA.convert (hash bytes :: Digest Blake2b_512)
    locker _ = pure held
    held = Lock (pure (Just (Lease (pure True) (pure ()))))
    mkNode _ = do -- todo: actually use node params
      publicKey <- Put.runPutS . serialize <$> rand
      pure $ R.Node "localhost" publicKey
    launchNode node = do
      (Just stdin, Just stdout, _, handle) <- P.createProcess_ "node-worker" cmd
      hSetBinaryMode stdin True
      B.hPut stdin . Put.runPutS $ do
        serialize ("ignored-private-key" :: B.ByteString)
        serialize node
        serialize (R.Universe "local-universe")
        serialize B.empty -- no sandbox specification
      hFlush stdin
      let proof = "not-real-delete-proof"
      pure (stdin, stdout, handle, proof)
    cmd = (P.shell "stack exec worker") {
        P.std_out = P.CreatePipe,
        P.std_in = P.CreatePipe,
        P.std_err = P.Inherit }
        -- P.std_err = P.UseHandle stdin }
  in do
    fileBS <- fileBS
    send <- C.make fileBS locker protocol mkNode launchNode
    S.scotty 8081 $ do
      S.middleware logStdoutDev
      S.addroute OPTIONS (S.regex ".*") $ NS.originOptions
      NS.postRoute "/compute/:nodepk" $ do
        nodepk <- S.param "nodepk"
        let node = R.Node "localhost" (Base64.decodeLenient nodepk)
        programtxt <- S.body
        let programstr = Text.unpack (decodeUtf8 (LB.toStrict programtxt))
        let prog = unsafeParseTerm programstr
        let destination = Put.runPutS (serialize node)
        -- todo: run typechecker on prog
        liftIO $ send (Mux.Packet destination $ Put.runPutS (serialize prog))
