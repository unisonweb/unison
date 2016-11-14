{-# Language BangPatterns #-}
{-# Language OverloadedStrings #-}
{-# Language PartialTypeSignatures #-}
{-# Language CPP #-}

module Main where

import Control.Exception (handle, throw, SomeException)
import Control.Monad.IO.Class
import Crypto.Hash (hash, Digest, Blake2b_512)
import Data.Bytes.Serial (serialize)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types.Method (StdMethod(OPTIONS))
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.IO (stdout)
import Unison.CodebaseServer as NS
import Unison.Hash (Hash)
import Unison.NodeProtocol.V0 (protocol)
import Unison.Parsers (unsafeParseTerm)
import Unison.Runtime.Lock (Lock(..),Lease(..))
import Web.Scotty as S
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Lazy as LB
import qualified Data.Bytes.Put as Put
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Unison.Builtin as Builtin
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.MemStore as Store
import qualified Unison.Config as Config
import qualified Unison.Cryptography as Cryptography
import qualified Unison.NodeContainer as C
import qualified Unison.NodeProtocol as NP
import qualified Unison.NodeWorker as NW
import qualified Unison.Note as Note
import qualified Unison.Parsers as Parsers
import qualified Unison.Remote as Remote
import qualified Unison.Runtime.ExtraBuiltins as ExtraBuiltins
import qualified Unison.Runtime.Multiplex as Mux
import qualified Unison.Runtime.Remote as Remote
import qualified Unison.SerializationAndHashing as SAH
import qualified Unison.Term as Term
import qualified Unison.Typechecker.Components as Components
import qualified Unison.Util.Logger as L

#ifdef leveldb
import qualified Unison.BlockStore.LevelDbStore as LDBS
#else
import qualified Unison.BlockStore.FileBlockStore as FBS
#endif

main :: IO ()
main = do
  logger <- Config.loggerTo stdout
  rand <- Mux.uniqueChannel
  let h bytes = BA.convert (hash bytes :: Digest Blake2b_512)
#ifdef leveldb
  putStrLn "using leveldb-based block store"
  blockstore <- LDBS.make rand h "blockstore.leveldb"
#else
  putStrLn "using file-based block store"
  blockstore <- FBS.make' rand h "blockstore"
#endif
  let !builtins0 = Builtin.make logger
  let !crypto = Cryptography.noop "todo-real-public-key"
  builtins1 <- ExtraBuiltins.make logger blockstore crypto
  store <- Store.make
  let codebase = Codebase.make SAH.hash store
  Codebase.addBuiltins (builtins0 ++ builtins1) store codebase
  loadDeclarations logger "unison-src/base.u" codebase
  loadDeclarations logger "unison-src/extra.u" codebase
  loadDeclarations logger "unison-src/dindex.u" codebase
  let locker _ = pure held
      held = Lock (pure (Just (Lease (pure True) (pure ()))))
      mkNode _ = do -- todo: actually use node params
        publicKey <- Put.runPutS . serialize <$> rand
        pure $ Remote.Node "localhost" publicKey
      lang :: Remote.Language SAH.TermV Hash
      lang = Remote.Language localDependencies eval Term.app Term.node
                             (Term.builtin "()") Term.channel local unRemote Term.remote
      local l = Term.remote (Remote.Step (Remote.Local l))
      unRemote (Term.Distributed' (Term.Remote r)) = Just r
      unRemote _ = Nothing
      codestore = Remote.makeCodestore blockstore :: Remote.Codestore SAH.TermV Hash
      localDependencies _ = Set.empty -- todo, compute this for real
      -- todo: may want to have this use evaluator + codestore directly
      whnf = Codebase.interpreter (builtins0 ++ builtins1) codebase
      eval t = handle (\e -> L.error logger (show (e :: SomeException)) >> throw e)
               $ Note.run (whnf t)
      -- evaluator = I.eval allprimops
      -- allbuiltins = b0 whnf ++ b1 whnf
      -- allprimops = Map.fromList [ (r, op) | Builtin.Builtin r (Just op) _ _ <- allbuiltins ]
      typecheck e = do
        bindings <- Note.run $ Codebase.allTermsByVarName Term.ref codebase
        L.debug logger $ "known symbols: " ++ show (map fst bindings)
        let e' = Parsers.bindBuiltins bindings [] e
        Note.unnote (Codebase.typeAt codebase e' []) >>= \t -> case t of
          Left note -> pure $ Left (show note)
          Right _ -> pure (Right e')
      launchNode logger node = do
        let u = Remote.Universe "local-universe"
        L.debug logger $ "launching node..."
        (send, recv, isActive) <- NW.make logger protocol crypto lang node u typecheck
        L.debug logger $ "...launched node"
        let proof = "todo: real-delete-proof, based on node private key"
        pure (send, recv, isActive, proof)

  send <- C.make blockstore locker protocol mkNode launchNode
  S.scotty 8081 $ do
    S.middleware logStdoutDev
    S.addroute OPTIONS (S.regex ".*") $ NS.originOptions
    NS.postRoute "/compute/:nodepk" $ do
      nodepk <- S.param "nodepk"
      let node = Remote.Node "localhost" (Put.runPutS . serialize . Base64.decodeLenient $ nodepk)
      programtxt <- S.body
      let programstr = Text.unpack (decodeUtf8 (LB.toStrict programtxt))
      let !prog = unsafeParseTerm programstr :: SAH.TermV
      let !prog' = Components.minimize' prog
      liftIO $ L.info logger "parsed"
      let destination = Put.runPutS (serialize node)
      let pk = Mux.Packet (Mux.channelId $ NP._localEval protocol) (Put.runPutS (serialize prog'))
      liftIO $ send (Mux.Packet destination (Put.runPutS (serialize pk)))

loadDeclarations logger path node = do
  txt <- decodeUtf8 <$> B.readFile path
  let str = Text.unpack txt
  r <- Note.run $ Codebase.declare' Term.ref str node
  L.info logger $ "loaded " ++ path
  L.debug' logger $ do
    ts <- Note.run $ Codebase.allTermsByVarName Term.ref node
    pure $ show ts
  pure r
