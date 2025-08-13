{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Unison.Runtime.Foreign.Function
  ( ForeignConvention (..),
    foreignCall,
    readsAtError,
    foreignConventionError,
    pseudoConstructors,
    functionReplacements,
    functionUnreplacements,
  )
where

import Control.Concurrent (ThreadId)
import Control.Concurrent as SYS
  ( killThread,
    threadDelay,
  )
import Control.Concurrent.MVar as SYS
import Control.Concurrent.STM qualified as STM
import Control.DeepSeq (NFData)
import Control.Exception
import Control.Exception.Safe qualified as Exception
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Primitive qualified as PA
import Crypto.Error (CryptoError (..), CryptoFailable (..))
import Crypto.Hash qualified as Hash
import Crypto.MAC.HMAC qualified as HMAC
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Crypto.PubKey.RSA.PKCS15 qualified as RSA
import Crypto.Random (getRandomBytes)
import Data.Avro qualified as Avro
import Data.Avro.Encoding.FromAvro qualified as FromAvro
import Data.Avro.Schema.ReadSchema qualified as ReadSchema
import Data.Avro.Schema.Schema qualified as AvroSchema
import Data.Binary.Get qualified as Get
import Data.Bitraversable (bimapM)
import Data.Bits (shiftL, (.|.))
import Data.ByteArray qualified as BA
import Data.ByteString (hGet, hGetSome, hPut)
import Data.ByteString.Lazy qualified as L
import Data.Char (chr, digitToInt, isDigit, ord)
import Data.Default (def)
import Data.Digest.Murmur64 (asWord64, hash64)
import Data.HashMap.Strict qualified as HashMap
import Data.IP (IP)
import Data.Map.Strict qualified as Map
import Data.Map.Strict.Internal qualified as Map
import Data.PEM (PEM, pemContent, pemParseLBS)
import Data.Sequence qualified as Sq
import Data.Tagged (Tagged (..))
import Data.Text qualified
import Data.Text.IO qualified as Text.IO
import Data.Text.Lazy qualified as TL
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock.POSIX as SYS
  ( getPOSIXTime,
    posixSecondsToUTCTime,
    utcTimeToPOSIXSeconds,
  )
import Data.Time.LocalTime (TimeZone (..), getTimeZone)
import Data.Vector qualified as Vector
import Data.X509 qualified as X
import Data.X509.CertificateStore qualified as X
import Data.X509.Memory qualified as X
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Conc qualified as STM
import GHC.Exts (Int (..), readWord8ArrayAsWord16#, readWord8ArrayAsWord32#, readWord8ArrayAsWord64#, writeWord8ArrayAsWord16#, writeWord8ArrayAsWord32#, writeWord8ArrayAsWord64#)
import GHC.Float (double2Float, float2Double)
import GHC.IO (IO (IO))
import GHC.Ptr (Ptr (..))
import GHC.Word (Word16 (W16#), Word32 (W32#), Word64 (W64#))
import Network.Simple.TCP as SYS
  ( HostPreference (..),
    bindSock,
    closeSock,
    connectSock,
    listenSock,
    recv,
    send,
  )
import Network.Socket (Socket)
import Network.Socket as SYS
  ( PortNumber,
    Socket,
    accept,
    recvBuf,
    sendBuf,
    socketPort,
  )
import Network.TLS as TLS
import Network.TLS.Extra.Cipher as Cipher
import Network.UDP (UDPSocket)
import Network.UDP as UDP
  ( ClientSockAddr,
    ListenSocket,
    clientSocket,
    close,
    recv,
    recvFrom,
    send,
    sendTo,
    serverSocket,
    stop,
  )
import Numeric (showHex)
import System.Clock (Clock (..), getTime, nsec, sec)
import System.Directory as SYS
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesPathExist,
    getCurrentDirectory,
    getDirectoryContents,
    getFileSize,
    getModificationTime,
    getTemporaryDirectory,
    removeDirectoryRecursive,
    removeFile,
    renameDirectory,
    renameFile,
    setCurrentDirectory,
  )
import System.Environment as SYS
  ( getArgs,
    getEnv,
  )
import System.Exit as SYS (ExitCode (..))
import System.FilePath (isPathSeparator)
import System.IO (BufferMode (..), Handle, IOMode, SeekMode (..))
import System.IO as SYS
  ( IOMode (..),
    hClose,
    hGetBuf,
    hGetBufSome,
    hGetBuffering,
    hGetChar,
    hGetEcho,
    hIsEOF,
    hIsOpen,
    hIsSeekable,
    hPutBuf,
    hReady,
    hSeek,
    hSetBuffering,
    hSetEcho,
    hTell,
    openFile,
    stderr,
    stdin,
    stdout,
  )
import System.IO.Temp (createTempDirectory)
import System.Process as SYS
  ( getProcessExitCode,
    proc,
    runInteractiveProcess,
    terminateProcess,
    waitForProcess,
    withCreateProcess,
  )
import System.X509 qualified as X
import Unison.Builtin.Decls qualified as Ty
import Unison.Prelude hiding (Text, some)
import Unison.Reference
import Unison.Referent (Referent, pattern Ref)
import Unison.Runtime.ANF qualified as ANF
import Unison.Runtime.ANF.Rehash (checkGroupHashes)
import Unison.Runtime.ANF.Serialize qualified as ANF
import Unison.Runtime.Array qualified as PA
import Unison.Runtime.Builtin
import Unison.Runtime.Crypto.Rsa qualified as Rsa
import Unison.Runtime.Exception
import Unison.Runtime.Foreign hiding (Failure)
import Unison.Runtime.Foreign qualified as F
import Unison.Runtime.Foreign.Function.Type
  ( ForeignFunc (..),
    foreignFuncBuiltinName,
  )
import Unison.Runtime.MCode
import Unison.Runtime.Referenced (Referenced, dereference)
import Unison.Runtime.Stack
import Unison.Runtime.TypeTags qualified as TT
import Unison.Symbol
import Unison.Type
  ( anyRef,
    listRef,
    textRef,
    typeLinkRef,
  )
import Unison.Type qualified as Ty
import Unison.Util.Bytes qualified as Bytes
import Unison.Util.RefPromise
  ( Promise,
    newPromise,
    readPromise,
    tryReadPromise,
    writePromise,
  )
import Unison.Util.Text (Text, fromLazyText, pack, toLazyText, unpack)
import Unison.Util.Text qualified as Util.Text
import Unison.Util.Text.Pattern qualified as TPat
import UnliftIO qualified

withMutableByteArrayContents :: (PA.PrimBase m) => PA.MutableByteArray (PA.PrimState m) -> (Ptr Word8 -> m a) -> m a
{-# INLINE withMutableByteArrayContents #-}
withMutableByteArrayContents mba =
  PA.keepAlive (PA.mutableByteArrayContents mba)

-- foreignCall is explicitly NOINLINE'd because it's a _huge_ chunk of code and negatively affects code caching.
-- Because we're not inlining it, we need a wrapper using an explicitly unboxed Stack so we don't block the
-- worker-wrapper optimizations in the main eval loop.
-- It looks dump to accept an unboxed stack and then immediately box it up, but GHC is sufficiently smart to
-- unbox all of 'foreignCallHelper' when we write it this way, but it's way less work to use the regular lifted stack
-- in its implementation.
{-# NOINLINE foreignCall #-}
foreignCall :: ForeignFunc -> Args -> XStack -> IOEXStack
foreignCall !ff !args !xstk =
  estackIOToIOX $ foreignCallHelper ff args (packXStack xstk)

{-# INLINE foreignCallHelper #-}
foreignCallHelper :: ForeignFunc -> Args -> Stack -> IO (Bool, Stack)
foreignCallHelper = \case
  IO_UDP_clientSocket_impl_v1 -> mkForeignIOF $ \(host :: Util.Text.Text, port :: Util.Text.Text) ->
    let hostStr = Util.Text.toString host
        portStr = Util.Text.toString port
     in UDP.clientSocket hostStr portStr True
  IO_UDP_UDPSocket_recv_impl_v1 -> mkForeignIOF $ \(sock :: UDPSocket) -> Bytes.fromArray <$> UDP.recv sock
  IO_UDP_UDPSocket_send_impl_v1 -> mkForeignIOF $
    \(sock :: UDPSocket, bytes :: Bytes.Bytes) ->
      UDP.send sock (Bytes.toArray bytes)
  IO_UDP_UDPSocket_close_impl_v1 -> mkForeignIOF $
    \(sock :: UDPSocket) -> UDP.close sock
  IO_UDP_ListenSocket_close_impl_v1 -> mkForeignIOF $
    \(sock :: ListenSocket) -> UDP.stop sock
  IO_UDP_UDPSocket_toText_impl_v1 -> mkForeign $
    \(sock :: UDPSocket) -> pure $ show sock
  IO_UDP_serverSocket_impl_v1 -> mkForeignIOF $
    \(ip :: Util.Text.Text, port :: Util.Text.Text) ->
      let maybeIp = readMaybe $ Util.Text.toString ip :: Maybe IP
          maybePort = readMaybe $ Util.Text.toString port :: Maybe PortNumber
       in case (maybeIp, maybePort) of
            (Nothing, _) -> fail "Invalid IP Address"
            (_, Nothing) -> fail "Invalid Port Number"
            (Just ip, Just pt) -> UDP.serverSocket (ip, pt)
  IO_UDP_ListenSocket_toText_impl_v1 -> mkForeign $
    \(sock :: ListenSocket) -> pure $ show sock
  IO_UDP_ListenSocket_recvFrom_impl_v1 ->
    mkForeignIOF $
      fmap (first Bytes.fromArray) <$> UDP.recvFrom
  IO_UDP_ClientSockAddr_toText_v1 -> mkForeign $
    \(sock :: ClientSockAddr) -> pure $ show sock
  IO_UDP_ListenSocket_sendTo_impl_v1 -> mkForeignIOF $
    \(socket :: ListenSocket, bytes :: Bytes.Bytes, addr :: ClientSockAddr) ->
      UDP.sendTo socket (Bytes.toArray bytes) addr
  IO_openFile_impl_v3 -> mkForeignIOF $ \(fnameText :: Util.Text.Text, mode :: IOMode) ->
    let fname = Util.Text.toString fnameText
     in openFile fname mode
  IO_closeFile_impl_v3 -> mkForeignIOF hClose
  IO_isFileEOF_impl_v3 -> mkForeignIOF hIsEOF
  IO_isFileOpen_impl_v3 -> mkForeignIOF hIsOpen
  IO_getEcho_impl_v1 -> mkForeignIOF hGetEcho
  IO_ready_impl_v1 -> mkForeignIOF hReady
  IO_getChar_impl_v1 -> mkForeignIOF hGetChar
  IO_isSeekable_impl_v3 -> mkForeignIOF hIsSeekable
  IO_seekHandle_impl_v3 -> mkForeignIOF $
    \(h, sm, n) -> hSeek h sm (fromIntegral (n :: Int))
  IO_handlePosition_impl_v3 ->
    -- TODO: truncating integer
    mkForeignIOF $
      \h -> fromInteger @Word64 <$> hTell h
  IO_getBuffering_impl_v3 -> mkForeignIOF hGetBuffering
  IO_setBuffering_impl_v3 ->
    mkForeignIOF $
      uncurry hSetBuffering
  IO_setEcho_impl_v1 -> mkForeignIOF $ uncurry hSetEcho
  IO_getLine_impl_v1 ->
    mkForeignIOF $
      fmap Util.Text.fromText . Text.IO.hGetLine
  IO_getBytes_impl_v3 -> mkForeignIOF $
    \(h, n) -> Bytes.fromArray <$> hGet h n
  IO_getSomeBytes_impl_v1 -> mkForeignIOF $
    \(h, n) -> Bytes.fromArray <$> hGetSome h n
  IO_putBytes_impl_v3 -> mkForeignIOF $ \(h, bs) -> hPut h (Bytes.toArray bs)
  -- TODO: Use `PA.withMutableByteArrayContents` here once we have Data.Primitive v9.
  IO_fillBuf_impl_v1 -> mkForeignIOF $ \(h, arr) -> let !sz = PA.sizeofMutableByteArray arr in withMutableByteArrayContents arr (\ptr -> hGetBuf h ptr sz)
  IO_putBuf_impl_v1 -> mkForeignIOF $ \(h, arr, n) -> do
    r <- checkBoundsPrim "IO.putBuf.impl.v1" (PA.sizeofMutableByteArray arr) n 0 . pure $ Right ()
    case r of
      Left (F.Failure _ err _) -> ioError (userError (Util.Text.unpack err))
      Right _ -> hPutBuf h (PA.mutableByteArrayContents arr) (fromIntegral n)
  IO_getBufSome_impl_v1 -> mkForeignIOF $ \(h, arr) -> let !sz = PA.sizeofMutableByteArray arr in withMutableByteArrayContents arr (\ptr -> hGetBufSome h ptr sz)
  IO_systemTime_impl_v3 -> mkForeignIOF $
    \() -> getPOSIXTime
  IO_systemTimeMicroseconds_v1 -> mkForeign $
    \() -> fmap (1e6 *) getPOSIXTime
  Clock_internals_monotonic_v1 -> mkForeignIOF $
    \() -> getTime Monotonic
  Clock_internals_realtime_v1 -> mkForeignIOF $
    \() -> getTime Realtime
  Clock_internals_processCPUTime_v1 -> mkForeignIOF $
    \() -> getTime ProcessCPUTime
  Clock_internals_threadCPUTime_v1 -> mkForeignIOF $
    \() -> getTime ThreadCPUTime
  Clock_internals_sec_v1 -> mkForeign (\n -> pure (fromIntegral $ sec n :: Word64))
  Clock_internals_nsec_v1 -> mkForeign (\n -> pure (fromIntegral $ nsec n :: Word64))
  Clock_internals_systemTimeZone_v1 ->
    mkForeign
      ( \secs -> do
          TimeZone offset summer name <- getTimeZone (posixSecondsToUTCTime (fromIntegral (secs :: Int)))
          pure (offset :: Int, summer, name)
      )
  IO_getTempDirectory_impl_v3 ->
    mkForeignIOF $
      \() -> chop <$> getTemporaryDirectory
  IO_createTempDirectory_impl_v3 -> mkForeignIOF $ \prefix -> do
    temp <- getTemporaryDirectory
    chop <$> createTempDirectory temp prefix
  IO_getCurrentDirectory_impl_v3 -> mkForeignIOF $
    \() -> getCurrentDirectory
  IO_setCurrentDirectory_impl_v3 -> mkForeignIOF setCurrentDirectory
  IO_fileExists_impl_v3 -> mkForeignIOF doesPathExist
  IO_getEnv_impl_v1 -> mkForeignIOF getEnv
  IO_getArgs_impl_v1 -> mkForeignIOF $
    \() -> fmap Util.Text.pack <$> SYS.getArgs
  IO_isDirectory_impl_v3 -> mkForeignIOF doesDirectoryExist
  IO_createDirectory_impl_v3 ->
    mkForeignIOF $
      createDirectoryIfMissing True
  IO_removeDirectory_impl_v3 -> mkForeignIOF removeDirectoryRecursive
  IO_renameDirectory_impl_v3 ->
    mkForeignIOF $
      uncurry renameDirectory
  IO_directoryContents_impl_v3 ->
    mkForeignIOF $
      (fmap Util.Text.pack <$>) . getDirectoryContents
  IO_removeFile_impl_v3 -> mkForeignIOF removeFile
  IO_renameFile_impl_v3 ->
    mkForeignIOF $
      uncurry renameFile
  IO_getFileTimestamp_impl_v3 ->
    mkForeignIOF $
      fmap utcTimeToPOSIXSeconds . getModificationTime
  IO_getFileSize_impl_v3 ->
    -- TODO: truncating integer
    mkForeignIOF $
      \fp -> fromInteger @Word64 <$> getFileSize fp
  IO_serverSocket_impl_v3 ->
    mkForeignIOF $
      \( mhst :: Maybe Util.Text.Text,
         port
         ) ->
          fst <$> SYS.bindSock (hostPreference mhst) port
  Socket_toText -> mkForeign $
    \(sock :: Socket) -> pure $ show sock
  Handle_toText -> mkForeign $
    \(hand :: Handle) -> pure $ show hand
  ThreadId_toText -> mkForeign $
    \(threadId :: ThreadId) -> pure $ show threadId
  IO_socketPort_impl_v3 -> mkForeignIOF $
    \(handle :: Socket) -> do
      n <- SYS.socketPort handle
      return (fromIntegral n :: Word64)
  IO_listen_impl_v3 -> mkForeignIOF $
    \sk -> SYS.listenSock sk 2048
  IO_clientSocket_impl_v3 ->
    mkForeignIOF $
      fmap fst . uncurry SYS.connectSock
  IO_closeSocket_impl_v3 -> mkForeignIOF SYS.closeSock
  IO_socketAccept_impl_v3 ->
    mkForeignIOF $
      fmap fst . SYS.accept
  IO_socketSend_impl_v3 -> mkForeignIOF $
    \(sk, bs) -> SYS.send sk (Bytes.toArray bs)
  IO_socketReceive_impl_v3 -> mkForeignIOF $
    \(hs, n) ->
      maybe mempty Bytes.fromArray <$> SYS.recv hs n
  IO_socketSendBuf_impl_v1 -> mkForeignIOF $
    \(sk, buf, n) -> do
      r <- checkBoundsPrim "IO.socketSendBuf.impl.v1" (PA.sizeofMutableByteArray buf) n 0 . pure $ Right ()
      case r of
        Left (F.Failure _ err _) -> ioError (userError (Util.Text.unpack err))
        Right _ -> withMutableByteArrayContents buf (\ptr -> SYS.sendBuf sk ptr (fromIntegral n))
  IO_socketReceiveBuf_impl_v1 -> mkForeignIOF $
    \(sk, buf, n) -> do
      r <- checkBoundsPrim "IO.socketReceiveBuf.impl.v1" (PA.sizeofMutableByteArray buf) n 0 . pure $ Right ()
      case r of
        Left (F.Failure _ err _) -> ioError (userError (Util.Text.unpack err))
        Right _ -> withMutableByteArrayContents buf (\ptr -> SYS.recvBuf sk ptr (fromIntegral n))
  IO_kill_impl_v3 -> mkForeignIOF killThread
  IO_delay_impl_v3 -> mkForeignIOF customDelay
  IO_stdHandle -> mkForeign $
    \case
      StdIn -> pure SYS.stdin
      StdOut -> pure SYS.stdout
      StdErr -> pure SYS.stderr
  IO_process_call -> mkForeign $
    \(exe, map Util.Text.unpack -> args) ->
      withCreateProcess (proc exe args) $ \_ _ _ p ->
        exitDecode <$> waitForProcess p
  IO_process_start -> mkForeign $ \(exe, map Util.Text.unpack -> args) ->
    runInteractiveProcess exe args Nothing Nothing
  IO_process_kill -> mkForeign $ terminateProcess
  IO_process_wait -> mkForeign $
    \ph -> exitDecode <$> waitForProcess ph
  IO_process_exitCode ->
    mkForeign $
      fmap (fmap exitDecode) . getProcessExitCode
  MVar_new -> mkForeign $
    \(c :: Val) -> newMVar c
  MVar_newEmpty_v2 -> mkForeign $
    \() -> newEmptyMVar @Val
  MVar_take_impl_v3 -> mkForeignIOF $
    \(mv :: MVar Val) -> takeMVar mv
  MVar_tryTake -> mkForeign $
    \(mv :: MVar Val) -> tryTakeMVar mv
  MVar_put_impl_v3 -> mkForeignIOF $
    \(mv :: MVar Val, x) -> putMVar mv x
  MVar_tryPut_impl_v3 -> mkForeignIOF $
    \(mv :: MVar Val, x) -> tryPutMVar mv x
  MVar_swap_impl_v3 -> mkForeignIOF $
    \(mv :: MVar Val, x) -> swapMVar mv x
  MVar_isEmpty -> mkForeign $
    \(mv :: MVar Val) -> isEmptyMVar mv
  MVar_read_impl_v3 -> mkForeignIOF $
    \(mv :: MVar Val) -> readMVar mv
  MVar_tryRead_impl_v3 -> mkForeignIOF $
    \(mv :: MVar Val) -> tryReadMVar mv
  Char_toText -> mkForeign $
    \(ch :: Char) -> pure (Util.Text.singleton ch)
  Text_repeat -> mkForeign $
    \(n :: Word64, txt :: Util.Text.Text) -> pure (Util.Text.replicate (fromIntegral n) txt)
  Text_reverse ->
    mkForeign $
      pure . Util.Text.reverse
  Text_toUppercase ->
    mkForeign $
      pure . Util.Text.toUppercase
  Text_toLowercase ->
    mkForeign $
      pure . Util.Text.toLowercase
  Text_toUtf8 ->
    mkForeign $
      pure . Util.Text.toUtf8
  Text_fromUtf8_impl_v3 ->
    mkForeign $
      pure . mapLeft (\t -> F.Failure Ty.ioFailureRef (Util.Text.pack t) unitValue) . Util.Text.fromUtf8
  Tls_ClientConfig_default -> mkForeign $
    \(hostName :: Util.Text.Text, serverId :: Bytes.Bytes) ->
      fmap
        ( \store ->
            (defaultParamsClient (Util.Text.unpack hostName) (Bytes.toArray serverId))
              { TLS.clientSupported = def {TLS.supportedCiphers = Cipher.ciphersuite_strong},
                TLS.clientShared = def {TLS.sharedCAStore = store}
              }
        )
        X.getSystemCertificateStore
  Tls_ServerConfig_default ->
    mkForeign $
      \(certs :: [X.SignedCertificate], key :: X.PrivKey) ->
        pure $
          (def :: TLS.ServerParams)
            { TLS.serverSupported = def {TLS.supportedCiphers = Cipher.ciphersuite_strong},
              TLS.serverShared = def {TLS.sharedCredentials = Credentials [(X.CertificateChain certs, key)]}
            }
  Tls_ClientConfig_certificates_set ->
    let updateClient :: X.CertificateStore -> TLS.ClientParams -> TLS.ClientParams
        updateClient certs client = client {TLS.clientShared = ((clientShared client) {TLS.sharedCAStore = certs})}
     in mkForeign $
          \(certs :: [X.SignedCertificate], params :: ClientParams) -> pure $ updateClient (X.makeCertificateStore certs) params
  Tls_ServerConfig_certificates_set ->
    let updateServer :: X.CertificateStore -> TLS.ServerParams -> TLS.ServerParams
        updateServer certs client = client {TLS.serverShared = ((serverShared client) {TLS.sharedCAStore = certs})}
     in mkForeign $
          \(certs :: [X.SignedCertificate], params :: ServerParams) -> pure $ updateServer (X.makeCertificateStore certs) params
  TVar_new -> mkForeign $
    \(c :: Val) -> unsafeSTMToIO $ STM.newTVar c
  TVar_read -> mkForeign $
    \(v :: STM.TVar Val) -> unsafeSTMToIO $ STM.readTVar v
  TVar_write -> mkForeign $
    \(v :: STM.TVar Val, c :: Val) ->
      unsafeSTMToIO $ STM.writeTVar v c
  TVar_newIO -> mkForeign $
    \(c :: Val) -> STM.newTVarIO c
  TVar_readIO -> mkForeign $
    \(v :: STM.TVar Val) -> STM.readTVarIO v
  TVar_swap -> mkForeign $
    \(v, c :: Val) -> unsafeSTMToIO $ STM.swapTVar v c
  STM_retry -> mkForeign $
    \() -> unsafeSTMToIO STM.retry :: IO Val
  Promise_new -> mkForeign $
    \() -> newPromise @Val
  Promise_read -> mkForeign $
    \(p :: Promise Val) -> readPromise p
  Promise_tryRead -> mkForeign $
    \(p :: Promise Val) -> tryReadPromise p
  Promise_write -> mkForeign $
    \(p :: Promise Val, a :: Val) -> writePromise p a
  Tls_newClient_impl_v3 ->
    mkForeignTls $
      \( config :: TLS.ClientParams,
         socket :: SYS.Socket
         ) -> Tls socket <$> TLS.contextNew socket config
  Tls_newServer_impl_v3 ->
    mkForeignTls $
      \( config :: TLS.ServerParams,
         socket :: SYS.Socket
         ) -> Tls socket <$> TLS.contextNew socket config
  Tls_handshake_impl_v3 -> mkForeignTls $
    \(tls :: Tls) -> TLS.handshake tls.context
  Tls_send_impl_v3 ->
    mkForeignTls $
      \( tls :: Tls,
         bytes :: Bytes.Bytes
         ) -> TLS.sendData tls.context (Bytes.toLazyByteString bytes)
  Tls_decodeCert_impl_v3 ->
    let wrapFailure t = F.Failure Ty.tlsFailureRef (Util.Text.pack t) unitValue
        decoded :: Bytes.Bytes -> Either String PEM
        decoded bytes = case pemParseLBS $ Bytes.toLazyByteString bytes of
          Right (pem : _) -> Right pem
          Right [] -> Left "no PEM found"
          Left l -> Left l
        asCert :: PEM -> Either String X.SignedCertificate
        asCert pem = X.decodeSignedCertificate $ pemContent pem
     in mkForeignTlsE $
          \(bytes :: Bytes.Bytes) -> pure $ mapLeft wrapFailure $ (decoded >=> asCert) bytes
  Tls_encodeCert -> mkForeign $
    \(cert :: X.SignedCertificate) -> pure $ Bytes.fromArray $ X.encodeSignedObject cert
  Tls_decodePrivateKey -> mkForeign $
    \(bytes :: Bytes.Bytes) -> pure $ X.readKeyFileFromMemory $ L.toStrict $ Bytes.toLazyByteString bytes
  Tls_encodePrivateKey -> mkForeign $
    \(privateKey :: X.PrivKey) -> pure $ Util.Text.toUtf8 $ Util.Text.pack $ show privateKey
  Tls_receive_impl_v3 -> mkForeignTls $
    \(tls :: Tls) -> do
      bs <- TLS.recvData tls.context
      pure $ Bytes.fromArray bs
  Tls_terminate_impl_v3 -> mkForeignTls $
    \(tls :: Tls) -> TLS.bye tls.context
  Code_validateLinks -> mkForeignExn $
    \(lsgs0 :: [(Referent, Referenced ANF.Code)]) -> do
      let f (msg, rs) =
            F.Failure Ty.miscFailureRef (Util.Text.fromText msg) rs
      pure . first f . checkGroupHashes $ second dereference <$> lsgs0
  Code_dependencies -> mkForeign $
    \(dereference -> ANF.CodeRep sg _) ->
      -- note: it's not correct to use the stored references of a
      -- `Referenced Code` because they may over-estimate the actual
      -- occurrences.
      pure $ Ref <$> ANF.groupTermLinks sg
  Code_serialize -> mkForeign $
    \(co :: Referenced ANF.Code) ->
      pure . Bytes.fromArray $ ANF.serializeCode False co
  Code_serialize_versioned -> mkForeign $
    \(ver :: Word64, co :: Referenced ANF.Code) ->
      ANF.serializeCodeWithVersion ver False co >>= \case
        Left err -> die err
        Right bs -> pure $ Bytes.fromLazyByteString bs
  Code_deserialize ->
    mkForeign $
      pure . ANF.deserializeCode . Bytes.toArray
  Code_display -> mkForeign $
    \(nm, (dereference -> ANF.CodeRep sg _)) ->
      pure $ ANF.prettyGroup @Symbol (Util.Text.unpack nm) sg ""
  Value_dependencies ->
    mkForeign $
      pure . fmap (Wrap Ty.termLinkRef . Ref) . ANF.valueTermLinks . dereference
  Value_serialize ->
    mkForeign $
      pure . Bytes.fromArray . ANF.serializeValue
  Value_serialize_versioned ->
    mkForeign $
      fmap Bytes.fromLazyByteString . uncurry ANF.serializeValueWithVersion
  Value_deserialize ->
    mkForeign $
      pure . ANF.deserializeValue . Bytes.toLazyByteString
  Crypto_HashAlgorithm_Sha3_512 -> mkHashAlgorithm "Sha3_512" Hash.SHA3_512
  Crypto_HashAlgorithm_Sha3_256 -> mkHashAlgorithm "Sha3_256" Hash.SHA3_256
  Crypto_HashAlgorithm_Sha2_512 -> mkHashAlgorithm "Sha2_512" Hash.SHA512
  Crypto_HashAlgorithm_Sha2_256 -> mkHashAlgorithm "Sha2_256" Hash.SHA256
  Crypto_HashAlgorithm_Sha1 -> mkHashAlgorithm "Sha1" Hash.SHA1
  Crypto_HashAlgorithm_Blake2b_512 -> mkHashAlgorithm "Blake2b_512" Hash.Blake2b_512
  Crypto_HashAlgorithm_Blake2b_256 -> mkHashAlgorithm "Blake2b_256" Hash.Blake2b_256
  Crypto_HashAlgorithm_Blake2s_256 -> mkHashAlgorithm "Blake2s_256" Hash.Blake2s_256
  Crypto_HashAlgorithm_Md5 -> mkHashAlgorithm "Md5" Hash.MD5
  Crypto_hashBytes -> mkForeign $
    \(HashAlgorithm _ alg, b :: Bytes.Bytes) ->
      let ctx = Hash.hashInitWith alg
       in pure . Bytes.fromArray . Hash.hashFinalize $ Hash.hashUpdates ctx (Bytes.byteStringChunks b)
  Crypto_hmacBytes -> mkForeign $
    \(HashAlgorithm _ alg, key :: Bytes.Bytes, msg :: Bytes.Bytes) ->
      let out = u alg $ HMAC.hmac (Bytes.toArray @BA.Bytes key) (Bytes.toArray @BA.Bytes msg)
          u :: a -> HMAC.HMAC a -> HMAC.HMAC a
          u _ h = h -- to help typechecker along
       in pure $ Bytes.fromArray out
  Crypto_hash -> mkForeign $
    \(HashAlgorithm _ alg, x) ->
      let hashlazy ::
            (Hash.HashAlgorithm a) =>
            a ->
            L.ByteString ->
            Hash.Digest a
          hashlazy _ l = Hash.hashlazy l
       in pure . Bytes.fromArray . hashlazy alg . ANF.serializeValueForHash $ dereference x
  Crypto_hmac -> mkForeign $
    \(HashAlgorithm _ alg, key, x) ->
      let hmac ::
            (Hash.HashAlgorithm a) => a -> L.ByteString -> HMAC.HMAC a
          hmac _ s =
            HMAC.finalize
              . HMAC.updates
                (HMAC.initialize $ Bytes.toArray @BA.Bytes key)
              $ L.toChunks s
       in pure . Bytes.fromArray . hmac alg . ANF.serializeValueForHash $ dereference x
  Crypto_Ed25519_sign_impl ->
    mkForeign $
      pure . signEd25519Wrapper
  Crypto_Ed25519_verify_impl ->
    mkForeign $
      pure . verifyEd25519Wrapper
  Crypto_Rsa_sign_impl ->
    mkForeign $
      pure . signRsaWrapper
  Crypto_Rsa_verify_impl ->
    mkForeign $
      pure . verifyRsaWrapper
  Universal_murmurHash ->
    mkForeign $
      pure . asWord64 . hash64 . ANF.serializeValueForHash . dereference
  IO_randomBytes -> mkForeign $
    \n -> Bytes.fromArray <$> getRandomBytes @IO @ByteString n
  Bytes_zlib_compress -> mkForeign $ pure . Bytes.zlibCompress
  Bytes_gzip_compress -> mkForeign $ pure . Bytes.gzipCompress
  Bytes_zlib_decompress -> mkForeign $ \bs ->
    catchAll (pure (Bytes.zlibDecompress bs))
  Bytes_gzip_decompress -> mkForeign $ \bs ->
    catchAll (pure (Bytes.gzipDecompress bs))
  Bytes_toBase16 -> mkForeign $ pure . Bytes.toBase16
  Bytes_toBase32 -> mkForeign $ pure . Bytes.toBase32
  Bytes_toBase64 -> mkForeign $ pure . Bytes.toBase64
  Bytes_toBase64UrlUnpadded -> mkForeign $ pure . Bytes.toBase64UrlUnpadded
  Bytes_fromBase16 ->
    mkForeign $
      pure . mapLeft Util.Text.fromText . Bytes.fromBase16
  Bytes_fromBase32 ->
    mkForeign $
      pure . mapLeft Util.Text.fromText . Bytes.fromBase32
  Bytes_fromBase64 ->
    mkForeign $
      pure . mapLeft Util.Text.fromText . Bytes.fromBase64
  Bytes_fromBase64UrlUnpadded ->
    mkForeign $
      pure . mapLeft Util.Text.fromText . Bytes.fromBase64UrlUnpadded
  Bytes_decodeNat64be -> mkForeign $ pure . Bytes.decodeNat64be
  Bytes_decodeNat64le -> mkForeign $ pure . Bytes.decodeNat64le
  Bytes_decodeNat32be -> mkForeign $ pure . Bytes.decodeNat32be
  Bytes_decodeNat32le -> mkForeign $ pure . Bytes.decodeNat32le
  Bytes_decodeNat16be -> mkForeign $ pure . Bytes.decodeNat16be
  Bytes_decodeNat16le -> mkForeign $ pure . Bytes.decodeNat16le
  Bytes_encodeNat64be -> mkForeign $ pure . Bytes.encodeNat64be
  Bytes_encodeNat64le -> mkForeign $ pure . Bytes.encodeNat64le
  Bytes_encodeNat32be -> mkForeign $ pure . Bytes.encodeNat32be
  Bytes_encodeNat32le -> mkForeign $ pure . Bytes.encodeNat32le
  Bytes_encodeNat16be -> mkForeign $ pure . Bytes.encodeNat16be
  Bytes_encodeNat16le -> mkForeign $ pure . Bytes.encodeNat16le
  MutableArray_copyTo_force -> mkForeignExn $
    \(dst, doff, src, soff, l) ->
      let name = "MutableArray.copyTo!"
       in if l == 0
            then pure (Right ())
            else
              checkBounds name (PA.sizeofMutableArray dst) (doff + l - 1) $
                checkBounds name (PA.sizeofMutableArray src) (soff + l - 1) $
                  Right
                    <$> PA.copyMutableArray @IO @Val
                      dst
                      (fromIntegral doff)
                      src
                      (fromIntegral soff)
                      (fromIntegral l)
  MutableByteArray_copyTo_force -> mkForeignExn $
    \(dst, doff, src, soff, l) ->
      let name = "MutableByteArray.copyTo!"
       in if l == 0
            then pure (Right ())
            else
              checkBoundsPrim name (PA.sizeofMutableByteArray dst) (doff + l) 0 $
                checkBoundsPrim name (PA.sizeofMutableByteArray src) (soff + l) 0 $
                  Right
                    <$> PA.copyMutableByteArray @IO
                      dst
                      (fromIntegral doff)
                      src
                      (fromIntegral soff)
                      (fromIntegral l)
  ImmutableArray_copyTo_force -> mkForeignExn $
    \(dst, doff, src, soff, l) ->
      let name = "ImmutableArray.copyTo!"
       in if l == 0
            then pure (Right ())
            else
              checkBounds name (PA.sizeofMutableArray dst) (doff + l - 1) $
                checkBounds name (PA.sizeofArray src) (soff + l - 1) $
                  Right
                    <$> PA.copyArray @IO @Val
                      dst
                      (fromIntegral doff)
                      src
                      (fromIntegral soff)
                      (fromIntegral l)
  ImmutableArray_size ->
    mkForeign $
      pure . fromIntegral @Int @Word64 . PA.sizeofArray @Val
  MutableArray_size ->
    mkForeign $
      pure . fromIntegral @Int @Word64 . PA.sizeofMutableArray @PA.RealWorld @Val
  ImmutableByteArray_size ->
    mkForeign $
      pure . fromIntegral @Int @Word64 . PA.sizeofByteArray
  MutableByteArray_size ->
    mkForeign $
      pure . fromIntegral @Int @Word64 . PA.sizeofMutableByteArray @PA.RealWorld
  ImmutableByteArray_copyTo_force -> mkForeignExn $
    \(dst, doff, src, soff, l) ->
      let name = "ImmutableByteArray.copyTo!"
       in if l == 0
            then pure (Right ())
            else
              checkBoundsPrim name (PA.sizeofMutableByteArray dst) (doff + l) 0 $
                checkBoundsPrim name (PA.sizeofByteArray src) (soff + l) 0 $
                  Right
                    <$> PA.copyByteArray @IO
                      dst
                      (fromIntegral doff)
                      src
                      (fromIntegral soff)
                      (fromIntegral l)
  MutableArray_read ->
    mkForeignExn $
      checkedRead "MutableArray.read"
  MutableByteArray_read8 ->
    mkForeignExn $
      checkedRead8 "MutableByteArray.read8"
  MutableByteArray_read16be ->
    mkForeignExn $
      checkedRead16 BigEndian "MutableByteArray.read16be"
  MutableByteArray_read24be ->
    mkForeignExn $
      checkedRead24 BigEndian "MutableByteArray.read24be"
  MutableByteArray_read32be ->
    mkForeignExn $
      checkedRead32 BigEndian "MutableByteArray.read32be"
  MutableByteArray_read40be ->
    mkForeignExn $
      checkedRead40 BigEndian "MutableByteArray.read40be"
  MutableByteArray_read64be ->
    mkForeignExn $
      checkedRead64 BigEndian "MutableByteArray.read64be"
  MutableByteArray_read16le ->
    mkForeignExn $
      checkedRead16 LittleEndian "MutableByteArray.read16le"
  MutableByteArray_read24le ->
    mkForeignExn $
      checkedRead24 LittleEndian "MutableByteArray.read24le"
  MutableByteArray_read32le ->
    mkForeignExn $
      checkedRead32 LittleEndian "MutableByteArray.read32le"
  MutableByteArray_read40le ->
    mkForeignExn $
      checkedRead40 LittleEndian "MutableByteArray.read40le"
  MutableByteArray_read64le ->
    mkForeignExn $
      checkedRead64 LittleEndian "MutableByteArray.read64le"
  MutableArray_write ->
    mkForeignExn $
      checkedWrite "MutableArray.write"
  MutableByteArray_write8 ->
    mkForeignExn $
      checkedWrite8 "MutableByteArray.write8"
  MutableByteArray_write16be ->
    mkForeignExn $
      checkedWrite16 BigEndian "MutableByteArray.write16be"
  MutableByteArray_write32be ->
    mkForeignExn $
      checkedWrite32 BigEndian "MutableByteArray.write32be"
  MutableByteArray_write64be ->
    mkForeignExn $
      checkedWrite64 BigEndian "MutableByteArray.write64be"
  MutableByteArray_write16le ->
    mkForeignExn $
      checkedWrite16 LittleEndian "MutableByteArray.write16le"
  MutableByteArray_write32le ->
    mkForeignExn $
      checkedWrite32 LittleEndian "MutableByteArray.write32le"
  MutableByteArray_write64le ->
    mkForeignExn $
      checkedWrite64 LittleEndian "MutableByteArray.write64le"
  ImmutableArray_read ->
    mkForeignExn $
      checkedIndex "ImmutableArray.read"
  ImmutableByteArray_read8 ->
    mkForeignExn $
      checkedIndex8 "ImmutableByteArray.read8"
  ImmutableByteArray_read16be ->
    mkForeignExn $
      checkedIndex16 "ImmutableByteArray.read16be"
  ImmutableByteArray_read24be ->
    mkForeignExn $
      checkedIndex24 "ImmutableByteArray.read24be"
  ImmutableByteArray_read32be ->
    mkForeignExn $
      checkedIndex32 "ImmutableByteArray.read32be"
  ImmutableByteArray_read40be ->
    mkForeignExn $
      checkedIndex40 "ImmutableByteArray.read40be"
  ImmutableByteArray_read64be ->
    mkForeignExn $
      checkedIndex64 "ImmutableByteArray.read64be"
  MutableByteArray_freeze_force ->
    mkForeign PA.unsafeFreezeByteArray
  MutableArray_freeze_force ->
    mkForeign $
      PA.unsafeFreezeArray @IO @Val
  MutableByteArray_freeze -> mkForeignExn $
    \(src, off, len) ->
      if len == 0
        then fmap Right . PA.unsafeFreezeByteArray =<< PA.newByteArray 0
        else
          checkBoundsPrim
            "MutableByteArray.freeze"
            (PA.sizeofMutableByteArray src)
            (off + len)
            0
            $ Right <$> PA.freezeByteArray src (fromIntegral off) (fromIntegral len)
  MutableArray_freeze -> mkForeignExn $
    \(src :: PA.MutableArray PA.RealWorld Val, off, len) ->
      if len == 0
        then fmap Right . PA.unsafeFreezeArray =<< PA.newArray 0 emptyVal
        else
          checkBounds
            "MutableArray.freeze"
            (PA.sizeofMutableArray src)
            (off + len - 1)
            $ Right <$> PA.freezeArray src (fromIntegral off) (fromIntegral len)
  MutableByteArray_length ->
    mkForeign $
      pure . PA.sizeofMutableByteArray @PA.RealWorld
  ImmutableByteArray_length ->
    mkForeign $
      pure . PA.sizeofByteArray
  ImmutableByteArray_toBytes -> mkForeignExn $ \(ba :: PA.ByteArray, off, len) ->
    if len == 0
      then pure (Right Bytes.empty)
      else
        checkBoundsPrim
          "ImmutableByteArray_toBytes"
          (PA.sizeofByteArray ba)
          (off + len)
          0
          $ pure
          $ Right
          $ Bytes.fromByteArray (fromIntegral off) (fromIntegral len) ba
  ImmutableByteArray_fromBytes -> mkForeign $ \(ba :: Bytes.Bytes) -> Bytes.toByteArray ba
  PinnedByteArray_cast -> mkForeign $ \(ba :: PA.MutableByteArray PA.RealWorld) -> pure ba
  IO_array -> mkForeign $
    \n -> PA.newArray n emptyVal
  IO_arrayOf -> mkForeign $
    \(v :: Val, n) -> PA.newArray n v
  IO_bytearray -> mkForeign $ PA.newByteArray
  IO_bytearrayOf -> mkForeign $
    \(init, sz) -> do
      arr <- PA.newByteArray sz
      PA.fillByteArray arr 0 sz init
      pure arr
  IO_pinnedByteArray -> mkForeign $ PA.newPinnedByteArray
  IO_pinnedByteArrayOf -> mkForeign $
    \(init, sz) -> do
      arr <- PA.newPinnedByteArray sz
      PA.fillByteArray arr 0 sz init
      pure arr
  Scope_array -> mkForeign $
    \n -> PA.newArray n emptyVal
  Scope_arrayOf -> mkForeign $
    \(v :: Val, n) -> PA.newArray n v
  Scope_bytearray -> mkForeign $ PA.newByteArray
  Scope_bytearrayOf -> mkForeign $
    \(init, sz) -> do
      arr <- PA.newByteArray sz
      PA.fillByteArray arr 0 sz init
      pure arr
  Scope_pinnedByteArray -> mkForeign $ PA.newPinnedByteArray
  Scope_pinnedByteArrayOf -> mkForeign $
    \(init, sz) -> do
      arr <- PA.newPinnedByteArray sz
      PA.fillByteArray arr 0 sz init
      pure arr
  Text_patterns_literal -> mkForeign $
    \txt -> evaluate . TPat.cpattern $ TPat.Literal txt
  Text_patterns_digit ->
    mkForeign $
      let v = TPat.cpattern (TPat.Char (TPat.CharRange '0' '9')) in \() -> pure v
  Text_patterns_letter ->
    mkForeign $
      let v = TPat.cpattern (TPat.Char (TPat.CharClass TPat.Letter)) in \() -> pure v
  Text_patterns_space ->
    mkForeign $
      let v = TPat.cpattern (TPat.Char (TPat.CharClass TPat.Whitespace)) in \() -> pure v
  Text_patterns_punctuation ->
    mkForeign $
      let v = TPat.cpattern (TPat.Char (TPat.CharClass TPat.Punctuation)) in \() -> pure v
  Text_patterns_anyChar ->
    mkForeign $
      let v = TPat.cpattern (TPat.Char TPat.Any) in \() -> pure v
  Text_patterns_eof ->
    mkForeign $
      let v = TPat.cpattern TPat.Eof in \() -> pure v
  Text_patterns_charRange -> mkForeign $
    \(beg, end) -> evaluate . TPat.cpattern . TPat.Char $ TPat.CharRange beg end
  Text_patterns_notCharRange -> mkForeign $
    \(beg, end) -> evaluate . TPat.cpattern . TPat.Char . TPat.Not $ TPat.CharRange beg end
  Text_patterns_charIn -> mkForeign $ \ccs -> do
    cs <- for ccs $ \case
      CharVal c -> pure c
      _ -> die "Text.patterns.charIn: non-character closure"
    evaluate . TPat.cpattern . TPat.Char $ TPat.CharSet cs
  Text_patterns_notCharIn -> mkForeign $ \ccs -> do
    cs <- for ccs $ \case
      CharVal c -> pure c
      _ -> die "Text.patterns.notCharIn: non-character closure"
    evaluate . TPat.cpattern . TPat.Char . TPat.Not $ TPat.CharSet cs
  Pattern_many -> mkForeign $
    \(TPat.CP p _) -> evaluate . TPat.cpattern $ TPat.Many False p
  Pattern_many_corrected -> mkForeign $
    \(TPat.CP p _) -> evaluate . TPat.cpattern $ TPat.Many True p
  Pattern_capture -> mkForeign $
    \(TPat.CP p _) -> evaluate . TPat.cpattern $ TPat.Capture p
  Pattern_captureAs -> mkForeign $
    \(t, (TPat.CP p _)) -> evaluate . TPat.cpattern $ TPat.CaptureAs t p
  Pattern_join -> mkForeign $ \ps ->
    evaluate . TPat.cpattern . TPat.Join $ map (\(TPat.CP p _) -> p) ps
  Pattern_or -> mkForeign $
    \(TPat.CP l _, TPat.CP r _) -> evaluate . TPat.cpattern $ TPat.Or l r
  Pattern_lookahead -> mkForeign $
    \(TPat.CP p _) -> evaluate . TPat.cpattern $ TPat.Lookahead p
  Pattern_negativeLookahead -> mkForeign $
    \(TPat.CP p _) -> evaluate . TPat.cpattern $ TPat.NegativeLookahead p
  Pattern_replicate -> mkForeign $
    \(m0 :: Word64, n0 :: Word64, TPat.CP p _) ->
      let m = fromIntegral m0; n = fromIntegral n0
       in evaluate . TPat.cpattern $ TPat.Replicate m n p
  Pattern_run -> mkForeign $
    \(TPat.CP _ matcher, input :: Text) -> pure $ matcher input
  Pattern_isMatch -> mkForeign $
    \(TPat.CP _ matcher, input :: Text) -> pure . isJust $ matcher input
  Char_Class_any -> mkForeign $ \() -> pure TPat.Any
  Char_Class_not -> mkForeign $ pure . TPat.Not
  Char_Class_and -> mkForeign $ \(a, b) -> pure $ TPat.Intersect a b
  Char_Class_or -> mkForeign $ \(a, b) -> pure $ TPat.Union a b
  Char_Class_range -> mkForeign $ \(a, b) -> pure $ TPat.CharRange a b
  Char_Class_anyOf -> mkForeign $ \ccs -> do
    cs <- for ccs $ \case
      CharVal c -> pure c
      _ -> die "Text.patterns.charIn: non-character closure"
    evaluate $ TPat.CharSet cs
  Char_Class_alphanumeric -> mkForeign $ \() -> pure (TPat.CharClass TPat.AlphaNum)
  Char_Class_upper -> mkForeign $ \() -> pure (TPat.CharClass TPat.Upper)
  Char_Class_lower -> mkForeign $ \() -> pure (TPat.CharClass TPat.Lower)
  Char_Class_whitespace -> mkForeign $ \() -> pure (TPat.CharClass TPat.Whitespace)
  Char_Class_control -> mkForeign $ \() -> pure (TPat.CharClass TPat.Control)
  Char_Class_printable -> mkForeign $ \() -> pure (TPat.CharClass TPat.Printable)
  Char_Class_mark -> mkForeign $ \() -> pure (TPat.CharClass TPat.MarkChar)
  Char_Class_number -> mkForeign $ \() -> pure (TPat.CharClass TPat.Number)
  Char_Class_punctuation -> mkForeign $ \() -> pure (TPat.CharClass TPat.Punctuation)
  Char_Class_symbol -> mkForeign $ \() -> pure (TPat.CharClass TPat.Symbol)
  Char_Class_separator -> mkForeign $ \() -> pure (TPat.CharClass TPat.Separator)
  Char_Class_letter -> mkForeign $ \() -> pure (TPat.CharClass TPat.Letter)
  Char_Class_is -> mkForeign $ \(cl, c) -> evaluate $ TPat.charPatternPred cl c
  Text_patterns_char -> mkForeign $ \c ->
    let v = TPat.cpattern (TPat.Char c) in pure v
  Text_patterns_lookbehind1 -> mkForeign $ \cp ->
    let v = TPat.cpattern (TPat.Lookbehind1 cp) in pure v
  Text_patterns_negativeLookbehind1 -> mkForeign $ \cp ->
    let v = TPat.cpattern (TPat.NegativeLookbehind1 cp) in pure v
  Map_tip -> mkForeign $ \() -> pure (Map.empty @Val @Val)
  Map_bin -> mkForeign $ \(sz :: Word64, k :: Val, v :: Val, l, r) ->
    pure (Map.Bin (fromIntegral sz) k v l r)
  Map_insert -> mkForeign $ \(k :: Val, v :: Val, m :: Map Val Val) ->
    evaluate $ Map.insert k v m
  Map_lookup -> mkForeign $ \(k :: Val, v :: Map Val Val) ->
    evaluate $ Map.lookup k v
  Map_fromList -> mkForeign $ \(l :: [(Val, Val)]) ->
    evaluate $ Map.fromList l
  Map_eq -> mkForeign $ \(l :: Map Val Val, r :: Map Val Val) ->
    pure $ l == r
  Map_union -> mkForeign $ \(l :: Map Val Val, r :: Map Val Val) ->
    evaluate $ Map.union l r
  Map_intersect -> mkForeign $ \(l :: Map Val Val, r :: Map Val Val) ->
    evaluate $ Map.intersection l r
  Map_toList -> mkForeign $ \(m :: Map Val Val) ->
    evaluate . forceListSpine $ Map.toList m
  List_range -> mkForeign $ \(m :: Word64, n :: Word64) ->
    let sz
          | m < n = fromIntegral $ n - m
          | otherwise = 0
        mk i = NatVal $ m + fromIntegral i
     in evaluate . forceListSpine $ Sq.fromFunction sz mk
  List_sort -> mkForeign $ \(l :: Seq Val) -> pure $ Sq.unstableSort l
  Multimap_fromList -> mkForeign $ \(l :: [(Val, Val)]) -> do
    let listVals = l <&> \(k, v) -> (k, Sq.singleton v)
    -- Haskell Map.fromList calls the semigroup in reverse order, so we correct for it by flipping.
    let result :: Map Val Val = fmap encodeVal $ Map.fromListWith (flip (<>)) listVals
    evaluate result
  Set_fromList -> mkForeign $ \(l :: [Val]) -> do
    m <- evaluate $ Map.fromList $ zip l (repeat unitValue)
    pure . Data1 Ty.setRef TT.setWrapTag $ encodeVal m
  Set_union -> mkForeign $ \case
    (Data1 _ _ vl, Data1 _ _ vr) -> do
      (l :: Map Val Val) <- decodeVal vl
      (r :: Map Val Val) <- decodeVal vr
      m <- evaluate $ Map.union l r
      pure . Data1 Ty.setRef TT.setWrapTag $ encodeVal m
    _ -> die "Set.union: bad closure"
  Set_intersect -> mkForeign $ \case
    (Data1 _ _ vl, Data1 _ _ vr) -> do
      (l :: Map Val Val) <- decodeVal vl
      (r :: Map Val Val) <- decodeVal vr
      m <- evaluate $ Map.intersection l r
      pure . Data1 Ty.setRef TT.setWrapTag $ encodeVal m
    _ -> die "Set.insersect: bad closure"
  Set_toList -> mkForeign $ \case
    (Data1 _ _ vs) -> do
      (s :: Map Val Val) <- decodeVal vs
      evaluate . forceListSpine $ Map.keys s
    _ -> die "Set.toList: bad closure"
  Json_toText -> mkForeign $ \(clo :: Closure) -> do
    evaluate =<< emitJson clo
  Json_unconsText -> mkForeignExn $ \(txt :: Text) ->
    pure . bimap mkErr (second encodeVal) $ parseJson txt
    where
      mkErr err = F.Failure Ty.parseErrorRef msg errv
        where
          msg = renderJsonParseError err
          errv = encodeJsonParseError err
  Json_tryUnconsText -> mkForeign $ \(txt :: Text) ->
    pure . bimap encodeJsonParseError (second encodeVal) $ parseJson txt
  Avro_decodeBinary -> mkForeign $ \(env :: Closure, readSchema :: Closure, bytes :: Bytes.Bytes) -> do
    avroDecodeBinary env readSchema bytes
  where
    forceListSpine xs = foldl (\u x -> x `seq` u) xs xs
    chop = reverse . dropWhile isPathSeparator . reverse

    hostPreference :: Maybe Util.Text.Text -> SYS.HostPreference
    hostPreference Nothing = SYS.HostAny
    hostPreference (Just host) = SYS.Host $ Util.Text.unpack host

    mx :: Word64
    mx = fromIntegral (maxBound :: Int)

    customDelay :: Word64 -> IO ()
    customDelay n
      | n < mx = threadDelay (fromIntegral n)
      | otherwise = threadDelay maxBound >> customDelay (n - mx)

    exitDecode ExitSuccess = 0
    exitDecode (ExitFailure n) = n

    catchAll :: (MonadCatch m, MonadIO m, NFData a) => m a -> m (Either Util.Text.Text a)
    catchAll e = do
      e <- Exception.tryAnyDeep e
      pure $ case e of
        Left se -> Left (Util.Text.pack (show se))
        Right a -> Right a

{-# INLINE mkHashAlgorithm #-}
mkHashAlgorithm :: forall alg. (Hash.HashAlgorithm alg) => Data.Text.Text -> alg -> Args -> Stack -> IO (Bool, Stack)
mkHashAlgorithm txt alg =
  let algoRef = Builtin ("crypto.HashAlgorithm." <> txt)
   in mkForeign $ \() -> pure (HashAlgorithm algoRef alg)

-- | mkForeign is the most basic helper for implementing a Unison foreign function.
--   It takes a function from Unison arguments (decoded from the stack) to an IO result,
--   writes the result back to the stack, and returns a tuple indicating whether an exception occurred (always False here).
{-# INLINE mkForeign #-}
mkForeign :: (ForeignConvention a, ForeignConvention b) => (a -> IO b) -> Args -> Stack -> IO (Bool, Stack)
mkForeign !f !args !stk = do
  r <- f =<< readsAt stk args
  stk <- bump stk
  (False, stk) <$ writeBack stk r

-- | mkForeignIOF is like mkForeign, but it wraps the IO action in exception handling for IOExceptions.
--   If an IOException occurs, it returns a Failure value; otherwise, it returns the result.
--   This is useful for foreign functions that may throw IOExceptions, and you want to propagate those as Unison failures.
{-# INLINE mkForeignIOF #-}
mkForeignIOF ::
  (ForeignConvention a, ForeignConvention r) =>
  (a -> IO r) ->
  Args ->
  Stack ->
  IO (Bool, Stack)
mkForeignIOF f = mkForeign $ \a -> tryIOE (f a)
  where
    tryIOE :: IO a -> IO (Either (F.Failure Val) a)
    tryIOE = fmap handleIOE . UnliftIO.try
    handleIOE :: Either IOException a -> Either (F.Failure Val) a
    handleIOE (Left e) = Left $ F.Failure Ty.ioFailureRef (Util.Text.pack (show e)) unitValue
    handleIOE (Right a) = Right a

-- | mkForeignExn is for foreign functions that may return either a failure or a result (as an Either).
--   If the function returns a Left (failure), it writes the failure to the stack and returns (True, stack).
--   If it returns a Right (result), it writes the result and returns (False, stack).
--   This is for functions that have their own error reporting, not just IOExceptions.
{-# INLINE mkForeignExn #-}
mkForeignExn ::
  (ForeignConvention a, ForeignConvention e, ForeignConvention r) =>
  (a -> IO (Either (F.Failure e) r)) ->
  Args ->
  Stack ->
  IO (Bool, Stack)
mkForeignExn f args stk =
  readsAt stk args >>= f >>= \case
    Left e -> do
      stk <- bump stk
      (True, stk) <$ writeBack stk e
    Right r -> do
      stk <- bump stk
      (False, stk) <$ writeBack stk r

-- | mkForeignTls is for foreign functions that may throw TLS-specific exceptions or IOExceptions.
--   It wraps the IO action in two layers of exception handling: first for TLS exceptions, then for IOExceptions.
--   If an exception occurs, it returns a Failure value with the appropriate type (ioFailureRef or tlsFailureRef).
--   Otherwise, it returns the result.
{-# INLINE mkForeignTls #-}
mkForeignTls ::
  forall a r.
  (ForeignConvention a, ForeignConvention r) =>
  (a -> IO r) ->
  Args ->
  Stack ->
  IO (Bool, Stack)
mkForeignTls f = mkForeign $ \a -> fmap flatten (tryIO2 (tryIO1 (f a)))
  where
    tryIO1 :: IO r -> IO (Either TLS.TLSException r)
    tryIO1 = UnliftIO.try
    tryIO2 :: IO (Either TLS.TLSException r) -> IO (Either IOException (Either TLS.TLSException r))
    tryIO2 = UnliftIO.try
    flatten :: Either IOException (Either TLS.TLSException r) -> Either ((F.Failure Val)) r
    flatten (Left e) = Left (F.Failure Ty.ioFailureRef (Util.Text.pack (show e)) unitValue)
    flatten (Right (Left e)) = Left (F.Failure Ty.tlsFailureRef (Util.Text.pack (show e)) unitValue)
    flatten (Right (Right a)) = Right a

-- | mkForeignTlsE is like mkForeignTls, but for functions that may return an Either Failure r,
--   in addition to possibly throwing TLS or IO exceptions.
--   It flattens all three error sources (IO, TLS, and custom Failure) into a single Either Failure r.
{-# INLINE mkForeignTlsE #-}
mkForeignTlsE ::
  forall a r.
  (ForeignConvention a, ForeignConvention r) =>
  (a -> IO (Either Failure r)) ->
  Args ->
  Stack ->
  IO (Bool, Stack)
mkForeignTlsE f = mkForeign $ \a -> fmap flatten (tryIO2 (tryIO1 (f a)))
  where
    tryIO1 :: IO (Either Failure r) -> IO (Either TLS.TLSException (Either Failure r))
    tryIO1 = UnliftIO.try
    tryIO2 :: IO (Either TLS.TLSException (Either Failure r)) -> IO (Either IOException (Either TLS.TLSException (Either Failure r)))
    tryIO2 = UnliftIO.try
    flatten :: Either IOException (Either TLS.TLSException (Either Failure r)) -> Either Failure r
    flatten (Left e) = Left (F.Failure Ty.ioFailureRef (Util.Text.pack (show e)) unitValue)
    flatten (Right (Left e)) = Left (F.Failure Ty.tlsFailureRef (Util.Text.pack (show e)) unitValue)
    flatten (Right (Right (Left e))) = Left e
    flatten (Right (Right (Right a))) = Right a

{-# INLINE unsafeSTMToIO #-}
unsafeSTMToIO :: STM.STM a -> IO a
unsafeSTMToIO (STM.STM m) = IO m

signEd25519Wrapper ::
  (Bytes.Bytes, Bytes.Bytes, Bytes.Bytes) -> Either Failure Bytes.Bytes
signEd25519Wrapper (secret0, public0, msg0) = case validated of
  CryptoFailed err ->
    Left (F.Failure Ty.cryptoFailureRef (errMsg err) unitValue)
  CryptoPassed (secret, public) ->
    Right . Bytes.fromArray $ Ed25519.sign secret public msg
  where
    msg = Bytes.toArray msg0 :: ByteString
    validated =
      (,)
        <$> Ed25519.secretKey (Bytes.toArray secret0 :: ByteString)
        <*> Ed25519.publicKey (Bytes.toArray public0 :: ByteString)

    errMsg CryptoError_PublicKeySizeInvalid =
      "ed25519: Public key size invalid"
    errMsg CryptoError_SecretKeySizeInvalid =
      "ed25519: Secret key size invalid"
    errMsg CryptoError_SecretKeyStructureInvalid =
      "ed25519: Secret key structure invalid"
    errMsg _ = "ed25519: unexpected error"

verifyEd25519Wrapper ::
  (Bytes.Bytes, Bytes.Bytes, Bytes.Bytes) -> Either Failure Bool
verifyEd25519Wrapper (public0, msg0, sig0) = case validated of
  CryptoFailed err ->
    Left $ F.Failure Ty.cryptoFailureRef (errMsg err) unitValue
  CryptoPassed (public, sig) ->
    Right $ Ed25519.verify public msg sig
  where
    msg = Bytes.toArray msg0 :: ByteString
    validated =
      (,)
        <$> Ed25519.publicKey (Bytes.toArray public0 :: ByteString)
        <*> Ed25519.signature (Bytes.toArray sig0 :: ByteString)

    errMsg CryptoError_PublicKeySizeInvalid =
      "ed25519: Public key size invalid"
    errMsg CryptoError_SecretKeySizeInvalid =
      "ed25519: Secret key size invalid"
    errMsg CryptoError_SecretKeyStructureInvalid =
      "ed25519: Secret key structure invalid"
    errMsg _ = "ed25519: unexpected error"

signRsaWrapper ::
  (Bytes.Bytes, Bytes.Bytes) -> Either Failure Bytes.Bytes
signRsaWrapper (secret0, msg0) = case validated of
  Left err ->
    Left (F.Failure Ty.cryptoFailureRef err unitValue)
  Right secret ->
    case RSA.sign Nothing (Just Hash.SHA256) secret msg of
      Left err -> Left (F.Failure Ty.cryptoFailureRef (Rsa.rsaErrorToText err) unitValue)
      Right signature -> Right $ Bytes.fromByteString signature
  where
    msg = Bytes.toArray msg0 :: ByteString
    validated = Rsa.parseRsaPrivateKey (Bytes.toArray secret0 :: ByteString)

verifyRsaWrapper ::
  (Bytes.Bytes, Bytes.Bytes, Bytes.Bytes) -> Either Failure Bool
verifyRsaWrapper (public0, msg0, sig0) = case validated of
  Left err ->
    Left $ F.Failure Ty.cryptoFailureRef err unitValue
  Right public ->
    Right $ RSA.verify (Just Hash.SHA256) public msg sig
  where
    msg = Bytes.toArray msg0 :: ByteString
    sig = Bytes.toArray sig0 :: ByteString
    validated = Rsa.parseRsaPublicKey (Bytes.toArray public0 :: ByteString)

type Failure = F.Failure Val

checkBounds :: Text -> Int -> Word64 -> IO (Either Failure b) -> IO (Either Failure b)
checkBounds name l w act
  | w < fromIntegral l = act
  | otherwise = pure $ Left err
  where
    msg = name <> ": array index out of bounds"
    err = F.Failure Ty.arrayFailureRef msg (natValue w)

-- Performs a bounds check on a byte array. Strategy is as follows:
--
--   isz = signed array size-in-bytes
--   off = unsigned byte offset into the array
--   esz = unsigned number of bytes to be read
--
--   1. Turn the signed size-in-bytes of the array unsigned
--   2. Add the offset to the to-be-read number to get the maximum size needed
--   3. Check that the actual array size is at least as big as the needed size
--   4. Check that the offset is less than the size
--
-- Step 4 ensures that step 3 has not overflowed. Since an actual array size can
-- only be 63 bits (since it is signed), the only way for 3 to overflow is if
-- the offset is larger than a possible array size, since it would need to be
-- 2^64-k, where k is the small (<=8) number of bytes to be read.
checkBoundsPrim ::
  Text -> Int -> Word64 -> Word64 -> IO (Either Failure b) -> IO (Either Failure b)
checkBoundsPrim name isz off esz act
  | w > bsz || off > bsz = pure $ Left err
  | otherwise = act
  where
    msg = name <> ": array index out of bounds"
    err = F.Failure Ty.arrayFailureRef msg (natValue off)

    bsz = fromIntegral isz
    w = off + esz

type RW = PA.PrimState IO

checkedRead ::
  Text -> (PA.MutableArray RW Val, Word64) -> IO (Either Failure Val)
checkedRead name (arr, w) =
  checkBounds
    name
    (PA.sizeofMutableArray arr)
    w
    (Right <$> PA.readArray arr (fromIntegral w))

checkedWrite ::
  Text -> (PA.MutableArray RW Val, Word64, Val) -> IO (Either Failure ())
checkedWrite name (arr, w, v) =
  checkBounds
    name
    (PA.sizeofMutableArray arr)
    w
    (Right <$> PA.writeArray arr (fromIntegral w) v)

checkedIndex ::
  Text -> (PA.Array Val, Word64) -> IO (Either Failure Val)
checkedIndex name (arr, w) =
  checkBounds
    name
    (PA.sizeofArray arr)
    w
    (Right <$> PA.indexArrayM arr (fromIntegral w))

checkedRead8 :: Text -> (PA.MutableByteArray RW, Word64) -> IO (Either Failure Word64)
checkedRead8 name (arr, i) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) i 1 $
    Right . fromIntegral <$> PA.readByteArray @Word8 arr j
  where
    j = fromIntegral i

uncheckedRead16 ::
  ByteOrder -> -- desired byte order
  PA.MutableByteArray RW ->
  Int -> -- byte offset
  IO Word16
uncheckedRead16 byteOrder arr off = do
  let fixEndianness :: Word16 -> Word16
      fixEndianness w =
        if targetByteOrder == byteOrder then w else byteSwap16 w
  w <- PA.primitive $ \s0 ->
    case arr of
      PA.MutableByteArray mba# ->
        case off of
          I# off# ->
            case readWord8ArrayAsWord16# mba# off# s0 of
              (# s1, w16# #) -> (# s1, W16# w16# #)
  pure (fixEndianness w)

checkedRead16 ::
  ByteOrder -> -- desired byte order
  Text ->
  (PA.MutableByteArray RW, Word64) -> -- (array, byte offset)
  IO (Either Failure Word64)
checkedRead16 byteOrder name (arr, iW) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) iW 2 $ do
    let !off = fromIntegral iW :: Int
    w <- uncheckedRead16 byteOrder arr off
    pure $ Right (fromIntegral w)

checkedRead24 ::
  ByteOrder ->
  Text ->
  (PA.MutableByteArray RW, Word64) ->
  IO (Either Failure Word64)
checkedRead24 byteOrder name (arr, iW) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) iW 3 $ do
    let !off = fromIntegral iW :: Int
    w16 <- uncheckedRead16 byteOrder arr off
    w8 <- PA.readByteArray @Word8 arr (off + 2)
    let result =
          if byteOrder == BigEndian
            then (fromIntegral w16 `shiftL` 8) .|. fromIntegral w8
            else (fromIntegral w8 `shiftL` 16) .|. fromIntegral w16
    pure $ Right result

uncheckedRead32 ::
  ByteOrder -> -- desired byte order
  PA.MutableByteArray RW ->
  Int -> -- byte offset
  IO Word32
uncheckedRead32 byteOrder arr off = do
  let fixEndianness :: Word32 -> Word32
      fixEndianness w =
        if targetByteOrder == byteOrder then w else byteSwap32 w
  w <- PA.primitive $ \s0 ->
    case arr of
      PA.MutableByteArray mba# ->
        case off of
          I# off# ->
            case readWord8ArrayAsWord32# mba# off# s0 of
              (# s1, w32# #) -> (# s1, W32# w32# #)
  pure (fixEndianness w)

checkedRead32 :: ByteOrder -> Text -> (PA.MutableByteArray RW, Word64) -> IO (Either Failure Word64)
checkedRead32 byteOrder name (arr, iW) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) iW 4 $ do
    let !off = fromIntegral iW :: Int
    w <- uncheckedRead32 byteOrder arr off
    pure $ Right (fromIntegral w)

checkedRead40 :: ByteOrder -> Text -> (PA.MutableByteArray RW, Word64) -> IO (Either Failure Word64)
checkedRead40 byteOrder name (arr, iW) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) iW 6 $ do
    let !off = fromIntegral iW :: Int
    w32 <- uncheckedRead32 byteOrder arr off
    w16 <- uncheckedRead16 byteOrder arr (off + 4)
    let result =
          if byteOrder == BigEndian
            then (fromIntegral w32 `shiftL` 16) .|. fromIntegral w16
            else (fromIntegral w16 `shiftL` 32) .|. fromIntegral w32
    pure $ Right result

checkedRead64 :: ByteOrder -> Text -> (PA.MutableByteArray RW, Word64) -> IO (Either Failure Word64)
checkedRead64 byteOrder name (arr, i) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) i 8 $ do
    let !off = fromIntegral i :: Int
        fixEndianness :: Word64 -> Word64
        fixEndianness w =
          if targetByteOrder == byteOrder then w else byteSwap64 w
    w <- PA.primitive $ \s0 ->
      case arr of
        PA.MutableByteArray mba# ->
          case off of
            I# off# ->
              case readWord8ArrayAsWord64# mba# off# s0 of
                (# s1, w64# #) -> (# s1, W64# w64# #)
    pure $ Right (fromIntegral (fixEndianness w))

mk16 :: Word8 -> Word8 -> Either Failure Word64
mk16 b0 b1 = Right $ (fromIntegral b0 `shiftL` 8) .|. (fromIntegral b1)

mk24 :: Word8 -> Word8 -> Word8 -> Either Failure Word64
mk24 b0 b1 b2 =
  Right $
    (fromIntegral b0 `shiftL` 16)
      .|. (fromIntegral b1 `shiftL` 8)
      .|. (fromIntegral b2)

mk32 :: Word8 -> Word8 -> Word8 -> Word8 -> Either Failure Word64
mk32 b0 b1 b2 b3 =
  Right $
    (fromIntegral b0 `shiftL` 24)
      .|. (fromIntegral b1 `shiftL` 16)
      .|. (fromIntegral b2 `shiftL` 8)
      .|. (fromIntegral b3)

mk40 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Either Failure Word64
mk40 b0 b1 b2 b3 b4 =
  Right $
    (fromIntegral b0 `shiftL` 32)
      .|. (fromIntegral b1 `shiftL` 24)
      .|. (fromIntegral b2 `shiftL` 16)
      .|. (fromIntegral b3 `shiftL` 8)
      .|. (fromIntegral b4)

mk64 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Either Failure Word64
mk64 b0 b1 b2 b3 b4 b5 b6 b7 =
  Right $
    (fromIntegral b0 `shiftL` 56)
      .|. (fromIntegral b1 `shiftL` 48)
      .|. (fromIntegral b2 `shiftL` 40)
      .|. (fromIntegral b3 `shiftL` 32)
      .|. (fromIntegral b4 `shiftL` 24)
      .|. (fromIntegral b5 `shiftL` 16)
      .|. (fromIntegral b6 `shiftL` 8)
      .|. (fromIntegral b7)

checkedWrite8 :: Text -> (PA.MutableByteArray RW, Word64, Word64) -> IO (Either Failure ())
checkedWrite8 name (arr, i, v) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) i 1 $ do
    PA.writeByteArray arr j (fromIntegral v :: Word8)
    pure (Right ())
  where
    j = fromIntegral i

checkedWrite16 :: ByteOrder -> Text -> (PA.MutableByteArray RW, Word64, Word64) -> IO (Either Failure ())
checkedWrite16 byteOrder name (arr, iW, v0) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) iW 2 $ do
    let !off = fromIntegral iW :: Int
        !vBE =
          if targetByteOrder == byteOrder
            then fromIntegral v0 :: Word16
            else byteSwap16 (fromIntegral v0 :: Word16)
    PA.primitive_ $ \s0 ->
      case arr of
        PA.MutableByteArray mba# ->
          case off of
            I# off# ->
              case vBE of
                W16# w# ->
                  writeWord8ArrayAsWord16# mba# off# w# s0
    pure (Right ())

checkedWrite32 :: ByteOrder -> Text -> (PA.MutableByteArray RW, Word64, Word64) -> IO (Either Failure ())
checkedWrite32 byteOrder name (arr, iW, v0) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) iW 4 $ do
    let !off = fromIntegral iW :: Int
        !vBE =
          if targetByteOrder == byteOrder
            then fromIntegral v0 :: Word32
            else byteSwap32 (fromIntegral v0 :: Word32)
    PA.primitive_ $ \s0 ->
      case arr of
        PA.MutableByteArray mba# ->
          case off of
            I# off# ->
              case vBE of
                W32# w# ->
                  writeWord8ArrayAsWord32# mba# off# w# s0
    pure (Right ())

checkedWrite64 :: ByteOrder -> Text -> (PA.MutableByteArray RW, Word64, Word64) -> IO (Either Failure ())
checkedWrite64 byteOrder name (arr, iW, v0) =
  checkBoundsPrim name (PA.sizeofMutableByteArray arr) iW 8 $ do
    let !off = fromIntegral iW :: Int
        !vBE =
          if targetByteOrder == byteOrder
            then fromIntegral v0 :: Word64
            else byteSwap64 (fromIntegral v0 :: Word64)
    PA.primitive_ $ \s0 ->
      case arr of
        PA.MutableByteArray mba# ->
          case off of
            I# off# ->
              case vBE of
                W64# w# ->
                  writeWord8ArrayAsWord64# mba# off# w# s0
    pure (Right ())

-- index single byte
checkedIndex8 :: Text -> (PA.ByteArray, Word64) -> IO (Either Failure Word64)
checkedIndex8 name (arr, i) =
  checkBoundsPrim name (PA.sizeofByteArray arr) i 1 . pure $
    let j = fromIntegral i
     in Right . fromIntegral $ PA.indexByteArray @Word8 arr j

-- index 16 big-endian
checkedIndex16 :: Text -> (PA.ByteArray, Word64) -> IO (Either Failure Word64)
checkedIndex16 name (arr, i) =
  checkBoundsPrim name (PA.sizeofByteArray arr) i 2 . pure $
    let j = fromIntegral i
     in mk16 (PA.indexByteArray arr j) (PA.indexByteArray arr (j + 1))

-- index 32 big-endian
checkedIndex24 :: Text -> (PA.ByteArray, Word64) -> IO (Either Failure Word64)
checkedIndex24 name (arr, i) =
  checkBoundsPrim name (PA.sizeofByteArray arr) i 3 . pure $
    let j = fromIntegral i
     in mk24
          (PA.indexByteArray arr j)
          (PA.indexByteArray arr (j + 1))
          (PA.indexByteArray arr (j + 2))

-- index 32 big-endian
checkedIndex32 :: Text -> (PA.ByteArray, Word64) -> IO (Either Failure Word64)
checkedIndex32 name (arr, i) =
  checkBoundsPrim name (PA.sizeofByteArray arr) i 4 . pure $
    let j = fromIntegral i
     in mk32
          (PA.indexByteArray arr j)
          (PA.indexByteArray arr (j + 1))
          (PA.indexByteArray arr (j + 2))
          (PA.indexByteArray arr (j + 3))

-- index 40 big-endian
checkedIndex40 :: Text -> (PA.ByteArray, Word64) -> IO (Either Failure Word64)
checkedIndex40 name (arr, i) =
  checkBoundsPrim name (PA.sizeofByteArray arr) i 5 . pure $
    let j = fromIntegral i
     in mk40
          (PA.indexByteArray arr j)
          (PA.indexByteArray arr (j + 1))
          (PA.indexByteArray arr (j + 2))
          (PA.indexByteArray arr (j + 3))
          (PA.indexByteArray arr (j + 4))

-- index 64 big-endian
checkedIndex64 :: Text -> (PA.ByteArray, Word64) -> IO (Either Failure Word64)
checkedIndex64 name (arr, i) =
  checkBoundsPrim name (PA.sizeofByteArray arr) i 8 . pure $
    let j = fromIntegral i
     in mk64
          (PA.indexByteArray arr j)
          (PA.indexByteArray arr (j + 1))
          (PA.indexByteArray arr (j + 2))
          (PA.indexByteArray arr (j + 3))
          (PA.indexByteArray arr (j + 4))
          (PA.indexByteArray arr (j + 5))
          (PA.indexByteArray arr (j + 6))
          (PA.indexByteArray arr (j + 7))

-- JSON replacement implementations
jsonNull, jsonTrue, jsonFalse :: Val
jsonNull = BoxedVal $ Enum Ty.jsonRef TT.jsonNullTag
jsonTrue = BoxedVal . Data1 Ty.jsonRef TT.jsonBoolTag $ BoolVal True
jsonFalse = BoxedVal . Data1 Ty.jsonRef TT.jsonBoolTag $ BoolVal False

jsonArr, jsonObj :: Seq Val -> Val
jsonArr sq = BoxedVal . Data1 Ty.jsonRef TT.jsonArrTag $ encodeVal sq
jsonObj sq = BoxedVal . Data1 Ty.jsonRef TT.jsonObjTag $ encodeVal sq

jsonNum :: TL.Text -> Val
jsonNum n = BoxedVal . Data1 Ty.jsonRef TT.jsonNumTag $ encodeVal n

jsonText :: Val -> Val
jsonText v = BoxedVal $ Data1 Ty.jsonRef TT.jsonTextTag v

data JsonParseError = JPErr Text Int TL.Text

renderJsonParseError :: JsonParseError -> Text
renderJsonParseError (JPErr msg pos rem) =
  "JSON parsing error at position "
    <> pack (show pos)
    <> ": "
    <> msg
    <> "\n  Remainder of line: "
    <> fromLazyText line
  where
    line = TL.takeWhile (not . (== '\n')) rem

encodeJsonParseError :: JsonParseError -> Val
encodeJsonParseError (JPErr msg pos rem) =
  BoxedVal $
    DataC
      Ty.parseErrorRef
      TT.jsonParseErrorTag
      [encodeVal msg, NatVal n, encodeVal rem]
  where
    n
      | pos < 0 = 0
      | otherwise = fromIntegral pos

parseJson :: Text -> Either JsonParseError (Val, Text)
parseJson initial =
  fmap fromLazyText <$> root (toLazyText initial)
  where
    err :: Text -> TL.Text -> Either JsonParseError a
    err msg rest = Left $ JPErr msg pos rest
      where
        pos = Util.Text.size initial - fromIntegral (TL.length rest)

    root = main . TL.stripStart

    numberStart '-' = True
    numberStart c = isDigit c

    number txt = case sign txt of
      0 -> Nothing
      n -> Just (TL.splitAt n txt)

    sign txt = case TL.uncons txt of
      Just ('-', txt) -> firstDigit 1 txt
      _ -> firstDigit 0 txt

    firstDigit !n txt = case TL.uncons txt of
      Just ('0', txt) -> decimal (n + 1) txt
      Just (c, txt)
        | '1' <= c, c <= '9' -> whole (n + 1) txt
      _ -> 0

    whole !n (TL.span isDigit -> (pre, txt)) =
      decimal (n + TL.length pre) txt

    decimal !n txt = case TL.uncons txt of
      Just ('.', txt)
        | (pre, txt) <- TL.span isDigit txt,
          not (TL.null pre) ->
            exponent (n + 1 + TL.length pre) txt
      _ -> exponent n txt

    exponent !n txt = case TL.uncons txt of
      Just (c, txt) | c == 'e' || c == 'E' -> case TL.uncons txt of
        Just (c, txt) | c == '-' || c == '+' -> digits (n + 2) txt
        _ -> digits (n + 1) txt
      _ -> n

    digits !n (TL.takeWhile isDigit -> pre) = n + TL.length pre

    main txt0 = case TL.uncons txt0 of
      Nothing -> err "unexpected end of file" txt0
      Just ('{', txt) -> obj Sq.empty txt
      Just ('[', txt) -> array Sq.empty txt
      Just ('"', _) -> first jsonText <$> textLit txt0
      Just ('n', txt)
        | (pre, post) <- TL.splitAt 3 txt ->
            if pre == "ull"
              then pure (jsonNull, post)
              else err "expected null" txt0
      Just ('t', txt)
        | (pre, post) <- TL.splitAt 3 txt ->
            if pre == "rue"
              then pure (jsonTrue, post)
              else err "expected true" txt0
      Just ('f', txt)
        | (pre, post) <- TL.splitAt 4 txt ->
            if pre == "alse"
              then pure (jsonFalse, post)
              else err "expected false" txt0
      Just (c, _)
        | numberStart c,
          Just (n, rest) <- number txt0 ->
            pure (jsonNum n, rest)
      _ -> err ("unknown token: " <> tok) txt0
        where
          tok = fromLazyText (TL.take 10 txt0)

    array :: Sq.Seq Val -> TL.Text -> Either JsonParseError (Val, TL.Text)
    array acc (TL.stripStart -> txt) = case TL.uncons txt of
      Nothing ->
        err "unexpected end of file while parsing an array" txt
      Just (']', rest) ->
        pure (jsonArr acc, rest)
      _ ->
        main txt >>= \case
          (el, TL.stripStart -> rest) -> case TL.uncons rest of
            Just (',', rest) -> array (acc Sq.|> el) rest
            Just (']', rest) -> pure (jsonArr $ acc Sq.|> el, rest)
            _ ->
              err "expected ',' or ']'" rest

    obj :: Sq.Seq Val -> TL.Text -> Either JsonParseError (Val, TL.Text)
    obj acc (TL.stripStart -> txt) = case TL.uncons txt of
      Nothing ->
        err "unexpected end of file while parsing an object" txt
      Just ('}', rest) ->
        pure (jsonObj acc, rest)
      _ ->
        entry txt >>= \case
          (el, TL.stripStart -> rest) -> case TL.uncons rest of
            Just (',', rest) -> obj (acc Sq.|> el) rest
            Just ('}', rest) -> pure (jsonObj $ acc Sq.|> el, rest)
            _ -> err "expected ',' or '}'" rest

    entry txt =
      textLit txt >>= \case
        (key, TL.stripStart -> txt) -> case TL.uncons txt of
          Just (':', txt) ->
            first (Tup2V key) <$> root txt
          _ -> err "expected ':'" txt

    textLit :: TL.Text -> Either JsonParseError (Val, TL.Text)
    textLit txt = case TL.uncons txt of
      Just ('"', rest) -> textBody txt [] rest
      _ -> err "expected text literal" txt

    hexDig txt =
      TL.uncons txt >>= \case
        (c, rest)
          | '0' <= c, c <= '9' -> Just (digitToInt c, rest)
          | 'a' <= c, c <= 'f' -> Just (10 + (ord c - ord 'a'), rest)
          | 'A' <= c, c <= 'F' -> Just (10 + (ord c - ord 'A'), rest)
          | otherwise -> Nothing

    uescape txt = do
      (a, txt) <- hexDig txt
      (b, txt) <- hexDig txt
      (c, txt) <- hexDig txt
      (d, txt) <- hexDig txt
      pure (((((a * 16) + b) * 16) + c) * 16 + d, txt)

    special c = c == '"' || c == '\\'

    textBody :: TL.Text -> [TL.Text] -> TL.Text -> Either JsonParseError (Val, TL.Text)
    textBody txt0 acc txt
      | (pre, txt) <- TL.break special txt,
        acc <- pre : acc =
          case TL.uncons txt of
            Just ('"', txt) ->
              pure (encodeVal @TL.Text . TL.concat $ reverse acc, txt)
            Just ('\\', txt) -> case TL.uncons txt of
              Just ('f', txt) -> textBody txt0 ("\f" : acc) txt
              Just ('n', txt) -> textBody txt0 ("\n" : acc) txt
              Just ('r', txt) -> textBody txt0 ("\r" : acc) txt
              Just ('t', txt) -> textBody txt0 ("\t" : acc) txt
              Just ('b', txt) -> textBody txt0 ("\b" : acc) txt
              Just ('/', txt) -> textBody txt0 ("/" : acc) txt
              Just ('\\', txt) -> textBody txt0 ("\\" : acc) txt
              Just ('"', txt) -> textBody txt0 ("\"" : acc) txt
              Just ('u', txt)
                | Just (n, txt) <- uescape txt ->
                    textBody txt0 (TL.singleton (chr n) : acc) txt
              _ -> err "expected text literal" txt0
            _ -> err "expected text literal" txt0

emitJson :: Closure -> IO Text
emitJson = \case
  Enum _ t
    | TT.jsonNullTag == t -> pure "null"
  Data1 _ t v
    | TT.jsonBoolTag == t,
      BoolVal b <- v ->
        pure $ if b then "true" else "false"
    | TT.jsonNumTag == t ->
        decodeVal @Text v
    | TT.jsonObjTag == t ->
        fmap renderObject . traverse emitPair =<< decodeVal @(Seq Val) v
    | TT.jsonTextTag == t ->
        literalForm <$> decodeVal @Text v
    | TT.jsonArrTag == t ->
        fmap renderArray . traverse emitJsonVal =<< decodeVal @(Seq Val) v
  c -> die $ "Json.toText: unrecognized Json value: " ++ show c
  where
    emitJsonVal (BoxedVal c) = emitJson c
    emitJsonVal v =
      die $ "Json.toText: unrecognized Json value: " ++ show v

    commaSep = fold . Sq.intersperse ","
    renderArray s = "[" <> commaSep s <> "]"
    renderObject s = "{" <> commaSep s <> "}"

    emitPair (Tup2V x y) =
      mapping <$> decodeVal @Text x <*> emitJsonVal y
    emitPair v =
      die $ "Json.toText: unrecognized Json object pair: " ++ show v

    mapping key val = literalForm key <> ":" <> val

    special c = TL.any (== c) "\"\\/\b\f\n\r\t" || ord c <= 31

    literalForm tx =
      "\"" <> fromLazyText (escape [] (toLazyText tx)) <> "\""

    escape acc tx
      | TL.null tx = TL.concat (reverse acc)
      | (pre, rest) <- TL.break special tx =
          escape1 (pre : acc) rest

    hexCode c = TL.pack $ replicate (2 - length s) '0' ++ s
      where
        s = showHex (ord c) ""

    escape1 acc tx = case TL.uncons tx of
      Nothing -> TL.concat (reverse acc)
      Just (c, rest) -> escape (chs : acc) rest
        where
          chs
            | '"' <- c = "\\\""
            | '\\' <- c = "\\\\"
            | '\b' <- c = "\\b"
            | '\f' <- c = "\\f"
            | '\n' <- c = "\\n"
            | '\r' <- c = "\\r"
            | '\t' <- c = "\\t"
            | ord c <= 31 = "\\u00" <> hexCode c
            | otherwise = TL.singleton c

-- Avro replacement implementations
avroNull, avroTrue, avroFalse :: Val
avroNull = BoxedVal $ Enum Ty.avroRef TT.avroNullTag
avroTrue = BoxedVal $ Data1 Ty.avroRef TT.avroBooleanTag $ BoolVal True
avroFalse = BoxedVal $ Data1 Ty.avroRef TT.avroBooleanTag $ BoolVal False

avroDecodeReadSchema :: Closure -> IO Avro.ReadSchema
avroDecodeReadSchema = \case
  Enum _ t
    | TT.avroReadSchemaNullTag == t -> pure ReadSchema.Null
    | TT.avroReadSchemaBooleanTag == t -> pure ReadSchema.Boolean
  Data1 _ t v@(BoxedVal c)
    | TT.avroReadSchemaIntTag == t -> ReadSchema.Int <$> decodeMaybe avroDecodeLogicalInt c
    | TT.avroReadSchemaFloatTag == t -> ReadSchema.Float <$> avroDecodeReadFloat c
    | TT.avroReadSchemaDoubleTag == t -> ReadSchema.Double <$> avroDecodeReadDouble c
    | TT.avroReadSchemaBytesTag == t -> ReadSchema.Bytes <$> decodeMaybe avroDecodeLogicalBytes c
    | TT.avroReadSchemaStringTag == t -> ReadSchema.String <$> decodeMaybe avroDecodeLogicalString c
    | TT.avroReadSchemaNamedTypeTag == t -> ReadSchema.NamedType <$> avroDecodeTypeName c
    | TT.avroReadSchemaUnionTag == t -> ReadSchema.Union . Vector.fromList <$> ((decodeVal v :: IO [(Int, Closure)]) >>= traverse (\case (ix, c) -> (ix,) <$> avroDecodeReadSchema c))
    | TT.avroReadSchemaRecordTag == t -> avroDecodeReadRecord c
    | TT.avroReadSchemaEnumTag == t -> do
        (name, aliases, doc, symbols) <- avroDecodeEnum c
        pure $ ReadSchema.Enum name aliases doc symbols
    | TT.avroReadSchemaFixedTag == t -> do
        (name, aliases, size, logicalType) <- avroDecodeFixed c
        pure $ ReadSchema.Fixed name aliases size logicalType
    | TT.avroReadSchemaNullTag == t || TT.avroReadSchemaBooleanTag == t || TT.avroReadSchemaStringTag == t || TT.avroReadSchemaFloatTag == t || TT.avroReadSchemaFixedTag == t || TT.avroReadSchemaDoubleTag == t || TT.avroReadSchemaBytesTag == t || TT.avroReadSchemaNamedTypeTag == t || TT.avroReadSchemaArrayTag == t || TT.avroReadSchemaMapTag == t || TT.avroReadSchemaLongTag == t || TT.avroReadSchemaFreeUnionTag == t || TT.avroReadSchemaEnumTag == t || TT.avroReadSchemaUnionTag == t || TT.avroReadSchemaArrayTag == t ->
        die $ "avroDecodeReadSchema: type error: mismatched data1 tag " ++ show t ++ " " ++ show v
    | otherwise -> die $ "avroDecodeReadSchema: type error: unknown data1 tag " ++ show t ++ " " ++ show v ++ " " ++ show (TT.unpackTags t)
  Data2 _ t (BoxedVal c1) (BoxedVal c2)
    | TT.avroReadSchemaArrayTag == t -> ReadSchema.Array <$> avroDecodeReadSchema c1
    | TT.avroReadSchemaMapTag == t -> ReadSchema.Map <$> avroDecodeReadSchema c1
    | TT.avroReadSchemaLongTag == t -> ReadSchema.Long <$> avroDecodeReadLong c1 <*> decodeMaybe avroDecodeLogicalTypeLong c2
  Data2 _ t v1 (BoxedVal c2)
    | TT.avroReadSchemaFreeUnionTag == t -> ReadSchema.FreeUnion <$> decodeVal v1 <*> avroDecodeReadSchema c2
  d -> die $ "avroDecodeReadSchema: type error: " ++ show d

avroDecodeSchema :: Closure -> IO AvroSchema.Schema
avroDecodeSchema = \case
  Enum _ t
    | TT.avroSchemaNullTag == t -> pure AvroSchema.Null
    | TT.avroSchemaBooleanTag == t -> pure AvroSchema.Boolean
    | TT.avroSchemaFloatTag == t -> pure AvroSchema.Float
    | TT.avroSchemaDoubleTag == t -> pure AvroSchema.Double
  Data1 _ t v@(BoxedVal c)
    | TT.avroSchemaIntTag == t -> AvroSchema.Int <$> decodeMaybe avroDecodeLogicalInt c
    | TT.avroSchemaLongTag == t -> AvroSchema.Long <$> decodeMaybe avroDecodeLogicalTypeLong c
    | TT.avroSchemaStringTag == t -> AvroSchema.String <$> decodeMaybe avroDecodeLogicalString c
    | TT.avroSchemaBytesTag == t -> AvroSchema.Bytes <$> decodeMaybe avroDecodeLogicalBytes c
    | TT.avroSchemaFixedTag == t -> do
        (name, aliases, size, logicalType) <- avroDecodeFixed c
        pure $ AvroSchema.Fixed name aliases size logicalType
    | TT.avroSchemaEnumTag == t -> do
        (name, aliases, doc, symbols) <- avroDecodeEnum c
        pure $ AvroSchema.Enum name aliases doc symbols
    | TT.avroSchemaRecordTag == t -> avroDecodeRecord c
    | TT.avroSchemaMapTag == t -> AvroSchema.Map <$> avroDecodeSchema c
    | TT.avroSchemaArrayTag == t -> AvroSchema.Array <$> avroDecodeSchema c
    | TT.avroSchemaNamedTypeTag == t -> AvroSchema.NamedType <$> avroDecodeTypeName c
    | TT.avroSchemaUnionTag == t -> AvroSchema.Union . Vector.fromList <$> (traverse avroDecodeSchema =<< decodeVal v)
  d -> die $ "avroDecodeSchema: type error: " ++ show d

avroDecodeLogicalTypeLong :: Closure -> IO ReadSchema.LogicalTypeLong
avroDecodeLogicalTypeLong = \case
  Enum _ t
    | TT.avroLogicalLongTimeMicrosTag == t -> pure ReadSchema.TimeMicros
    | TT.avroLogicalLongTimestampMillisTag == t -> pure ReadSchema.TimestampMillis
    | TT.avroLogicalLongTimestampMicrosTag == t -> pure ReadSchema.TimestampMicros
    | TT.avroLogicalLongLocalTimestampMillisTag == t -> pure ReadSchema.LocalTimestampMillis
    | TT.avroLogicalLongLocalTimestampMicrosTag == t -> pure ReadSchema.LocalTimestampMicros
  Data1 _ _ (BoxedVal c) -> ReadSchema.DecimalL <$> avroDecodeDecimal c
  d -> die $ "avroDecodeLogicalTypeLong: type error: " ++ show d

avroDecodeReadRecord :: Closure -> IO ReadSchema.ReadSchema
avroDecodeReadRecord = \case
  DataC _ _ [BoxedVal name, aliases, doc, fields] -> do
    name' <- avroDecodeTypeName name
    aliases' <- traverse avroDecodeTypeName =<< (decodeVal aliases :: IO [Closure])
    doc' <- fmap Util.Text.toText <$> decodeVal doc
    fields' <- traverse avroDecodeReadField =<< (decodeVal fields :: IO [Closure])
    pure $ ReadSchema.Record name' aliases' doc' fields'
  d -> die $ "avroDecodeReadRecord: type error: " ++ show d

avroDecodeRecord :: Closure -> IO AvroSchema.Schema
avroDecodeRecord = \case
  DataC _ _ [BoxedVal name, aliases, doc, fields] -> AvroSchema.Record <$> avroDecodeTypeName name <*> (traverse avroDecodeTypeName =<< (decodeVal aliases :: IO [Closure])) <*> (fmap Util.Text.toText <$> decodeVal doc) <*> (traverse avroDecodeField =<< (decodeVal fields :: IO [Closure]))
  d -> die $ "avroDecodeReadRecord: type error: " ++ show d

avroDecodeReadField :: Closure -> IO ReadSchema.ReadField
avroDecodeReadField = \case
  DataC _ _ [name, aliases, doc, BoxedVal typ, BoxedVal status, BoxedVal order, BoxedVal def] -> (ReadSchema.ReadField . Util.Text.toText <$> decodeVal name) <*> (map Util.Text.toText <$> decodeVal aliases) <*> (fmap Util.Text.toText <$> decodeVal doc) <*> decodeMaybe avroDecodeOrder order <*> avroDecodeFieldStatus status <*> avroDecodeReadSchema typ <*> decodeMaybe avroDecodeDefaultValue def
  d -> die $ "avroDecodeReadField: type error: " ++ show d

avroDecodeField :: Closure -> IO AvroSchema.Field
avroDecodeField = \case
  DataC _ _ [name, doc, BoxedVal typ, aliases, BoxedVal order, BoxedVal def] -> (AvroSchema.Field . Util.Text.toText <$> decodeVal name) <*> (map Util.Text.toText <$> decodeVal aliases) <*> (fmap Util.Text.toText <$> decodeVal doc) <*> decodeMaybe avroDecodeOrder order <*> avroDecodeSchema typ <*> decodeMaybe avroDecodeDefaultValue def
  d -> die $ "avroDecodeField: type error: " ++ show d

avroDecodeEnum :: Closure -> IO (AvroSchema.TypeName, [AvroSchema.TypeName], Maybe Data.Text.Text, Vector.Vector Data.Text.Text)
avroDecodeEnum = \case
  DataC _ _ [BoxedVal name, doc, aliases, symbols, _] -> do
    name' <- avroDecodeTypeName name
    aliases' <- traverse avroDecodeTypeName =<< (decodeVal aliases :: IO [Closure])
    doc' <- fmap Util.Text.toText <$> decodeVal doc
    symbols' <- Vector.fromList . map Util.Text.toText <$> decodeVal symbols
    pure (name', aliases', doc', symbols')
  d -> die $ "avroDecodeEnum: type error: " ++ show d

avroDecodeFixed :: Closure -> IO (Avro.TypeName, [Avro.TypeName], Int, Maybe ReadSchema.LogicalTypeFixed)
avroDecodeFixed = \case
  DataC _ _ [BoxedVal name, _, aliases, size, BoxedVal logicalType] -> do
    name' <- avroDecodeTypeName name
    aliases' <- traverse avroDecodeTypeName =<< (decodeVal aliases :: IO [Closure])
    size' <- decodeVal size
    logicalType' <- decodeMaybe avroDecodeLogicalFixed logicalType
    pure (name', aliases', size', logicalType')
  d -> die $ "avroDecodeFixed: type error: " ++ show d

avroDecodeLogicalFixed :: Closure -> IO ReadSchema.LogicalTypeFixed
avroDecodeLogicalFixed = \case
  Enum _ t | TT.avroLogicalFixedDurationTag == t -> pure ReadSchema.Duration
  Data1 _ t (BoxedVal v) | TT.avroLogicalFixedDecimalTag == t -> ReadSchema.DecimalF <$> avroDecodeDecimal v
  d -> die $ "avroDecodeLogicalFixed: type error: " ++ show d

avroDecodeReadLong :: Closure -> IO ReadSchema.ReadLong
avroDecodeReadLong = \case
  Enum _ t
    | TT.avroReadLongInt32Tag == t -> pure ReadSchema.LongFromInt
    | TT.avroReadLongTag == t -> pure ReadSchema.ReadLong
  d -> die $ "avroDecodeReadLong: type error: " ++ show d

avroDecodeLogicalInt :: Closure -> IO ReadSchema.LogicalTypeInt
avroDecodeLogicalInt = \case
  Enum _ t
    | TT.avroLogicalIntDateTag == t -> pure ReadSchema.Date
    | TT.avroLogicalIntTimeTag == t -> pure ReadSchema.TimeMillis
  Data1 _ t (BoxedVal v) | TT.avroLogicalIntDecimalTag == t -> ReadSchema.DecimalI <$> avroDecodeDecimal v
  d -> die $ "avroDecodeLogicalInt: type error: " ++ show d

avroDecodeLogicalBytes :: Closure -> IO ReadSchema.LogicalTypeBytes
avroDecodeLogicalBytes = \case
  Data1 _ t (BoxedVal v) | TT.avroLogicalBytesDecimalTag == t -> ReadSchema.DecimalB <$> avroDecodeDecimal v
  d -> die $ "avroDecodeLogicalBytes: type error: " ++ show d

avroDecodeLogicalString :: Closure -> IO ReadSchema.LogicalTypeString
avroDecodeLogicalString = \case
  Enum _ t
    | TT.avroLogicalStringUuidTag == t -> pure ReadSchema.UUID
  d -> die $ "avroDecodeLogicalString: type error: " ++ show d

avroDecodeTypeName :: Closure -> IO Avro.TypeName
avroDecodeTypeName = \case
  Data2 _ _ name namespace -> (Avro.TN . Util.Text.toText <$> decodeVal name) <*> (map Util.Text.toText <$> decodeVal namespace)
  d -> die $ "avroDecodeTypeName: type error: " ++ show d

avroDecodeReadFloat :: Closure -> IO ReadSchema.ReadFloat
avroDecodeReadFloat = \case
  Enum _ t
    | TT.avroReadFloatFromInt32Tag == t -> pure ReadSchema.FloatFromInt
    | TT.avroReadFloatFromInt64Tag == t -> pure ReadSchema.FloatFromLong
    | TT.avroReadFloatTag == t -> pure ReadSchema.ReadFloat
  d -> die $ "avroDecodeReadFloat: type error: " ++ show d

avroDecodeReadDouble :: Closure -> IO ReadSchema.ReadDouble
avroDecodeReadDouble = \case
  Enum _ t
    | TT.avroReadDoubleFromInt32Tag == t -> pure ReadSchema.DoubleFromInt
    | TT.avroReadDoubleFromInt64Tag == t -> pure ReadSchema.DoubleFromLong
    | TT.avroReadDoubleFromFloatTag == t -> pure ReadSchema.DoubleFromFloat
    | TT.avroReadDoubleTag == t -> pure ReadSchema.ReadDouble
  d -> die $ "avroDecodeReadDouble: type error: " ++ show d

avroDecodeDecimal :: Closure -> IO ReadSchema.Decimal
avroDecodeDecimal = \case
  Data2 _ _ precision scale -> ReadSchema.Decimal <$> fmap fromIntegral (decodeVal precision :: IO Int) <*> fmap fromIntegral (decodeVal scale :: IO Int)
  d -> die $ "avroDecodeDecimal: type error: " ++ show d

avroDecodeFieldStatus :: Closure -> IO ReadSchema.FieldStatus
avroDecodeFieldStatus = \case
  Enum _ t
    | TT.avroFieldStatusIgnoredTag == t -> pure ReadSchema.Ignored
  Data1 _ _ v -> ReadSchema.AsIs . fromIntegral <$> (decodeVal v :: IO Word64)
  Data2 _ _ v1 (BoxedVal v2) -> ReadSchema.Defaulted <$> decodeVal v1 <*> avroDecodeDefaultValue v2
  d -> die $ "avroDecodeFieldStatus: type error: " ++ show d

avroDecodeOrder :: Closure -> IO Avro.Order
avroDecodeOrder = \case
  Enum _ t
    | TT.avroOrderAscendingTag == t -> pure Avro.Ascending
    | TT.avroOrderDescendingTag == t -> pure Avro.Descending
    | TT.avroOrderIgnoreTag == t -> pure Avro.Ignore
  d -> die $ "avroDecodeOrder: type error: " ++ show d

avroDecodeDefaultValue :: Closure -> IO AvroSchema.DefaultValue
avroDecodeDefaultValue = \case
  Enum _ t
    | TT.avroDefaultValueNullTag == t -> pure AvroSchema.DNull
  Data1 _ t v
    | TT.avroDefaultValueBooleanTag == t -> AvroSchema.DBoolean <$> decodeVal v
    | TT.avroDefaultValueArrayTag == t -> AvroSchema.DArray . Vector.fromList <$> (traverse avroDecodeDefaultValue =<< decodeVal v)
    | TT.avroDefaultValueMapTag == t -> AvroSchema.DMap . HashMap.fromList <$> (traverse (bimapM (pure . Util.Text.toText) avroDecodeDefaultValue) =<< decodeVal v)
  Data2 _ t (BoxedVal c1) v2
    | TT.avroDefaultValueIntTag == t -> AvroSchema.DInt <$> avroDecodeSchema c1 <*> (fromIntegral <$> (decodeVal v2 :: IO Int))
    | TT.avroDefaultValueLongTag == t -> AvroSchema.DLong <$> avroDecodeSchema c1 <*> (fromIntegral <$> (decodeVal v2 :: IO Int))
    | TT.avroDefaultValueFloatTag == t -> AvroSchema.DFloat <$> avroDecodeSchema c1 <*> (double2Float <$> (decodeVal v2 :: IO Double))
    | TT.avroDefaultValueDoubleTag == t -> AvroSchema.DDouble <$> avroDecodeSchema c1 <*> decodeVal v2
    | TT.avroDefaultValueBytesTag == t -> AvroSchema.DBytes <$> avroDecodeSchema c1 <*> (Bytes.toByteString <$> decodeVal v2)
    | TT.avroDefaultValueStringTag == t -> AvroSchema.DString <$> avroDecodeSchema c1 <*> (Util.Text.toText <$> decodeVal v2)
    | TT.avroDefaultValueRecordTag == t -> AvroSchema.DRecord <$> avroDecodeSchema c1 <*> fmap HashMap.fromList (traverse (bimapM (pure . Util.Text.toText) avroDecodeDefaultValue) =<< decodeVal v2)
    | TT.avroDefaultValueFixedTag == t -> AvroSchema.DFixed <$> avroDecodeSchema c1 <*> (Bytes.toByteString <$> decodeVal v2)
  DataC _ t [schemas, BoxedVal schema, BoxedVal defaultVal] | TT.avroDefaultValueUnionTag == t -> AvroSchema.DUnion <$> fmap Vector.fromList (traverse avroDecodeSchema =<< decodeVal schemas) <*> avroDecodeSchema schema <*> (avroDecodeDefaultValue defaultVal)
  DataC _ t [BoxedVal schema, ix, symbol] | TT.avroDefaultValueEnumTag == t -> AvroSchema.DEnum <$> avroDecodeSchema schema <*> decodeVal ix <*> (Util.Text.toText <$> decodeVal symbol)
  d -> die $ "avroDecodeDefaultValue: type error: " ++ show d

avroEncodeLogicalTypeInt :: ReadSchema.LogicalTypeInt -> Val
avroEncodeLogicalTypeInt = \case
  ReadSchema.Date -> BoxedVal $ Enum Ty.avroLogicalIntRef TT.avroLogicalIntDateTag
  ReadSchema.TimeMillis -> BoxedVal $ Enum Ty.avroLogicalIntRef TT.avroLogicalIntTimeTag
  ReadSchema.DecimalI (ReadSchema.Decimal precision scale) -> BoxedVal $ Data1 Ty.avroLogicalIntRef TT.avroLogicalIntDecimalTag (BoxedVal $ Data2 Ty.avroDecimalRef TT.avroDecimalTag (encodeVal (fromIntegral precision :: Int)) (encodeVal (fromIntegral scale :: Int)))

avroEncodeReadLong :: ReadSchema.ReadLong -> Val
avroEncodeReadLong = \case
  ReadSchema.LongFromInt -> BoxedVal $ Enum Ty.avroReadLongRef TT.avroReadLongInt32Tag
  ReadSchema.ReadLong -> BoxedVal $ Enum Ty.avroReadLongRef TT.avroReadLongTag

avroEncodeReadFloat :: ReadSchema.ReadFloat -> Val
avroEncodeReadFloat = \case
  ReadSchema.FloatFromInt -> BoxedVal $ Enum Ty.avroReadFloatRef TT.avroReadFloatFromInt32Tag
  ReadSchema.FloatFromLong -> BoxedVal $ Enum Ty.avroReadFloatRef TT.avroReadFloatFromInt64Tag
  ReadSchema.ReadFloat -> BoxedVal $ Enum Ty.avroReadFloatRef TT.avroReadFloatTag

avroEncodeReadDouble :: ReadSchema.ReadDouble -> Val
avroEncodeReadDouble = \case
  ReadSchema.DoubleFromInt -> BoxedVal $ Enum Ty.avroReadDoubleRef TT.avroReadDoubleFromInt32Tag
  ReadSchema.DoubleFromLong -> BoxedVal $ Enum Ty.avroReadDoubleRef TT.avroReadDoubleFromInt64Tag
  ReadSchema.DoubleFromFloat -> BoxedVal $ Enum Ty.avroReadDoubleRef TT.avroReadDoubleFromFloatTag
  ReadSchema.ReadDouble -> BoxedVal $ Enum Ty.avroReadDoubleRef TT.avroReadDoubleTag

avroEncodeReadSchema :: Avro.ReadSchema -> Val
avroEncodeReadSchema = \case
  ReadSchema.Null -> BoxedVal $ Enum Ty.avroReadSchemaRef TT.avroReadSchemaNullTag
  ReadSchema.Boolean -> BoxedVal $ Enum Ty.avroReadSchemaRef TT.avroReadSchemaBooleanTag
  ReadSchema.Int logicalType -> BoxedVal $ Data1 Ty.avroReadSchemaRef TT.avroReadSchemaIntTag (encodeVal (fmap avroEncodeLogicalTypeInt logicalType))
  ReadSchema.Long readLong logicalType -> BoxedVal $ Data2 Ty.avroReadSchemaRef TT.avroReadSchemaLongTag (avroEncodeReadLong readLong) (encodeVal (fmap avroEncodeLogicalTypeLong logicalType))
  ReadSchema.Float readFloat -> BoxedVal $ Data1 Ty.avroReadSchemaRef TT.avroReadSchemaFloatTag (avroEncodeReadFloat readFloat)
  ReadSchema.Double readDouble -> BoxedVal $ Data1 Ty.avroReadSchemaRef TT.avroReadSchemaDoubleTag (avroEncodeReadDouble readDouble)
  ReadSchema.Bytes logicalType -> BoxedVal $ Data1 Ty.avroReadSchemaRef TT.avroReadSchemaBytesTag (encodeVal (fmap avroEncodeLogicalTypeBytes logicalType))
  ReadSchema.String logicalType -> BoxedVal $ Data1 Ty.avroReadSchemaRef TT.avroReadSchemaStringTag (encodeVal (fmap avroEncodeLogicalTypeString logicalType))
  ReadSchema.Array readSchema -> BoxedVal $ Data2 Ty.avroReadSchemaRef TT.avroReadSchemaArrayTag (avroEncodeReadSchema readSchema) (encodeVal (fmap avroEncodeDefaultValue []))
  ReadSchema.Map readSchema -> BoxedVal $ Data2 Ty.avroReadSchemaRef TT.avroReadSchemaMapTag (avroEncodeReadSchema readSchema) (BoxedVal $ Enum Ty.mapRef TT.mapTipTag)
  ReadSchema.NamedType tn -> BoxedVal $ Data1 Ty.avroReadSchemaRef TT.avroReadSchemaNamedTypeTag (avroEncodeTypeName tn)
  ReadSchema.Record name aliases doc fields -> BoxedVal $ Data1 Ty.avroReadSchemaRef TT.avroReadSchemaRecordTag (BoxedVal $ DataG Ty.avroReadRecordRef TT.avroReadRecordTag (boxedSeg [getBoxedVal (avroEncodeTypeName name), getBoxedVal (encodeVal (map avroEncodeTypeName aliases)), getBoxedVal (encodeVal (Util.Text.fromText <$> doc)), getBoxedVal (encodeVal (map avroEncodeReadField fields))]))
  ReadSchema.Enum name aliases doc symbols -> BoxedVal $ Data1 Ty.avroReadSchemaRef TT.avroReadSchemaEnumTag (BoxedVal $ DataG Ty.avroEnumRef TT.avroEnumTag (boxedSeg [getBoxedVal (avroEncodeTypeName name), getBoxedVal (encodeVal (Util.Text.fromText <$> doc)), getBoxedVal (encodeVal (map avroEncodeTypeName aliases)), getBoxedVal (encodeVal (map Util.Text.fromText (Vector.toList symbols))), getBoxedVal (encodeVal (Nothing :: Maybe Text))]))
  ReadSchema.Union options -> BoxedVal $ Data1 Ty.avroReadSchemaRef TT.avroReadSchemaUnionTag (encodeVal (map (second avroEncodeReadSchema) (Vector.toList options)))
  ReadSchema.Fixed name aliases size logicalType -> BoxedVal $ Data1 Ty.avroReadSchemaRef TT.avroReadSchemaFixedTag (BoxedVal $ DataG Ty.avroFixedRef TT.avroFixedTag (boxedSeg [getBoxedVal (avroEncodeTypeName name), getBoxedVal (encodeVal (map avroEncodeTypeName aliases)), getBoxedVal (encodeVal (Nothing :: Maybe Text)), getBoxedVal (encodeVal size), getBoxedVal (encodeVal (fmap avroEncodeLogicalTypeFixed logicalType))]))
  ReadSchema.FreeUnion pos ty -> BoxedVal $ Data2 Ty.avroReadSchemaRef TT.avroReadSchemaFreeUnionTag (encodeVal pos) (avroEncodeReadSchema ty)

avroEncodeTypeName :: Avro.TypeName -> Val
avroEncodeTypeName (Avro.TN baseName namespace) = BoxedVal $ Data2 Ty.avroTypeNameRef TT.avroTypeNameTag (encodeVal (Util.Text.fromText baseName)) (encodeVal (map Util.Text.fromText namespace))

avroEncodeReadField :: ReadSchema.ReadField -> Val
avroEncodeReadField = \case
  ReadSchema.ReadField name aliases doc order status typ def -> BoxedVal $ DataG Ty.avroReadFieldRef TT.avroReadFieldTag (boxedSeg [getBoxedVal (encodeVal (Util.Text.fromText name)), getBoxedVal (encodeVal (map Util.Text.fromText aliases)), getBoxedVal (encodeVal (Util.Text.fromText <$> doc)), getBoxedVal (avroEncodeReadSchema typ), getBoxedVal (avroEncodeFieldStatus status), getBoxedVal (encodeVal (fmap avroEncodeOrder order)), getBoxedVal (encodeVal (fmap avroEncodeDefaultValue def))])

avroEncodeLogicalTypeString :: ReadSchema.LogicalTypeString -> Val
avroEncodeLogicalTypeString = \case
  ReadSchema.UUID -> BoxedVal $ Enum Ty.avroLogicalStringRef TT.avroLogicalStringUuidTag

avroEncodeFieldStatus :: ReadSchema.FieldStatus -> Val
avroEncodeFieldStatus = \case
  ReadSchema.AsIs n -> BoxedVal $ Data1 Ty.avroFieldStatusRef TT.avroFieldStatusAsIsTag (encodeVal n)
  ReadSchema.Ignored -> BoxedVal $ Enum Ty.avroFieldStatusRef TT.avroFieldStatusIgnoredTag
  ReadSchema.Defaulted n def -> BoxedVal $ Data2 Ty.avroFieldStatusRef TT.avroFieldStatusDefaultedTag (encodeVal n) (avroEncodeDefaultValue def)

avroEncodeOrder :: Avro.Order -> Val
avroEncodeOrder = \case
  Avro.Ascending -> BoxedVal $ Enum Ty.avroOrderRef TT.avroOrderAscendingTag
  Avro.Descending -> BoxedVal $ Enum Ty.avroOrderRef TT.avroOrderDescendingTag
  Avro.Ignore -> BoxedVal $ Enum Ty.avroOrderRef TT.avroOrderIgnoreTag

avroEncodeLogicalTypeLong :: ReadSchema.LogicalTypeLong -> Val
avroEncodeLogicalTypeLong = \case
  ReadSchema.TimeMicros -> BoxedVal $ Enum Ty.avroLogicalLongRef TT.avroLogicalLongTimeMicrosTag
  ReadSchema.TimestampMillis -> BoxedVal $ Enum Ty.avroLogicalLongRef TT.avroLogicalLongTimestampMillisTag
  ReadSchema.TimestampMicros -> BoxedVal $ Enum Ty.avroLogicalLongRef TT.avroLogicalLongTimestampMicrosTag
  ReadSchema.LocalTimestampMillis -> BoxedVal $ Enum Ty.avroLogicalLongRef TT.avroLogicalLongLocalTimestampMillisTag
  ReadSchema.LocalTimestampMicros -> BoxedVal $ Enum Ty.avroLogicalLongRef TT.avroLogicalLongLocalTimestampMicrosTag
  ReadSchema.DecimalL (ReadSchema.Decimal precision scale) -> BoxedVal $ Data1 Ty.avroLogicalLongRef TT.avroLogicalLongDecimalTag (BoxedVal $ Data2 Ty.avroDecimalRef TT.avroDecimalTag (encodeVal (fromIntegral precision :: Int)) (encodeVal (fromIntegral scale :: Int)))

avroEncodeLogicalTypeBytes :: ReadSchema.LogicalTypeBytes -> Val
avroEncodeLogicalTypeBytes = \case
  ReadSchema.DecimalB (ReadSchema.Decimal precision scale) -> BoxedVal $ Data1 Ty.avroLogicalBytesRef TT.avroLogicalBytesDecimalTag (BoxedVal $ Data2 Ty.avroDecimalRef TT.avroDecimalTag (encodeVal (fromIntegral precision :: Int)) (encodeVal (fromIntegral scale :: Int)))

avroEncodeLogicalTypeFixed :: ReadSchema.LogicalTypeFixed -> Val
avroEncodeLogicalTypeFixed = \case
  ReadSchema.DecimalF (ReadSchema.Decimal precision scale) -> BoxedVal $ Data1 Ty.avroLogicalFixedRef TT.avroLogicalFixedDecimalTag (BoxedVal $ Data2 Ty.avroDecimalRef TT.avroDecimalTag (encodeVal (fromIntegral precision :: Int)) (encodeVal (fromIntegral scale :: Int)))
  ReadSchema.Duration -> BoxedVal $ Enum Ty.avroLogicalFixedRef TT.avroLogicalFixedDurationTag

avroEncodeValue :: FromAvro.Value -> Val
avroEncodeValue = \case
  FromAvro.Null -> avroNull
  FromAvro.Boolean b -> if b then avroTrue else avroFalse
  FromAvro.Int schema n ->
    BoxedVal $
      Data2 Ty.avroRef TT.avroIntTag (avroEncodeReadSchema schema) (encodeVal (fromIntegral n :: Int))
  FromAvro.Long schema n -> BoxedVal $ Data2 Ty.avroRef TT.avroLongTag (avroEncodeReadSchema schema) (encodeVal (fromIntegral n :: Int))
  FromAvro.Float schema n ->
    BoxedVal $ Data2 Ty.avroRef TT.avroFloatTag (avroEncodeReadSchema schema) (encodeVal (float2Double n))
  FromAvro.Double schema n ->
    BoxedVal $ Data2 Ty.avroRef TT.avroDoubleTag (avroEncodeReadSchema schema) (encodeVal n)
  FromAvro.Bytes schema bs ->
    BoxedVal $ Data2 Ty.avroRef TT.avroBytesTag (avroEncodeReadSchema schema) (encodeVal (Bytes.fromByteString bs))
  FromAvro.String schema s ->
    BoxedVal $ Data2 Ty.avroRef TT.avroStringTag (avroEncodeReadSchema schema) (encodeVal (Util.Text.fromText s))
  FromAvro.Array xs ->
    BoxedVal $ Data1 Ty.avroRef TT.avroArrayTag (encodeVal (map avroEncodeValue (Vector.toList xs)))
  FromAvro.Map xs ->
    let m = HashMap.toList xs
        encoded = map (second avroEncodeValue) m
     in BoxedVal $ Data1 Ty.avroRef TT.avroMapTag $ BoxedVal $ Foreign (Wrap Ty.hmapRef (Map.fromList encoded))
  FromAvro.Record schema fields ->
    BoxedVal $ Data2 Ty.avroRef TT.avroRecordTag (avroEncodeReadSchema schema) (encodeVal (map avroEncodeValue (Vector.toList fields)))
  FromAvro.Union schema tag v ->
    BoxedVal $ DataG Ty.avroRef TT.avroUnionTag (segFromList [avroEncodeReadSchema schema, encodeVal tag, avroEncodeValue v])
  FromAvro.Fixed schema bytes ->
    BoxedVal $ Data2 Ty.avroRef TT.avroFixedTag (avroEncodeReadSchema schema) (encodeVal (Bytes.fromByteString bytes))
  FromAvro.Enum schema ix v ->
    BoxedVal $ DataG Ty.avroRef TT.avroEnumTag (segFromList [avroEncodeReadSchema schema, encodeVal ix, encodeVal (Util.Text.fromText v)])

avroEncodeDefaultValue :: AvroSchema.DefaultValue -> Val
avroEncodeDefaultValue = \case
  AvroSchema.DNull -> BoxedVal $ Enum Ty.avroDefaultValueRef TT.avroDefaultValueNullTag
  AvroSchema.DBoolean b -> BoxedVal $ Data1 Ty.avroDefaultValueRef TT.avroDefaultValueBooleanTag (encodeVal b)
  AvroSchema.DInt schema n -> BoxedVal $ Data2 Ty.avroDefaultValueRef TT.avroDefaultValueIntTag (avroEncodeSchema schema) (encodeVal (fromIntegral n :: Int))
  AvroSchema.DLong schema n -> BoxedVal $ Data2 Ty.avroDefaultValueRef TT.avroDefaultValueLongTag (avroEncodeSchema schema) (encodeVal (fromIntegral n :: Int))
  AvroSchema.DFloat schema f -> BoxedVal $ Data2 Ty.avroDefaultValueRef TT.avroDefaultValueFloatTag (avroEncodeSchema schema) (encodeVal (float2Double f))
  AvroSchema.DDouble schema d -> BoxedVal $ Data2 Ty.avroDefaultValueRef TT.avroDefaultValueDoubleTag (avroEncodeSchema schema) (encodeVal d)
  AvroSchema.DString schema s -> BoxedVal $ Data2 Ty.avroDefaultValueRef TT.avroDefaultValueStringTag (avroEncodeSchema schema) (encodeVal (Util.Text.fromText s))
  AvroSchema.DBytes schema bs -> BoxedVal $ Data2 Ty.avroDefaultValueRef TT.avroDefaultValueBytesTag (avroEncodeSchema schema) (encodeVal (Bytes.fromByteString bs))
  AvroSchema.DArray xs -> BoxedVal $ Data1 Ty.avroDefaultValueRef TT.avroDefaultValueArrayTag (encodeVal (map avroEncodeDefaultValue (Vector.toList xs)))
  AvroSchema.DMap xs -> BoxedVal $ Data1 Ty.avroDefaultValueRef TT.avroDefaultValueMapTag (encodeVal (map (bimap Util.Text.fromText avroEncodeDefaultValue) (HashMap.toList xs)))
  AvroSchema.DRecord schema fields -> BoxedVal $ Data2 Ty.avroDefaultValueRef TT.avroDefaultValueRecordTag (avroEncodeSchema schema) (encodeVal (map (bimap Util.Text.fromText avroEncodeDefaultValue) (HashMap.toList fields)))
  AvroSchema.DUnion schemas schema v -> BoxedVal $ DataG Ty.avroDefaultValueRef TT.avroDefaultValueUnionTag (segFromList [encodeVal (map avroEncodeSchema (Vector.toList schemas)), avroEncodeSchema schema, avroEncodeDefaultValue v])
  AvroSchema.DFixed schema bytes -> BoxedVal $ Data2 Ty.avroDefaultValueRef TT.avroDefaultValueFixedTag (avroEncodeSchema schema) (encodeVal (Bytes.fromByteString bytes))
  AvroSchema.DEnum schema ix v -> BoxedVal $ DataG Ty.avroDefaultValueRef TT.avroDefaultValueEnumTag (segFromList [avroEncodeSchema schema, encodeVal ix, encodeVal (Util.Text.fromText v)])

avroEncodeSchema :: AvroSchema.Schema -> Val
avroEncodeSchema = \case
  AvroSchema.Null -> BoxedVal $ Enum Ty.avroSchemaRef TT.avroSchemaNullTag
  AvroSchema.Boolean -> BoxedVal $ Enum Ty.avroSchemaRef TT.avroSchemaBooleanTag
  AvroSchema.Int lt -> BoxedVal $ Data1 Ty.avroSchemaRef TT.avroSchemaIntTag (encodeVal (fmap avroEncodeLogicalTypeInt lt))
  AvroSchema.Long lt -> BoxedVal $ Data1 Ty.avroSchemaRef TT.avroSchemaLongTag (encodeVal (fmap avroEncodeLogicalTypeLong lt))
  AvroSchema.Float -> BoxedVal $ Enum Ty.avroSchemaRef TT.avroSchemaFloatTag
  AvroSchema.Double -> BoxedVal $ Enum Ty.avroSchemaRef TT.avroSchemaDoubleTag
  AvroSchema.Bytes lt -> BoxedVal $ Data1 Ty.avroSchemaRef TT.avroSchemaBytesTag (encodeVal (fmap avroEncodeLogicalTypeBytes lt))
  AvroSchema.String lt -> BoxedVal $ Data1 Ty.avroSchemaRef TT.avroSchemaStringTag (encodeVal (fmap avroEncodeLogicalTypeString lt))
  AvroSchema.Array schema -> BoxedVal $ Data2 Ty.avroSchemaRef TT.avroSchemaArrayTag (avroEncodeSchema schema) (encodeVal ([] :: [Val]))
  AvroSchema.Map schema -> BoxedVal $ Data2 Ty.avroSchemaRef TT.avroSchemaMapTag (avroEncodeSchema schema) (BoxedVal $ Enum Ty.mapRef TT.mapTipTag)
  AvroSchema.NamedType tn -> BoxedVal $ Data1 Ty.avroSchemaRef TT.avroSchemaNamedTypeTag (avroEncodeTypeName tn)
  AvroSchema.Record name aliases doc fields -> BoxedVal $ Data1 Ty.avroSchemaRef TT.avroSchemaRecordTag (BoxedVal $ DataG Ty.avroRecordRef TT.avroRecordTypeTag (segFromList [avroEncodeTypeName name, encodeVal (map avroEncodeTypeName aliases), encodeVal (Util.Text.fromText <$> doc), encodeVal (map avroEncodeField fields)]))
  AvroSchema.Enum name aliases doc symbols -> BoxedVal $ Data1 Ty.avroSchemaRef TT.avroSchemaEnumTag (BoxedVal $ DataG Ty.avroEnumRef TT.avroEnumTypeTag (segFromList [avroEncodeTypeName name, encodeVal (Util.Text.fromText <$> doc), encodeVal (map avroEncodeTypeName aliases), encodeVal (map Util.Text.fromText (Vector.toList symbols))]))
  AvroSchema.Union schemas -> BoxedVal $ Data1 Ty.avroSchemaRef TT.avroSchemaUnionTag (encodeVal (map avroEncodeSchema (Vector.toList schemas)))
  AvroSchema.Fixed name aliases size lt -> BoxedVal $ Data1 Ty.avroSchemaRef TT.avroSchemaFixedTag (BoxedVal $ DataG Ty.avroFixedRef TT.avroFixedTypeTag (segFromList [avroEncodeTypeName name, encodeVal (map avroEncodeTypeName aliases), encodeVal (Nothing :: Maybe Text), encodeVal size, encodeVal (fmap avroEncodeLogicalTypeFixed lt)]))

avroEncodeField :: AvroSchema.Field -> Val
avroEncodeField = \case
  AvroSchema.Field name aliases doc order typ def -> BoxedVal $ DataG Ty.avroFieldRef TT.avroFieldTag (segFromList [encodeVal (Util.Text.fromText name), encodeVal (map Util.Text.fromText aliases), encodeVal (Util.Text.fromText <$> doc), avroEncodeSchema typ, encodeVal (fmap avroEncodeOrder order), encodeVal (fmap avroEncodeDefaultValue def)])

avroDecodeBinary :: Closure -> Closure -> Bytes.Bytes -> IO Val
avroDecodeBinary _env readSchema bytes = do
  -- envVal <- decodeVal @[(Closure, Closure)] (BoxedVal env)
  -- envDecoded <- traverse (bimapM avroDecodeTypeName avroDecodeReadSchema) envVal
  readSchemaDecoded <- avroDecodeReadSchema readSchema
  -- let envMap = (HashMap.fromList envDecoded) <> ReadSchema.extractBindings readSchemaDecoded
  -- TODO: Modify the avro library to allow us to call getField directly
  case Get.runGetOrFail (FromAvro.getValue readSchemaDecoded) (L.fromStrict (Bytes.toByteString bytes)) of
    Left (_, _, err) -> pure $ encodeVal @(Either String Val) (Left err)
    Right (_, _, value) -> pure $ encodeVal @(Either String Val) (Right (avroEncodeValue value))

-- A ForeignConvention explains how to encode foreign values as
-- unison types. Depending on the situation, this can take three
-- forms.
--
--   1. Reading/writing directly from/to the stack
--   2. Reading a tuple directly from the stack
--   3. Translating a standalone value
--
-- The first is used when the value in question is the one that is
-- going to be directly on the stack, to allow for slight
-- optimization (e.g. an `Either` only requires reading/writing the
-- boxed portion of the stack). For compound types, though, it's
-- necessary to be able to de/encode a value that was nested inside
-- something else.
--
-- The second is used for multi-argument foreign functions. The
-- default implementation expects a single argument, and reads at
-- that specific index. But, tuples and the unit type can override
-- to read multiple arguments directly from the stack. This works
-- out better than having a separate class with a default
-- ForeignConvention instance, because the latter requires
-- incoherence to work as expected.
--
-- We can give a default implementation of the stack operations in
-- terms of the other coding.
class ForeignConvention a where
  readAtIndex :: Stack -> Int -> IO a
  readsAt :: Stack -> Args -> IO a
  decodeVal :: Val -> IO a

  readAtIndex stk i = peekOff stk i >>= decodeVal

  readsAt stk (VArg1 i) = readAtIndex stk i
  readsAt _ args = readsAtError "one argument" args

  writeBack :: Stack -> a -> IO ()
  encodeVal :: a -> Val

  writeBack stk v = poke stk (encodeVal v)

decodeMaybe :: (Closure -> IO a) -> Closure -> IO (Maybe a)
decodeMaybe f c = decodeVal (BoxedVal c) >>= traverse f

readsAtError :: String -> Args -> IO a
readsAtError expect args = throwIO $ Panic msg Nothing
  where
    msg = "readsAt: expected " ++ expect ++ ", got: " ++ show args

foreignConventionError :: String -> Val -> IO a
foreignConventionError ty v = throwIO $ Panic msg (Just v)
  where
    msg = "mismatched foreign calling convention for `" ++ ty ++ "`"

instance
  ( ForeignConvention a,
    ForeignConvention b
  ) =>
  ForeignConvention (Either a b)
  where
  decodeVal (BoxedVal (Data1 _ t v))
    | t == TT.leftTag = Left <$> decodeVal v
    | otherwise = Right <$> decodeVal v
  decodeVal v = foreignConventionError "Either" v

  encodeVal (Left x) =
    BoxedVal . Data1 Ty.eitherRef TT.leftTag $ encodeVal x
  encodeVal (Right y) =
    BoxedVal . Data1 Ty.eitherRef TT.rightTag $ encodeVal y

  readAtIndex stk i =
    bpeekOff stk i >>= \case
      Data1 _ t v
        | t == TT.leftTag -> Left <$> decodeVal v
        | otherwise -> Right <$> decodeVal v
      c -> foreignConventionError "Either" (BoxedVal c)

  writeBack stk (Left x) =
    bpoke stk . Data1 Ty.eitherRef TT.leftTag $ encodeVal x
  writeBack stk (Right y) =
    bpoke stk . Data1 Ty.eitherRef TT.rightTag $ encodeVal y

instance (ForeignConvention a) => ForeignConvention (Maybe a) where
  decodeVal (BoxedVal (Enum _ _)) = pure Nothing
  decodeVal (BoxedVal (Data1 _ _ v)) = Just <$> decodeVal v
  decodeVal v = foreignConventionError "Maybe" v

  encodeVal Nothing = noneVal
  encodeVal (Just v) = someVal (encodeVal v)

  readAtIndex stk i =
    bpeekOff stk i >>= \case
      Data1 _ _ v -> Just <$> decodeVal v
      Enum _ _ -> pure Nothing
      c -> foreignConventionError "Maybe" (BoxedVal c)

  writeBack stk Nothing = bpoke stk noneClo
  writeBack stk (Just v) = bpoke stk (someClo (encodeVal v))

noneClo :: Closure
noneClo = Enum Ty.optionalRef TT.noneTag

noneVal :: Val
noneVal = BoxedVal noneClo

someClo :: Val -> Closure
someClo v = Data1 Ty.optionalRef TT.someTag v

someVal :: Val -> Val
someVal v = BoxedVal (someClo v)

instance ForeignConvention Int where
  decodeVal (IntVal v) = pure v
  decodeVal (NatVal v) = pure $ fromIntegral v
  decodeVal v = foreignConventionError "Int" v
  encodeVal = IntVal

  readAtIndex stk i = upeekOff stk i
  writeBack stk v = upokeT stk v intTypeTag

-- We don't have a clear mapping from these types to Unison types, most are just mapped to Nats.

instance ForeignConvention Word8 where
  decodeVal (NatVal v) = pure $ fromIntegral v
  decodeVal v = foreignConventionError "Word8" v
  encodeVal w = NatVal $ fromIntegral w

  readAtIndex stk i = fromIntegral <$> peekOffN stk i
  writeBack stk v = pokeN stk $ fromIntegral v

instance ForeignConvention Word16 where
  decodeVal (NatVal v) = pure $ fromIntegral v
  decodeVal v = foreignConventionError "Word16" v
  encodeVal w = NatVal $ fromIntegral w

  readAtIndex stk i = fromIntegral <$> peekOffN stk i
  writeBack stk v = pokeN stk $ fromIntegral v

instance ForeignConvention Word32 where
  decodeVal (NatVal v) = pure $ fromIntegral v
  decodeVal v = foreignConventionError "Word32" v
  encodeVal w = NatVal $ fromIntegral w

  readAtIndex stk i = fromIntegral <$> upeekOff stk i
  writeBack stk v = pokeN stk $ fromIntegral v

instance ForeignConvention Word64 where
  decodeVal (NatVal w) = pure w
  decodeVal v = foreignConventionError "Word64" v
  encodeVal w = NatVal w

  readAtIndex stk i = peekOffN stk i
  writeBack stk w = pokeN stk w

instance ForeignConvention Char where
  decodeVal (CharVal c) = pure c
  decodeVal v = foreignConventionError "Char" v

  encodeVal c = CharVal c

  readAtIndex = peekOffC
  writeBack = pokeC

unitClo :: Closure
unitClo = Enum Ty.unitRef TT.unitTag

unitVal :: Val
unitVal = BoxedVal unitClo

instance ForeignConvention () where
  decodeVal _ = pure ()
  encodeVal _ = unitVal

  readsAt _ ZArgs = pure ()
  readsAt _ as = readsAtError "zero arguments" as

  readAtIndex _ _ = pure ()
  writeBack stk _ = bpoke stk $ unitClo

pattern ConsC :: Val -> Val -> Closure
pattern ConsC x y <- Data2 _ _ x y
  where
    ConsC x y = Data2 Ty.pairRef TT.pairTag x y

pattern ConsV x y = BoxedVal (ConsC x y)

pattern Tup2C :: Val -> Val -> Closure
pattern Tup2C x y <- ConsC x (ConsV y _)
  where
    Tup2C x y = ConsC x (ConsV y unitVal)

pattern Tup2V x y = BoxedVal (Tup2C x y)

decodeTup2 :: (ForeignConvention a, ForeignConvention b) => Closure -> IO (a, b)
decodeTup2 (Tup2C x y) = (,) <$> decodeVal x <*> decodeVal y
decodeTup2 c = foreignConventionError "Pair" (BoxedVal c)

encodeTup2 :: (ForeignConvention a, ForeignConvention b) => (a, b) -> Closure
encodeTup2 (x, y) = Tup2C (encodeVal x) (encodeVal y)

instance
  ( ForeignConvention a,
    ForeignConvention b
  ) =>
  ForeignConvention (a, b)
  where
  decodeVal (BoxedVal v) = decodeTup2 v
  decodeVal v = foreignConventionError "Pair" v
  encodeVal p = BoxedVal $ encodeTup2 p

  readsAt stk (VArg2 i j) =
    (,)
      <$> readAtIndex stk i
      <*> readAtIndex stk j
  readsAt _ as = readsAtError "two arguments" as

  readAtIndex stk i = bpeekOff stk i >>= decodeTup2
  writeBack stk p = bpoke stk $ encodeTup2 p

pattern Tup3C x y z = ConsC x (Tup2V y z)

pattern Tup3V x y z = BoxedVal (Tup3C x y z)

decodeTup3 :: (ForeignConvention a, ForeignConvention b, ForeignConvention c) => Closure -> IO (a, b, c)
decodeTup3 (Tup3C x y z) =
  (,,) <$> decodeVal x <*> decodeVal y <*> decodeVal z
decodeTup3 c = foreignConventionError "Triple" (BoxedVal c)

encodeTup3 :: (ForeignConvention a, ForeignConvention b, ForeignConvention c) => (a, b, c) -> Closure
encodeTup3 (x, y, z) = Tup3C (encodeVal x) (encodeVal y) (encodeVal z)

instance
  ( ForeignConvention a,
    ForeignConvention b,
    ForeignConvention c
  ) =>
  ForeignConvention (a, b, c)
  where
  decodeVal (BoxedVal v) = decodeTup3 v
  decodeVal v = foreignConventionError "Triple" v
  encodeVal p = BoxedVal $ encodeTup3 p

  readsAt stk (VArgN v) =
    (,,)
      <$> readAtIndex stk (PA.indexPrimArray v 0)
      <*> readAtIndex stk (PA.indexPrimArray v 1)
      <*> readAtIndex stk (PA.indexPrimArray v 2)
  readsAt _ as = readsAtError "three arguments" as

  readAtIndex stk i = bpeekOff stk i >>= decodeTup3
  writeBack stk p = bpoke stk $ encodeTup3 p

pattern Tup4C w x y z = ConsC w (Tup3V x y z)

pattern Tup4V w x y z = BoxedVal (Tup4C w x y z)

decodeTup4 :: (ForeignConvention a, ForeignConvention b, ForeignConvention c, ForeignConvention d) => Closure -> IO (a, b, c, d)
decodeTup4 (Tup4C w x y z) =
  (,,,) <$> decodeVal w <*> decodeVal x <*> decodeVal y <*> decodeVal z
decodeTup4 c = foreignConventionError "Quadruple" (BoxedVal c)

encodeTup4 :: (ForeignConvention a, ForeignConvention b, ForeignConvention c, ForeignConvention d) => (a, b, c, d) -> Closure
encodeTup4 (w, x, y, z) =
  Tup4C (encodeVal w) (encodeVal x) (encodeVal y) (encodeVal z)

instance
  ( ForeignConvention a,
    ForeignConvention b,
    ForeignConvention c,
    ForeignConvention d
  ) =>
  ForeignConvention (a, b, c, d)
  where
  decodeVal (BoxedVal v) = decodeTup4 v
  decodeVal v = foreignConventionError "Quadruple" v

  encodeVal p = BoxedVal $ encodeTup4 p

  readsAt stk (VArgN v) =
    (,,,)
      <$> readAtIndex stk (PA.indexPrimArray v 0)
      <*> readAtIndex stk (PA.indexPrimArray v 1)
      <*> readAtIndex stk (PA.indexPrimArray v 2)
      <*> readAtIndex stk (PA.indexPrimArray v 3)
  readsAt _ as = readsAtError "four arguments" as

  readAtIndex stk i = bpeekOff stk i >>= decodeTup4
  writeBack stk p = bpoke stk $ encodeTup4 p

pattern Tup5C v w x y z = ConsC v (Tup4V w x y z)

decodeTup5 :: (ForeignConvention a, ForeignConvention b, ForeignConvention c, ForeignConvention d, ForeignConvention e) => Closure -> IO (a, b, c, d, e)
decodeTup5 (Tup5C v w x y z) =
  (,,,,) <$> decodeVal v <*> decodeVal w <*> decodeVal x <*> decodeVal y <*> decodeVal z
decodeTup5 c = foreignConventionError "Quintuple" (BoxedVal c)

encodeTup5 :: (ForeignConvention a, ForeignConvention b, ForeignConvention c, ForeignConvention d, ForeignConvention e) => (a, b, c, d, e) -> Closure
encodeTup5 (v, w, x, y, z) =
  Tup5C (encodeVal v) (encodeVal w) (encodeVal x) (encodeVal y) (encodeVal z)

instance
  ( ForeignConvention a,
    ForeignConvention b,
    ForeignConvention c,
    ForeignConvention d,
    ForeignConvention e
  ) =>
  ForeignConvention (a, b, c, d, e)
  where
  decodeVal (BoxedVal c) = decodeTup5 c
  decodeVal v = foreignConventionError "Quintuple" v

  encodeVal = BoxedVal . encodeTup5

  readsAt stk (VArgN v) =
    (,,,,)
      <$> readAtIndex stk (PA.indexPrimArray v 0)
      <*> readAtIndex stk (PA.indexPrimArray v 1)
      <*> readAtIndex stk (PA.indexPrimArray v 2)
      <*> readAtIndex stk (PA.indexPrimArray v 3)
      <*> readAtIndex stk (PA.indexPrimArray v 4)
  readsAt _ as = readsAtError "five arguments" as

  readAtIndex stk i = bpeekOff stk i >>= decodeTup5
  writeBack stk p = bpoke stk $ encodeTup5 p

decodeFailure :: (ForeignConvention a) => Closure -> IO (F.Failure a)
decodeFailure (DataG _ _ (_, args)) =
  F.Failure
    <$> decodeTypeLink (PA.indexArray args 0)
    <*> decodeText (PA.indexArray args 1)
    <*> decodeAny (PA.indexArray args 2)
decodeFailure c = foreignConventionError "Failure" (BoxedVal c)

encodeFailure :: (ForeignConvention a) => F.Failure a -> Closure
encodeFailure (F.Failure r msg v) = DataG Ty.failureRef TT.failureTag payload
  where
    payload = boxedSeg [encodeTypeLink r, encodeText msg, encodeAny v]

boxedSeg :: [Closure] -> Seg
boxedSeg cs = (useg (0 <$ cs), bseg cs)

decodeTypeLink :: Closure -> IO Reference
decodeTypeLink = marshalUnwrapForeignIO

encodeTypeLink :: Reference -> Closure
encodeTypeLink rf = Foreign (Wrap typeLinkRef rf)

encodeAny :: (ForeignConvention a) => a -> Closure
encodeAny v = Data1 anyRef TT.anyTag (encodeVal v)

decodeAny :: (ForeignConvention a) => Closure -> IO a
decodeAny (Data1 _ _ v) = decodeVal v
decodeAny c = foreignConventionError "Any" (BoxedVal c)

decodeText :: Closure -> IO Text
decodeText = marshalUnwrapForeignIO

encodeText :: Text -> Closure
encodeText tx = Foreign (Wrap textRef tx)

instance (ForeignConvention a) => ForeignConvention (F.Failure a) where
  decodeVal (BoxedVal v) = decodeFailure v
  decodeVal v = foreignConventionError "Failure" v
  encodeVal v = BoxedVal $ encodeFailure v

  readAtIndex stk i = bpeekOff stk i >>= decodeFailure
  writeBack stk f = bpoke stk $ encodeFailure f

decodeForeignClo :: String -> Closure -> IO a
decodeForeignClo _ (Foreign x) = pure $ unwrapForeign x
decodeForeignClo ty c = foreignConventionError ty (BoxedVal c)

encodeForeignClo :: Reference -> a -> Closure
encodeForeignClo r = Foreign . Wrap r

decodeBuiltin :: forall a. (BuiltinForeign a) => Val -> IO a
decodeBuiltin v
  | BoxedVal c <- v = decodeForeignClo ty c
  | otherwise = foreignConventionError ty v
  where
    Tagged ty = foreignName :: Tagged a String

encodeBuiltin :: forall a. (BuiltinForeign a) => a -> Val
encodeBuiltin = BoxedVal . encodeForeignClo r
  where
    Tagged r = foreignRef :: Tagged a Reference

readBuiltinAt :: forall a. (BuiltinForeign a) => Stack -> Int -> IO a
readBuiltinAt stk i = bpeekOff stk i >>= decodeForeignClo ty
  where
    Tagged ty = foreignName :: Tagged a String

writeBuiltin :: forall a. (BuiltinForeign a) => Stack -> a -> IO ()
writeBuiltin stk = bpoke stk . encodeForeignClo r
  where
    Tagged r = foreignRef :: Tagged a Reference

decodeAsBuiltin :: (BuiltinForeign t) => (t -> a) -> Val -> IO a
decodeAsBuiltin k = fmap k . decodeBuiltin

encodeAsBuiltin :: (BuiltinForeign t) => (a -> t) -> a -> Val
encodeAsBuiltin k = encodeBuiltin . k

readAsBuiltin ::
  (BuiltinForeign t) => (t -> a) -> Stack -> Int -> IO a
readAsBuiltin k stk i = k <$> readBuiltinAt stk i

writeAsBuiltin :: (BuiltinForeign t) => (a -> t) -> Stack -> a -> IO ()
writeAsBuiltin k stk = writeBuiltin stk . k

instance ForeignConvention POSIXTime where
  decodeVal (IntVal i) = pure (fromIntegral i)
  decodeVal v = foreignConventionError "POSIXTime" v
  encodeVal pt = IntVal (round pt)
  readAtIndex stk i = fromIntegral <$> peekOffI stk i
  writeBack stk pt = pokeI stk (round pt)

decodeBufferMode :: Closure -> IO BufferMode
decodeBufferMode (Enum _ t)
  | t == TT.noBufTag = pure NoBuffering
  | t == TT.lineBufTag = pure LineBuffering
  | t == TT.blockBufTag = pure $ BlockBuffering Nothing
decodeBufferMode (Data1 _ t (NatVal i))
  | t == TT.sizedBlockBufTag = pure . BlockBuffering $ Just (fromIntegral i)
decodeBufferMode c = foreignConventionError "BufferMode" (BoxedVal c)

encodeBufferMode :: BufferMode -> Closure
encodeBufferMode NoBuffering = no'buf
encodeBufferMode LineBuffering = line'buf
encodeBufferMode (BlockBuffering Nothing) = block'buf
encodeBufferMode (BlockBuffering (Just n)) =
  Data1 Ty.bufferModeRef TT.sizedBlockBufTag . NatVal $ fromIntegral n

no'buf, line'buf, block'buf :: Closure
no'buf = Enum Ty.bufferModeRef TT.noBufTag
line'buf = Enum Ty.bufferModeRef TT.lineBufTag
block'buf = Enum Ty.bufferModeRef TT.blockBufTag

instance ForeignConvention BufferMode where
  decodeVal (BoxedVal c) = decodeBufferMode c
  decodeVal v = foreignConventionError "BufferMode" v

  encodeVal = BoxedVal . encodeBufferMode

  readAtIndex stk i = bpeekOff stk i >>= decodeBufferMode
  writeBack stk bm = bpoke stk (encodeBufferMode bm)

decodeIOMode :: Closure -> IO IOMode
decodeIOMode (Enum _ t)
  | t == TT.readModeTag = pure ReadMode
  | t == TT.writeModeTag = pure WriteMode
  | t == TT.appendModeTag = pure AppendMode
  | t == TT.readWriteModeTag = pure ReadWriteMode
decodeIOMode c = foreignConventionError "IOMode" (BoxedVal c)

encodeIOMode :: IOMode -> Closure
encodeIOMode ReadMode = read'mode
encodeIOMode WriteMode = write'mode
encodeIOMode AppendMode = append'mode
encodeIOMode ReadWriteMode = read'write'mode

read'mode, write'mode, append'mode, read'write'mode :: Closure
read'mode = Enum Ty.bufferModeRef TT.readModeTag
write'mode = Enum Ty.bufferModeRef TT.writeModeTag
append'mode = Enum Ty.bufferModeRef TT.appendModeTag
read'write'mode = Enum Ty.bufferModeRef TT.readWriteModeTag

instance ForeignConvention IOMode where
  decodeVal (BoxedVal c) = decodeIOMode c
  decodeVal v = foreignConventionError "IOMode" v

  encodeVal = BoxedVal . encodeIOMode

  readAtIndex stk i = bpeekOff stk i >>= decodeIOMode
  writeBack stk im = bpoke stk (encodeIOMode im)

decodeSeekMode :: Closure -> IO SeekMode
decodeSeekMode (Enum _ t)
  | t == TT.seekAbsoluteTag = pure AbsoluteSeek
  | t == TT.seekRelativeTag = pure RelativeSeek
  | t == TT.seekEndTag = pure SeekFromEnd
decodeSeekMode v = foreignConventionError "SeekMode" (BoxedVal v)

encodeSeekMode :: SeekMode -> Closure
encodeSeekMode AbsoluteSeek = absolute'seek
encodeSeekMode RelativeSeek = relative'seek
encodeSeekMode SeekFromEnd = seek'from'end

absolute'seek, relative'seek, seek'from'end :: Closure
absolute'seek = Enum Ty.seekModeRef TT.seekAbsoluteTag
relative'seek = Enum Ty.seekModeRef TT.seekRelativeTag
seek'from'end = Enum Ty.seekModeRef TT.seekEndTag

instance ForeignConvention SeekMode where
  decodeVal (BoxedVal c) = decodeSeekMode c
  decodeVal v = foreignConventionError "SeekMode" v

  encodeVal = BoxedVal . encodeSeekMode

  readAtIndex stk i = bpeekOff stk i >>= decodeSeekMode
  writeBack stk sm = bpoke stk (encodeSeekMode sm)

data StdHnd = StdIn | StdOut | StdErr

decodeStdHnd :: Closure -> IO StdHnd
decodeStdHnd (Enum _ t)
  | t == TT.stdInTag = pure StdIn
  | t == TT.stdOutTag = pure StdOut
  | t == TT.stdErrTag = pure StdErr
decodeStdHnd c = foreignConventionError "StdHandle" (BoxedVal c)

encodeStdHnd :: StdHnd -> Closure
encodeStdHnd StdIn = std'in
encodeStdHnd StdOut = std'out
encodeStdHnd StdErr = std'err

std'in, std'out, std'err :: Closure
std'in = Enum Ty.stdHandleRef TT.stdInTag
std'out = Enum Ty.stdHandleRef TT.stdOutTag
std'err = Enum Ty.stdHandleRef TT.stdErrTag

instance ForeignConvention StdHnd where
  decodeVal (BoxedVal c) = decodeStdHnd c
  decodeVal v = foreignConventionError "StdHandle" v

  encodeVal = BoxedVal . encodeStdHnd

  readAtIndex stk i = bpeekOff stk i >>= decodeStdHnd
  writeBack stk = bpoke stk . encodeStdHnd

-- In reality this fixes the type to be 'RClosure', but allows us to defer
-- the typechecker a bit and avoid a bunch of annoying type annotations.
-- instance {-# OVERLAPPING #-} ForeignConvention [Val] where
--   decodeVal = decode
--   readForeign (i : args) stk =
--     (args,) . toList <$> peekOffS stk i
--   readForeign _ _ = foreignCCError "[Val]"
--   writeForeign stk l = do
--     stk <- bump stk
--     stk <$ pokeS stk (Sq.fromList l)

-- In reality this fixes the type to be 'RClosure', but allows us to defer
-- the typechecker a bit and avoid a bunch of annoying type annotations.
-- instance {-# OVERLAPPING #-} ForeignConvention [Closure] where
--   readForeign (i : args) stk =
--     (args,) . fmap getBoxedVal . toList <$> peekOffS stk i
--   readForeign _ _ = foreignCCError "[Closure]"
--   writeForeign stk l = do
--     stk <- bump stk
--     stk <$ pokeS stk (Sq.fromList . fmap BoxedVal $ l)
--
-- instance ForeignConvention [Foreign] where
--   readForeign = readForeignAs (fmap marshalToForeign)
--   writeForeign = writeForeignAs (fmap Foreign)
--

instance {-# OVERLAPPING #-} ForeignConvention String where
  decodeVal = decodeAsBuiltin unpack
  encodeVal = encodeAsBuiltin pack

  readAtIndex = readAsBuiltin unpack
  writeBack = writeAsBuiltin pack

instance ForeignConvention Bool where
  decodeVal (BoolVal b) = pure b
  decodeVal v = foreignConventionError "Bool" v

  encodeVal = BoolVal

  readAtIndex = peekOffBool
  writeBack = pokeBool

instance ForeignConvention TL.Text where
  decodeVal = decodeAsBuiltin toLazyText
  encodeVal = encodeAsBuiltin fromLazyText

  readAtIndex = readAsBuiltin toLazyText
  writeBack = writeAsBuiltin fromLazyText

instance ForeignConvention Double where
  decodeVal (DoubleVal d) = pure d
  decodeVal v = foreignConventionError "Double" v

  encodeVal = DoubleVal

  readAtIndex = peekOffD
  writeBack = pokeD

instance ForeignConvention Val where
  decodeVal = pure
  encodeVal = id

  readAtIndex = peekOff
  writeBack = poke

instance ForeignConvention Closure where
  decodeVal (BoxedVal c) = pure c
  decodeVal v = foreignConventionError "Closure" v

  encodeVal = BoxedVal

  readAtIndex = bpeekOff
  writeBack = bpoke

instance ForeignConvention Foreign where
  decodeVal (BoxedVal (Foreign f)) = pure f
  decodeVal v = foreignConventionError "Foreign" v
  encodeVal f = BoxedVal (Foreign f)

  readAtIndex stk i =
    bpeekOff stk i >>= \case
      Foreign f -> pure f
      c -> foreignConventionError "Foreign" (BoxedVal c)
  writeBack stk f = bpoke stk (Foreign f)

instance ForeignConvention (Seq Val) where
  decodeVal (BoxedVal (Foreign f)) =
    pure $ unwrapForeign @(Seq Val) f
  decodeVal v = foreignConventionError "Seq" v

  encodeVal = BoxedVal . Foreign . Wrap listRef

  readAtIndex = peekOffS

  writeBack = pokeS

instance (ForeignConvention a) => ForeignConvention [a] where
  decodeVal (BoxedVal (Foreign f))
    | (sq :: Sq.Seq Val) <- unwrapForeign f = traverse decodeVal (toList sq)
  decodeVal v = foreignConventionError "List" v

  encodeVal l =
    BoxedVal . Foreign . Wrap listRef . Sq.fromList $ encodeVal <$> l

  readAtIndex stk i = traverse decodeVal . toList =<< peekOffS stk i

  writeBack stk sq = pokeS stk . Sq.fromList $ encodeVal <$> sq

instance {-# OVERLAPPABLE #-} (BuiltinForeign b) => ForeignConvention b where
  decodeVal = decodeBuiltin
  encodeVal = encodeBuiltin
  readAtIndex = readBuiltinAt
  writeBack = writeBuiltin

-- Replacing Functions/Data Types
--
-- Below are mappings that replace unison definitions with direct
-- implementations in the interpreter. It is possible both to
-- replace data types and to replace functions with custom
-- implementations.
--
-- For data types, they will presumably be replaced by analogous
-- builtin types represented as wrapped 'foreign' values. For
-- instance, below the unison Map is replaced with Maps from the
-- containers library. To do this, the following steps are
-- necessary:
--
--   1. Create a builtin reference for the foreign type. See e.g.
--      `hmapRef` from the Map example. Note that it is _not_
--      necessary to add these to e.g. `Unison.Builtin`, because
--      they are not intended to be visible to unison users, just
--      implementation details.
--   2. Create new foreign function cases corresponding to the
--      unison type's constructors. These should take the same
--      arguments and produce the builtin value. Adding the cases
--      to the ForeignFunc type will trigger errors where you need
--      to supply implementations and such.
--   3. Add these foreign functions to the `pseudoConstructors`
--      mapping below. This maps the unison reference of the type
--      to be replaced to a mapping from its constructor tags to
--      their replacements. You may need to add the unison type
--      definition to the `Unison.Builtin.Decls` module to arrange
--      for this.
--   4. In the `dataBranch` function in `Unison.Runtime.Machine`,
--      add cases that make the wrapped builtin value behave like a
--      data type.
--   5. In `formDataReplaced` in `Unison.Runtime.Stack`, add cases
--      for building the builtin type when reifying the unison
--      data.
--   6. In `reflectValue` in `Unison.Runtime.Machine`, add a case
--      that reflects the builtin values as values of the original
--      unison type. These last two steps ensure that sending
--      values between machines doesn't need to know anything about
--      replacements.
--   7. Implement `universalCompare` and `universalEq` cases for
--      the builtin values.
--   8. Add a case in `Unison.Runtime.Decompile` to decompile the
--      builtin values as the original unison values.
--
-- With these steps done, the unison data type will be implemented
-- with the builtin values behind the scenes. In my testing, this
-- didn't seem to perform much worse than the unison data types
-- when running unison code, so this can be done without slowing
-- down pure unison functions much.
--
--
-- To replace unison _functions_, follow these steps:
--
--   1. Create a new foreign function case for the function you
--      want to replace. It is _not_ necessary to add to
--      `Unison.Builtin`, because they shouldn't be visible to the
--      user. Adding the case will give errors where it's necessary
--      for you to add implementations and such.
--   2. Add `declareForeign` statements in `Unison.Runtime.Builtin`
--      for your new foreign functions. These do not cause the
--      functions to be visible to users, but they make the runtime
--      aware of them.
--   3. Add your foreign function to the `functionReplacementList`
--      below. This requires that you find the _runtime hash_ of
--      the function you want to replace, in base32hex. You can
--      find this information using the @unison/internal library,
--      by calling:
--
--        Reference.toText (Reference.fromTermLink! (termLink ...))
--
-- Note: in the last step, pay special attention to the reference.
-- If it is _not_ just a string of base32 letters/numbers, and
-- instead ends with something like `.N`, then the reference is
-- part of a mutually recursive binding group, and it is not the
-- primary member of the group. The code below assumes that all
-- replacements _are_ the primary member of the group, so the code
-- needs to be augmented if this is ever not the case. Contact Dan
-- if you run into this.
--
-- With these steps done, any calls to the unison function should
-- instead execute the builtin, hopefully with significantly
-- improved performance.
--
-- It is not necessarily the case that all types involved need to
-- be replaced to replace a function. It should be possible to
-- replace a function by acting directly on the unison
-- representation of the arguments. This would involve taking `Val`
-- arguments to the foreign function and matching on the `Closure`
-- cases and so on. This might not be pleasant, however.

pseudoConstructors :: Map Reference (Map TT.CTag ForeignFunc)
pseudoConstructors =
  Map.singleton Ty.mapRef $
    Map.fromList
      [ (fromIntegral Ty.mapTip, Map_tip),
        (fromIntegral Ty.mapBin, Map_bin)
      ]

functionReplacementList :: [(Data.Text.Text, Pos, ForeignFunc)]
functionReplacementList =
  [ ( "03hqp8knrcgdc733mitcunjlug4cpi9headkggu8h9d87nfgneo6e",
      0,
      Map_insert
    ),
    ( "03g44bb2bp3g5eld8eh07g6e8iq7oiqiplapeb6jerbs7ee3icq9s",
      0,
      Map_lookup
    ),
    ( "005mc1fq7ojq72c238qlm2rspjgqo2furjodf28icruv316odu6du",
      0,
      Map_fromList
    ),
    ( "01qqpul0ttlgjhr5i2gtmdr2uarns2hbtnjpipmk1575ipkrlug42",
      0,
      Map_union
    ),
    ( "00c363e340il8q0fai6peiv3586o931nojj98qfek09hg1tjkm9ma",
      0,
      Map_intersect
    ),
    ( "03pjq0jijrr7ebf6s3tuqi4d5hi5mrv19nagp7ql2j9ltm55c32ek",
      0,
      Map_toList
    ),
    ( "03putoun7i5n0lhf8iu990u9p08laklnp668i170dka2itckmadlq",
      0,
      Multimap_fromList
    ),
    ( "03q6giac0qlva6u4mja29tr7mv0jqnsugk8paibatdrns8lhqqb92",
      0,
      Set_fromList
    ),
    ( "03362vaalqq28lcrmmsjhha637is312j01jme3juj980ugd93up28",
      0,
      Set_union
    ),
    ( "01lm6ejo31na1ti6u85bv0klliefll7q0c0da2qnefvcrq1l8rlqe",
      0,
      Set_intersect
    ),
    ( "01p7ot36tg62na408mnk1psve6rc7fog30gv6n7thkrv6t3na2gdm",
      0,
      Set_toList
    ),
    ( "03c559iihi2vj0qps6cln48nv31ajup2srhas4pd05b9k46ds8jvk",
      0,
      Map_eq
    ),
    ( "01f446li3b0j5gcnj7fa99jfqir43shs0jqu779oo0npb7v8d3v22",
      0,
      List_range
    ),
    ( "00jh7o3l67okqqalho1sqgl4ei9n2sdhrpqobgkf7j390v4e938km",
      0,
      List_sort
    ),
    ( "02n2eflppo81c4ako71f2ji347ljf1qoiij08q8tbid1p4k3n62k0",
      1,
      Json_toText
    ),
    ( "02d659vubpd4m2cqbupec8qg3jpdfkgotpqtera3hh72bc3b9o6m6",
      0,
      Json_unconsText
    ),
    ( "01pl56v6v0n2labp71cp6darcbftlj7d4h9t718mkfpj6lc905ro4",
      0,
      Json_tryUnconsText
    ),
    ( "01csmdujt5ot550j9t0o1gfop4ephtssv358rkfqdo2e01knekgds",
      0,
      Avro_decodeBinary
    )
  ]

-- Built at the same time to attempt to share references.
functionReplacements, functionUnreplacements :: Map Reference Reference
(functionReplacements, functionUnreplacements) =
  (Map.fromList processed, Map.fromList $ swap <$> processed)
  where
    swap (x, y) = (y, x)
    processed = process <$> functionReplacementList

-- Note: using index 0 right now. Generalize if ever replacing
-- part of a mutually recursive group.
process :: (Data.Text.Text, Pos, ForeignFunc) -> (Reference, Reference)
process (str, pos, ff) = case derivedBase32Hex str pos of
  Nothing -> error $ "Could not create reference for " ++ sname
  Just r -> (r, Builtin name)
  where
    name = foreignFuncBuiltinName ff
    sname = Data.Text.unpack name
