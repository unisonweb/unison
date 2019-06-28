{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Runtime.Rt1IO where

import           Control.Exception              ( try
                                                , throwIO
                                                , Exception, SomeException
                                                , finally
                                                , bracket
                                                )
import           Control.Concurrent             ( ThreadId
                                                , forkIO
                                                , killThread
                                                , threadDelay
                                                )
import           Control.Concurrent.MVar        ( MVar
                                                , modifyMVar_
                                                , readMVar
                                                , newMVar
                                                , newEmptyMVar
                                                , takeMVar
                                                , putMVar
                                                )
import           Control.Lens
import           Control.Monad.Trans            ( lift )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Morph            ( hoist )
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                , ask
                                                )
import           Control.Monad.Except           ( ExceptT(..)
                                                , runExceptT
                                                , throwError
                                                )
import           Data.Foldable                  ( traverse_ )
import           Data.Functor                   ( void )
import           Data.GUID                      ( genText )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( isJust )
import           Data.Text                      ( Text )
import           Data.Text                     as Text
import           Data.Time.Clock.POSIX         as Time
import qualified Network.Simple.TCP            as Net
import qualified Network.Socket                as Sock
--import qualified Network.Socket                as Sock
import           System.IO                      ( Handle
                                                , IOMode(..)
                                                , SeekMode(..)
                                                , openFile
                                                , hClose
                                                , hPutStr
                                                , stdin
                                                , stdout
                                                , stderr
                                                , hIsEOF
                                                , hGetLine
                                                , hGetContents
                                                , hIsSeekable
                                                , hSeek
                                                , hTell
                                                )
import qualified System.IO.Error               as SysError
import           Type.Reflection                ( Typeable )
import           Unison.DataDeclaration        as DD
import           Unison.Symbol
import qualified Unison.Reference              as R
import qualified Unison.Runtime.Rt1            as RT
import qualified Unison.Runtime.IR             as IR
import qualified Unison.Term                   as Term
-- import Debug.Trace
-- import qualified Unison.Util.Pretty            as Pretty
-- import           Unison.TermPrinter             ( prettyTop )
import           Unison.Codebase.Runtime        ( Runtime(Runtime) )
import qualified Unison.Runtime.IOSource       as IOSrc
import qualified Unison.Util.Bytes             as Bytes
import qualified Unison.Var                    as Var
import qualified Unison.Util.Pretty as P
import qualified Unison.TermPrinter as TermPrinter

-- TODO: Make this exception more structured?
data UnisonRuntimeException = UnisonRuntimeException Text
  deriving (Typeable, Show)

instance Exception UnisonRuntimeException

type GUID = Text

data IOState = IOState
  { _handleMap :: HandleMap
  , _socketMap :: SocketMap
  , _threadMap :: ThreadMap
  }

type UIO a = ExceptT IOError (ReaderT S IO) a
type HandleMap = Map GUID Handle
type SocketMap = Map GUID Net.Socket
type ThreadMap = Map GUID ThreadId

data S = S { _ioState :: MVar IOState }

makeLenses 'S
makeLenses 'IOState

haskellMode :: Text -> IOMode
haskellMode mode = case mode of
  "IOMode.Read"      -> ReadMode
  "IOMode.Write"     -> WriteMode
  "IOMode.Append"    -> AppendMode
  "IOMode.ReadWrite" -> ReadWriteMode
  _                  -> error . Text.unpack $ "Unknown IO mode " <> mode

newUnisonHandle :: Handle -> UIO RT.Value
newUnisonHandle h = do
  t <- liftIO $ genText
  m <- view ioState
  liftIO . modifyMVar_ m $ pure . (over handleMap) (Map.insert t h)
  pure $ IR.Data IOSrc.handleReference IOSrc.handleId [IR.T t]

newUnisonSocket :: Net.Socket -> UIO RT.Value
newUnisonSocket s = do
  t <- liftIO $ genText
  m <- view ioState
  liftIO . modifyMVar_ m $ pure . (over socketMap) (Map.insert t s)
  pure $ IR.Data IOSrc.socketReference IOSrc.socketId [IR.T t]

deleteUnisonHandle :: Text -> UIO ()
deleteUnisonHandle h = do
  m <- view $ ioState
  liftIO . modifyMVar_ m $ pure . (over handleMap) (Map.delete h)

getHaskellHandle :: Text -> UIO (Maybe Handle)
getHaskellHandle h = do
  m <- view $ ioState
  v <- liftIO $ readMVar m
  pure . Map.lookup h $ view handleMap v

getHaskellHandleOrThrow :: Text -> UIO Handle
getHaskellHandleOrThrow h = getHaskellHandle h >>= maybe throwHandleClosed pure

getHaskellSocket :: Text -> UIO (Maybe Net.Socket)
getHaskellSocket s = do
  m <- view $ ioState
  v <- liftIO $ readMVar m
  pure . Map.lookup s $ view socketMap v

getHaskellSocketOrThrow :: Text -> UIO Net.Socket
getHaskellSocketOrThrow s = getHaskellSocket s >>= maybe throwSocketClosed pure

constructLeft :: RT.Value -> RT.Value
constructLeft v = IR.Data IOSrc.eitherReference IOSrc.eitherLeftId [v]

constructRight :: RT.Value -> RT.Value
constructRight v = IR.Data IOSrc.eitherReference IOSrc.eitherRightId [v]

constructSome :: RT.Value -> RT.Value
constructSome v = IR.Data IOSrc.optionReference IOSrc.someId [v]

constructNone :: RT.Value
constructNone = IR.Data IOSrc.optionReference IOSrc.noneId []

convertMaybe :: Maybe (RT.Value) -> RT.Value
convertMaybe Nothing  = constructNone
convertMaybe (Just v) = constructSome v

constructPair :: RT.Value -> RT.Value -> RT.Value
constructPair a b = IR.Data DD.pairRef 0 [a, b]

convertErrorType :: IOError -> IR.ConstructorId
convertErrorType (SysError.ioeGetErrorType -> e)
  | SysError.isAlreadyExistsErrorType e    = IOSrc.alreadyExistsId
  | SysError.isDoesNotExistErrorType e     = IOSrc.noSuchThingId
  | SysError.isAlreadyInUseErrorType e     = IOSrc.resourceBusyId
  | SysError.isFullErrorType e             = IOSrc.resourceExhaustedId
  | SysError.isEOFErrorType e              = IOSrc.eofId
  | SysError.isIllegalOperationErrorType e = IOSrc.illegalOperationId
  | SysError.isPermissionErrorType e       = IOSrc.permissionDeniedId
  | otherwise                              = IOSrc.userErrorId

haskellSeekMode :: Text -> SeekMode
haskellSeekMode mode = case mode of
  "SeekMode.Absolute" -> AbsoluteSeek
  "SeekMode.Relative" -> RelativeSeek
  "SeekMode.FromEnd"  -> SeekFromEnd
  _                   -> error . Text.unpack $ "Unknown seek mode " <> mode

hostPreference :: [RT.Value] -> Net.HostPreference
hostPreference []                        = Net.HostAny
hostPreference [IR.Data _ _ [IR.T host]] = Net.Host $ Text.unpack host
hostPreference x =
  error $ "Runtime bug! Not a valid host preference: " <> show x

constructIoError :: IOError -> RT.Value
constructIoError e = IR.Data
  IOSrc.errorReference
  IOSrc.ioErrorId
  [ IR.Data IOSrc.errorTypeReference (convertErrorType e) []
  , IR.T . Text.pack $ show e
  ]

handleIO'
  :: RT.CompilationEnv
  -> S
  -> R.Reference
  -> IR.ConstructorId
  -> [RT.Value]
  -> IO RT.Result
handleIO' cenv s rid cid vs = case rid of
  R.DerivedId x | x == IOSrc.ioHash -> flip runReaderT s $ do
    ev <- runExceptT $ handleIO cenv cid vs
    case ev of
      Left  e -> pure . RT.RDone . constructLeft $ constructIoError e
      Right v -> pure . RT.RDone $ constructRight v
  _ -> do
    k <- RT.idContinuation
    pure $ RT.RRequest (IR.Req rid cid vs k)

reraiseIO :: IO a -> UIO a
reraiseIO a = ExceptT . lift $ try @IOError $ liftIO a

throwHandleClosed :: UIO a
throwHandleClosed = throwError $ illegalOperation "handle is closed"

throwSocketClosed :: UIO a
throwSocketClosed = throwError $ illegalOperation "socket is closed"

illegalOperation :: String -> IOError
illegalOperation msg =
  SysError.mkIOError SysError.illegalOperationErrorType msg Nothing Nothing

handleIO :: RT.CompilationEnv -> IR.ConstructorId -> [RT.Value] -> UIO RT.Value
handleIO cenv cid args = go (IOSrc.constructorName IOSrc.ioReference cid) args
 where
  go "IO.throw" [IR.Data _ _ [IR.Data _ _ [], IR.T message]] =
    liftIO . throwIO $ UnisonRuntimeException message
  go "IO.openFile" [IR.Data _ 0 [IR.T filePath], IR.Data _ mode _] = do
    let n = IOSrc.constructorName IOSrc.ioModeReference mode
    h <- reraiseIO . openFile (Text.unpack filePath) $ haskellMode n
    newUnisonHandle h
  go "IO.closeFile" [IR.Data _ 0 [IR.T handle]] = do
    hh <- getHaskellHandle handle
    reraiseIO $ maybe (pure ()) hClose hh
    deleteUnisonHandle handle
    pure IR.unit
  go "IO.putText_" [IR.Data _ 0 [IR.T handle], IR.T string] = do
    hh <- getHaskellHandleOrThrow handle
    reraiseIO . hPutStr hh $ Text.unpack string
    pure IR.unit
  go "IO.isFileEOF" [IR.Data _ 0 [IR.T handle]] = do
    hh    <- getHaskellHandleOrThrow handle
    isEOF <- reraiseIO $ hIsEOF hh
    pure $ IR.B isEOF
  go "IO.isFileOpen" [IR.Data _ 0 [IR.T handle]] =
    IR.B . isJust <$> getHaskellHandle handle
  go "IO.getLine" [IR.Data _ 0 [IR.T handle]] = do
    hh   <- getHaskellHandleOrThrow handle
    line <- reraiseIO $ hGetLine hh
    pure . IR.T $ Text.pack line
  go "IO.getText" [IR.Data _ 0 [IR.T handle]] = do
    hh   <- getHaskellHandleOrThrow handle
    text <- reraiseIO $ hGetContents hh
    pure . IR.T $ Text.pack text
  go "IO.isSeekable" [IR.Data _ 0 [IR.T handle]] = do
    hh       <- getHaskellHandleOrThrow handle
    seekable <- reraiseIO $ hIsSeekable hh
    pure $ IR.B seekable
  go "IO.seek" [IR.Data _ 0 [IR.T handle], IR.Data _ seekMode [], IR.I int] =
    do
      hh <- getHaskellHandleOrThrow handle
      let mode = IOSrc.constructorName IOSrc.seekModeReference seekMode
      reraiseIO . hSeek hh (haskellSeekMode mode) $ fromIntegral int
      pure $ IR.unit
  go "IO.position" [IR.Data _ 0 [IR.T handle]] = do
    hh  <- getHaskellHandleOrThrow handle
    pos <- reraiseIO $ hTell hh
    pure . IR.I $ fromIntegral pos
  go "IO.serverSocket" [IR.Data _ _ mayHost, IR.Data _ _ [IR.T port]] = do
    (s, _) <- reraiseIO
      $ Net.bindSock (hostPreference mayHost) (Text.unpack port)
    newUnisonSocket s
  go "IO.listen" [IR.Data _ _ [IR.T socket]] = do
    hs <- getHaskellSocketOrThrow socket
    reraiseIO $ Net.listenSock hs 2048
    pure $ IR.unit
  go "IO.clientSocket" [IR.Data _ _ [IR.T host], IR.Data _ _ [IR.T port]] = do
    (s, _) <- reraiseIO . Net.connectSock (Text.unpack host) $ Text.unpack port
    newUnisonSocket s
  go "IO.closeSocket" [IR.Data _ _ [IR.T socket]] = do
    hs <- getHaskellSocket socket
    reraiseIO $ traverse_ Net.closeSock hs
    pure IR.unit
  go "IO.accept" [IR.Data _ _ [IR.T socket]] = do
    hs   <- getHaskellSocketOrThrow socket
    conn <- reraiseIO $ Sock.accept hs
    newUnisonSocket $ fst conn
  go "IO.send" [IR.Data _ _ [IR.T socket], IR.Bs bs] = do
    hs <- getHaskellSocketOrThrow socket
    reraiseIO . Net.send hs $ Bytes.toByteString bs
    pure IR.unit
  go "IO.receive" [IR.Data _ _ [IR.T socket], IR.N n] = do
    hs <- getHaskellSocketOrThrow socket
    bs <- reraiseIO . Net.recv hs $ fromIntegral n
    pure . convertMaybe $ IR.Bs . Bytes.fromByteString <$> bs
  go "IO.fork" [IR.Lam _ _ ir] = do
    s    <- ask
    t    <- liftIO genText
    lock <- liftIO newEmptyMVar
    m    <- view ioState
    id   <- reraiseIO . forkIO . void $ do
      void $ takeMVar lock
      forceThunk cenv s ir
        `finally` modifyMVar_ m (pure . (over threadMap) (Map.delete t))
    liftIO . modifyMVar_ m $ pure . (over threadMap) (Map.insert t id)
    liftIO $ putMVar lock ()
    pure $ IR.Data IOSrc.threadIdReference IOSrc.threadIdId [IR.T t]
  go "IO.kill" [IR.Data _ _ [IR.T thread]] = do
    m   <- view ioState
    map <- liftIO $ view threadMap <$> readMVar m
    liftIO $ case Map.lookup thread map of
      Nothing -> pure IR.unit
      Just ht -> do
        killThread ht
        pure IR.unit
  go "IO.bracket" [IR.Lam _ _ acquire, IR.Lam _ _ release, IR.Lam _ _ use] = do
    s <- ask
    let resultToVal (RT.RDone v) = pure v
        resultToVal v = fail $ "IO bracket expected a value but got " <> show v
    reraiseIO $ resultToVal =<< bracket
      (resultToVal =<< forceThunk cenv s acquire)
      (lamToHask cenv s release)
      (lamToHask cenv s use)
  go "IO.delay" [IR.N n] = do
    reraiseIO . threadDelay $ fromIntegral n
    pure IR.unit
  go "IO.systemTime_" [] = do
    t <- reraiseIO $ fmap round Time.getPOSIXTime
    pure $ IR.Data IOSrc.epochTimeReference IOSrc.epochTimeId [IR.N t]
  go a b =
    error
      $  "IO handler called with unimplemented cid "
      <> show cid
      <> " and "
      <> show a
      <> " args "
      <> show b

forceThunk :: RT.CompilationEnv -> S -> RT.IR -> IO RT.Result
forceThunk cenv s ir = lamToHask cenv s ir IR.unit

lamToHask :: RT.CompilationEnv -> S -> RT.IR -> RT.Value -> IO RT.Result
lamToHask cenv s ir val = RT.run (handleIO' cenv s) cenv $ task val
  where task x = IR.Let (Var.named "_") (IR.Leaf (IR.Val x)) ir mempty

runtime :: Runtime Symbol
runtime = Runtime terminate eval
 where
  terminate :: IO ()
  terminate = pure ()
  eval cl' ppe term = do
    let cl = void (hoist (pure . runIdentity) IOSrc.codeLookup) <> cl'
    -- traceM $ Pretty.render 80 (prettyTop mempty term)
    cenv <- RT.compilationEnv cl term -- in `m`
    mmap <- newMVar $ IOState
      (Map.fromList [("stdin", stdin), ("stdout", stdout), ("stderr", stderr)])
      Map.empty
      Map.empty
    r <- try $ RT.run (handleIO' cenv $ S mmap)
                 cenv
                 (IR.compile cenv $ Term.amap (const ()) term)
    case r of
      Right (RT.RDone result) -> Right <$> IR.decompile result
      Right (RT.RMatchFail _ _ scrute) -> do
        scrute <- IR.decompile scrute
        pure . Left . P.callout icon . P.lines $ [
          P.wrap ("I've encountered a" <> P.red "pattern match failure"
                  <> "while scrutinizing:"), "",
          P.indentN 2 $ TermPrinter.prettyTop ppe scrute,
          "",
          P.wrap "This happens when calling a function that doesn't handle all possible inputs.",
          "", sorryMsg
          ]
      Right (RT.RRequest (IR.Req r cid vs _)) -> do
        vs <- traverse IR.decompile vs
        let tm = Term.apps' (Term.request() r cid) vs
        pure . Left . P.callout icon . P.lines $ [
          P.wrap ("I stopped evaluation after encountering an " <> P.red "unhandled request:"), "",
          P.indentN 2 $ TermPrinter.prettyTop ppe tm,
          "",
          P.wrap "This happens when using a handler that doesn't handle all possible requests.",
          "", sorryMsg
          ]
      Left e -> pure . Left . P.callout icon . P.lines $ [
        P.wrap ("I stopped evaluation after encountering " <> P.red "an error:"), "",
        P.indentN 2 $ P.string (show (e :: SomeException)),
        "", sorryMsg
        ]
      where
        icon = "💔💥"
        sorryMsg = P.wrap $ "I'm sorry this message doesn't have more detail about"
                         <> "the location of the failure."
                         <> "My makers plan to fix this in a future release. 😢"
