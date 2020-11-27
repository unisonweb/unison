{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Runtime.Rt1IO where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Concurrent.MVar
  ( MVar,
    modifyMVar_,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
  )
import Control.Exception
  ( AsyncException (UserInterrupt),
    asyncExceptionFromException,
    bracket,
    finally,
    throwIO,
  )
import Control.Lens
import Control.Monad.Except
  ( ExceptT (..),
    runExceptT,
    throwError,
  )
import Control.Monad.Morph (hoist)
import Control.Monad.Reader
  ( ReaderT,
    ask,
    runReaderT,
  )
import Data.GUID (genText)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Time.Clock.POSIX as Time
import qualified Network.Simple.TCP as Net
import qualified Network.Socket as Sock
--import qualified Network.Socket                as Sock

import System.Directory
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
import System.IO
  ( BufferMode (..),
    Handle,
    IOMode (..),
    SeekMode (..),
    hClose,
    hGetBuffering,
    hIsEOF,
    hIsSeekable,
    hSeek,
    hSetBuffering,
    hTell,
    openFile,
    stderr,
    stdin,
    stdout,
  )
import qualified System.IO.Error as SysError
import Type.Reflection (Typeable)
import Unison.Builtin.Decls as DD
-- import Debug.Trace
-- import qualified Unison.Util.Pretty            as Pretty
-- import           Unison.TermPrinter             ( pretty )

import Unison.Codebase.MainTerm (nullaryMain, nullaryTest)
import Unison.Codebase.Runtime (Runtime (Runtime))
import Unison.Parser (Ann (External))
import Unison.Prelude
import qualified Unison.PrettyPrintEnv as PPE
import qualified Unison.Reference as R
import qualified Unison.Runtime.IOSource as IOSrc
import qualified Unison.Runtime.IR as IR
import qualified Unison.Runtime.Rt1 as RT
import Unison.Symbol
import qualified Unison.Term as Term
import qualified Unison.TermPrinter as TermPrinter
import qualified Unison.Typechecker.Components as Components
import qualified Unison.Util.Bytes as Bytes
import qualified Unison.Util.Pretty as P
import qualified Unison.Var as Var

-- TODO: Make this exception more structured?
newtype UnisonRuntimeException = UnisonRuntimeException Text
  deriving (Typeable, Show)

instance Exception UnisonRuntimeException

type GUID = Text

data IOState = IOState
  { _handleMap :: HandleMap,
    _socketMap :: SocketMap,
    _threadMap :: ThreadMap
  }

type UIO a = ExceptT IOError (ReaderT S IO) a

type HandleMap = Map GUID Handle

type SocketMap = Map GUID Net.Socket

type ThreadMap = Map GUID ThreadId

newtype S = S {_ioState :: MVar IOState}

makeLenses 'S
makeLenses 'IOState

haskellMode :: Text -> IOMode
haskellMode mode = case mode of
  "io.Mode.Read" -> ReadMode
  "io.Mode.Write" -> WriteMode
  "io.Mode.Append" -> AppendMode
  "io.Mode.ReadWrite" -> ReadWriteMode
  _ -> error . Text.unpack $ "Unknown IO mode " <> mode

newUnisonHandle :: Handle -> UIO RT.Value
newUnisonHandle h = do
  t <- liftIO genText
  m <- view ioState
  liftIO . modifyMVar_ m $ pure . over handleMap (Map.insert t h)
  pure $ IR.Data IOSrc.handleReference IOSrc.handleId [IR.T t]

newUnisonSocket :: Net.Socket -> UIO RT.Value
newUnisonSocket s = do
  t <- liftIO genText
  m <- view ioState
  liftIO . modifyMVar_ m $ pure . over socketMap (Map.insert t s)
  pure $ IR.Data IOSrc.socketReference IOSrc.socketId [IR.T t]

deleteUnisonHandle :: Text -> UIO ()
deleteUnisonHandle h = do
  m <- view ioState
  liftIO . modifyMVar_ m $ pure . over handleMap (Map.delete h)

getHaskellHandle :: Text -> UIO (Maybe Handle)
getHaskellHandle h = do
  m <- view ioState
  v <- liftIO $ readMVar m
  pure . Map.lookup h $ view handleMap v

getHaskellHandleOrThrow :: Text -> UIO Handle
getHaskellHandleOrThrow h = getHaskellHandle h >>= maybe throwHandleClosed pure

getHaskellSocket :: Text -> UIO (Maybe Net.Socket)
getHaskellSocket s = do
  m <- view ioState
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

convertMaybe :: Maybe RT.Value -> RT.Value
convertMaybe Nothing = constructNone
convertMaybe (Just v) = constructSome v

convertOptional :: RT.Value -> Maybe RT.Value
convertOptional (IR.Data _ _ []) = Nothing
convertOptional (IR.Data _ _ [x]) = Just x
convertOptional v =
  error $
    "Compiler bug! This value showed up at runtime where "
      <> "an Optional was expected: "
      <> show v

constructPair :: RT.Value -> RT.Value -> RT.Value
constructPair a b = IR.Data DD.pairRef 0 [a, b]

convertErrorType :: IOError -> IR.ConstructorId
convertErrorType (SysError.ioeGetErrorType -> e)
  | SysError.isAlreadyExistsErrorType e = IOSrc.alreadyExistsId
  | SysError.isDoesNotExistErrorType e = IOSrc.noSuchThingId
  | SysError.isAlreadyInUseErrorType e = IOSrc.resourceBusyId
  | SysError.isFullErrorType e = IOSrc.resourceExhaustedId
  | SysError.isEOFErrorType e = IOSrc.eofId
  | SysError.isIllegalOperationErrorType e = IOSrc.illegalOperationId
  | SysError.isPermissionErrorType e = IOSrc.permissionDeniedId
  | otherwise = IOSrc.userErrorId

haskellSeekMode :: Text -> SeekMode
haskellSeekMode mode = case mode of
  "io.SeekMode.Absolute" -> AbsoluteSeek
  "io.SeekMode.Relative" -> RelativeSeek
  "io.SeekMode.FromEnd" -> SeekFromEnd
  _ -> error . Text.unpack $ "Unknown seek mode " <> mode

haskellBufferMode :: RT.Value -> BufferMode
haskellBufferMode mode = case mode of
  IR.Data _ _ [] -> NoBuffering
  IR.Data _ _ [IR.Data _ _ []] -> LineBuffering
  IR.Data _ _ [IR.Data _ _ [IR.Data _ _ []]] -> BlockBuffering Nothing
  IR.Data _ _ [IR.Data _ _ [IR.Data _ _ [IR.N n]]] ->
    BlockBuffering (Just $ fromIntegral n)
  _ -> error $ "Unknown buffer mode " <> show mode

unisonBufferMode :: BufferMode -> RT.Value
unisonBufferMode mode = case mode of
  NoBuffering -> constructNone
  LineBuffering ->
    constructSome (IR.Data IOSrc.bufferModeReference IOSrc.bufferModeLineId [])
  BlockBuffering Nothing ->
    constructSome
      (IR.Data IOSrc.bufferModeReference IOSrc.bufferModeBlockId [constructNone])
  BlockBuffering (Just size) ->
    constructSome
      ( IR.Data
          IOSrc.bufferModeReference
          IOSrc.bufferModeBlockId
          [constructSome . IR.N $ fromIntegral size]
      )

unisonFilePath :: FilePath -> RT.Value
unisonFilePath fp =
  IR.Data IOSrc.filePathReference IOSrc.filePathId [IR.T $ Text.pack fp]

hostPreference :: [RT.Value] -> Net.HostPreference
hostPreference [] = Net.HostAny
hostPreference [IR.Data _ _ [IR.T host]] = Net.Host $ Text.unpack host
hostPreference x =
  error $ "Runtime bug! Not a valid host preference: " <> show x

constructIoError :: IOError -> RT.Value
constructIoError e =
  IR.Data
    IOSrc.errorReference
    IOSrc.ioErrorId
    [ IR.Data IOSrc.errorTypeReference (convertErrorType e) [],
      IR.T . Text.pack $ show e
    ]

handleIO' ::
  RT.CompilationEnv ->
  S ->
  R.Reference ->
  IR.ConstructorId ->
  [RT.Value] ->
  IO RT.Result
handleIO' cenv s rid cid vs = case rid of
  R.DerivedId x | x == IOSrc.ioHash -> flip runReaderT s $ do
    ev <- runExceptT $ handleIO cenv cid vs
    case ev of
      Left e -> pure . RT.RDone . constructLeft $ constructIoError e
      Right v -> pure . RT.RDone $ constructRight v
  _ -> RT.RRequest . IR.Req rid cid vs <$> RT.idContinuation

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
handleIO cenv cid = go (IOSrc.constructorName IOSrc.ioReference cid)
  where
    go "io.IO.openFile_" [IR.Data _ 0 [IR.T filePath], IR.Data _ mode _] = do
      let n = IOSrc.constructorName IOSrc.ioModeReference mode
      h <- reraiseIO . openFile (Text.unpack filePath) $ haskellMode n
      newUnisonHandle h
    go "io.IO.closeFile_" [IR.Data _ 0 [IR.T handle]] = do
      hh <- getHaskellHandle handle
      reraiseIO $ maybe (pure ()) hClose hh
      deleteUnisonHandle handle
      pure IR.unit
    go "io.IO.isFileEOF_" [IR.Data _ 0 [IR.T handle]] = do
      hh <- getHaskellHandleOrThrow handle
      isEOF <- reraiseIO $ hIsEOF hh
      pure $ IR.B isEOF
    go "io.IO.isFileOpen_" [IR.Data _ 0 [IR.T handle]] =
      IR.B . isJust <$> getHaskellHandle handle
    go "io.IO.getLine_" [IR.Data _ 0 [IR.T handle]] = do
      hh <- getHaskellHandleOrThrow handle
      line <- reraiseIO $ TextIO.hGetLine hh
      pure . IR.T $ line
    go "io.IO.getText_" [IR.Data _ 0 [IR.T handle]] = do
      hh <- getHaskellHandleOrThrow handle
      text <- reraiseIO $ TextIO.hGetContents hh
      pure . IR.T $ text
    go "io.IO.putText_" [IR.Data _ 0 [IR.T handle], IR.T string] = do
      hh <- getHaskellHandleOrThrow handle
      reraiseIO . TextIO.hPutStr hh $ string
      pure IR.unit
    go "io.IO.throw" [IR.Data _ _ [IR.Data _ _ [], IR.T message]] =
      liftIO . throwIO $ UnisonRuntimeException message
    go "io.IO.isSeekable_" [IR.Data _ 0 [IR.T handle]] = do
      hh <- getHaskellHandleOrThrow handle
      seekable <- reraiseIO $ hIsSeekable hh
      pure $ IR.B seekable
    go "io.IO.seek_" [IR.Data _ 0 [IR.T handle], IR.Data _ seekMode [], IR.I int] =
      do
        hh <- getHaskellHandleOrThrow handle
        let mode = IOSrc.constructorName IOSrc.seekModeReference seekMode
        reraiseIO . hSeek hh (haskellSeekMode mode) $ fromIntegral int
        pure IR.unit
    go "io.IO.position_" [IR.Data _ 0 [IR.T handle]] = do
      hh <- getHaskellHandleOrThrow handle
      pos <- reraiseIO $ hTell hh
      pure . IR.I $ fromIntegral pos
    go "io.IO.getBuffering_" [IR.Data _ 0 [IR.T handle]] = do
      hh <- getHaskellHandleOrThrow handle
      bufMode <- reraiseIO $ hGetBuffering hh
      pure $ unisonBufferMode bufMode
    go "io.IO.setBuffering_" [IR.Data _ 0 [IR.T handle], o] = do
      hh <- getHaskellHandleOrThrow handle
      reraiseIO . hSetBuffering hh $ haskellBufferMode o
      pure IR.unit
    go "io.IO.systemTime_" [] = do
      t <- reraiseIO $ fmap round Time.getPOSIXTime
      pure $ IR.Data IOSrc.epochTimeReference IOSrc.epochTimeId [IR.N t]
    go "io.IO.getTemporaryDirectory_" [] =
      reraiseIO $ unisonFilePath <$> getTemporaryDirectory
    go "io.IO.getCurrentDirectory_" [] =
      reraiseIO $ unisonFilePath <$> getCurrentDirectory
    go "io.IO.setCurrentDirectory_" [IR.Data _ _ [IR.T dir]] = do
      reraiseIO . setCurrentDirectory $ Text.unpack dir
      pure IR.unit
    go "io.IO.directoryContents_" [IR.Data _ _ [IR.T dir]] =
      reraiseIO $
        IR.Sequence
          . Seq.fromList
          . fmap unisonFilePath
          <$> getDirectoryContents (Text.unpack dir)
    go "io.IO.fileExists_" [IR.Data _ _ [IR.T dir]] =
      reraiseIO $ IR.B <$> doesPathExist (Text.unpack dir)
    go "io.IO.isDirectory_" [IR.Data _ _ [IR.T dir]] =
      reraiseIO $ IR.B <$> doesDirectoryExist (Text.unpack dir)
    go "io.IO.createDirectory_" [IR.Data _ _ [IR.T dir]] = do
      reraiseIO $ createDirectoryIfMissing True (Text.unpack dir)
      pure IR.unit
    go "io.IO.removeDirectory_" [IR.Data _ _ [IR.T dir]] = do
      reraiseIO . removeDirectoryRecursive $ Text.unpack dir
      pure IR.unit
    go "io.IO.renameDirectory_" [IR.Data _ _ [IR.T from], IR.Data _ _ [IR.T to]] =
      do
        reraiseIO $ renameDirectory (Text.unpack from) (Text.unpack to)
        pure IR.unit
    go "io.IO.removeFile_" [IR.Data _ _ [IR.T file]] = do
      reraiseIO . removeFile $ Text.unpack file
      pure IR.unit
    go "io.IO.renameFile_" [IR.Data _ _ [IR.T from], IR.Data _ _ [IR.T to]] = do
      reraiseIO $ renameFile (Text.unpack from) (Text.unpack to)
      pure IR.unit
    go "io.IO.getFileTimestamp_" [IR.Data _ _ [IR.T file]] = do
      t <- reraiseIO $ getModificationTime (Text.unpack file)
      pure $
        IR.Data
          IOSrc.epochTimeReference
          IOSrc.epochTimeId
          [IR.N . round $ Time.utcTimeToPOSIXSeconds t]
    go "io.IO.getFileSize_" [IR.Data _ _ [IR.T file]] =
      reraiseIO $ IR.N . fromIntegral <$> getFileSize (Text.unpack file)
    go "io.IO.serverSocket_" [IR.Data _ _ mayHost, IR.Data _ _ [IR.T port]] = do
      (s, _) <-
        reraiseIO $
          Net.bindSock (hostPreference mayHost) (Text.unpack port)
      newUnisonSocket s
    go "io.IO.listen_" [IR.Data _ _ [IR.T socket]] = do
      hs <- getHaskellSocketOrThrow socket
      reraiseIO $ Net.listenSock hs 2048
      pure IR.unit
    go "io.IO.clientSocket_" [IR.Data _ _ [IR.T host], IR.Data _ _ [IR.T port]] =
      do
        (s, _) <-
          reraiseIO . Net.connectSock (Text.unpack host) $
            Text.unpack
              port
        newUnisonSocket s
    go "io.IO.closeSocket_" [IR.Data _ _ [IR.T socket]] = do
      hs <- getHaskellSocket socket
      reraiseIO $ traverse_ Net.closeSock hs
      pure IR.unit
    go "io.IO.accept_" [IR.Data _ _ [IR.T socket]] = do
      hs <- getHaskellSocketOrThrow socket
      conn <- reraiseIO $ Sock.accept hs
      newUnisonSocket $ fst conn
    go "io.IO.send_" [IR.Data _ _ [IR.T socket], IR.Bs bs] = do
      hs <- getHaskellSocketOrThrow socket
      reraiseIO . Net.send hs $ Bytes.toArray bs
      pure IR.unit
    go "io.IO.receive_" [IR.Data _ _ [IR.T socket], IR.N n] = do
      hs <- getHaskellSocketOrThrow socket
      bs <- reraiseIO . Net.recv hs $ fromIntegral n
      pure . convertMaybe $ IR.Bs . Bytes.fromArray <$> bs
    go "io.IO.fork_" [IR.Lam _ _ ir] = do
      s <- ask
      t <- liftIO genText
      lock <- liftIO newEmptyMVar
      m <- view ioState
      id <- reraiseIO . forkIO . void $ do
        void $ takeMVar lock
        forceThunk cenv s ir
          `finally` modifyMVar_ m (pure . over threadMap (Map.delete t))
      liftIO . modifyMVar_ m $ pure . over threadMap (Map.insert t id)
      liftIO $ putMVar lock ()
      pure $ IR.Data IOSrc.threadIdReference IOSrc.threadIdId [IR.T t]
    go "io.IO.kill_" [IR.Data _ _ [IR.T thread]] = do
      m <- view ioState
      map <- liftIO $ view threadMap <$> readMVar m
      liftIO $ case Map.lookup thread map of
        Nothing -> pure IR.unit
        Just ht -> do
          killThread ht
          pure IR.unit
    go "io.IO.delay_" [IR.N n] = do
      reraiseIO . threadDelay $ fromIntegral n
      pure IR.unit
    go "io.IO.bracket_" [IR.Lam _ _ acquire, IR.Lam _ _ release, IR.Lam _ _ use] =
      do
        s <- ask
        let resultToVal (RT.RDone v) = pure v
            resultToVal v =
              fail $ "IO bracket expected a value but got " <> show v
        reraiseIO $
          resultToVal
            =<< bracket
              (resultToVal =<< forceThunk cenv s acquire)
              (lamToHask cenv s release)
              (lamToHask cenv s use)
    go a _b = error $ show a <> " is not implemented yet."

-- error
--   $  "IO handler called with unimplemented cid "
--   <> show cid
--   <> " and "
--   <> show a
--   <> " args "
--   <> show b

forceThunk :: RT.CompilationEnv -> S -> RT.IR -> IO RT.Result
forceThunk cenv s ir = lamToHask cenv s ir IR.unit

lamToHask :: RT.CompilationEnv -> S -> RT.IR -> RT.Value -> IO RT.Result
lamToHask cenv s ir val = RT.run (handleIO' cenv s) cenv $ task val
  where
    task x = IR.Let (Var.named "_") (IR.Leaf (IR.Val x)) ir mempty

runtime :: Runtime Symbol
runtime = Runtime terminate eval (nullaryMain External) (nullaryTest External) True
  where
    terminate :: IO ()
    terminate = pure ()
    eval cl' ppe term = do
      let cl = void (hoist (pure . runIdentity) IOSrc.codeLookup) <> cl'
      -- traceM $ Pretty.render 80 (pretty mempty term)
      cenv <- RT.compilationEnv cl term -- in `m`
      mmap <-
        newMVar $
          IOState
            (Map.fromList [("stdin", stdin), ("stdout", stdout), ("stderr", stderr)])
            Map.empty
            Map.empty
      term <- case Components.minimize' term of
        Left es ->
          fail . reportBug "B23784210" $
            "Term contains duplicate definitions: " <> show (fst <$> es)
        Right term -> pure term
      r <-
        try $
          RT.run
            (handleIO' cenv $ S mmap)
            cenv
            (IR.compile cenv $ Term.amap (const ()) term)
      toTermOrError ppe r

toTermOrError ::
  PPE.PrettyPrintEnv ->
  Either SomeException RT.Result ->
  IO (Either (P.Pretty P.ColorText) (IR.Term Symbol))
toTermOrError ppe r = case r of
  Right (RT.RDone result) -> Right <$> IR.decompile result
  Right (RT.RMatchFail _ _ scrute) -> do
    scrute <- IR.decompile scrute
    pure . Left . P.callout icon . P.lines $
      [ P.wrap
          ( "I've encountered a" <> P.red "pattern match failure"
              <> "while scrutinizing:"
          ),
        "",
        P.indentN 2 $ TermPrinter.pretty ppe scrute,
        "",
        P.wrap "This happens when calling a function that doesn't handle all possible inputs.",
        "",
        sorryMsg
      ]
  Right (RT.RError t val) -> do
    msg <- IR.decompile val
    let errorType = case t of
          RT.ErrorTypeTodo -> "builtin.todo"
          RT.ErrorTypeBug -> "builtin.bug"
    pure . Left . P.callout icon . P.lines $
      [ P.wrap
          ( "I've encountered a call to" <> P.red errorType
              <> "with the following value:"
          ),
        "",
        P.indentN 2 $ TermPrinter.pretty ppe msg,
        "",
        sorryMsg
      ]
  Right (RT.RRequest (IR.Req r cid vs _)) -> do
    vs <- traverse IR.decompile vs
    let tm = Term.apps' (Term.request () r cid) vs
    pure . Left . P.callout icon . P.lines $
      [ P.wrap ("I stopped evaluation after encountering an " <> P.red "unhandled request:"),
        "",
        P.indentN 2 $ TermPrinter.pretty ppe tm,
        "",
        P.wrap "This happens when using a handler that doesn't handle all possible requests.",
        "",
        sorryMsg
      ]
  Left (asyncExceptionFromException -> Just e) -> pure . Left . P.callout "⏹" $
    case e of
      UserInterrupt -> P.wrap $ "I've" <> P.purple "cancelled evaluation."
      e ->
        P.wrap $
          "I've stopped evaluation after receiving a "
            <> P.purple (P.shown e)
            <> "signal."
  Left e ->
    pure . Left . P.callout icon . P.lines $
      [ P.wrap ("I stopped evaluation after encountering " <> P.red "an error:"),
        "",
        P.indentN 2 $ P.string (show (e :: SomeException)),
        "",
        sorryMsg
      ]
  where
    icon = "💔💥"
    sorryMsg =
      P.wrap $
        "I'm sorry this message doesn't have more detail about"
          <> "the location of the failure."
          <> "My makers plan to fix this in a future release. 😢"
