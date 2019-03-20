{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Runtime.Rt1IO where

import           Control.Exception              ( try
                                                , throwIO
                                                , Exception
                                                )
import           Control.Concurrent.MVar        ( MVar
                                                , modifyMVar_
                                                , readMVar
                                                , newMVar
                                                )
import           Control.Lens
import           Control.Monad.Trans            ( lift )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Morph            ( hoist )
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                )
import           Control.Monad.Except           ( ExceptT(..)
                                                , runExceptT
                                                , throwError
                                                )
import           Data.Bifunctor                 ( first
                                                , second
                                                )
import           Data.Foldable                  ( traverse_ )
import           Data.Functor                   ( void )
import           Data.GUID                      ( genText )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( isJust )
import           Data.Text                      ( Text )
import           Data.Text                     as Text
import qualified Network.Simple.TCP            as Net
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
import qualified Unison.Codebase.CodeLookup    as CL
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

-- TODO: Make this exception more structured?
data UnisonRuntimeException = UnisonRuntimeException Text
  deriving (Typeable, Show)

instance Exception UnisonRuntimeException

type GUID = Text
type IOState = MVar (HandleMap, SocketMap)

type UIO ann a = ExceptT IOError (ReaderT (S ann) IO) a
type HandleMap = Map GUID Handle
type SocketMap = Map GUID Net.Socket

data S a = S { _ioState :: IOState }

makeLenses 'S

haskellMode :: Text -> IOMode
haskellMode mode = case mode of
  "IOMode.Read"      -> ReadMode
  "IOMode.Write"     -> WriteMode
  "IOMode.Append"    -> AppendMode
  "IOMode.ReadWrite" -> ReadWriteMode
  _                  -> error . Text.unpack $ "Unknown IO mode " <> mode

newUnisonHandle :: Handle -> UIO a RT.Value
newUnisonHandle h = do
  t <- liftIO $ genText
  m <- view ioState
  liftIO . modifyMVar_ m $ pure . first (Map.insert t h)
  pure $ IR.T t

newUnisonSocket :: Net.Socket -> UIO a RT.Value
newUnisonSocket s = do
  t <- liftIO $ genText
  m <- view ioState
  liftIO . modifyMVar_ m $ pure . second (Map.insert t s)
  pure $ IR.T t

deleteUnisonHandle :: Text -> UIO a ()
deleteUnisonHandle h = do
  m <- view $ ioState
  liftIO . modifyMVar_ m $ pure . first (Map.delete h)

getHaskellHandle :: Text -> UIO a (Maybe Handle)
getHaskellHandle h = do
  m <- view $ ioState
  v <- liftIO $ readMVar m
  pure . Map.lookup h $ fst v

getHaskellHandleOrThrow :: Text -> UIO a Handle
getHaskellHandleOrThrow h = getHaskellHandle h >>= maybe throwHandleClosed pure

getHaskellSocket :: Text -> UIO a (Maybe Net.Socket)
getHaskellSocket s = do
  m <- view $ ioState
  v <- liftIO $ readMVar m
  pure . Map.lookup s $ snd v

getHaskellSocketOrThrow :: Text -> UIO a Net.Socket
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

handleIO' :: S a -> R.Reference -> IR.ConstructorId -> [RT.Value] -> IO RT.Value
handleIO' s rid cid vs = case rid of
  R.DerivedId x | x == IOSrc.ioHash -> flip runReaderT s $ do
    ev <- runExceptT $ handleIO cid vs
    case ev of
      Left  e -> pure . constructLeft $ constructIoError e
      Right v -> pure v
  _ -> fail $ "This ability is not an I/O ability: " <> show rid

reraiseIO :: IO a -> UIO x a
reraiseIO a = ExceptT . lift $ try @IOError $ liftIO a

throwHandleClosed :: UIO x a
throwHandleClosed = throwError $ illegalOperation "handle is closed"

throwSocketClosed :: UIO x a
throwSocketClosed = throwError $ illegalOperation "socket is closed"

illegalOperation :: String -> IOError
illegalOperation msg =
  SysError.mkIOError SysError.illegalOperationErrorType msg Nothing Nothing

handleIO :: IR.ConstructorId -> [RT.Value] -> UIO a RT.Value
handleIO cid args = go (IOSrc.constructorName IOSrc.ioReference cid) args
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
  go "IO.putText" [IR.Data _ 0 [IR.T handle], IR.T string] = do
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
  go a b =
    error
      $  "IO handler called with unimplemented cid "
      <> show cid
      <> " and "
      <> show a
      <> " args "
      <> show b

runtime :: Runtime Symbol
runtime = Runtime terminate eval
 where
  terminate :: IO ()
  terminate = pure ()
  eval
    :: CL.CodeLookup Symbol IO () -> Term.Term Symbol -> IO (Term.Term Symbol)
  eval cl' term = do
    let cl = void (hoist (pure . runIdentity) IOSrc.codeLookup) <> cl'
    -- traceM $ Pretty.render 80 (prettyTop mempty term)
    cenv <- RT.compilationEnv cl term -- in `m`
    mmap <- newMVar
      ( Map.fromList [("stdin", stdin), ("stdout", stdout), ("stderr", stderr)]
      , Map.empty
      )
    RT.RDone result <- RT.run (handleIO' $ S mmap)
                              cenv
                              (IR.compile cenv $ Term.amap (const ()) term)
    decompiled <- IR.decompile result
    pure decompiled
