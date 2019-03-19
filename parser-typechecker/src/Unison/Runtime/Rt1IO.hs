{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Runtime.Rt1IO where

import           Control.Exception              ( try, throwIO, Exception )
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
import           Data.Functor                   ( void )
import           Data.GUID                      ( genText )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import           Data.Text                     as Text
import           System.IO                      ( Handle
                                                , IOMode(..)
                                                , openFile
                                                , hClose
                                                , hPutStr
                                                , stdin
                                                , stdout
                                                , stderr
                                                , hIsEOF
                                                )
import qualified System.IO.Error               as SysError
import           Type.Reflection                ( Typeable )
import qualified Unison.Codebase.CodeLookup    as CL
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
type IOState = MVar HandleMap

type UIO ann a = ExceptT IOError (ReaderT (S ann) IO) a
type HandleMap = Map GUID Handle

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
  liftIO . modifyMVar_ m $ pure . Map.insert t h
  pure $ IR.T t

deleteUnisonHandle :: Text -> UIO a ()
deleteUnisonHandle h = do
  m <- view ioState
  liftIO . modifyMVar_ m $ pure . Map.delete h

getHaskellHandle :: Text -> UIO a (Maybe Handle)
getHaskellHandle h = do
  m <- view ioState
  v <- liftIO $ readMVar m
  pure $ Map.lookup h v

constructLeft :: RT.Value -> RT.Value
constructLeft v = IR.Data IOSrc.eitherReference IOSrc.eitherLeftId [v]

constructRight :: RT.Value -> RT.Value
constructRight v = IR.Data IOSrc.eitherReference IOSrc.eitherRightId [v]

constructSome :: RT.Value -> RT.Value
constructSome v = IR.Data IOSrc.optionReference IOSrc.someId [v]

constructNone :: RT.Value
constructNone = IR.Data IOSrc.optionReference IOSrc.noneId []

convertMaybe :: Maybe (RT.Value) -> RT.Value
convertMaybe Nothing = constructNone
convertMaybe (Just v) = constructSome v

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

swallow :: (a -> IO ()) -> Maybe a -> UIO ann ()
swallow f = reraiseIO . maybe (pure ()) f

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
    swallow hClose hh
    deleteUnisonHandle handle
    pure IR.unit
  go "IO.putText" [IR.Data _ 0 [IR.T handle], IR.T string] = do
    hh <- getHaskellHandle handle
    swallow (`hPutStr` Text.unpack string) hh
    pure IR.unit
  go "IO.isFileEOF" [IR.Data _ 0 [IR.T handle]] = do
    hh       <- getHaskellHandle handle
    isClosed <- case hh of
      Just h  -> reraiseIO $ hIsEOF h
      Nothing -> throwError $ SysError.mkIOError
        SysError.illegalOperationErrorType
        "handle is closed"
        Nothing
        Nothing
    pure $ IR.B isClosed
  go a b =
    error
      $  "IO handler called with cid "
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
      $ Map.fromList [("stdin", stdin), ("stdout", stdout), ("stderr", stderr)]
    RT.RDone result <- RT.run (handleIO' $ S mmap)
                              cenv
                              (IR.compile cenv $ Term.amap (const ()) term)
    decompiled <- IR.decompile result
    pure decompiled
