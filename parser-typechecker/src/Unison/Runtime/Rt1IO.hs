{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Runtime.Rt1IO where

import           Control.Exception              ( try
                                                , IOException
                                                )
import           Control.Concurrent.MVar        ( MVar
                                                , modifyMVar_
                                                , readMVar
                                                , newMVar
                                                )
import           Control.Lens
import           Control.Monad.Trans            ( lift )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                )
import           Control.Monad.Except           ( ExceptT(..)
                                                , runExceptT
                                                )
import           Data.Functor                   ( void )
import           Data.GUID                      ( genText )
import           Data.List                      ( genericIndex )
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
                                                )
import           Unison.Symbol
import qualified Unison.Reference              as R
import qualified Unison.Runtime.Rt1            as RT
import qualified Unison.Runtime.IR             as IR
import qualified Unison.Term                   as Term
import qualified Unison.Codebase.CodeLookup    as CL
import           Unison.DataDeclaration
import qualified Unison.Var                    as Var
import           Unison.Var                     ( Var )
-- import Debug.Trace
-- import qualified Unison.Util.Pretty            as Pretty
-- import           Unison.TermPrinter             ( prettyTop )
import           Unison.Codebase.Runtime        ( Runtime(Runtime) )
import qualified Unison.UnisonFile             as UF
import qualified Unison.Runtime.IOSource       as IOSrc

type GUID = Text
type IOState = MVar HandleMap

type UIO ann a = ExceptT IOException (ReaderT (S ann) IO) a
type HandleMap = Map GUID Handle

data S a = S
  { _ioState :: IOState
  , _codeLookup :: CL.CodeLookup IO Symbol a
  }

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

constructorName :: R.Id -> IR.ConstructorId -> UIO a Text
constructorName hash cid = do
  cl <- view codeLookup
  lift . lift $ constructorName' cl hash cid

constructorName'
  :: (Var v, Monad m)
  => CL.CodeLookup m v a
  -> R.Id
  -> IR.ConstructorId
  -> m Text
constructorName' cl hash cid = do
  mayDecl <- CL.getTypeDeclaration cl hash
  case mayDecl of
    Nothing -> fail $ "Unknown type: " <> show hash <> " " <> show cid
    Just (Left (EffectDeclaration dd)) -> go dd
    Just (Right dd) -> go dd
 where
  go (DataDeclaration _ _ ctors) =
    pure . Var.name $ view _2 $ genericIndex ctors cid

handleIO' :: S a -> R.Reference -> IR.ConstructorId -> [RT.Value] -> IO RT.Value
handleIO' s rid cid vs = case rid of
  R.DerivedId x | x == IOSrc.ioHash -> flip runReaderT s $ do
    ev <- runExceptT $ handleIO cid vs
    case ev of
      Left  e -> pure $ Data IOSrc.eitherReference
      Right v -> undefined
  _ -> fail $ "This ability is not an I/O ability: " <> show rid

reraiseIO :: IO a -> UIO x a
reraiseIO a = ExceptT . lift $ try @IOException $ liftIO a

handleIO :: IR.ConstructorId -> [RT.Value] -> UIO a RT.Value
handleIO cid args = do
  cname <- constructorName IOSrc.ioHash cid
  go cname args
 where
  go "IO.openFile" [IR.T filePath, IR.Data _ mode _] = do
    n <- constructorName IOSrc.ioModeHash mode
    h <- reraiseIO . openFile (Text.unpack filePath) $ haskellMode n
    newUnisonHandle h
  go "IO.closeFile" [IR.T handle] = do
    hh <- getHaskellHandle handle
    reraiseIO $ maybe (pure ()) hClose hh
    deleteUnisonHandle handle
    pure IR.unit
  go "IO.putText" [IR.Data _ _ [IR.T handle], IR.T string] = do
    hh <- getHaskellHandle handle
    case hh of
      Just h  -> reraiseIO . hPutStr h $ Text.unpack string
      Nothing -> pure ()
    pure IR.unit
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
    :: CL.CodeLookup IO Symbol () -> Term.Term Symbol -> IO (Term.Term Symbol)
  eval cl' term = do
    let
      cl =
        void (CL.fromUnisonFile $ UF.discardTypes IOSrc.typecheckedFile) <> cl'
    -- traceM $ Pretty.render 80 (prettyTop mempty term)
    cenv <- RT.compilationEnv cl term -- in `m`
    mmap <- newMVar
      $ Map.fromList [("stdin", stdin), ("stdout", stdout), ("stderr", stderr)]
    RT.RDone result <- RT.run (handleIO' $ S mmap cl)
                              cenv
                              (IR.compile cenv $ Term.amap (const ()) term)
    decompiled <- IR.decompile result
    pure decompiled
