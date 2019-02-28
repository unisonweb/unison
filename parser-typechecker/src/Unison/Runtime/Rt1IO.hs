{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Unison.Runtime.Rt1IO where

import           Control.Lens
import           Control.Concurrent.MVar        ( MVar
                                                , modifyMVar_
                                                , readMVar
                                                )
import           Control.Monad.IO.Class         ( liftIO, MonadIO )
import           Control.Monad.Reader           ( ReaderT, ask, MonadReader )
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
                                                )
import           Unison.Symbol
import qualified Unison.Reference              as R
import           Unison.Parser                  ( Ann )
import qualified Unison.Runtime.Rt1            as RT
import qualified Unison.Runtime.IR             as IR
import qualified Unison.Codebase.CodeLookup    as CL
import           Unison.DataDeclaration
import qualified Unison.Var                    as Var
import           Unison.Var                     ( Var )
import qualified Unison.Hash                   as Hash

type GUID = Text
type IOState = MVar HandleMap

type UIO a = ReaderT S IO a
type HandleMap = Map GUID Handle

data S = S
  { _ioState :: IOState
  , _codeLookup :: CL.CodeLookup IO Symbol Ann
  , _size :: RT.Size
  , _stack :: RT.Stack
  }

makeLenses 'S

haskellMode :: Text -> IOMode
haskellMode mode = case mode of
  "IOMode.Read"      -> ReadMode
  "IOMode.Write"     -> WriteMode
  "IOMode.Append"    -> AppendMode
  "IOMode.ReadWrite" -> ReadWriteMode
  _ -> error . Text.unpack $ "Unknown IO mode " <> mode

newUnisonHandle :: Handle -> UIO RT.Value
newUnisonHandle h = do
  t <- liftIO $ genText
  m <- view ioState
  liftIO . modifyMVar_ m $ pure . Map.insert t h
  pure $ IR.T t

getHaskellHandle :: Text -> UIO (Maybe Handle)
getHaskellHandle h = do
  m <- view ioState
  v <- liftIO $ readMVar m
  pure $ Map.lookup h v

atText :: (MonadIO m, MonadReader S m) => RT.Z -> m Text
atText z = ask >>= \t -> liftIO $ RT.att (view size t) z (view stack t)

atData
  :: (MonadIO m, MonadReader S m)
  => RT.Z
  -> m (R.Reference, IR.ConstructorId, [RT.Value])
atData z = ask >>= \t -> liftIO $ RT.atd (view size t) z (view stack t)

constructorName :: R.Id -> IR.ConstructorId -> UIO Text
constructorName hash cid = do
  cl <- view codeLookup
  liftIO $ constructorName' cl hash cid

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

-- TODO: Put the actual hashes of these types in here
ioHash :: R.Id
ioHash = R.Id (Hash.unsafeFromBase58 "abracadabra") 0 1

ioModeHash :: R.Id
ioModeHash = R.Id (Hash.unsafeFromBase58 "abracadabra1") 0 1

handleIO :: IR.ConstructorId -> [RT.Z] -> UIO RT.Value
handleIO cid = (constructorName ioHash cid >>=) . flip go
 where
  go "IO.openFile" [filePath, ioMode] = do
    fp           <- atText filePath
    (_, mode, _) <- atData ioMode
    n            <- constructorName ioModeHash mode
    h            <- liftIO . openFile (Text.unpack fp) $ haskellMode n
    newUnisonHandle h
  go "IO.closeFile" [handle] = do
    h  <- atText handle
    hh <- getHaskellHandle h
    liftIO $ maybe (fail . Text.unpack $ "Missing file handle " <> h) hClose hh
    pure IR.unit
  go _ _ = undefined
