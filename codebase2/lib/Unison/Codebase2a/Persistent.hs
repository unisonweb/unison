{-
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Control.Monad.IO.Unlift
import           Data.Text
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Conduit

share [mkPersist sqlSettings, mkSave "entityDefs"] [persistLowerCase|

Hash
  base32 Text
  HashHash base32


HashObject
  hashId HashId
  objectId ObjectId
  HashObjectHash hashId

Object
  primaryHashId HashId
  type ObjectType
  bytes ByteString
  ObjectPrimaryHash primaryHashId

Builtin
  name Text
  BuiltinName name

Derived
  hashId HashId
  componentIndex Int
  Derived hashId componentIndex

+Reference
  builtin BuiltinId
  derived DerivedId

Constructor
  reference ReferenceId
  constructorIndex Int
  constructorType ConstructorType
  Constructor reference constructorIndex

+Referent
  ref ReferenceId
  con ConstructorId
  deriving Show

Causal
  selfId HashId
  valueId HashId
  Causal selfId

CausalParent
  causalId CausalId
  parentId CausalId
  CausalParent causalId parentId
|]

runSqlite' :: (MonadUnliftIO m)
  => Text -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runSqlite' = runSqlite

-- Welcome to the Cargo Cult.

main :: IO ()
main = runSqlite' ":memory:" $ do
    -- runMigration $ migrate entityDefs $ entityDef (Nothing :: Maybe Person)
    michaelId <- insert $ Person "Michael" $ Just 26
    michael <- get michaelId
    liftIO $ print michael

-}
