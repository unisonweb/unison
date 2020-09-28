{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Unison.Server.CodebaseServer where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Except           ( runExceptT )
import           Data.Aeson
import qualified Data.ByteString.Lazy          as LZ
import           Data.Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import           GHC.Generics
import           Servant.API
import           Servant.Server
import           Servant                        ( throwError )
import qualified Unison.Codebase               as Codebase
import           Unison.Codebase                ( Codebase )
import qualified Unison.HashQualified          as HQ
import qualified Unison.Codebase.Path          as Path
import qualified Unison.Codebase.Branch        as Branch
import           Unison.Name                    ( Name(..) )
import           Unison.Parser                  ( Ann )
import qualified Unison.Server.Backend         as Backend
import           Unison.Symbol                  ( Symbol(..) )

--import GHC.TypeLits
--import Network.Wai.Handler.Warp

type HashQualifiedName = Text

type Size = Int

type UnisonName = Text

type UnisonHash = Text

type NamespaceAPI
  = "list" :> QueryParam "namespace" HashQualifiedName :> Get '[JSON] [Backend.ShallowListEntry Symbol Ann]

type FooAPI = "foo" :> Get '[JSON] ()

type API = NamespaceAPI :<|> FooAPI

data NamespaceListing = NamespaceListing
  { namespaceListingName :: UnisonName
  , namespaceListingHash :: UnisonHash
  , namespaceListingChildren :: [NamespaceEntry]
  } deriving Generic

instance ToJSON NamespaceListing

data NamespaceEntry = NamespaceEntry
  { namespaceEntryObject :: NamespaceObject
  , namespaceEntryMetadata :: [NamedTerm]
  } deriving Generic

instance ToJSON NamespaceEntry

data NamespaceObject = Namespace NamedNamespace| Term NamedTerm | Type NamedType
  deriving Generic

instance ToJSON NamespaceObject

data NamedNamespace = NamedNamespace
  { namespaceName :: UnisonName
  , namespaceHash :: UnisonHash
  , namespaceSize :: Size
  } deriving Generic

instance ToJSON NamedNamespace

data NamedTerm = NamedTerm
  { termName :: HashQualifiedName
  , termHash :: UnisonHash
  , termType :: TypeExpression
  } deriving Generic

instance ToJSON NamedTerm

data NamedType = NamedType
  { typeName :: HashQualifiedName
  , typeHash :: UnisonHash
  , typeKind :: KindExpression
  } deriving Generic

instance ToJSON NamedType

data NamedPatch = NamedPatch
  { patchName :: HashQualifiedName
  , patchHash :: UnisonHash
  } deriving Generic

instance ToJSON NamedPatch

data TypeExpression = TypeExpression
  { expression :: Text
  , dependencies :: Map HashQualifiedName UnisonHash
  } deriving Generic

instance ToJSON TypeExpression

newtype KindExpression = KindExpression { kindExpressionText :: Text }
  deriving Generic

instance ToJSON KindExpression

munge :: Text -> LZ.ByteString
munge = LZ.fromStrict . Text.encodeUtf8

mungeShow :: Show s => s -> LZ.ByteString
mungeShow = mungeString . show

mungeString :: String -> LZ.ByteString
mungeString = munge . Text.pack

badHQN :: HashQualifiedName -> ServerError
badHQN hqn = err400
  { errBody = munge hqn
              <> " is not a well-formed name, hash, or hash-qualified name. "
              <> "I expected something like `foo`, `#abc123`, or `foo#abc123`."
  }

backendError :: Backend.BackendError -> ServerError
backendError = \case
  Backend.NoSuchNamespace n -> noSuchNamespace . Path.toText $ Path.unabsolute n
  Backend.BadRootBranch e -> rootBranchError e

rootBranchError :: Codebase.GetRootBranchError -> ServerError
rootBranchError rbe = err500
  { errBody = case rbe of
                Codebase.NoRootBranch -> "Couldn't identify a root namespace."
                Codebase.CouldntLoadRootBranch h ->
                  "Couldn't load root branch " <> mungeShow h
                Codebase.CouldntParseRootBranch h ->
                  "Couldn't parse root branch head " <> mungeShow h
  }

badNamespace :: String -> String -> ServerError
badNamespace err namespace = err400
  { errBody = "Malformed namespace: "
              <> mungeString namespace
              <> ". "
              <> mungeString err
  }

noSuchNamespace :: HashQualifiedName -> ServerError
noSuchNamespace namespace =
  err404 { errBody = "The namespace " <> munge namespace <> " does not exist." }

server :: Codebase IO Symbol Ann -> Server API
server codebase = serveNamespace :<|> foo
 where
  foo = pure ()
  serveNamespace
    :: Maybe HashQualifiedName -> Handler [Backend.ShallowListEntry Symbol Ann]
  serveNamespace hqn = case hqn of
    Nothing  -> undefined -- list the root
    -- parse client-specified hash-qualified name
    Just hqn -> case HQ.fromText hqn of
      Nothing -> throwError $ badHQN hqn
      -- Check if namespace path is present in client input
      -- by parsing client-specified namespace path
      Just (HQ.NameOnly (Name (Text.unpack -> n))) ->
        case Path.parsePath' n of
          Left  e     -> throwError $ badNamespace e n
          Right path' -> do
            -- get the root namespace of the codebase
            gotRoot <- liftIO $ Codebase.getRootBranch codebase
            case gotRoot of
              Left  e    -> throwError $ rootBranchError e
              Right root -> case Branch.getAt (Path.fromPath' path') root of
                Nothing                 -> throwError $ noSuchNamespace hqn
                Just (Branch.head -> _) -> do
                  let
                    p = either id (Path.Absolute . Path.unrelative)
                      $ Path.unPath' path'
                  ea <- liftIO . runExceptT $ Backend.findShallow codebase p
                  either (throwError . backendError) pure ea
      Just (HQ.HashOnly h       ) -> undefined h
          -- if hash present, look up branch by hash in codebase
      Just (HQ.HashQualified _ h) -> undefined h
            -- if hash present, look up branch by hash in codebase
        -- error if path not found
        -- gather the immediate children under the path
        -- list them out
