{-# OPTIONS_GHC -Wno-orphans #-}
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
import           Data.Maybe                     ( fromMaybe )
import           Data.Proxy
import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as Text
import qualified Data.Text.Lazy.Encoding       as Text
import           GHC.Generics
import           Network.Wai.Handler.Warp       ( run )
import           Network.HTTP.Types.Status      ( ok200 )
import           Network.Wai                    ( responseLBS )
import           Servant.Docs
import           Servant.API
import           Servant.Server
import           Servant                        ( throwError )
import qualified Unison.Codebase               as Codebase
import           Unison.Codebase                ( Codebase )
import qualified Unison.Codebase.Causal        as Causal
import qualified Unison.Hash                   as Hash
import qualified Unison.HashQualified          as HQ
import qualified Unison.HashQualified'         as HQ'
import qualified Unison.Codebase.Path          as Path
import qualified Unison.Codebase.Branch        as Branch
import           Unison.ConstructorType         ( ConstructorType )
import           Unison.Name                    ( Name(..) )
import qualified Unison.Name                   as Name
import qualified Unison.NameSegment            as NameSegment
import           Unison.Parser                  ( Ann )
import           Unison.Pattern                 ( SeqOp )
import qualified Unison.PrettyPrintEnv         as PPE
import qualified Unison.Reference              as Reference
import qualified Unison.Referent               as Referent
import qualified Unison.Server.Backend         as Backend
import           Unison.ShortHash               ( ShortHash )
import           Unison.Type                    ( Type )
import           Unison.Util.Pretty             ( render
                                                , Width
                                                )
import           Unison.Util.SyntaxText         ( SyntaxText' )
import qualified Unison.Util.SyntaxText        as SyntaxText
import           Unison.Var                     ( Var )
import qualified Unison.TypePrinter            as TypePrinter


--import GHC.TypeLits
--import Network.Wai.Handler.Warp

type HashQualifiedName = Text

type Size = Int

type UnisonName = Text

type UnisonHash = Text

type NamespaceAPI
  = "list" :> QueryParam "namespace" HashQualifiedName :> Get '[JSON] NamespaceListing

type UnisonAPI = NamespaceAPI :<|> Raw

instance ToParam (QueryParam "namespace" Text) where
  toParam _ = DocQueryParam
    "namespace"
    [".", ".base.List", "foo.bar"]
    "The fully qualified name of a namespace. The leading `.` is optional."
    Normal

instance ToSample NamespaceListing where
  toSamples _ =
    [ ( "When no value is provided for `namespace`, the root namespace `.` is listed by default"
      , NamespaceListing
        "."
        "gjlk0dna8dongct6lsd19d1o9hi5n642t8jttga5e81e91fviqjdffem0tlddj7ahodjo5"
        [Subnamespace $ NamedNamespace "base" 1244]
      )
    ]

docsBS :: LZ.ByteString
docsBS = mungeString . markdown $ docsWithIntros [intro] api
  where intro = DocIntro "Unison Codebase Manager API Server" []

data NamespaceListing = NamespaceListing
  { namespaceListingName :: UnisonName
  , namespaceListingHash :: UnisonHash
  , namespaceListingChildren :: [NamespaceObject]
  } deriving (Generic, Show)

instance ToJSON NamespaceListing

data NamespaceObject
  = Subnamespace NamedNamespace
  | TermObject NamedTerm
  | TypeObject NamedType
  | PatchObject NamedPatch
  deriving (Generic, Show)

instance ToJSON NamespaceObject

data NamedNamespace = NamedNamespace
  { namespaceName :: UnisonName
  , namespaceSize :: Size
  } deriving (Generic, Show)

instance ToJSON NamedNamespace

data NamedTerm = NamedTerm
  { termName :: HashQualifiedName
  , termHash :: UnisonHash
  , termType :: Maybe (SyntaxText' ShortHash)
  } deriving (Generic, Show)

instance ToJSON NamedTerm

data NamedType = NamedType
  { typeName :: HashQualifiedName
  , typeHash :: UnisonHash
  } deriving (Generic, Show)

instance ToJSON NamedType

data NamedPatch = NamedPatch
  { patchName :: HashQualifiedName
  } deriving (Generic, Show)

instance ToJSON NamedPatch

newtype KindExpression = KindExpression { kindExpressionText :: Text }
  deriving (Generic, Show)

instance ToJSON KindExpression

formatType
  :: Var v => PPE.PrettyPrintEnv -> Width -> Type v a -> SyntaxText' ShortHash
formatType ppe w =
  fmap (fmap Reference.toShortHash) . render w . TypePrinter.pretty0 ppe
                                                                     mempty
                                                                     (-1)

defaultWidth :: Width
defaultWidth = 80

mayDefault :: Maybe Width -> Width
mayDefault = fromMaybe defaultWidth

instance ToJSON Name
instance ToJSON ShortHash
instance ToJSON HQ.HashQualified
instance ToJSON ConstructorType
instance ToJSON SeqOp
instance ToJSON r => ToJSON (Referent.Referent' r)
instance ToJSON r => ToJSON (SyntaxText.Element r)
instance ToJSON r => ToJSON (SyntaxText' r)

backendListEntryToNamespaceObject
  :: Var v
  => PPE.PrettyPrintEnv
  -> Maybe Width
  -> Backend.ShallowListEntry v a
  -> NamespaceObject
backendListEntryToNamespaceObject ppe typeWidth = \case
  Backend.ShallowTermEntry r name mayType -> TermObject $ NamedTerm
    { termName = HQ'.toText name
    , termHash = Referent.toText r
    , termType = formatType ppe (mayDefault typeWidth) <$> mayType
    }
  Backend.ShallowTypeEntry r name -> TypeObject $ NamedType
    { typeName = HQ'.toText name
    , typeHash = Reference.toText r
    }
  Backend.ShallowBranchEntry name size -> Subnamespace $ NamedNamespace
    { namespaceName = NameSegment.toText name
    , namespaceSize = size
    }
  Backend.ShallowPatchEntry name ->
    PatchObject . NamedPatch $ NameSegment.toText name

munge :: Text -> LZ.ByteString
munge = Text.encodeUtf8 . Text.fromStrict

mungeShow :: Show s => s -> LZ.ByteString
mungeShow = mungeString . show

mungeString :: String -> LZ.ByteString
mungeString = Text.encodeUtf8 . Text.pack

badHQN :: HashQualifiedName -> ServerError
badHQN hqn = err400
  { errBody = Text.encodeUtf8 (Text.fromStrict hqn)
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
noSuchNamespace namespace = err404
  { errBody = "The namespace "
              <> munge namespace
              <> " does not exist."
  }

api :: Proxy UnisonAPI
api = Proxy

app :: Var v => Codebase IO v Ann -> Application
app codebase = serve api $ server codebase

start :: Var v => Codebase IO v Ann -> Int -> IO ()
start codebase port = run port $ app codebase

server :: Var v => Codebase IO v Ann -> Server UnisonAPI
server codebase = serveNamespace codebase :<|> Tagged serveDocs
 where
  serveDocs _ respond = respond $ responseLBS ok200 [plain] docsBS
  plain = ("Content-Type", "text/plain")

discard :: Applicative m => a -> m ()
discard = const $ pure ()

serveNamespace
  :: Var v
  => Codebase IO v Ann
  -> Maybe HashQualifiedName
  -> Handler NamespaceListing
serveNamespace codebase mayHQN = case mayHQN of
  Nothing  -> serveNamespace codebase $ Just "."
  Just hqn -> do
    parsedName <- parseHQN hqn
    case parsedName of
      HQ.NameOnly n -> do
        path'      <- parsePath $ Name.toString n
        gotRoot    <- liftIO $ Codebase.getRootBranch codebase
        root       <- errFromEither rootBranchError gotRoot
        hashLength <- liftIO $ Codebase.hashLength codebase
        let
          p = either id (Path.Absolute . Path.unrelative) $ Path.unPath' path'
          ppe =
            Backend.basicSuffixifiedNames hashLength root $ Path.fromPath' path'
        entries <- findShallow p
        pure
          . NamespaceListing
              (Name.toText n)
              (Hash.base32Hex . Causal.unRawHash $ Branch.headHash root)
          $ fmap (backendListEntryToNamespaceObject ppe Nothing) entries
      HQ.HashOnly _        -> hashOnlyNotSupported
      HQ.HashQualified _ _ -> hashQualifiedNotSupported
 where
  errFromMaybe e = maybe (throwError e) pure
  errFromEither f = either (throwError . f) pure
  parseHQN hqn = errFromMaybe (badHQN hqn) $ HQ.fromText hqn
  parsePath p = errFromEither (flip badNamespace p) $ Path.parsePath' p
  findShallow p = do
    ea <- liftIO . runExceptT $ Backend.findShallow codebase p
    errFromEither backendError ea
  hashOnlyNotSupported = throwError $ err400
    { errBody = "This server does not yet support searching namespaces by hash."
    }
  hashQualifiedNotSupported = throwError $ err400
    { errBody = "This server does not yet support searching namespaces by "
                  <> "hash-qualified name."
    }

