{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unison.Server.Endpoints.ListNamespace where

import           Control.Error                  ( runExceptT )
import           Data.Aeson                     ( ToJSON )
import           Data.OpenApi                   ( ToSchema )
import           Servant                        ( Get
                                                , JSON
                                                , QueryParam
                                                , ServerError(errBody)
                                                , err400
                                                , throwError
                                                , (:>)
                                                )
import           Servant.Docs                   ( DocQueryParam(..)
                                                , ParamKind(Normal)
                                                , ToParam(..)
                                                , ToSample(..)
                                                )
import           Servant.OpenApi                ( )
import           Servant.Server                 ( Handler )
import           Unison.Prelude
import           Unison.Codebase                ( Codebase )
import qualified Unison.Codebase               as Codebase
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.Codebase.Causal        as Causal
import qualified Unison.Codebase.Path          as Path
import qualified Unison.Hash                   as Hash
import qualified Unison.HashQualified          as HQ
import qualified Unison.HashQualified'         as HQ'
import qualified Unison.Name                   as Name
import qualified Unison.NameSegment            as NameSegment
import           Unison.Parser                  ( Ann )
import qualified Unison.PrettyPrintEnv         as PPE
import qualified Unison.Reference              as Reference
import qualified Unison.Referent               as Referent
import qualified Unison.Server.Backend         as Backend
import           Unison.Server.Errors           ( backendError
                                                , badHQN
                                                , badNamespace
                                                , rootBranchError
                                                )
import           Unison.Server.Types            ( HashQualifiedName
                                                , Size
                                                , UnisonHash
                                                , UnisonName
                                                , mayDefault
                                                , formatType
                                                )
import           Unison.Util.Pretty             ( Width )
import           Unison.Var                     ( Var )
import qualified Unison.Codebase.ShortBranchHash
                                               as SBH
import qualified Unison.ShortHash              as ShortHash
import qualified Data.Text                     as Text
import           Unison.Server.Syntax           ( SyntaxText )

type NamespaceAPI =
  "list" :> QueryParam "namespace" HashQualifiedName
    :> Get '[JSON] NamespaceListing

instance ToParam (QueryParam "namespace" Text) where
  toParam _ =
    DocQueryParam
      "namespace"
      [".", ".base.List", "foo.bar"]
      "The fully qualified name of a namespace. The leading `.` is optional."
      Normal

instance ToSample NamespaceListing where
  toSamples _ =
    [ ( "When no value is provided for `namespace`, the root namespace `.` is "
        <> "listed by default"
      , NamespaceListing
        (Just ".")
        "#gjlk0dna8dongct6lsd19d1o9hi5n642t8jttga5e81e91fviqjdffem0tlddj7ahodjo5"
        [Subnamespace $ NamedNamespace "base" "#19d1o9hi5n642t8jttg" 1244]
      )
    ]

data NamespaceListing = NamespaceListing
  { namespaceListingName :: Maybe UnisonName,
    namespaceListingHash :: UnisonHash,
    namespaceListingChildren :: [NamespaceObject]
  }
  deriving (Generic, Show)

instance ToJSON NamespaceListing

deriving instance ToSchema NamespaceListing

data NamespaceObject
  = Subnamespace NamedNamespace
  | TermObject NamedTerm
  | TypeObject NamedType
  | PatchObject NamedPatch
  deriving (Generic, Show)

instance ToJSON NamespaceObject

deriving instance ToSchema NamespaceObject

data NamedNamespace = NamedNamespace
  { namespaceName :: UnisonName
  , namespaceHash :: UnisonHash
  , namespaceSize :: Size
  }
  deriving (Generic, Show)

instance ToJSON NamedNamespace

deriving instance ToSchema NamedNamespace

data NamedTerm = NamedTerm
  { termName :: HashQualifiedName
  , termHash :: UnisonHash
  , termType :: Maybe SyntaxText
  , termTag :: Maybe Backend.TermTag
  }
  deriving (Generic, Show)

instance ToJSON NamedTerm

deriving instance ToSchema NamedTerm

data NamedType = NamedType
  { typeName :: HashQualifiedName
  , typeHash :: UnisonHash
  , typeTag :: Backend.TypeTag
  }
  deriving (Generic, Show)

instance ToJSON NamedType

deriving instance ToSchema NamedType

newtype NamedPatch = NamedPatch { patchName :: HashQualifiedName }
  deriving (Generic, Show)

instance ToJSON NamedPatch

deriving instance ToSchema NamedPatch

newtype KindExpression = KindExpression {kindExpressionText :: Text}
  deriving (Generic, Show)

instance ToJSON KindExpression

deriving instance ToSchema KindExpression

instance ToJSON Backend.TermTag

deriving instance ToSchema Backend.TermTag

instance ToJSON Backend.TypeTag

deriving instance ToSchema Backend.TypeTag

backendListEntryToNamespaceObject
  :: Var v
  => PPE.PrettyPrintEnv
  -> Maybe Width
  -> Backend.ShallowListEntry v a
  -> NamespaceObject
backendListEntryToNamespaceObject ppe typeWidth = \case
  Backend.ShallowTermEntry r name mayType tag -> TermObject $ NamedTerm
    { termName = HQ'.toText name
    , termHash = Referent.toText r
    , termType = formatType ppe (mayDefault typeWidth) <$> mayType
    , termTag  = tag
    }
  Backend.ShallowTypeEntry r name tag -> TypeObject $ NamedType
    { typeName = HQ'.toText name
    , typeHash = Reference.toText r
    , typeTag  = tag
    }
  Backend.ShallowBranchEntry name hash size -> Subnamespace $ NamedNamespace
    { namespaceName = NameSegment.toText name
    , namespaceHash = "#" <> SBH.toText hash
    , namespaceSize = size
    }
  Backend.ShallowPatchEntry name ->
    PatchObject . NamedPatch $ NameSegment.toText name

serveNamespace
  :: Var v
  => Codebase IO v Ann
  -> Maybe HashQualifiedName
  -> Handler NamespaceListing
serveNamespace codebase mayHQN = case mayHQN of
  Nothing  -> serveNamespace codebase $ Just "."
  Just hqn -> do
    parsedName <- parseHQN hqn
    hashLength <- liftIO $ Codebase.hashLength codebase
    case parsedName of
      HQ.NameOnly n -> do
        path'      <- parsePath $ Name.toString n
        gotRoot    <- liftIO $ Codebase.getRootBranch codebase
        root       <- errFromEither rootBranchError gotRoot
        let
          p = either id (Path.Absolute . Path.unrelative) $ Path.unPath' path'
          ppe =
            Backend.basicSuffixifiedNames hashLength root $ Path.fromPath' path'
        entries <- findShallow p
        processEntries
          ppe
          (Just $ Name.toText n)
          (("#" <>) . Hash.base32Hex . Causal.unRawHash $ Branch.headHash root)
          entries
      HQ.HashOnly sh -> case SBH.fromText $ ShortHash.toText sh of
        Nothing ->
          throwError
            . badNamespace "Malformed branch hash."
            $ ShortHash.toString sh
        Just h -> doBackend $ do
          hash    <- Backend.expandShortBranchHash codebase h
          branch  <- Backend.resolveBranchHash (Just hash) codebase
          entries <- Backend.findShallowInBranch codebase branch
          let ppe = Backend.basicSuffixifiedNames hashLength branch mempty
              sbh = Text.pack . show $ SBH.fullFromHash hash
          processEntries ppe Nothing sbh entries
      HQ.HashQualified _ _ -> hashQualifiedNotSupported
 where
  errFromMaybe e = maybe (throwError e) pure
  errFromEither f = either (throwError . f) pure
  parseHQN hqn = errFromMaybe (badHQN hqn) $ HQ.fromText hqn
  parsePath p = errFromEither (`badNamespace` p) $ Path.parsePath' p
  doBackend a = do
    ea <- liftIO $ runExceptT a
    errFromEither backendError ea
  findShallow p = doBackend $ Backend.findShallow codebase p
  processEntries ppe name hash entries =
    pure . NamespaceListing name hash $ fmap
      (backendListEntryToNamespaceObject ppe Nothing)
      entries
  hashQualifiedNotSupported = throwError $ err400
    { errBody = "This server does not yet support searching namespaces by "
                  <> "hash-qualified name."
    }

