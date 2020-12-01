module Unison.Server.Endpoints.GetDefinitions where

type DefinitionsAPI =
  "getDefinition" :> QueryParam "refs" [UnisonHash]
  :> Get '[JSON] [Definition]

data Definition = Definition { definitionHash :: UnisonHash
                             , definition :: SyntaxText' ShortHash
                             }
                             deriving (Generic, Show)

instance ToParam (QueryParam "refs" [Text]) where
  toParam _ = DocQueryParam
    "refs" [] ("A list of fully qualified names, hash-qualified names, " <>
    "or hashes.")

instance ToJSON Definition

deriving instance ToSchema Definition

serveDefinitions
  :: Var v
  => Codebase IO v Ann
  -> Maybe HashQualifiedName
  -> [HashQualifiedName]
  -> Maybe Width
  -> Handler [Definition]
serveDefinitions codebase relativePath width hashes = do
  hs       <- parseHashes hqns
  rel      <- parsePath $ Text.unpack relativePath
  termRefs <-
    liftIO $ (>>= toList) <$> traverse Codebase.termReferencesByPrefix hs
  terms   <- liftIO $ (>>= toList) <$> traverse getTerm termRefs
  gotRoot <- liftIO $ Codebase.getRootBranch codebase
  root    <- errFromEither rootBranchError gotRoot
  let ppe = Backend.basicSuffixifiedNames hashLength root (fromMaybe root rel)
  pure $ formatTerm ppe (mayDefault width) <$> terms
 where
  parsePath p = errFromEither (`badNamespace` p) $ Path.parsePath' p
  parseHashes = traverse (parseHQN >=> extractHash)
  parseHQN    = errFromMaybe (badHQN hqn) . HQ.fromText
  extractHash = \case
    HQ.HashOnly h        -> h
    HQ.NameOnly n        -> nameOnlyNotSupported
    HQ.HashQualified _ h -> h
  nameOnlyNotSupported = throwError $ err400
    { errBody = "This server does not yet support getting definitions by name."
    }
