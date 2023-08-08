module Unison.Codebase.Editor.HandleInput.UI (openUI) where

import Control.Lens qualified as Lens
import Control.Monad.Reader (ask)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.Branch.Type qualified as V2Branch
import U.Codebase.Reference qualified as V2 (Reference)
import U.Codebase.Referent qualified as V2 (Referent)
import U.Codebase.Referent qualified as V2.Referent
import U.Codebase.Sqlite.Project qualified as Project
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch qualified as ProjectBranch
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as Project
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Conversions qualified as Conversions
import Unison.ConstructorType qualified as ConstructorType
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.Project (ProjectAndBranch)
import Unison.Project.Util (projectBranchPath)
import Unison.Referent qualified as Referent
import Unison.Server.CodebaseServer qualified as Server
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Web.Browser (openBrowser)

openUI :: Path.Path' -> Cli ()
openUI path' = do
  Cli.Env {serverBaseUrl} <- ask
  currentPath <- Cli.getCurrentPath
  let absPath = Path.resolve currentPath path'
  whenJust serverBaseUrl \url -> do
    Project.getProjectBranchForPath absPath >>= \case
      Nothing -> openUIForLooseCode url path'
      Just (projectBranch, pathWithinBranch) -> openUIForProject url projectBranch pathWithinBranch

openUIForProject :: Server.BaseUrl -> ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch -> Path.Path -> Cli ()
openUIForProject url projectAndBranch pathFromProjectRoot = do
  mayDefinitionRef <- getDefinitionRef
  let projectBranchNames = bimap Project.name ProjectBranch.name projectAndBranch
  _success <- liftIO . openBrowser . Text.unpack $ Server.urlFor (Server.ProjectBranchUI projectBranchNames mayDefinitionRef) url
  pure ()
  where
    pathToBranchFromCodebaseRoot :: Path.Absolute
    pathToBranchFromCodebaseRoot = projectBranchPath (bimap Project.projectId ProjectBranch.branchId projectAndBranch)
    -- If the provided ui path matches a definition, find it.
    getDefinitionRef :: Cli (Maybe (Server.DefinitionReference))
    getDefinitionRef = runMaybeT $ do
      Cli.Env {codebase} <- lift ask
      let absPathToDefinition = Path.unabsolute $ Path.resolve pathToBranchFromCodebaseRoot (Path.Relative pathFromProjectRoot)
      (pathToDefinitionNamespace, _nameSeg) <- hoistMaybe $ Lens.unsnoc absPathToDefinition
      namespaceBranch <- lift $ Cli.runTransaction (Codebase.getShallowBranchAtPath pathToDefinitionNamespace Nothing)
      let fqn = Path.unsafeToName pathFromProjectRoot
      def <- MaybeT $ getTermOrTypeRef codebase namespaceBranch fqn
      pure def

getTermOrTypeRef :: Codebase m Symbol Ann -> V2Branch.Branch n -> Name -> Cli (Maybe Server.DefinitionReference)
getTermOrTypeRef codebase namespaceBranch fqn = runMaybeT $ do
  let nameSeg = Name.lastSegment fqn
  let terms = do
        matchingTerms <- hoistMaybe $ Map.lookup nameSeg (V2Branch.terms namespaceBranch)
        oneTerm <- hoistMaybe $ Set.lookupMin $ Map.keysSet matchingTerms
        lift $ Cli.runTransaction (toTermReference codebase fqn oneTerm)
  let types = do
        matchingTypes <- hoistMaybe $ Map.lookup nameSeg (V2Branch.types namespaceBranch)
        oneType <- hoistMaybe $ Set.lookupMin $ Map.keysSet matchingTypes
        pure (toTypeReference fqn oneType)
  terms <|> types

openUIForLooseCode :: Server.BaseUrl -> Path.Path' -> Cli ()
openUIForLooseCode url path' = do
  Cli.Env {codebase} <- ask
  (perspective, definitionRef) <- getUIUrlParts codebase

  _success <- liftIO . openBrowser . Text.unpack $ Server.urlFor (Server.LooseCodeUI perspective definitionRef) url
  pure ()
  where
    getUIUrlParts :: Codebase m Symbol Ann -> Cli (Path.Absolute, Maybe (Server.DefinitionReference))
    getUIUrlParts codebase = do
      currentPath <- Cli.getCurrentPath
      let absPath = Path.resolve currentPath path'
      let name = Path.unsafeToName $ Path.fromPath' path'
      let perspective =
            if Path.isAbsolute path'
              then Path.absoluteEmpty
              else currentPath
      case Lens.unsnoc absPath of
        Just (abs, _nameSeg) -> do
          namespaceBranch <-
            Cli.runTransaction
              (Codebase.getShallowBranchAtPath (Path.unabsolute abs) Nothing)
          mayDefRef <- getTermOrTypeRef codebase namespaceBranch name
          case mayDefRef of
            Nothing -> pure (absPath, Nothing)
            Just defRef -> pure (perspective, Just defRef)
        Nothing ->
          pure (absPath, Nothing)

toTypeReference :: Name -> V2.Reference -> Server.DefinitionReference
toTypeReference name reference =
  Server.TypeReference $
    HQ.fromNamedReference name (Conversions.reference2to1 reference)

toTermReference :: Codebase m Symbol Ann -> Name -> V2.Referent -> Sqlite.Transaction Server.DefinitionReference
toTermReference codebase name referent = do
  case referent of
    V2.Referent.Ref reference ->
      pure $
        Server.TermReference $
          HQ.fromNamedReference name (Conversions.reference2to1 reference)
    V2.Referent.Con _ _ -> do
      v1Referent <- Conversions.referent2to1 (Codebase.getDeclType codebase) referent
      let hq = HQ.fromNamedReferent name v1Referent

      pure case v1Referent of
        Referent.Con _ ConstructorType.Data ->
          Server.DataConstructorReference hq
        Referent.Con _ ConstructorType.Effect ->
          Server.AbilityConstructorReference hq
        Referent.Ref _ -> error "Impossible! *twirls mustache*"
