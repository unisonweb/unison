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
  Cli.Env {codebase} <- ask
  mayDefinitionRef <- getDefinitionRef codebase
  let projectBranchNames = bimap Project.name ProjectBranch.name projectAndBranch
  _success <- liftIO . openBrowser . Text.unpack $ Server.urlFor (Server.ProjectBranchUI projectBranchNames mayDefinitionRef) url
  pure ()
  where
    -- If the provided ui path matches a definition, find it.
    getDefinitionRef :: Codebase m Symbol Ann -> Cli (Maybe (Server.DefinitionReference))
    getDefinitionRef codebase = runMaybeT $ do
      (pathToDefinitionNamespace, _nameSeg) <- hoistMaybe $ Lens.unsnoc pathFromProjectRoot
      namespaceBranch <- lift $ Cli.runTransaction (Codebase.getShallowBranchAtPath pathToDefinitionNamespace Nothing)
      let fqn = Path.unsafeToName pathFromProjectRoot
      getTermOrTypeRef codebase namespaceBranch fqn

getTermOrTypeRef :: Codebase m Symbol Ann -> V2Branch.Branch n -> Name -> MaybeT Cli Server.DefinitionReference
getTermOrTypeRef codebase namespaceBranch fqn = do
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

      case Lens.unsnoc absPath of
        Just (abs, nameSeg) -> do
          namespaceBranch <-
            Cli.runTransaction
              (Codebase.getShallowBranchAtPath (Path.unabsolute abs) Nothing)

          let terms = maybe Set.empty Map.keysSet (Map.lookup nameSeg (V2Branch.terms namespaceBranch))
          let types = maybe Set.empty Map.keysSet (Map.lookup nameSeg (V2Branch.types namespaceBranch))

          -- Only safe to force in toTypeReference and toTermReference
          case (Set.lookupMin terms, Set.lookupMin types) of
            (Just te, _) -> do
              let name = Path.unsafeToName $ Path.fromPath' path'
              defRef <- Cli.runTransaction (toTermReference codebase name te)

              if Path.isAbsolute path'
                then pure (Path.absoluteEmpty, Just defRef)
                else pure (currentPath, Just defRef)
            (Nothing, Just ty) ->
              let name = Path.unsafeToName $ Path.fromPath' path'
                  defRef = toTypeReference name ty
               in if Path.isAbsolute path'
                    then pure (Path.absoluteEmpty, Just defRef)
                    else pure (currentPath, Just defRef)
            -- Catch all that uses the absPath to build the perspective.
            -- Also catches the case where a namespace arg was given.
            (Nothing, Nothing) ->
              pure (absPath, Nothing)
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
