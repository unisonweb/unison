module Unison.CommandLine.FZFResolvers
  ( FZFResolver (..),
    IncludeLibFZF (..),
    definitionOptions,
    termDefinitionOptions,
    typeDefinitionOptions,
    namespaceOptions,
    projectDependencyResolver,
    projectNameOptions,
    projectBranchOptions,
    projectBranchOptionsWithinCurrentProject,
    fuzzySelectFromList,
    multiResolver,
    definitionResolver,
    typeDefinitionResolver,
    termDefinitionResolver,
    namespaceResolver,
    namespaceOrDefinitionResolver,
    projectAndOrBranchArg,
    projectOrBranchResolver,
    projectBranchResolver,
    projectBranchWithinCurrentProjectResolver,
    projectNameResolver,
    fuzzySelectHeader,
  )
where

import Control.Lens
import Data.List.Extra qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.Sqlite.Project as SqliteProject
import U.Codebase.Sqlite.Queries qualified as Q
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Path (Path, Path' (..))
import Unison.Codebase.Path qualified as Path
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Position qualified as Position
import Unison.Prelude
import Unison.Project.Util (ProjectContext (..))
import Unison.Symbol (Symbol)
import Unison.Syntax.HashQualified qualified as HQ (toText)
import Unison.Syntax.NameSegment qualified as NameSegment
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Relation qualified as Relation

type OptionFetcher = Codebase IO Symbol Ann -> ProjectContext -> Branch0 IO -> IO [Text]

data FZFResolver = FZFResolver
  { getOptions :: OptionFetcher
  }

instance Show FZFResolver where
  show _ = "<FZFResolver>"

-- | Whether to include things within lib in FZF results.
-- You will still be able to explicitly select things within lib.
data IncludeLibFZF
  = IncludeDepsFZF
  | ExcludeDepsFZF
  deriving stock (Eq, Ord, Show)

-- | Select a definition from the given branch.
-- Returned names will match the provided 'Position' type.
genericDefinitionOptions :: IncludeLibFZF -> Bool -> Bool -> OptionFetcher
genericDefinitionOptions includeLib includeTerms includeTypes _codebase _projCtx searchBranch0 = liftIO do
  let b = case includeLib of
        IncludeDepsFZF -> Branch.withoutTransitiveLibs searchBranch0
        ExcludeDepsFZF -> Branch.withoutLib searchBranch0
  let termsAndTypes =
        Monoid.whenM includeTerms Relation.dom (Names.hashQualifyTermsRelation (Relation.swap $ Branch.deepTerms b))
          <> Monoid.whenM includeTypes Relation.dom (Names.hashQualifyTypesRelation (Relation.swap $ Branch.deepTypes b))
  termsAndTypes
    & Set.toList
    & map (HQ.toText . fmap (Name.setPosition Position.Relative))
    & pure

-- | Select a definition from the given branch.
definitionOptions :: IncludeLibFZF -> OptionFetcher
definitionOptions includeLibFZF = genericDefinitionOptions includeLibFZF True True

-- | Select a term definition from the given branch.
-- Returned names will match the provided 'Position' type.
termDefinitionOptions :: IncludeLibFZF -> OptionFetcher
termDefinitionOptions includeLibFZF = genericDefinitionOptions includeLibFZF True False

-- | Select a type definition from the given branch.
-- Returned names will match the provided 'Position' type.
typeDefinitionOptions :: IncludeLibFZF -> OptionFetcher
typeDefinitionOptions includeLibFZF = genericDefinitionOptions includeLibFZF False True

-- | Select a namespace from the given branch.
-- Returned Path's will match the provided 'Position' type.
namespaceOptions :: OptionFetcher
namespaceOptions _codebase _projCtx searchBranch0 = do
  let intoPath' :: Path -> Path'
      intoPath' = Path' . Right . Path.Relative
  searchBranch0
    & Branch.deepPaths
    & Set.delete (Path.empty {- The current path just renders as an empty string which isn't a valid arg -})
    & Set.toList
    & map (Path.toText' . intoPath')
    & pure

-- | Lists all dependencies of the current project.
--
-- E.g. if the current project has `lib.base` and `lib.distributed`, it will list:
-- ["base", "distributed"]
projectDependencyOptions :: OptionFetcher
projectDependencyOptions _codebase _projCtx searchBranch0 = do
  searchBranch0
    & Branch.getAt0 (Path.singleton NameSegment.libSegment)
    & Branch.nonEmptyChildren
    & Map.keys
    & fmap NameSegment.toEscapedText
    & pure

-- | Select a namespace from the given branch.
-- Returned Path's will match the provided 'Position' type.
fuzzySelectFromList :: [Text] -> FZFResolver
fuzzySelectFromList options =
  (FZFResolver {getOptions = \_codebase _projCtx _branch -> pure options})

-- | Combine multiple option fetchers into one resolver.
multiResolver :: [OptionFetcher] -> FZFResolver
multiResolver resolvers =
  let getOptions :: Codebase IO Symbol Ann -> ProjectContext -> Branch0 IO -> IO [Text]
      getOptions codebase projCtx searchBranch0 = do
        List.nubOrd <$> foldMapM (\f -> f codebase projCtx searchBranch0) resolvers
   in (FZFResolver {getOptions})

definitionResolver :: IncludeLibFZF -> FZFResolver
definitionResolver includeLibFZF = FZFResolver {getOptions = definitionOptions includeLibFZF}

typeDefinitionResolver :: IncludeLibFZF -> FZFResolver
typeDefinitionResolver includeLibFZF = FZFResolver {getOptions = typeDefinitionOptions includeLibFZF}

termDefinitionResolver :: IncludeLibFZF -> FZFResolver
termDefinitionResolver includeLibFZF = FZFResolver {getOptions = termDefinitionOptions includeLibFZF}

namespaceResolver :: FZFResolver
namespaceResolver = FZFResolver {getOptions = namespaceOptions}

namespaceOrDefinitionResolver :: IncludeLibFZF -> FZFResolver
namespaceOrDefinitionResolver includeLibFZF = multiResolver [definitionOptions includeLibFZF, namespaceOptions]

projectDependencyResolver :: FZFResolver
projectDependencyResolver = FZFResolver {getOptions = projectDependencyOptions}

-- | A project name, branch name, or both.
projectAndOrBranchArg :: FZFResolver
projectAndOrBranchArg = multiResolver [projectBranchOptions, projectNameOptions]

projectOrBranchResolver :: FZFResolver
projectOrBranchResolver = multiResolver [projectBranchOptions, namespaceOptions]

projectBranchResolver :: FZFResolver
projectBranchResolver = FZFResolver {getOptions = projectBranchOptions}

projectBranchWithinCurrentProjectResolver :: FZFResolver
projectBranchWithinCurrentProjectResolver = FZFResolver {getOptions = projectBranchOptionsWithinCurrentProject}

projectNameResolver :: FZFResolver
projectNameResolver = FZFResolver {getOptions = projectNameOptions}

-- | All possible local project names
-- E.g. '@unison/base'
projectNameOptions :: OptionFetcher
projectNameOptions codebase _projCtx _searchBranch0 = do
  fmap (into @Text . SqliteProject.name) <$> Codebase.runTransaction codebase Q.loadAllProjects

-- | All possible local project/branch names.
-- E.g. '@unison/base/main'
projectBranchOptions :: OptionFetcher
projectBranchOptions codebase _projCtx _searchBranch0 = do
  Codebase.runTransaction codebase Q.loadAllProjectBranchNamePairs
    <&> fmap (into @Text . fst)

-- | All possible local branch names within the current project.
-- E.g. '@unison/base/main'
projectBranchOptionsWithinCurrentProject :: OptionFetcher
projectBranchOptionsWithinCurrentProject codebase projCtx _searchBranch0 = do
  case projCtx of
    LooseCodePath _ -> pure []
    ProjectBranchPath currentProjectId _projectBranchId _path -> do
      Codebase.runTransaction codebase (Q.loadAllProjectBranchesBeginningWith currentProjectId Nothing)
        <&> fmap (into @Text . snd)

-- | Exported from here just so the debug command and actual implementation can use the same
-- messaging.
--
-- >>> fuzzySelectHeader "definition to view"
-- "Select a definition to view:"
--
-- >>> fuzzySelectHeader "alias name"
-- "Select an alias name:"
fuzzySelectHeader :: Text -> Text
fuzzySelectHeader argDesc = "Select " <> aOrAn argDesc <> " " <> argDesc <> ":"
  where
    aOrAn :: Text -> Text
    aOrAn txt =
      Text.uncons txt & \case
        Just (c, _) | c `elem` ("aeiou" :: [Char]) -> "an"
        _ -> "a"
