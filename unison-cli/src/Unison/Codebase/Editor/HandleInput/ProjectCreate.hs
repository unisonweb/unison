-- | @project.create@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectCreate
  ( projectCreate,
  )
where

import Data.Text qualified as Text
import Data.UUID.V4 qualified as UUID
import System.Random.Shuffle qualified as RandomShuffle
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli (stepAt)
import Unison.Cli.ProjectUtils (projectBranchPath)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Sqlite qualified as Sqlite
import Witch (unsafeFrom)

-- | Create a new project.
--
-- 1. If a project already exists with the given name, bail.
--
-- 2. Otherwise, create a scaffold out a new project with a "main" branch, and add it to the namespace (at a magic
--    location that the user isn't supposed to look at).
--
-- Big danger: we first commit the project identity and metadata (like its name) to the codebase, then manipulate our
-- in-memory namespace and flush its contents out in a separate transaction. This means that if lightning strikes at the
-- wrong time, we'll be in an inconsistent state.
--
-- This could be fixed in a few different ways:
--
--   1. Make a better `stepAt` helper that can mutate the namespace in a transaction.
--
--   2. Add more code to detect the inconsistency and work around it. For example, if we ever see that a project id
--      exists in the codebase but not at its corresponding place in the namespace, we could consider it garbage and
--      delete it. Then, any user who tried to create a project called "foo" shortly before getting hit by lightning
--      could simply try creating "foo" again later.
--
--   3. Don't store projects in the root namespace at all. We don't even want them there, it's just a little too
--      convenient because *not* storing them in the root namespace would require a lot of reworking and rewriting. We'd
--      rather hit some shorter-term project milestones and clean our mess up Later (TM).
--
-- For now, it doesn't seem worth it to do (1) or (2), since we want to do (3) eventually, and we'd rather not waste too
-- much time getting everything perfectly correct before we get there.
projectCreate :: Maybe ProjectName -> Cli ()
projectCreate maybeProjectName = do
  projectId <- liftIO (ProjectId <$> UUID.nextRandom)
  branchId <- liftIO (ProjectBranchId <$> UUID.nextRandom)

  let branchName = unsafeFrom @Text "main"

  projectName <-
    case maybeProjectName of
      Nothing -> do
        randomProjectNames <- liftIO generateRandomProjectNames
        Cli.runTransaction do
          let loop = \case
                [] -> error (reportBug "E066388" "project name supply is supposed to be infinite")
                projectName : projectNames ->
                  Queries.projectExistsByName projectName >>= \case
                    False -> do
                      insertProjectAndBranch projectId projectName branchId branchName
                      pure projectName
                    True -> loop projectNames
          loop randomProjectNames
      Just projectName -> do
        Cli.runEitherTransaction do
          Queries.projectExistsByName projectName >>= \case
            False -> do
              insertProjectAndBranch projectId projectName branchId branchName
              pure (Right projectName)
            True -> pure (Left (Output.ProjectNameAlreadyExists projectName))

  let path = projectBranchPath ProjectAndBranch {project = projectId, branch = branchId}
  Cli.stepAt "project.create" (Path.unabsolute path, const Branch.empty0)
  Cli.respond (Output.CreatedProject (isNothing maybeProjectName) projectName)
  Cli.cd path

insertProjectAndBranch :: ProjectId -> ProjectName -> ProjectBranchId -> ProjectBranchName -> Sqlite.Transaction ()
insertProjectAndBranch projectId projectName branchId branchName = do
  Queries.insertProject projectId projectName
  Queries.insertProjectBranch
    Sqlite.ProjectBranch
      { projectId,
        branchId,
        name = branchName,
        parentBranchId = Nothing
      }
  Queries.setMostRecentBranch projectId branchId

-- An infinite list of random project names that looks like
--
--   [
--     -- We have some reasonable amount of base names...
--     "happy-giraffe",   "happy-gorilla",   "silly-giraffe",   "silly-gorilla",
--
--     -- But if we need more, we just add append a number, and so on...
--     "happy-giraffe-2", "happy-gorilla-2", "silly-giraffe-2", "silly-gorilla-2",
--
--     ...
--   ]
--
-- It's in IO because the base supply (without numbers) gets shuffled.
generateRandomProjectNames :: IO [ProjectName]
generateRandomProjectNames = do
  baseNames <-
    RandomShuffle.shuffleM do
      adjective <-
        [ "adorable",
          "beautiful",
          "charming",
          "delightful",
          "excited",
          "friendly",
          "gentle",
          "helpful",
          "innocent",
          "jolly",
          "kind",
          "lucky",
          "magnificent",
          "nice",
          "outstanding",
          "pleasant",
          "quiet",
          "responsible",
          "silly",
          "thoughtful",
          "useful",
          "witty"
          ]
      noun <-
        [ "alpaca",
          "blobfish",
          "camel",
          "donkey",
          "earwig",
          "ferret",
          "gerbil",
          "hamster",
          "ibis",
          "jaguar",
          "koala",
          "lemur",
          "marmot",
          "narwhal",
          "ostrich",
          "puffin",
          "quahog",
          "reindeer",
          "seahorse",
          "turkey",
          "urchin",
          "vole",
          "walrus",
          "yak",
          "zebra"
          ]

      pure (adjective <> "-" <> noun)

  let namesWithNumbers = do
        n <- [(2 :: Int) ..]
        name <- baseNames
        pure (name <> "-" <> Text.pack (show n))

  pure (map (unsafeFrom @Text) (baseNames ++ namesWithNumbers))
