-- | @project.create@ input handler
module Unison.Codebase.Editor.HandleInput.ProjectCreate
  ( projectCreate,
  )
where

import Control.Lens (over, (^.))
import Control.Monad.Reader (ask)
import Data.Map.Strict qualified as Map
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
import Unison.Cli.Share.Projects qualified as Share
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.HandleInput.Pull qualified as Pull
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (NameSegment))
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectName)
import Unison.Share.API.Hash qualified as Share.API
import Unison.Sqlite qualified as Sqlite
import Unison.Sync.Common qualified as Sync.Common
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
projectCreate :: Bool -> Maybe ProjectName -> Cli ()
projectCreate tryDownloadingBase maybeProjectName = do
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
        Cli.runTransactionWithRollback \rollback -> do
          Queries.projectExistsByName projectName >>= \case
            False -> do
              insertProjectAndBranch projectId projectName branchId branchName
              pure projectName
            True -> rollback (Output.ProjectNameAlreadyExists projectName)

  let path = projectBranchPath ProjectAndBranch {project = projectId, branch = branchId}
  Cli.respond (Output.CreatedProject (isNothing maybeProjectName) projectName)
  Cli.cd path

  maybeBaseLatestReleaseBranchObject <-
    if tryDownloadingBase
      then do
        Cli.respond Output.FetchingLatestReleaseOfBase

        -- Make an effort to pull the latest release of base, which can go wrong in a number of ways, the most likely of
        -- which is that the user is offline.
        maybeBaseLatestReleaseBranchObject <-
          Cli.label \done -> do
            baseProject <-
              Share.getProjectByName' (unsafeFrom @Text "@unison/base") >>= \case
                Right (Just baseProject) -> pure baseProject
                _ -> done Nothing
            ver <- baseProject ^. #latestRelease & onNothing (done Nothing)
            let baseProjectId = baseProject ^. #projectId
            let baseLatestReleaseBranchName = unsafeFrom @Text ("releases/" <> into @Text ver)
            response <-
              Share.getProjectBranchByName' Share.NoSquashedHead (ProjectAndBranch baseProjectId baseLatestReleaseBranchName)
                & onLeftM \_err -> done Nothing
            baseLatestReleaseBranch <-
              case response of
                Share.GetProjectBranchResponseBranchNotFound -> done Nothing
                Share.GetProjectBranchResponseProjectNotFound -> done Nothing
                Share.GetProjectBranchResponseSuccess branch -> pure branch
            let useSquashed = False
            Pull.downloadShareProjectBranch useSquashed baseLatestReleaseBranch
            Cli.Env {codebase} <- ask
            baseLatestReleaseBranchObject <-
              liftIO $
                Codebase.expectBranchForHash
                  codebase
                  (Sync.Common.hash32ToCausalHash (Share.API.hashJWTHash (baseLatestReleaseBranch ^. #branchHead)))
            pure (Just baseLatestReleaseBranchObject)
        when (isNothing maybeBaseLatestReleaseBranchObject) do
          Cli.respond Output.FailedToFetchLatestReleaseOfBase
        pure maybeBaseLatestReleaseBranchObject
      else pure Nothing

  let projectBranchObject =
        case maybeBaseLatestReleaseBranchObject of
          Nothing -> Branch.empty0
          Just baseLatestReleaseBranchObject ->
            let -- lib.base
                projectBranchLibBaseObject =
                  over
                    Branch.children
                    (Map.insert (NameSegment "base") baseLatestReleaseBranchObject)
                    Branch.empty0
                projectBranchLibObject = Branch.cons projectBranchLibBaseObject Branch.empty
             in over
                  Branch.children
                  (Map.insert Name.libSegment projectBranchLibObject)
                  Branch.empty0

  Cli.stepAt reflogDescription (Path.unabsolute path, const projectBranchObject)

  Cli.respond Output.HappyCoding
  where
    reflogDescription =
      case maybeProjectName of
        Nothing -> "project.create"
        Just projectName -> "project.create " <> into @Text projectName

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
        [ "adept",
          "adorable",
          "ambitious",
          "beautiful",
          "brave",
          "brilliant",
          "courageous",
          "charming",
          "dazzling",
          "delightful",
          "determined",
          "devoted",
          "elegant",
          "enchanting",
          "energetic",
          "engaging",
          "excited",
          "fantastic",
          "fortuitous",
          "friendly",
          "gentle",
          "helpful",
          "heartwarming",
          "hilarious",
          "humorous",
          "incredible",
          "imaginative",
          "innocent",
          "insightful",
          "jolly",
          "joyous",
          "kind",
          "lucky",
          "magnificent",
          "marvelous",
          "nice",
          "outstanding",
          "patient",
          "philosophical",
          "pleasant",
          "proficient",
          "quiet",
          "relaxed",
          "resourceful",
          "responsible",
          "silly",
          "sincere",
          "sensible",
          "sparkling",
          "spectacular",
          "spellbinding",
          "stellar",
          "thoughtful",
          "useful",
          "vibrant",
          "warm-hearted",
          "witty",
          "wondrous",
          "zestful"
          ]
      noun <-
        [ "alpaca",
          "armadillo",
          "axolotl",
          "badger",
          "blobfish",
          "bobcat",
          "camel",
          "capybara",
          "caracal",
          "cheetah",
          "chameleon",
          "chinchilla",
          "chipmunk",
          "donkey",
          "dormouse",
          "earwig",
          "egret",
          "elk",
          "ferret",
          "fennec",
          "fox",
          "frog",
          "gecko",
          "gerbil",
          "gibbon",
          "giraffe",
          "hamster",
          "hedgehog",
          "herron",
          "hippo",
          "ibis",
          "jaguar",
          "kangaroo",
          "kiwi",
          "koala",
          "ladybug",
          "lemur",
          "leopard",
          "lizard",
          "llama",
          "mallard",
          "marmot",
          "mole",
          "moonrat",
          "moose",
          "mouse",
          "narwhal",
          "ocelot",
          "ostrich",
          "otter",
          "owl",
          "panda",
          "pangolin",
          "penguin",
          "platypus",
          "polecat",
          "porcupine",
          "possum",
          "puffin",
          "quahog",
          "racoon",
          "reindeer",
          "rhino",
          "seahorse",
          "seal",
          "serval",
          "shrew",
          "sloth",
          "starling",
          "tapir",
          "tiger",
          "toad",
          "toucan",
          "turkey",
          "turtle",
          "urchin",
          "vole",
          "walrus",
          "wallaby",
          "wallaroo",
          "weasel",
          "woodchuck",
          "wolverine",
          "wombat",
          "yak",
          "zebra"
          ]

      pure (adjective <> "-" <> noun)

  let namesWithNumbers = do
        n <- [(2 :: Int) ..]
        name <- baseNames
        pure (name <> "-" <> Text.pack (show n))

  pure (map (unsafeFrom @Text) (baseNames ++ namesWithNumbers))
