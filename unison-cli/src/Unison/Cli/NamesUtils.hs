-- | Utilities that have to do with constructing names objects.
module Unison.Cli.NamesUtils
  ( projectRootNames,
    projectRootNamesWithoutTransitiveLibs,
    basicParseNames,
    basicPrettyPrintNamesA,
    displayNames,
    findHistoricalHQs,
    getBasicPrettyPrintNames,
    makeHistoricalParsingNames,
    makePrintNamesFromLabeled',
    makeShadowedPrintNamesFromHQ,
  )
where

import Control.Lens
import Control.Monad.Reader
import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Branch.Names.Cache qualified as NamesCache
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified qualified as HQ
import Unison.LabeledDependency (LabeledDependency)
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.NamesWithHistory (NamesWithHistory (..))
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Prelude
import Unison.Server.Backend qualified as Backend
import Unison.Syntax.Name qualified as Name (toString, unsafeFromString)
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.UnisonFile qualified as UF
import Unison.UnisonFile.Names qualified as UF
import Unison.Var (Var)

projectRootNames :: Cli Names
projectRootNames = do
  Cli.Env {codebase} <- ask
  causalHash <- Branch.headHash <$> Cli.getProjectRootBranch
  NamesCache.BranchNames {branchNames} <- liftIO $ NamesCache.expectNamesForBranch codebase causalHash
  pure branchNames

projectRootNamesWithoutTransitiveLibs :: Cli Names
projectRootNamesWithoutTransitiveLibs = do
  Cli.Env {codebase} <- ask
  causalHash <- Branch.headHash <$> Cli.getProjectRootBranch
  NamesCache.BranchNames {branchNamesWithoutTransitiveLibs} <- liftIO $ NamesCache.expectNamesForBranch codebase causalHash
  pure branchNamesWithoutTransitiveLibs

-- | This is kept around as an alias for backwards compatibility, but it's a bit imprecise
-- about the intent. In new code use a more specific combinator like 'projectRootNames' or
-- 'projectRootNamesWithoutTransitiveLibs' to be clear about exactly which names you want.
basicParseNames :: Cli Names
basicParseNames = projectRootNames

basicPrettyPrintNamesA :: Cli Names
basicPrettyPrintNamesA = snd <$> basicNames' Backend.AllNames

-- implementation detail of basicParseNames and basicPrettyPrintNames
basicNames' :: (Path -> Backend.NameScoping) -> Cli (Names, Names)
basicNames' nameScoping = do
  root' <- Cli.getRootBranch
  currentPath' <- Cli.getCurrentPath
  let (parse, pretty, _local) = Backend.namesForBranch root' (nameScoping $ Path.unabsolute currentPath')
  pure (parse, pretty)

-- | Produce a `Names` needed to display all the hashes used in the given file.
displayNames ::
  (Var v) =>
  TypecheckedUnisonFile v a ->
  Cli NamesWithHistory
displayNames unisonFile =
  -- voodoo
  makeShadowedPrintNamesFromLabeled
    (UF.termSignatureExternalLabeledDependencies unisonFile)
    (UF.typecheckedToNames unisonFile)

-- discards inputs that aren't hashqualified;
-- I'd enforce it with finer-grained types if we had them.
findHistoricalHQs :: Set (HQ.HashQualified Name) -> Cli Names
findHistoricalHQs lexedHQs0 = do
  root' <- Cli.getRootBranch
  curPath <- Cli.getCurrentPath
  let -- omg this nightmare name-to-path parsing code is littered everywhere.
      -- We need to refactor so that the absolute-ness of a name isn't represented
      -- by magical text combinations.
      -- Anyway, this function takes a name, tries to determine whether it is
      -- relative or absolute, and tries to return the corresponding name that is
      -- /relative/ to the Command.root.
      preprocess n = case Name.toString n of
        -- some absolute name that isn't just "."
        '.' : t@(_ : _) -> Name.unsafeFromString t
        -- something in current path
        _ ->
          case curPath of
            p@(_ :> _) -> Name.joinDot (Path.unsafeToName . Path.unabsolute $ p) n
            _ -> n

      lexedHQs = Set.map (fmap preprocess) . Set.filter HQ.hasHash $ lexedHQs0
  (_missing, rawHistoricalNames) <- liftIO $ Branch.findHistoricalHQs lexedHQs root'
  pure rawHistoricalNames

-- Any absolute names in the input which have `currentPath` as a prefix
-- are converted to names relative to current path. all other names are
-- converted to absolute names. For example:
--
-- e.g. if Command.currentPath = .foo.bar
--      then name foo.bar.baz becomes baz
--           name cat.dog     becomes .cat.dog
fixupNamesRelative :: Path.Absolute -> Names -> Names
fixupNamesRelative currentPath' = Names.map fixName
  where
    fixName n =
      if currentPath' == Path.absoluteEmpty
        then n
        else fromMaybe (Name.makeAbsolute n) do
          prefix <- Path.toName (Path.unabsolute currentPath')
          Name.stripNamePrefix prefix n

getBasicPrettyPrintNames :: Cli Names
getBasicPrettyPrintNames = do
  rootBranch <- Cli.getRootBranch
  currentPath <- Cli.getCurrentPath
  pure (Backend.prettyNamesForBranch rootBranch (Backend.AllNames (Path.unabsolute currentPath)))

makeHistoricalParsingNames :: Set (HQ.HashQualified Name) -> Cli NamesWithHistory
makeHistoricalParsingNames lexedHQs = do
  currentPath <- Cli.getCurrentPath

  rawHistoricalNames <- findHistoricalHQs lexedHQs
  basicNames <- basicParseNames
  pure $
    NamesWithHistory
      basicNames
      ( Names.makeAbsolute rawHistoricalNames
          <> fixupNamesRelative currentPath rawHistoricalNames
      )

makePrintNamesFromLabeled' :: Set LabeledDependency -> Cli NamesWithHistory
makePrintNamesFromLabeled' deps = do
  root' <- Cli.getRootBranch
  curPath <- Cli.getCurrentPath
  (_missing, rawHistoricalNames) <-
    liftIO $
      Branch.findHistoricalRefs
        deps
        root'
  basicNames <- basicPrettyPrintNamesA
  pure $ NamesWithHistory basicNames (fixupNamesRelative curPath rawHistoricalNames)

makeShadowedPrintNamesFromHQ :: Set (HQ.HashQualified Name) -> Names -> Cli NamesWithHistory
makeShadowedPrintNamesFromHQ lexedHQs shadowing = do
  rawHistoricalNames <- findHistoricalHQs lexedHQs
  basicNames <- basicPrettyPrintNamesA
  currentPath <- Cli.getCurrentPath
  -- The basic names go into "current", but are shadowed by "shadowing".
  -- They go again into "historical" as a hack that makes them available HQ-ed.
  pure $
    NamesWithHistory.shadowing
      shadowing
      (NamesWithHistory basicNames (fixupNamesRelative currentPath rawHistoricalNames))

makeShadowedPrintNamesFromLabeled :: Set LabeledDependency -> Names -> Cli NamesWithHistory
makeShadowedPrintNamesFromLabeled deps shadowing =
  NamesWithHistory.shadowing shadowing <$> makePrintNamesFromLabeled' deps
