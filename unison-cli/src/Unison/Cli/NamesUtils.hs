-- | Utilities that have to do with constructing names objects.
module Unison.Cli.NamesUtils
  ( basicParseNames,
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
import qualified Data.Set as Set
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.MonadUtils as Cli
import qualified Unison.Codebase.Branch.Names as Branch
import Unison.Codebase.Path (Path)
import qualified Unison.Codebase.Path as Path
import qualified Unison.HashQualified as HQ
import Unison.LabeledDependency (LabeledDependency)
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.Names (Names)
import qualified Unison.Names as Names
import Unison.NamesWithHistory (NamesWithHistory (..))
import qualified Unison.NamesWithHistory as NamesWithHistory
import Unison.Prelude
import qualified Unison.Server.Backend as Backend
import Unison.UnisonFile (TypecheckedUnisonFile)
import qualified Unison.UnisonFile as UF
import qualified Unison.UnisonFile.Names as UF
import Unison.Var (Var)

basicParseNames :: Cli Names
basicParseNames =
  fst <$> basicNames' Backend.Within

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
  Var v =>
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
