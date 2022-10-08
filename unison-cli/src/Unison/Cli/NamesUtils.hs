-- | Utilities that have to do with constructing names objects.
module Unison.Cli.NamesUtils
  ( displayNames,
    findHistoricalHQs,
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
import Unison.UnisonFile (TypecheckedUnisonFile)
import qualified Unison.UnisonFile as UF
import qualified Unison.UnisonFile.Names as UF
import Unison.Var (Var)

-- | Produce a `Names` needed to display all the hashes used in the given file.
displayNames ::
  Var v =>
  TypecheckedUnisonFile v a ->
  Cli r NamesWithHistory
displayNames unisonFile =
  -- voodoo
  makeShadowedPrintNamesFromLabeled
    (UF.termSignatureExternalLabeledDependencies unisonFile)
    (UF.typecheckedToNames unisonFile)

-- discards inputs that aren't hashqualified;
-- I'd enforce it with finer-grained types if we had them.
findHistoricalHQs :: Set (HQ.HashQualified Name) -> Cli r Names
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

makeHistoricalParsingNames :: Set (HQ.HashQualified Name) -> Cli r NamesWithHistory
makeHistoricalParsingNames lexedHQs = do
  currentPath <- Cli.getCurrentPath

  rawHistoricalNames <- findHistoricalHQs lexedHQs
  basicNames <- Cli.getCurrentNames
  pure $
    NamesWithHistory
      basicNames
      ( Names.makeAbsolute rawHistoricalNames
          <> fixupNamesRelative currentPath rawHistoricalNames
      )

makePrintNamesFromLabeled' :: Set LabeledDependency -> Cli r NamesWithHistory
makePrintNamesFromLabeled' deps = do
  root' <- Cli.getRootBranch
  curPath <- Cli.getCurrentPath
  (_missing, rawHistoricalNames) <-
    liftIO $
      Branch.findHistoricalRefs
        deps
        root'
  basicNames <- Cli.getCurrentNames
  pure $ NamesWithHistory basicNames (fixupNamesRelative curPath rawHistoricalNames)

makeShadowedPrintNamesFromHQ :: Set (HQ.HashQualified Name) -> Names -> Cli r NamesWithHistory
makeShadowedPrintNamesFromHQ lexedHQs shadowing = do
  rawHistoricalNames <- findHistoricalHQs lexedHQs
  basicNames <- Cli.getCurrentNames
  currentPath <- Cli.getCurrentPath
  -- The basic names go into "current", but are shadowed by "shadowing".
  -- They go again into "historical" as a hack that makes them available HQ-ed.
  pure $
    NamesWithHistory.shadowing
      shadowing
      (NamesWithHistory basicNames (fixupNamesRelative currentPath rawHistoricalNames))

makeShadowedPrintNamesFromLabeled :: Set LabeledDependency -> Names -> Cli r NamesWithHistory
makeShadowedPrintNamesFromLabeled deps shadowing =
  NamesWithHistory.shadowing shadowing <$> makePrintNamesFromLabeled' deps
