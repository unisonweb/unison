-- | Utilities that have to do with constructing names objects.
module Unison.Cli.NamesUtils
  ( basicParseNames,
    basicPrettyPrintNamesA,
    displayNames,
    getBasicPrettyPrintNames,
    makeHistoricalParsingNames,
    makePrintNamesFromLabeled',
    makeShadowedPrintNamesFromHQ,
  )
where

import Unison.Cli.Monad (Cli)
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Names (Names)
import Unison.NamesWithHistory (NamesWithHistory (..))
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Server.Backend qualified as Backend
import Unison.UnisonFile (TypecheckedUnisonFile)
import Unison.UnisonFile.Names qualified as UF
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
  (Var v) =>
  TypecheckedUnisonFile v a ->
  Cli NamesWithHistory
displayNames unisonFile =
  -- voodoo
  makeShadowedPrintNamesFromLabeled
    (UF.typecheckedToNames unisonFile)

getBasicPrettyPrintNames :: Cli Names
getBasicPrettyPrintNames = do
  rootBranch <- Cli.getRootBranch
  currentPath <- Cli.getCurrentPath
  pure (Backend.prettyNamesForBranch rootBranch (Backend.AllNames (Path.unabsolute currentPath)))

makeHistoricalParsingNames :: Cli NamesWithHistory
makeHistoricalParsingNames = do
  basicNames <- basicParseNames
  pure $ NamesWithHistory basicNames mempty

makePrintNamesFromLabeled' :: Cli NamesWithHistory
makePrintNamesFromLabeled' = do
  basicNames <- basicPrettyPrintNamesA
  pure $ NamesWithHistory basicNames mempty

makeShadowedPrintNamesFromHQ :: Names -> Cli NamesWithHistory
makeShadowedPrintNamesFromHQ shadowing = do
  basicNames <- basicPrettyPrintNamesA
  -- The basic names go into "current", but are shadowed by "shadowing".
  -- They go again into "historical" as a hack that makes them available HQ-ed.
  pure $ NamesWithHistory.shadowing shadowing (NamesWithHistory basicNames mempty)

makeShadowedPrintNamesFromLabeled :: Names -> Cli NamesWithHistory
makeShadowedPrintNamesFromLabeled shadowing =
  NamesWithHistory.shadowing shadowing <$> makePrintNamesFromLabeled'
