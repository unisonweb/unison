-- | Utilities that have to do with constructing names objects.
module Unison.Cli.NamesUtils
  ( basicParseNames,
    basicPrettyPrintNamesA,
    displayNames,
    getBasicPrettyPrintNames,
    makePrintNamesFromLabeled',
    makeShadowedPrintNamesFromHQ,
  )
where

import Unison.Cli.Monad (Cli)
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Names (Names)
import Unison.NamesWithHistory qualified as Names
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
  Cli Names
displayNames unisonFile =
  -- voodoo
  makeShadowedPrintNamesFromLabeled
    (UF.typecheckedToNames unisonFile)

getBasicPrettyPrintNames :: Cli Names
getBasicPrettyPrintNames = do
  rootBranch <- Cli.getRootBranch
  currentPath <- Cli.getCurrentPath
  pure (Backend.prettyNamesForBranch rootBranch (Backend.AllNames (Path.unabsolute currentPath)))

makePrintNamesFromLabeled' :: Cli Names
makePrintNamesFromLabeled' =
  basicPrettyPrintNamesA

makeShadowedPrintNamesFromHQ :: Names -> Cli Names
makeShadowedPrintNamesFromHQ shadowing = do
  basicNames <- basicPrettyPrintNamesA
  pure $ Names.shadowing shadowing basicNames

makeShadowedPrintNamesFromLabeled :: Names -> Cli Names
makeShadowedPrintNamesFromLabeled shadowing =
  Names.shadowing shadowing <$> makePrintNamesFromLabeled'
