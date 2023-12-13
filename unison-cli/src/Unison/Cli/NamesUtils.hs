-- | Utilities that have to do with constructing names objects.
module Unison.Cli.NamesUtils
  ( basicParseNames,
    basicPrettyPrintNamesA,
    displayNames,
    findHistoricalHQs,
    getBasicPrettyPrintNames,
    makeHistoricalParsingNames,
    makeShadowedPrintNames,
  )
where

import Control.Lens
import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.HashQualified qualified as HQ
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.NamesWithHistory (NamesWithHistory (..))
import Unison.Prelude
import Unison.Server.Backend qualified as Backend
import Unison.Syntax.Name qualified as Name (toString, unsafeFromString)
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
displayNames :: (Var v) => TypecheckedUnisonFile v a -> Cli Names
displayNames unisonFile =
  makeShadowedPrintNames (UF.typecheckedToNames unisonFile)

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

makeShadowedPrintNames :: Names -> Cli Names
makeShadowedPrintNames shadowing = do
  currentBranch0 <- Cli.getCurrentBranch0
  pure (shadowing `Names.unionLeft` Branch.toNames currentBranch0)
