module Unison.CommandLine.FZFResolvers
  ( fuzzySelectDefinition,
    fuzzySelectNamespace,
  )
where

import Control.Lens
import Data.Set qualified as Set
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Path (Path, Path' (..))
import Unison.Codebase.Path qualified as Path
import Unison.CommandLine.FuzzySelect qualified as Fuzzy
import Unison.CommandLine.InputPattern (FZFResolver (..))
import Unison.Name qualified as Name
import Unison.Names qualified as Names
import Unison.Position (Position (..))
import Unison.Prelude
import Unison.Syntax.HashQualified qualified as HQ (toText)
import Unison.Util.Relation qualified as Relation

-- | Select a definition from the given branch.
-- Returned names will match the provided 'Position' type.
fuzzySelectDefinition :: Text -> Position -> FZFResolver
fuzzySelectDefinition argDescription pos =
  let search :: Branch0 m -> IO [Text]
      search searchBranch0 = liftIO do
        let termsAndTypes =
              Relation.dom (Names.hashQualifyTermsRelation (Relation.swap $ Branch.deepTerms searchBranch0))
                <> Relation.dom (Names.hashQualifyTypesRelation (Relation.swap $ Branch.deepTypes searchBranch0))
        let inputs :: [Text]
            inputs =
              termsAndTypes
                & Set.toList
                & map (HQ.toText . fmap (Name.setPosition pos))
        fold <$> Fuzzy.fuzzySelect Fuzzy.defaultOptions id inputs
   in (FZFResolver {argDescription, search})

-- | Select a namespace from the given branch.
-- Returned Path's will match the provided 'Position' type.
fuzzySelectNamespace :: Text -> Position -> FZFResolver
fuzzySelectNamespace argDescription pos =
  let search :: Branch0 m -> IO [Text]
      search searchBranch0 = do
        let intoPath' :: Path -> Path'
            intoPath' = case pos of
              Relative -> Path' . Right . Path.Relative
              Absolute -> Path' . Left . Path.Absolute
        let inputs :: [Text]
            inputs =
              searchBranch0
                & Branch.deepPaths
                & Set.toList
                & map (Path.toText' . intoPath')
        fold
          <$> Fuzzy.fuzzySelect
            Fuzzy.defaultOptions {Fuzzy.allowMultiSelect = False}
            id
            inputs
   in (FZFResolver {argDescription, search})
