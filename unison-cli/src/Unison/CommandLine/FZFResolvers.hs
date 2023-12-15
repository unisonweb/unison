module Unison.CommandLine.FZFResolvers
  ( FZFResolver (..),
    definitionOptions,
    namespaceOptions,
    fuzzySelectFromList,
    multiResolver,
  )
where

import Control.Lens
import Data.List.Extra qualified as List
import Data.Set qualified as Set
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Path (Path, Path' (..))
import Unison.Codebase.Path qualified as Path
import Unison.Name qualified as Name
import Unison.Names qualified as Names
import Unison.Position (Position (..))
import Unison.Prelude
import Unison.Syntax.HashQualified qualified as HQ (toText)
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Relation qualified as Relation

type OptionFetcher = Branch0 IO -> IO [Text]

data FZFResolver = FZFResolver
  { argDescription :: Text,
    getOptions :: Branch0 IO -> IO [Text]
  }

instance Show FZFResolver where
  show _ = "<FZFResolver>"

-- | Select a definition from the given branch.
-- Returned names will match the provided 'Position' type.
definitionOptions :: Position -> OptionFetcher
definitionOptions pos searchBranch0 = liftIO do
  let termsAndTypes =
        Relation.dom (Names.hashQualifyTermsRelation (Relation.swap $ Branch.deepTerms searchBranch0))
          <> Relation.dom (Names.hashQualifyTypesRelation (Relation.swap $ Branch.deepTypes searchBranch0))
  termsAndTypes
    & Set.toList
    & map (HQ.toText . fmap (Name.setPosition pos))
    & pure

-- | Select a namespace from the given branch.
-- Returned Path's will match the provided 'Position' type.
namespaceOptions :: Position -> OptionFetcher
namespaceOptions pos searchBranch0 = do
  let intoPath' :: Path -> Path'
      intoPath' = case pos of
        Relative -> Path' . Right . Path.Relative
        Absolute -> Path' . Left . Path.Absolute
  searchBranch0
    & Branch.deepPaths
    & Set.toList
    & map (Path.toText' . intoPath')
    & pure

-- | Select a namespace from the given branch.
-- Returned Path's will match the provided 'Position' type.
fuzzySelectFromList :: Text -> [Text] -> FZFResolver
fuzzySelectFromList argDescription options =
  (FZFResolver {argDescription, getOptions = const $ pure options})

-- | Combine multiple option fetchers into one resolver.
multiResolver :: Text -> [OptionFetcher] -> FZFResolver
multiResolver argDescription resolvers =
  let getOptions :: Branch0 IO -> IO [Text]
      getOptions searchBranch0 = do
        List.nubOrd <$> foldMapM (\f -> f searchBranch0) resolvers
   in (FZFResolver {argDescription, getOptions})
