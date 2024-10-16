-- | Utilities related to resolving names to things.
module Unison.Cli.NameResolutionUtils
  ( resolveHQToLabeledDependencies,
  )
where

import Control.Monad.Reader (ask)
import Data.Set qualified as Set
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.NamesUtils qualified as Cli
import Unison.HashQualified qualified as HQ
import Unison.LabeledDependency (LabeledDependency)
import Unison.LabeledDependency qualified as LD
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.Server.NameSearch.Sqlite qualified as Sqlite

-- todo: compare to `getHQTerms` / `getHQTypes`.  Is one universally better?
resolveHQToLabeledDependencies :: HQ.HashQualified Name -> Cli (Set LabeledDependency)
resolveHQToLabeledDependencies = \case
  HQ.NameOnly n -> do
    names <- Cli.currentNames
    let terms, types :: Set LabeledDependency
        terms = Set.map LD.referent . Name.searchBySuffix n $ Names.terms names
        types = Set.map LD.typeRef . Name.searchBySuffix n $ Names.types names
    pure $ terms <> types
  -- rationale: the hash should be unique enough that the name never helps
  HQ.HashQualified _n sh -> resolveHashOnly sh
  HQ.HashOnly sh -> resolveHashOnly sh
  where
    resolveHashOnly sh = do
      Cli.Env {codebase} <- ask
      (terms, types) <-
        Cli.runTransaction do
          terms <- Sqlite.termReferentsByShortHash codebase sh
          types <- Sqlite.typeReferencesByShortHash sh
          pure (terms, types)
      pure $ Set.map LD.referent terms <> Set.map LD.typeRef types
