module Unison.PrettyPrintEnvDecl.Sqlite where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified U.Codebase.Sqlite.Operations as Ops
import qualified U.Codebase.Sqlite.Queries as Q
import Unison.Codebase.Path
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.SqliteCodebase.Conversions as CV
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import qualified Unison.PrettyPrintEnvDecl as PPED
import qualified Unison.PrettyPrintEnvDecl.Names as PPED
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import qualified Unison.Sqlite as Sqlite

-- | Given a set of references, return a PPE which contains names for only those references.
ppedForReferences :: Int -> Path -> Set LabeledDependency -> Sqlite.Transaction PPED.PrettyPrintEnvDecl
ppedForReferences hashLength perspective refs = do
  nameGroups <- Map.fromList <$> for (Set.toList refs) \ref -> (ref,) <$> namesForReference ref
  ifor nameGroups \ref names -> do
    for names \name -> Q.termNamesBySuffix name
  where
    pathText :: Text
    pathText = Path.toText perspective
    namesForReference :: LabeledDependency -> Sqlite.Transaction [Name]
    namesForReference = \case
      LD.TermReferent ref ->
        fmap (Name.fromReverseSegments . coerce) <$> Ops.termNamesWithinNamespace pathText (CV.referent1to2 ref)
      LD.TypeReference ref ->
        fmap (Name.fromReverseSegments . coerce) <$> Ops.typeNamesWithinNamespace pathText (CV.reference1to2 ref)
