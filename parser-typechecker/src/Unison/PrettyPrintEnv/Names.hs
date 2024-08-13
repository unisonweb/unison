module Unison.PrettyPrintEnv.Names
  ( makePPE,
    makeTermNames,
    makeTypeNames,
  )
where

import Data.Set qualified as Set
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.Namer (Namer)
import Unison.Namer qualified as Namer
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv (PrettyPrintEnv))
import Unison.Reference (TypeReference)
import Unison.Referent (Referent)
import Unison.Suffixifier (Suffixifier (..))

makePPE :: Namer (HQ'.HashQualified Name) -> Suffixifier -> PrettyPrintEnv
makePPE namer suffixifier =
  PrettyPrintEnv
    (makeTermNames namer suffixifier)
    (makeTypeNames namer suffixifier)

makeTermNames :: Namer (HQ'.HashQualified Name) -> Suffixifier -> Referent -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
makeTermNames namer suffixifier =
  prioritize . map (\name -> (name, suffixifier.suffixifyTerm <$> name)) . Set.toList . Namer.nameTerm namer

makeTypeNames :: Namer (HQ'.HashQualified Name) -> Suffixifier -> TypeReference -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
makeTypeNames namer suffixifier =
  prioritize . map (\name -> (name, suffixifier.suffixifyType <$> name)) . Set.toList . Namer.nameType namer

-- | Sort the names for a given ref by the following factors (in priority order):
--
-- 1. Prefer Relative Names to Absolute Names
-- 2. Prefer names that aren't hash qualified to those that are
-- 3. Prefer names which have fewer segments in their fully-qualified form
-- 4. Prefer names which have fewer segments in their suffixified form (if applicable)
prioritize :: [(HQ'.HashQualified Name, HQ'.HashQualified Name)] -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
prioritize =
  sortOn \case
    (fqn, HQ'.NameOnly name) -> (Name.isAbsolute name, Nothing, Name.countSegments (HQ'.toName fqn), Name.countSegments name)
    (fqn, HQ'.HashQualified name hash) -> (Name.isAbsolute name, Just hash, Name.countSegments (HQ'.toName fqn), Name.countSegments name)
