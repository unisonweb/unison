module Unison.Merge.DeclNameLookups
  ( makeDeclNameLookups
  ) where

import Unison.DeclCoherencyCheck (IncoherentDeclReason, checkDeclCoherency, lenientCheckDeclCoherency)
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.ThreeWay (GThreeWay (..), ThreeWay (..))
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.PartialDeclNameLookup (PartialDeclNameLookup)
import Unison.Prelude
import Unison.Reference (TypeReference, TypeReferenceId)
import Unison.Referent (Referent)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.Defns (Defns, DefnsF)
import Unison.Util.Nametree (Nametree, flattenNametrees)

makeDeclNameLookups ::
  ThreeWay (Nametree (DefnsF (Map NameSegment) Referent TypeReference)) ->
  Map TypeReferenceId Int ->
  Either (EitherWay IncoherentDeclReason) (GThreeWay PartialDeclNameLookup DeclNameLookup)
makeDeclNameLookups nametrees numConstructors = do
  let lca = lenientCheckDeclCoherency nametrees.lca numConstructors
  alice <- checkDeclCoherency nametrees.alice numConstructors & mapLeft Alice
  bob <- checkDeclCoherency nametrees.bob numConstructors & mapLeft Bob
  Right GThreeWay {lca, alice, bob}
