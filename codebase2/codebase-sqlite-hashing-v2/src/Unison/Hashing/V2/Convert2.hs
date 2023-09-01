-- | Description: Converts V2 types to the V2 hashing types
module Unison.Hashing.V2.Convert2
  ( v2ToH2Type,
    v2ToH2TypeD,
    h2ToV2Reference,
    v2ToH2Branch,
    hashBranchFormatToH2Branch,
  )
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import U.Codebase.Branch qualified as V2
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags
import U.Codebase.Kind qualified as V2
import U.Codebase.Reference qualified as V2
import U.Codebase.Reference qualified as V2Reference
import U.Codebase.Referent qualified as V2Referent
import U.Codebase.Sqlite.Branch.Full qualified as Memory.BranchFull
import U.Codebase.Term qualified as V2 (TypeRef)
import U.Codebase.Type qualified as V2.Type
import U.Core.ABT qualified as ABT
import Unison.Hash (Hash)
import Unison.Hashing.V2 qualified as H2
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude
import Unison.Util.Map qualified as Map

convertId :: Hash -> V2Reference.Id' (Maybe Hash) -> H2.ReferenceId
convertId defaultHash = \case
  V2.Id m p -> H2.ReferenceId (fromMaybe defaultHash m) p

v2ToH2Reference :: V2.Reference -> H2.Reference
v2ToH2Reference = convertReference' v2ToH2ReferenceId

v2ToH2ReferenceId :: V2Reference.Id -> H2.ReferenceId
v2ToH2ReferenceId = \(V2.Id a b) -> H2.ReferenceId a b

convertReference' :: (V2Reference.Id' hash -> H2.ReferenceId) -> V2.Reference' Text hash -> H2.Reference
convertReference' idConv = \case
  V2.ReferenceBuiltin x -> H2.ReferenceBuiltin x
  V2.ReferenceDerived x -> H2.ReferenceDerivedId (idConv x)

v2ToH2Type :: forall v. (Ord v) => V2.Type.TypeR V2.TypeRef v -> H2.Type v ()
v2ToH2Type = v2ToH2Type' v2ToH2Reference

v2ToH2TypeD :: forall v. (Ord v) => Hash -> V2.Type.TypeD v -> H2.Type v ()
v2ToH2TypeD defaultHash = v2ToH2Type' (convertReference' (convertId defaultHash))

v2ToH2Type' :: forall r v. (Ord v) => (r -> H2.Reference) -> V2.Type.TypeR r v -> H2.Type v ()
v2ToH2Type' mkReference = ABT.transform convertF
  where
    convertF :: forall a. V2.Type.F' r a -> H2.TypeF a
    convertF = \case
      V2.Type.Ref x -> H2.TypeRef (mkReference x)
      V2.Type.Arrow a b -> H2.TypeArrow a b
      V2.Type.Ann a k -> H2.TypeAnn a (convertKind k)
      V2.Type.App a b -> H2.TypeApp a b
      V2.Type.Effect a b -> H2.TypeEffect a b
      V2.Type.Effects a -> H2.TypeEffects a
      V2.Type.Forall a -> H2.TypeForall a
      V2.Type.IntroOuter a -> H2.TypeIntroOuter a

convertKind :: V2.Kind -> H2.Kind
convertKind = \case
  V2.Star -> H2.KindStar
  V2.Arrow a b -> H2.KindArrow (convertKind a) (convertKind b)

h2ToV2Reference :: H2.Reference -> V2.Reference
h2ToV2Reference = \case
  H2.ReferenceBuiltin txt -> V2.ReferenceBuiltin txt
  H2.ReferenceDerivedId (H2.ReferenceId x y) -> V2.ReferenceDerived (V2.Id x y)

v2ToH2Referent :: V2Referent.Referent -> H2.Referent
v2ToH2Referent = \case
  V2Referent.Ref r -> H2.ReferentRef (v2ToH2Reference r)
  V2Referent.Con r cid -> H2.ReferentCon (v2ToH2ReferenceId r) cid

v2ToH2Branch :: Monad m => V2.Branch m -> m H2.Branch
v2ToH2Branch V2.Branch {terms, types, patches, children} = do
  hterms <-
    traverse sequenceA terms
      <&> Map.bimap coerce (Map.bimap v2ToH2Referent v2ToH2MdValues)
  htypes <-
    traverse sequenceA types
      <&> Map.bimap coerce (Map.bimap v2ToH2Reference v2ToH2MdValues)
  let hpatches =
        patches
          & Map.bimap coerce (unPatchHash . fst)
  let hchildren = children & Map.bimap coerce (unCausalHash . Causal.causalHash)
  pure $ H2.Branch {types = htypes, terms = hterms, patches = hpatches, children = hchildren}

v2ToH2MdValues :: V2Branch.MdValues -> H2.MdValues
v2ToH2MdValues (V2Branch.MdValues mdMap) =
  mdMap
    & Map.keysSet
    & Set.map v2ToH2Reference
    & H2.MdValues

hashBranchFormatToH2Branch :: Memory.BranchFull.HashBranch -> H2.Branch
hashBranchFormatToH2Branch Memory.BranchFull.Branch {terms, types, patches, children} =
  H2.Branch
    { terms =
        terms
          & Map.bimap H2.NameSegment (Map.bimap cvreferent cvMdValues),
      types =
        types
          & Map.bimap H2.NameSegment (Map.bimap cvreference cvMdValues),
      patches = patches & Map.bimap H2.NameSegment unPatchHash,
      children = children & Map.bimap H2.NameSegment (unCausalHash . snd)
    }
  where
    cvMdValues :: Memory.BranchFull.MetadataSetFormat' Text ComponentHash -> H2.MdValues
    cvMdValues (Memory.BranchFull.Inline refSet) = H2.MdValues $ Set.map cvreference refSet
    cvreference :: V2Reference.Reference' Text ComponentHash -> H2.Reference
    cvreference = v2ToH2Reference . second unComponentHash
    cvreferent :: Memory.BranchFull.Referent'' Text ComponentHash -> H2.Referent
    cvreferent = \case
      V2Referent.Ref ref -> (H2.ReferentRef (v2ToH2Reference $ second unComponentHash ref))
      V2Referent.Con typeRef conId -> do
        (H2.ReferentCon (v2ToH2ReferenceId $ fmap unComponentHash typeRef) conId)
