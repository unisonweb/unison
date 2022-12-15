-- | Description: Converts V2 types to the V2 hashing types
module Unison.Hashing.V2.Convert2
  ( v2ToH2Type,
    v2ToH2TypeD,
    h2ToV2Reference,
  )
where

import qualified U.Codebase.Kind as V2
import qualified U.Codebase.Reference as V2
import qualified U.Codebase.Term as V2 (TypeRef)
import qualified U.Codebase.Type as V2.Type
import qualified U.Core.ABT as ABT
import Unison.Hash (Hash)
import qualified Unison.Hashing.V2 as H2
import Unison.Prelude

convertId :: Hash -> V2.Id' (Maybe Hash) -> H2.ReferenceId
convertId defaultHash = \case
  V2.Id m p -> H2.ReferenceId (fromMaybe defaultHash m) p

convertReference :: V2.Reference -> H2.Reference
convertReference = convertReference' (\(V2.Id a b) -> H2.ReferenceId a b)

convertReference' :: (V2.Id' hash -> H2.ReferenceId) -> V2.Reference' Text hash -> H2.Reference
convertReference' idConv = \case
  V2.ReferenceBuiltin x -> H2.ReferenceBuiltin x
  V2.ReferenceDerived x -> H2.ReferenceDerivedId (idConv x)

v2ToH2Type :: forall v. Ord v => V2.Type.TypeR V2.TypeRef v -> H2.Type v ()
v2ToH2Type = v2ToH2Type' convertReference

v2ToH2TypeD :: forall v. Ord v => Hash -> V2.Type.TypeD v -> H2.Type v ()
v2ToH2TypeD defaultHash = v2ToH2Type' (convertReference' (convertId defaultHash))

v2ToH2Type' :: forall r v. Ord v => (r -> H2.Reference) -> V2.Type.TypeR r v -> H2.Type v ()
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
