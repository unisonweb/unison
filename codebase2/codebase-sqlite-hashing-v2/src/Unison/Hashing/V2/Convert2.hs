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
import qualified U.Util.Hash as V2 (Hash)
import qualified Unison.Hashing.V2.Kind as H2
import qualified Unison.Hashing.V2.Reference as H2
import qualified Unison.Hashing.V2.Type as H2.Type
import Unison.Prelude

convertId :: V2.Hash -> V2.Id' (Maybe V2.Hash) -> H2.Id
convertId defaultHash = \case
  V2.Id m p -> H2.Id (fromMaybe defaultHash m) p

convertReference :: V2.Reference -> H2.Reference
convertReference = convertReference' (\(V2.Id a b) -> H2.Id a b)

convertReference' :: (V2.Id' hash -> H2.Id) -> V2.Reference' Text hash -> H2.Reference
convertReference' idConv = \case
  V2.ReferenceBuiltin x -> H2.Builtin x
  V2.ReferenceDerived x -> H2.DerivedId (idConv x)

v2ToH2Type :: forall v. Ord v => V2.Type.TypeR V2.TypeRef v -> H2.Type.Type v ()
v2ToH2Type = v2ToH2Type' convertReference

v2ToH2TypeD :: forall v. Ord v => V2.Hash -> V2.Type.TypeD v -> H2.Type.Type v ()
v2ToH2TypeD defaultHash = v2ToH2Type' (convertReference' (convertId defaultHash))

v2ToH2Type' :: forall r v. Ord v => (r -> H2.Reference) -> V2.Type.TypeR r v -> H2.Type.Type v ()
v2ToH2Type' mkReference = ABT.transform convertF
  where
    convertF :: forall a. V2.Type.F' r a -> H2.Type.F a
    convertF = \case
      V2.Type.Ref x -> H2.Type.Ref (mkReference x)
      V2.Type.Arrow a b -> H2.Type.Arrow a b
      V2.Type.Ann a k -> H2.Type.Ann a (convertKind k)
      V2.Type.App a b -> H2.Type.App a b
      V2.Type.Effect a b -> H2.Type.Effect a b
      V2.Type.Effects a -> H2.Type.Effects a
      V2.Type.Forall a -> H2.Type.Forall a
      V2.Type.IntroOuter a -> H2.Type.IntroOuter a

convertKind :: V2.Kind -> H2.Kind
convertKind = \case
  V2.Star -> H2.Star
  V2.Arrow a b -> H2.Arrow (convertKind a) (convertKind b)

h2ToV2Reference :: H2.Reference -> V2.Reference
h2ToV2Reference = \case
  H2.Builtin txt -> V2.ReferenceBuiltin txt
  H2.DerivedId (H2.Id x y) -> V2.ReferenceDerived (V2.Id x y)
