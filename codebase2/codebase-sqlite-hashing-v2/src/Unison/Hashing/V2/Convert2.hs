-- | Description: Converts V2 types to the V2 hashing types
module Unison.Hashing.V2.Convert2
  ( v2ToH2Term,
    v2ToH2Type,
    v2ToH2TypeD,
    h2ToV2Reference,
  )
where

import qualified U.Codebase.Kind as V2
import qualified U.Codebase.Reference as V2
import qualified U.Codebase.Referent as V2.Referent
import qualified U.Codebase.Term as V2 (F, F' (..), MatchCase (..), Pattern (..), SeqOp (..), TermRef, TypeRef)
import qualified U.Codebase.Type as V2.Type
import qualified U.Core.ABT as V2
import qualified U.Core.ABT as V2.ABT
import qualified U.Util.Hash as V2 (Hash)
import qualified Unison.ABT as H2 (transform)
import qualified Unison.ABT as V1.ABT
import qualified Unison.Hashing.V2.Kind as H2
import qualified Unison.Hashing.V2.Pattern as H2.Pattern
import qualified Unison.Hashing.V2.Reference as H2
import qualified Unison.Hashing.V2.Referent as H2.Referent
import qualified Unison.Hashing.V2.Term as H2
import qualified Unison.Hashing.V2.Type as H2.Type
import Unison.Prelude

-- | Delete me ASAP. I am defined elsewhere.
abt2to1 :: Functor f => V2.ABT.Term f v a -> V1.ABT.Term f v a
abt2to1 (V2.ABT.Term fv a out) = V1.ABT.Term fv a (go out)
  where
    go = \case
      V2.ABT.Cycle body -> V1.ABT.Cycle (abt2to1 body)
      V2.ABT.Abs v body -> V1.ABT.Abs v (abt2to1 body)
      V2.ABT.Var v -> V1.ABT.Var v
      V2.ABT.Tm tm -> V1.ABT.Tm (abt2to1 <$> tm)

v2ToH2Term :: forall v. Ord v => V2.Hash -> V2.Term (V2.F v) v () -> H2.Term v ()
v2ToH2Term thisTermComponentHash = H2.transform convertF . abt2to1
  where
    convertF :: forall x. V2.F v x -> H2.F v () () x
    convertF = \case
      V2.Int x -> H2.Int x
      V2.Nat x -> H2.Nat x
      V2.Float x -> H2.Float x
      V2.Boolean x -> H2.Boolean x
      V2.Text x -> H2.Text x
      V2.Char x -> H2.Char x
      V2.Ref x -> H2.Ref (convertTermRef thisTermComponentHash x)
      V2.Constructor a b -> H2.Constructor (convertReference a) b
      V2.Request a b -> H2.Request (convertReference a) b
      V2.Handle a b -> H2.Handle a b
      V2.App a b -> H2.App a b
      V2.Ann a b -> H2.Ann a (v2ToH2Type b)
      V2.List a -> H2.List a
      V2.If a b c -> H2.If a b c
      V2.And a b -> H2.And a b
      V2.Or a b -> H2.Or a b
      V2.Lam a -> H2.Lam a
      V2.LetRec a b -> H2.LetRec a b
      V2.Let a b -> H2.Let a b
      V2.Match a b -> H2.Match a (map convertMatchCase b)
      V2.TermLink a -> H2.TermLink (convertReferent thisTermComponentHash a)
      V2.TypeLink a -> H2.TypeLink (convertReference a)

convertMatchCase :: V2.MatchCase Text V2.TypeRef x -> H2.MatchCase () x
convertMatchCase (V2.MatchCase pat a b) = H2.MatchCase (convertPattern pat) a b

convertPattern :: V2.Pattern Text V2.TypeRef -> H2.Pattern.Pattern ()
convertPattern = \case
  V2.PUnbound -> H2.Pattern.Unbound ()
  V2.PVar -> H2.Pattern.Var ()
  V2.PBoolean a -> H2.Pattern.Boolean () a
  V2.PInt a -> H2.Pattern.Int () a
  V2.PNat a -> H2.Pattern.Nat () a
  V2.PFloat a -> H2.Pattern.Float () a
  V2.PText a -> H2.Pattern.Text () a
  V2.PChar a -> H2.Pattern.Char () a
  V2.PConstructor a b c -> H2.Pattern.Constructor () (convertReference a) b (map convertPattern c)
  V2.PAs a -> H2.Pattern.As () (convertPattern a)
  V2.PEffectPure a -> H2.Pattern.EffectPure () (convertPattern a)
  V2.PEffectBind a b c d -> H2.Pattern.EffectBind () (convertReference a) b (map convertPattern c) (convertPattern d)
  V2.PSequenceLiteral a -> H2.Pattern.SequenceLiteral () (map convertPattern a)
  V2.PSequenceOp a b c -> H2.Pattern.SequenceOp () (convertPattern a) (convertSeqOp b) (convertPattern c)
  where
    convertSeqOp = \case
      V2.PCons -> H2.Pattern.Cons
      V2.PSnoc -> H2.Pattern.Snoc
      V2.PConcat -> H2.Pattern.Concat

convertReferent ::
  V2.Hash ->
  V2.Referent.Referent' (V2.Reference' Text (Maybe V2.Hash)) (V2.Reference' Text V2.Hash) ->
  H2.Referent.Referent
convertReferent defaultHash = \case
  V2.Referent.Ref x -> H2.Referent.Ref (convertTermRef defaultHash x)
  V2.Referent.Con x cid -> H2.Referent.Con (convertReference x) cid

convertId :: V2.Hash -> V2.Id' (Maybe V2.Hash) -> H2.Id
convertId defaultHash = \case
  V2.Id m p -> H2.Id (fromMaybe defaultHash m) p

convertReference :: V2.Reference -> H2.Reference
convertReference = convertReference' (\(V2.Id a b) -> H2.Id a b)

convertTermRef :: V2.Hash -> V2.TermRef -> H2.Reference
convertTermRef = convertReference' . convertId

convertReference' :: (V2.Id' hash -> H2.Id) -> V2.Reference' Text hash -> H2.Reference
convertReference' idConv = \case
  V2.ReferenceBuiltin x -> H2.Builtin x
  V2.ReferenceDerived x -> H2.DerivedId (idConv x)

v2ToH2Type :: forall v. Ord v => V2.Type.TypeR V2.TypeRef v -> H2.Type.Type v ()
v2ToH2Type = v2ToH2Type' convertReference

v2ToH2TypeD :: forall v. Ord v => V2.Hash -> V2.Type.TypeD v -> H2.Type.Type v ()
v2ToH2TypeD defaultHash = v2ToH2Type' (convertReference' (convertId defaultHash))

v2ToH2Type' :: forall r v. Ord v => (r -> H2.Reference) -> V2.Type.TypeR r v -> H2.Type.Type v ()
v2ToH2Type' mkReference = H2.transform convertF . abt2to1
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
