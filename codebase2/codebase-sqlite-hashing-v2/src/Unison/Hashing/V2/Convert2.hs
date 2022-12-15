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
import qualified U.Core.ABT as ABT
import Unison.Hash (Hash)
import qualified Unison.Hashing.V2 as H2
import Unison.Prelude

v2ToH2Term :: forall v. Ord v => Hash -> ABT.Term (V2.F v) v () -> H2.Term v ()
v2ToH2Term thisTermComponentHash = ABT.transform convertF
  where
    convertF :: forall x. V2.F v x -> H2.TermF v () () x
    convertF = \case
      V2.Int x -> H2.TermInt x
      V2.Nat x -> H2.TermNat x
      V2.Float x -> H2.TermFloat x
      V2.Boolean x -> H2.TermBoolean x
      V2.Text x -> H2.TermText x
      V2.Char x -> H2.TermChar x
      V2.Ref x -> H2.TermRef (convertTermRef thisTermComponentHash x)
      V2.Constructor a b -> H2.TermConstructor (convertReference a) b
      V2.Request a b -> H2.TermRequest (convertReference a) b
      V2.Handle a b -> H2.TermHandle a b
      V2.App a b -> H2.TermApp a b
      V2.Ann a b -> H2.TermAnn a (v2ToH2Type b)
      V2.List a -> H2.TermList a
      V2.If a b c -> H2.TermIf a b c
      V2.And a b -> H2.TermAnd a b
      V2.Or a b -> H2.TermOr a b
      V2.Lam a -> H2.TermLam a
      V2.LetRec a b -> H2.TermLetRec a b
      V2.Let a b -> H2.TermLet a b
      V2.Match a b -> H2.TermMatch a (map convertMatchCase b)
      V2.TermLink a -> H2.TermTermLink (convertReferent thisTermComponentHash a)
      V2.TypeLink a -> H2.TermTypeLink (convertReference a)

convertMatchCase :: V2.MatchCase Text V2.TypeRef x -> H2.MatchCase () x
convertMatchCase (V2.MatchCase pat a b) = H2.MatchCase (convertPattern pat) a b

convertPattern :: V2.Pattern Text V2.TypeRef -> H2.Pattern ()
convertPattern = \case
  V2.PUnbound -> H2.PatternUnbound ()
  V2.PVar -> H2.PatternVar ()
  V2.PBoolean a -> H2.PatternBoolean () a
  V2.PInt a -> H2.PatternInt () a
  V2.PNat a -> H2.PatternNat () a
  V2.PFloat a -> H2.PatternFloat () a
  V2.PText a -> H2.PatternText () a
  V2.PChar a -> H2.PatternChar () a
  V2.PConstructor a b c -> H2.PatternConstructor () (convertReference a) b (map convertPattern c)
  V2.PAs a -> H2.PatternAs () (convertPattern a)
  V2.PEffectPure a -> H2.PatternEffectPure () (convertPattern a)
  V2.PEffectBind a b c d -> H2.PatternEffectBind () (convertReference a) b (map convertPattern c) (convertPattern d)
  V2.PSequenceLiteral a -> H2.PatternSequenceLiteral () (map convertPattern a)
  V2.PSequenceOp a b c -> H2.PatternSequenceOp () (convertPattern a) (convertSeqOp b) (convertPattern c)
  where
    convertSeqOp = \case
      V2.PCons -> H2.Cons
      V2.PSnoc -> H2.Snoc
      V2.PConcat -> H2.Concat

convertReferent ::
  Hash ->
  V2.Referent.Referent' (V2.Reference' Text (Maybe Hash)) (V2.Reference' Text Hash) ->
  H2.Referent
convertReferent defaultHash = \case
  V2.Referent.Ref x -> H2.ReferentRef (convertTermRef defaultHash x)
  V2.Referent.Con x cid -> H2.ReferentCon (convertReference x) cid

convertId :: Hash -> V2.Id' (Maybe Hash) -> H2.ReferenceId
convertId defaultHash = \case
  V2.Id m p -> H2.ReferenceId (fromMaybe defaultHash m) p

convertReference :: V2.Reference -> H2.Reference
convertReference = convertReference' (\(V2.Id a b) -> H2.ReferenceId a b)

convertTermRef :: Hash -> V2.TermRef -> H2.Reference
convertTermRef = convertReference' . convertId

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
