-- | Description: Converts V2 types to the V2 hashing types
module Unison.Hashing.V2.Convert2
  ( v2ToH2Type,
    v2ToH2TypeD,
    v2ToH2Term,
    h2ToV2Reference,
    h2ToV2ReferenceId,
    v2ToH2Branch,
  )
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import U.Codebase.Branch qualified as V2
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags (CausalHash (..), PatchHash (..))
import U.Codebase.Kind qualified as V2
import U.Codebase.Reference qualified as V2
import U.Codebase.Reference qualified as V2Reference
import U.Codebase.Referent qualified as V2Referent
import U.Codebase.Term qualified as V2.Term
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
v2ToH2Reference = convertReference' (\(V2.Id a b) -> H2.ReferenceId a b)

convertReference' :: (V2Reference.Id' hash -> H2.ReferenceId) -> V2.Reference' Text hash -> H2.Reference
convertReference' idConv = \case
  V2.ReferenceBuiltin x -> H2.ReferenceBuiltin x
  V2.ReferenceDerived x -> H2.ReferenceDerivedId (idConv x)

v2ToH2Type :: forall v. (Ord v) => V2.Type.TypeT v -> H2.Type v ()
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

v2ToH2Term :: forall v. (Ord v) => V2.Term.ClosedTerm v -> H2.Term v ()
v2ToH2Term = ABT.transform convertF
  where
    convertF :: forall a. V2.Term.ClosedF v a -> H2.TermF v () () a
    convertF = \case
      V2.Term.Int x -> H2.TermInt x
      V2.Term.Nat x -> H2.TermNat x
      V2.Term.Float x -> H2.TermFloat x
      V2.Term.Boolean x -> H2.TermBoolean x
      V2.Term.Text x -> H2.TermText x
      V2.Term.Char x -> H2.TermChar x
      V2.Term.Ref x -> H2.TermRef (v2ToH2Reference x)
      V2.Term.Constructor r cid -> H2.TermConstructor (v2ToH2Reference r) cid
      V2.Term.Request r cid -> H2.TermRequest (v2ToH2Reference r) cid
      V2.Term.Handle a b -> H2.TermHandle a b
      V2.Term.App a b -> H2.TermApp a b
      V2.Term.Ann a t -> H2.TermAnn a (v2ToH2Type t)
      V2.Term.List x -> H2.TermList x
      V2.Term.If a b c -> H2.TermIf a b c
      V2.Term.And a b -> H2.TermAnd a b
      V2.Term.Or a b -> H2.TermOr a b
      V2.Term.Lam a -> H2.TermLam a
      V2.Term.LetRec bindings body -> H2.TermLetRec bindings body
      V2.Term.Let binding body -> H2.TermLet binding body
      V2.Term.Match scrutinee cases ->
        H2.TermMatch
          scrutinee
          (v2ToH2MatchCase <$> cases)
      V2.Term.TermLink termLink -> H2.TermTermLink (v2ToH2Referent termLink)
      V2.Term.TypeLink typeLink -> H2.TermTypeLink (v2ToH2Reference typeLink)

v2ToH2MatchCase :: V2.Term.MatchCase Text V2.Reference a -> H2.MatchCase () a
v2ToH2MatchCase (V2.Term.MatchCase p g b) = H2.MatchCase (v2ToH2Pattern p) g b

v2ToH2Pattern :: V2.Term.Pattern Text V2.Reference -> H2.Pattern ()
v2ToH2Pattern = \case
  V2.Term.PUnbound -> H2.PatternUnbound ()
  V2.Term.PVar -> H2.PatternVar ()
  V2.Term.PBoolean x -> H2.PatternBoolean () x
  V2.Term.PInt x -> H2.PatternInt () x
  V2.Term.PNat x -> H2.PatternNat () x
  V2.Term.PFloat x -> H2.PatternFloat () x
  V2.Term.PText x -> H2.PatternText () x
  V2.Term.PChar x -> H2.PatternChar () x
  V2.Term.PConstructor r cid pats ->
    H2.PatternConstructor
      ()
      (v2ToH2Reference r)
      cid
      (map v2ToH2Pattern pats)
  V2.Term.PAs pat -> H2.PatternAs () (v2ToH2Pattern pat)
  V2.Term.PEffectPure pat -> H2.PatternEffectPure () (v2ToH2Pattern pat)
  V2.Term.PEffectBind r cid pats pat ->
    H2.PatternEffectBind
      ()
      (v2ToH2Reference r)
      cid
      (map v2ToH2Pattern pats)
      (v2ToH2Pattern pat)
  V2.Term.PSequenceLiteral pats -> H2.PatternSequenceLiteral () (map v2ToH2Pattern pats)
  V2.Term.PSequenceOp pat0 op pat1 ->
    H2.PatternSequenceOp
      ()
      (v2ToH2Pattern pat0)
      (v2ToH2SeqOp op)
      (v2ToH2Pattern pat1)

v2ToH2SeqOp :: V2.Term.SeqOp -> H2.SeqOp
v2ToH2SeqOp = \case
  V2.Term.PCons -> H2.Cons
  V2.Term.PSnoc -> H2.Snoc
  V2.Term.PConcat -> H2.Concat

convertKind :: V2.Kind -> H2.Kind
convertKind = \case
  V2.Star -> H2.KindStar
  V2.Arrow a b -> H2.KindArrow (convertKind a) (convertKind b)

h2ToV2Reference :: H2.Reference -> V2.Reference
h2ToV2Reference = \case
  H2.ReferenceBuiltin txt -> V2.ReferenceBuiltin txt
  H2.ReferenceDerivedId h2id -> V2.ReferenceDerived (h2ToV2ReferenceId h2id)

h2ToV2ReferenceId :: H2.ReferenceId -> V2.Id
h2ToV2ReferenceId (H2.ReferenceId x y) = V2.Id x y

v2ToH2Referent :: V2Referent.Referent -> H2.Referent
v2ToH2Referent = \case
  V2Referent.Ref r -> H2.ReferentRef (v2ToH2Reference r)
  V2Referent.Con r cid -> H2.ReferentCon (v2ToH2Reference r) cid

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
