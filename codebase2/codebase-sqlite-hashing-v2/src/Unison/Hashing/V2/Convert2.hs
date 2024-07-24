-- | Description: Converts V2 types to the V2 hashing types
module Unison.Hashing.V2.Convert2
  ( convertBranchV3,
    v2ToH2Type,
    v2ToH2TypeD,
    h2ToV2Reference,
    v2ToH2Referent,
    v2ToH2Branch,
    v2ToH2Term,
    v2ToH2Decl,
    hashBranchFormatToH2Branch,
    hashPatchFormatToH2Patch,
  )
where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.Branch qualified as V2
import U.Codebase.Branch qualified as V2Branch
import U.Codebase.BranchV3 (BranchV3 (..))
import U.Codebase.Causal qualified as Causal
import U.Codebase.Decl qualified as V2.Decl
import U.Codebase.HashTags
import U.Codebase.Kind qualified as V2
import U.Codebase.Reference qualified as V2
import U.Codebase.Reference qualified as V2Reference
import U.Codebase.Referent qualified as V2Referent
import U.Codebase.Sqlite.Branch.Full qualified as Memory.BranchFull
import U.Codebase.Sqlite.Patch.Full qualified as Memory.PatchFull
import U.Codebase.Sqlite.Patch.TermEdit qualified as Memory.TermEdit
import U.Codebase.Sqlite.Patch.TypeEdit qualified as Memory.TypeEdit
import U.Codebase.Term qualified as V2 (TypeRef)
import U.Codebase.Term qualified as V2.Term
import U.Codebase.Type qualified as V2.Type
import U.Core.ABT qualified as ABT
import Unison.Hash (Hash)
import Unison.Hashing.V2 qualified as H2
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.Prelude
import Unison.Symbol qualified as Unison
import Unison.Util.Map qualified as Map

-- | Convert a V3 branch to a hashing branch.
convertBranchV3 :: BranchV3 m -> H2.Branch
convertBranchV3 BranchV3 {children, terms, types} =
  H2.Branch
    { children = children & Map.bimap coerce (unCausalHash . Causal.causalHash),
      patches = Map.empty,
      terms = Map.bimap coerce (\ref -> Map.singleton (v2ToH2Referent ref) emptyMetadata) terms,
      types = Map.bimap coerce (\ref -> Map.singleton (v2ToH2Reference ref) emptyMetadata) types
    }
  where
    emptyMetadata = H2.MdValues Set.empty

convertId :: Hash -> V2Reference.Id' (Maybe Hash) -> H2.ReferenceId
convertId defaultHash = \case
  V2.Id m p -> H2.ReferenceId (fromMaybe defaultHash m) p

v2ToH2Reference :: V2.Reference -> H2.Reference
v2ToH2Reference = convertReference' (\(V2.Id a b) -> H2.ReferenceId a b)

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
  V2Referent.Con r cid -> H2.ReferentCon (v2ToH2Reference r) cid

v2ToH2Branch :: (Monad m) => V2.Branch m -> m H2.Branch
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
        (H2.ReferentCon (v2ToH2Reference $ second unComponentHash typeRef) conId)

hashPatchFormatToH2Patch :: Memory.PatchFull.HashPatch -> H2.Patch
hashPatchFormatToH2Patch Memory.PatchFull.Patch {termEdits, typeEdits} =
  H2.Patch
    { termEdits = Map.bimap cvreferent (Set.map cvTermEdit) termEdits,
      typeEdits = Map.bimap cvreference (Set.map cvTypeEdit) typeEdits
    }
  where
    cvTermEdit :: Memory.TermEdit.HashTermEdit -> H2.TermEdit
    cvTermEdit = \case
      Memory.TermEdit.Replace ref _typing -> H2.TermEditReplace (v2ToH2Referent . coerce $ ref)
      Memory.TermEdit.Deprecate -> H2.TermEditDeprecate
    cvTypeEdit :: Memory.TypeEdit.HashTypeEdit -> H2.TypeEdit
    cvTypeEdit = \case
      Memory.TypeEdit.Replace ref -> H2.TypeEditReplace (v2ToH2Reference . coerce $ ref)
      Memory.TypeEdit.Deprecate -> H2.TypeEditDeprecate
    cvreference :: V2Reference.Reference' Text ComponentHash -> H2.Reference
    cvreference = v2ToH2Reference . second unComponentHash
    cvreferent :: Memory.BranchFull.Referent'' Text ComponentHash -> H2.Referent
    cvreferent = \case
      V2Referent.Ref ref -> (H2.ReferentRef (v2ToH2Reference $ second unComponentHash ref))
      V2Referent.Con typeRef conId -> do
        (H2.ReferentCon (v2ToH2Reference $ second unComponentHash typeRef) conId)

v2ToH2Term :: forall v. (Ord v) => V2.Term.HashableTerm v -> H2.Term v ()
v2ToH2Term = ABT.transform convertF
  where
    convertF :: V2.Term.F' Text V2.Term.HashableTermRef V2.Term.TypeRef V2.Term.HashableTermLink V2.Term.TypeLink v a1 -> H2.TermF v () () a1
    convertF = \case
      V2.Term.Int i -> H2.TermInt i
      V2.Term.Nat i -> H2.TermNat i
      V2.Term.Float n -> H2.TermFloat n
      V2.Term.Boolean b -> H2.TermBoolean b
      V2.Term.Text t -> H2.TermText t
      V2.Term.Char c -> H2.TermChar c
      V2.Term.Ref r -> H2.TermRef (v2ToH2Reference r)
      V2.Term.Constructor r cid -> H2.TermConstructor (v2ToH2Reference r) cid
      V2.Term.Request r cid -> H2.TermRequest (v2ToH2Reference r) cid
      V2.Term.Handle a b -> H2.TermHandle a b
      V2.Term.App a b -> H2.TermApp a b
      V2.Term.Ann a k -> H2.TermAnn a (v2ToH2Type k)
      V2.Term.List a -> H2.TermList a
      V2.Term.If a b c -> H2.TermIf a b c
      V2.Term.And a b -> H2.TermAnd a b
      V2.Term.Or a b -> H2.TermOr a b
      V2.Term.Lam a -> H2.TermLam a
      V2.Term.LetRec as b -> H2.TermLetRec as b
      V2.Term.Let a b -> H2.TermLet a b
      V2.Term.Match a b -> H2.TermMatch a (fmap convertMatchCase b)
      V2.Term.TermLink a -> H2.TermTermLink (v2ToH2Referent a)
      V2.Term.TypeLink a -> H2.TermTypeLink (v2ToH2Reference a)

    convertMatchCase :: forall x. V2.Term.MatchCase Text V2.Term.TypeRef x -> H2.MatchCase () x
    convertMatchCase (V2.Term.MatchCase pat guard body) =
      H2.MatchCase (convertPattern pat) guard body

    convertPattern :: V2.Term.Pattern Text V2.Term.TypeRef -> H2.Pattern ()
    convertPattern = \case
      V2.Term.PUnbound -> H2.PatternUnbound ()
      V2.Term.PVar -> H2.PatternVar ()
      V2.Term.PBoolean b -> H2.PatternBoolean () b
      V2.Term.PInt i -> H2.PatternInt () i
      V2.Term.PNat i -> H2.PatternNat () i
      V2.Term.PFloat n -> H2.PatternFloat () n
      V2.Term.PText t -> H2.PatternText () t
      V2.Term.PChar c -> H2.PatternChar () c
      V2.Term.PConstructor r cid ps -> H2.PatternConstructor () (v2ToH2Reference r) cid (convertPattern <$> ps)
      V2.Term.PAs pat -> H2.PatternAs () (convertPattern pat)
      V2.Term.PEffectPure pat -> H2.PatternEffectPure () (convertPattern pat)
      V2.Term.PEffectBind r conId pats pat -> H2.PatternEffectBind () (v2ToH2Reference r) conId (convertPattern <$> pats) (convertPattern pat)
      V2.Term.PSequenceLiteral pats -> H2.PatternSequenceLiteral () (convertPattern <$> pats)
      V2.Term.PSequenceOp l op r -> H2.PatternSequenceOp () (convertPattern l) (convertSequenceOp op) (convertPattern r)

    convertSequenceOp :: V2.Term.SeqOp -> H2.SeqOp
    convertSequenceOp = \case
      V2.Term.PCons -> H2.Cons
      V2.Term.PSnoc -> H2.Snoc
      V2.Term.PConcat -> H2.Concat

v2ToH2Decl :: V2.Decl.HashableDecl Unison.Symbol -> H2.Decl Unison.Symbol ()
v2ToH2Decl (V2.Decl.DataDeclaration {declType, modifier, bound, constructorTypes}) =
  let tag = case declType of
        V2.Decl.Effect -> Left . H2.EffectDeclaration
        V2.Decl.Data -> Right
   in tag $
        H2.DataDeclaration
          { modifier = v2ToH2Modifier modifier,
            annotation = (),
            bound = bound,
            constructors' =
              constructorTypes
                & zip [0 ..]
                & fmap mkCtor
          }
  where
    mkCtor :: (Int, V2.Type.TypeR V2.Decl.HashableTypeRef Unison.Symbol) -> ((), Unison.Symbol, H2.Type Unison.Symbol ())
    mkCtor (n, t) = ((), Unison.symbol . Text.pack $ "Constructor" ++ show n, v2ToH2Type t)

    v2ToH2Modifier :: V2.Decl.Modifier -> H2.Modifier
    v2ToH2Modifier = \case
      V2.Decl.Structural -> H2.Structural
      V2.Decl.Unique t -> H2.Unique t
