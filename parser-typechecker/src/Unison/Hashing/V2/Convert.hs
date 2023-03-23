{-# LANGUAGE PartialTypeSignatures #-}

-- | Description: Converts V1 types to the V2 hashing types
module Unison.Hashing.V2.Convert
  ( ResolutionResult,
    hashBranch0,
    hashCausal,
    hashDataDecls,
    hashDecls,
    hashPatch,
    hashClosedTerm,
    hashTermComponents,
    hashTermComponentsWithoutTypes,
    typeToReference,
    typeToReferenceMentions,
  )
where

import Control.Applicative
import Control.Lens (over, _3)
import qualified Control.Lens as Lens
import Control.Monad.Trans.Writer.CPS (Writer)
import qualified Control.Monad.Trans.Writer.CPS as Writer
import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import U.Codebase.HashTags (CausalHash (..), PatchHash (..))
import qualified Unison.ABT as ABT
import qualified Unison.Codebase.Branch.Type as Memory.Branch
import qualified Unison.Codebase.Patch as Memory.Patch
import qualified Unison.Codebase.TermEdit as Memory.TermEdit
import qualified Unison.Codebase.TypeEdit as Memory.TypeEdit
import qualified Unison.ConstructorReference as Memory.ConstructorReference
import qualified Unison.ConstructorType as CT
import qualified Unison.ConstructorType as Memory.ConstructorType
import qualified Unison.DataDeclaration as Memory.DD
import Unison.Hash (Hash, HashFor (HashFor))
import qualified Unison.Hashing.V2 as Hashing
import qualified Unison.Kind as Memory.Kind
import qualified Unison.NameSegment as Memory.NameSegment
import Unison.Names.ResolutionResult (ResolutionResult)
import qualified Unison.Pattern as Memory.Pattern
import qualified Unison.Reference as Memory.Reference
import qualified Unison.Referent as Memory.Referent
import qualified Unison.Syntax.Name as Name (unsafeFromVar)
import qualified Unison.Term as Memory.Term
import qualified Unison.Type as Memory.Type
import qualified Unison.Util.Map as Map
import qualified Unison.Util.Relation as Relation
import qualified Unison.Util.Star3 as Memory.Star3
import Unison.Var (Var)

typeToReference :: (Var v) => Memory.Type.Type v a -> Memory.Reference.Reference
typeToReference = h2mReference . Hashing.typeToReference . m2hType . Memory.Type.removeAllEffectVars

typeToReferenceMentions :: (Var v) => Memory.Type.Type v a -> Set Memory.Reference.Reference
typeToReferenceMentions =
  Set.map h2mReference . Hashing.typeToReferenceMentions . m2hType . Memory.Type.removeAllEffectVars

-- TODO: remove non-prime version
-- include type in hash
hashTermComponents ::
  forall v a extra.
  Var v =>
  Map v (Memory.Term.Term v a, Memory.Type.Type v a, extra) ->
  Map v (Memory.Reference.Id, Memory.Term.Term v a, Memory.Type.Type v a, extra)
hashTermComponents mTerms =
  case h2mTermMap mTerms of
    (hTerms, constructorTypes) -> h2mTermResult (constructorTypes Map.!) <$> Hashing.hashTermComponents hTerms
  where
    h2mTermMap m =
      m
        & traverse (\(trm, typ, extra) -> liftA3 (,,) (m2hTerm trm) (pure $ m2hType typ) (pure extra))
        & Writer.runWriter
    h2mTermResult ::
      (Ord v) =>
      ( Memory.Reference.Reference ->
        Memory.ConstructorType.ConstructorType
      ) ->
      (Hashing.ReferenceId, Hashing.Term v a, Hashing.Type v a, extra) ->
      (Memory.Reference.Id, Memory.Term.Term v a, Memory.Type.Type v a, extra)
    h2mTermResult getCtorType (id, tm, typ, extra) = (h2mReferenceId id, h2mTerm getCtorType tm, h2mType typ, extra)

-- | This shouldn't be used when storing terms in the codebase, as it doesn't incorporate the type into the hash.
--   this should only be used in cases where you just need a way to identify some terms that you have, but won't be
--   saving them.
hashTermComponentsWithoutTypes ::
  forall v a.
  (Var v) =>
  Map v (Memory.Term.Term v a) ->
  Map v (Memory.Reference.Id, Memory.Term.Term v a)
hashTermComponentsWithoutTypes mTerms =
  case Writer.runWriter (traverse m2hTerm mTerms) of
    (hTerms, constructorTypes) -> h2mTermResult (constructorTypes Map.!) <$> Hashing.hashTermComponentsWithoutTypes hTerms
  where
    h2mTermResult :: (Ord v) => (Memory.Reference.Reference -> Memory.ConstructorType.ConstructorType) -> (Hashing.ReferenceId, Hashing.Term v a) -> (Memory.Reference.Id, Memory.Term.Term v a)
    h2mTermResult getCtorType (id, tm) = (h2mReferenceId id, h2mTerm getCtorType tm)

hashClosedTerm :: (Var v) => Memory.Term.Term v a -> Memory.Reference.Id
hashClosedTerm = h2mReferenceId . Hashing.hashClosedTerm . fst . Writer.runWriter . m2hTerm

m2hTerm :: (Ord v) => Memory.Term.Term v a -> Writer (Map Memory.Reference.Reference Memory.ConstructorType.ConstructorType) (Hashing.Term v a)
m2hTerm = ABT.transformM \case
  Memory.Term.Int i -> pure (Hashing.TermInt i)
  Memory.Term.Nat n -> pure (Hashing.TermNat n)
  Memory.Term.Float d -> pure (Hashing.TermFloat d)
  Memory.Term.Boolean b -> pure (Hashing.TermBoolean b)
  Memory.Term.Text t -> pure (Hashing.TermText t)
  Memory.Term.Char c -> pure (Hashing.TermChar c)
  Memory.Term.Blank b -> pure (Hashing.TermBlank b)
  Memory.Term.Ref r -> pure (Hashing.TermRef (m2hReference r))
  Memory.Term.Constructor (Memory.ConstructorReference.ConstructorReference r i) -> pure (Hashing.TermConstructor (m2hReference r) i)
  Memory.Term.Request (Memory.ConstructorReference.ConstructorReference r i) -> pure (Hashing.TermRequest (m2hReference r) i)
  Memory.Term.Handle x y -> pure (Hashing.TermHandle x y)
  Memory.Term.App f x -> pure (Hashing.TermApp f x)
  Memory.Term.Ann e t -> pure (Hashing.TermAnn e (m2hType t))
  Memory.Term.List as -> pure (Hashing.TermList as)
  Memory.Term.And p q -> pure (Hashing.TermAnd p q)
  Memory.Term.If c t f -> pure (Hashing.TermIf c t f)
  Memory.Term.Or p q -> pure (Hashing.TermOr p q)
  Memory.Term.Lam a -> pure (Hashing.TermLam a)
  Memory.Term.LetRec _isTop bs body -> pure (Hashing.TermLetRec bs body)
  Memory.Term.Let _isTop b body -> pure (Hashing.TermLet b body)
  Memory.Term.Match scr cases -> pure (Hashing.TermMatch scr (fmap m2hMatchCase cases))
  Memory.Term.TermLink r -> Hashing.TermTermLink <$> m2hReferent r
  Memory.Term.TypeLink r -> pure (Hashing.TermTypeLink (m2hReference r))

m2hMatchCase :: Memory.Term.MatchCase a a1 -> Hashing.MatchCase a a1
m2hMatchCase (Memory.Term.MatchCase pat m_a1 a1) = Hashing.MatchCase (m2hPattern pat) m_a1 a1

m2hPattern :: Memory.Pattern.Pattern a -> Hashing.Pattern a
m2hPattern = \case
  Memory.Pattern.Unbound loc -> Hashing.PatternUnbound loc
  Memory.Pattern.Var loc -> Hashing.PatternVar loc
  Memory.Pattern.Boolean loc b -> Hashing.PatternBoolean loc b
  Memory.Pattern.Int loc i -> Hashing.PatternInt loc i
  Memory.Pattern.Nat loc n -> Hashing.PatternNat loc n
  Memory.Pattern.Float loc f -> Hashing.PatternFloat loc f
  Memory.Pattern.Text loc t -> Hashing.PatternText loc t
  Memory.Pattern.Char loc c -> Hashing.PatternChar loc c
  Memory.Pattern.Constructor loc (Memory.ConstructorReference.ConstructorReference r i) ps ->
    Hashing.PatternConstructor loc (m2hReference r) i (fmap m2hPattern ps)
  Memory.Pattern.As loc p -> Hashing.PatternAs loc (m2hPattern p)
  Memory.Pattern.EffectPure loc p -> Hashing.PatternEffectPure loc (m2hPattern p)
  Memory.Pattern.EffectBind loc (Memory.ConstructorReference.ConstructorReference r i) ps k ->
    Hashing.PatternEffectBind loc (m2hReference r) i (fmap m2hPattern ps) (m2hPattern k)
  Memory.Pattern.SequenceLiteral loc ps -> Hashing.PatternSequenceLiteral loc (fmap m2hPattern ps)
  Memory.Pattern.SequenceOp loc l op r -> Hashing.PatternSequenceOp loc (m2hPattern l) (m2hSequenceOp op) (m2hPattern r)

m2hSequenceOp :: Memory.Pattern.SeqOp -> Hashing.SeqOp
m2hSequenceOp = \case
  Memory.Pattern.Cons -> Hashing.Cons
  Memory.Pattern.Snoc -> Hashing.Snoc
  Memory.Pattern.Concat -> Hashing.Concat

m2hReferent :: Memory.Referent.Referent -> Writer (Map Memory.Reference.Reference Memory.ConstructorType.ConstructorType) Hashing.Referent
m2hReferent = \case
  Memory.Referent.Ref ref -> pure (Hashing.ReferentRef (m2hReference ref))
  Memory.Referent.Con (Memory.ConstructorReference.ConstructorReference ref n) ct -> do
    Writer.tell (Map.singleton ref ct)
    pure (Hashing.ReferentCon (m2hReference ref) n)

h2mTerm :: (Ord v) => (Memory.Reference.Reference -> Memory.ConstructorType.ConstructorType) -> Hashing.Term v a -> Memory.Term.Term v a
h2mTerm getCT = ABT.transform \case
  Hashing.TermInt i -> Memory.Term.Int i
  Hashing.TermNat n -> Memory.Term.Nat n
  Hashing.TermFloat d -> Memory.Term.Float d
  Hashing.TermBoolean b -> Memory.Term.Boolean b
  Hashing.TermText t -> Memory.Term.Text t
  Hashing.TermChar c -> Memory.Term.Char c
  Hashing.TermBlank b -> Memory.Term.Blank b
  Hashing.TermRef r -> Memory.Term.Ref (h2mReference r)
  Hashing.TermConstructor r i -> Memory.Term.Constructor (Memory.ConstructorReference.ConstructorReference (h2mReference r) i)
  Hashing.TermRequest r i -> Memory.Term.Request (Memory.ConstructorReference.ConstructorReference (h2mReference r) i)
  Hashing.TermHandle x y -> Memory.Term.Handle x y
  Hashing.TermApp f x -> Memory.Term.App f x
  Hashing.TermAnn e t -> Memory.Term.Ann e (h2mType t)
  Hashing.TermList as -> Memory.Term.List as
  Hashing.TermIf c t f -> Memory.Term.If c t f
  Hashing.TermAnd p q -> Memory.Term.And p q
  Hashing.TermOr p q -> Memory.Term.Or p q
  Hashing.TermLam a -> Memory.Term.Lam a
  Hashing.TermLetRec bs body -> Memory.Term.LetRec False bs body
  Hashing.TermLet b body -> Memory.Term.Let False b body
  Hashing.TermMatch scr cases -> Memory.Term.Match scr (h2mMatchCase <$> cases)
  Hashing.TermTermLink r -> Memory.Term.TermLink (h2mReferent getCT r)
  Hashing.TermTypeLink r -> Memory.Term.TypeLink (h2mReference r)

h2mMatchCase :: Hashing.MatchCase a b -> Memory.Term.MatchCase a b
h2mMatchCase (Hashing.MatchCase pat m_b b) = Memory.Term.MatchCase (h2mPattern pat) m_b b

h2mPattern :: Hashing.Pattern a -> Memory.Pattern.Pattern a
h2mPattern = \case
  Hashing.PatternUnbound loc -> Memory.Pattern.Unbound loc
  Hashing.PatternVar loc -> Memory.Pattern.Var loc
  Hashing.PatternBoolean loc b -> Memory.Pattern.Boolean loc b
  Hashing.PatternInt loc i -> Memory.Pattern.Int loc i
  Hashing.PatternNat loc n -> Memory.Pattern.Nat loc n
  Hashing.PatternFloat loc f -> Memory.Pattern.Float loc f
  Hashing.PatternText loc t -> Memory.Pattern.Text loc t
  Hashing.PatternChar loc c -> Memory.Pattern.Char loc c
  Hashing.PatternConstructor loc r i ps ->
    Memory.Pattern.Constructor loc (Memory.ConstructorReference.ConstructorReference (h2mReference r) i) (h2mPattern <$> ps)
  Hashing.PatternAs loc p -> Memory.Pattern.As loc (h2mPattern p)
  Hashing.PatternEffectPure loc p -> Memory.Pattern.EffectPure loc (h2mPattern p)
  Hashing.PatternEffectBind loc r i ps k ->
    Memory.Pattern.EffectBind loc (Memory.ConstructorReference.ConstructorReference (h2mReference r) i) (h2mPattern <$> ps) (h2mPattern k)
  Hashing.PatternSequenceLiteral loc ps -> Memory.Pattern.SequenceLiteral loc (h2mPattern <$> ps)
  Hashing.PatternSequenceOp loc l op r -> Memory.Pattern.SequenceOp loc (h2mPattern l) (h2mSequenceOp op) (h2mPattern r)

h2mSequenceOp :: Hashing.SeqOp -> Memory.Pattern.SeqOp
h2mSequenceOp = \case
  Hashing.Cons -> Memory.Pattern.Cons
  Hashing.Snoc -> Memory.Pattern.Snoc
  Hashing.Concat -> Memory.Pattern.Concat

h2mReferent :: (Memory.Reference.Reference -> Memory.ConstructorType.ConstructorType) -> Hashing.Referent -> Memory.Referent.Referent
h2mReferent getCT = \case
  Hashing.ReferentRef ref -> Memory.Referent.Ref (h2mReference ref)
  Hashing.ReferentCon ref n ->
    let mRef = h2mReference ref
     in Memory.Referent.Con (Memory.ConstructorReference.ConstructorReference mRef n) (getCT mRef)

hashDataDecls ::
  (Var v) =>
  Map v (Memory.DD.DataDeclaration v a) ->
  ResolutionResult v a [(v, Memory.Reference.Id, Memory.DD.DataDeclaration v a)]
hashDataDecls memDecls = do
  let hashingDecls = fmap m2hDecl memDecls
  hashingResult <- Hashing.hashDecls Name.unsafeFromVar hashingDecls
  pure $ map h2mDeclResult hashingResult
  where
    h2mDeclResult :: (Ord v) => (v, Hashing.ReferenceId, Hashing.DataDeclaration v a) -> (v, Memory.Reference.Id, Memory.DD.DataDeclaration v a)
    h2mDeclResult (v, id, dd) = (v, h2mReferenceId id, h2mDecl dd)

hashDecls ::
  (Var v) =>
  Map v (Memory.DD.Decl v a) ->
  ResolutionResult v a [(v, Memory.Reference.Id, Memory.DD.Decl v a)]
hashDecls memDecls = do
  -- want to unwrap the decl before doing the rehashing, and then wrap it back up the same way
  let howToReassemble =
        memDecls <&> \case
          Left {} -> CT.Effect
          Right {} -> CT.Data
      memDeclsAsDDs = Memory.DD.asDataDecl <$> memDecls
  result <- hashDataDecls memDeclsAsDDs
  pure $
    result <&> \(v, id', decl) ->
      case Map.lookup v howToReassemble of
        Nothing -> error "Unknown v in hashDecls'"
        Just ct -> (v, id', retag ct decl)
  where
    retag :: CT.ConstructorType -> Memory.DD.DataDeclaration v a -> Memory.DD.Decl v a
    retag CT.Effect = Left . Memory.DD.EffectDeclaration
    retag CT.Data = Right

m2hDecl :: (Ord v) => Memory.DD.DataDeclaration v a -> Hashing.DataDeclaration v a
m2hDecl (Memory.DD.DataDeclaration mod ann bound ctors) =
  Hashing.DataDeclaration (m2hModifier mod) ann bound $ fmap (Lens.over _3 m2hType) ctors

m2hType :: (Ord v) => Memory.Type.Type v a -> Hashing.Type v a
m2hType = ABT.transform \case
  Memory.Type.Ref ref -> Hashing.TypeRef (m2hReference ref)
  Memory.Type.Arrow a1 a1' -> Hashing.TypeArrow a1 a1'
  Memory.Type.Ann a1 ki -> Hashing.TypeAnn a1 (m2hKind ki)
  Memory.Type.App a1 a1' -> Hashing.TypeApp a1 a1'
  Memory.Type.Effect a1 a1' -> Hashing.TypeEffect a1 a1'
  Memory.Type.Effects a1s -> Hashing.TypeEffects a1s
  Memory.Type.Forall a1 -> Hashing.TypeForall a1
  Memory.Type.IntroOuter a1 -> Hashing.TypeIntroOuter a1

m2hKind :: Memory.Kind.Kind -> Hashing.Kind
m2hKind = \case
  Memory.Kind.Star -> Hashing.KindStar
  Memory.Kind.Arrow k1 k2 -> Hashing.KindArrow (m2hKind k1) (m2hKind k2)

m2hReference :: Memory.Reference.Reference -> Hashing.Reference
m2hReference = \case
  Memory.Reference.Builtin t -> Hashing.ReferenceBuiltin t
  Memory.Reference.DerivedId d -> Hashing.ReferenceDerivedId (m2hReferenceId d)

m2hReferenceId :: Memory.Reference.Id -> Hashing.ReferenceId
m2hReferenceId (Memory.Reference.Id h i) = Hashing.ReferenceId h i

h2mModifier :: Hashing.Modifier -> Memory.DD.Modifier
h2mModifier = \case
  Hashing.Structural -> Memory.DD.Structural
  Hashing.Unique text -> Memory.DD.Unique text

m2hModifier :: Memory.DD.Modifier -> Hashing.Modifier
m2hModifier = \case
  Memory.DD.Structural -> Hashing.Structural
  Memory.DD.Unique text -> Hashing.Unique text

h2mDecl :: (Ord v) => Hashing.DataDeclaration v a -> Memory.DD.DataDeclaration v a
h2mDecl (Hashing.DataDeclaration mod ann bound ctors) =
  Memory.DD.DataDeclaration (h2mModifier mod) ann bound (over _3 h2mType <$> ctors)

h2mType :: (Ord v) => Hashing.Type v a -> Memory.Type.Type v a
h2mType = ABT.transform \case
  Hashing.TypeRef ref -> Memory.Type.Ref (h2mReference ref)
  Hashing.TypeArrow a1 a1' -> Memory.Type.Arrow a1 a1'
  Hashing.TypeAnn a1 ki -> Memory.Type.Ann a1 (h2mKind ki)
  Hashing.TypeApp a1 a1' -> Memory.Type.App a1 a1'
  Hashing.TypeEffect a1 a1' -> Memory.Type.Effect a1 a1'
  Hashing.TypeEffects a1s -> Memory.Type.Effects a1s
  Hashing.TypeForall a1 -> Memory.Type.Forall a1
  Hashing.TypeIntroOuter a1 -> Memory.Type.IntroOuter a1

h2mKind :: Hashing.Kind -> Memory.Kind.Kind
h2mKind = \case
  Hashing.KindStar -> Memory.Kind.Star
  Hashing.KindArrow k1 k2 -> Memory.Kind.Arrow (h2mKind k1) (h2mKind k2)

h2mReference :: Hashing.Reference -> Memory.Reference.Reference
h2mReference = \case
  Hashing.ReferenceBuiltin t -> Memory.Reference.Builtin t
  Hashing.ReferenceDerivedId d -> Memory.Reference.DerivedId (h2mReferenceId d)

h2mReferenceId :: Hashing.ReferenceId -> Memory.Reference.Id
h2mReferenceId (Hashing.ReferenceId h i) = Memory.Reference.Id h i

m2hPatch :: Memory.Patch.Patch -> Hashing.Patch
m2hPatch (Memory.Patch.Patch termEdits typeEdits) =
  Hashing.Patch termEdits' typeEdits'
  where
    typeEdits' =
      Map.fromList
        . map (bimap m2hReference (Set.map m2hTypeEdit))
        . Map.toList
        $ Relation.toMultimap typeEdits
    termEdits' =
      Map.fromList
        . map (bimap (Hashing.ReferentRef . m2hReference) (Set.map m2hTermEdit))
        . Map.toList
        $ Relation.toMultimap termEdits
    m2hTermEdit = \case
      Memory.TermEdit.Replace r _ -> Hashing.TermEditReplace (Hashing.ReferentRef $ m2hReference r)
      Memory.TermEdit.Deprecate -> Hashing.TermEditDeprecate
    m2hTypeEdit = \case
      Memory.TypeEdit.Replace r -> Hashing.TypeEditReplace (m2hReference r)
      Memory.TypeEdit.Deprecate -> Hashing.TypeEditDeprecate

hashPatch :: Memory.Patch.Patch -> Hash
hashPatch = Hashing.contentHash . m2hPatch

hashBranch0 :: Memory.Branch.Branch0 m -> Hash
hashBranch0 = Hashing.contentHash . m2hBranch0

hashCausal :: (Hashing.ContentAddressable e) => e -> Set CausalHash -> (CausalHash, HashFor e)
hashCausal e tails =
  let valueHash = Hashing.contentHash e
      causalHash =
        CausalHash . Hashing.contentHash $
          Hashing.Causal valueHash (Set.map unCausalHash tails)
   in (causalHash, HashFor valueHash)

m2hBranch0 :: Memory.Branch.Branch0 m -> Hashing.Branch
m2hBranch0 b =
  Hashing.Branch
    (doTerms (Memory.Branch._terms b))
    (doTypes (Memory.Branch._types b))
    (doPatches (Memory.Branch._edits b))
    (doChildren (Memory.Branch._children b))
  where
    -- is there a more readable way to structure these that's also linear?
    doTerms ::
      Memory.Branch.Star Memory.Referent.Referent Memory.NameSegment.NameSegment ->
      Map Hashing.NameSegment (Map Hashing.Referent Hashing.MdValues)
    doTerms s =
      Map.fromList
        [ (m2hNameSegment ns, m2)
          | ns <- toList . Relation.ran $ Memory.Star3.d1 s,
            let m2 =
                  Map.fromList
                    [ (fst (Writer.runWriter (m2hReferent r)), md)
                      | r <- toList . Relation.lookupRan ns $ Memory.Star3.d1 s,
                        let mdrefs1to2 (_typeR1, valR1) = m2hReference valR1
                            md = Hashing.MdValues . Set.map mdrefs1to2 . Relation.lookupDom r $ Memory.Star3.d3 s
                    ]
        ]

    doTypes ::
      Memory.Branch.Star Memory.Reference.Reference Memory.NameSegment.NameSegment ->
      Map Hashing.NameSegment (Map Hashing.Reference Hashing.MdValues)
    doTypes s =
      Map.fromList
        [ (m2hNameSegment ns, m2)
          | ns <- toList . Relation.ran $ Memory.Star3.d1 s,
            let m2 =
                  Map.fromList
                    [ (m2hReference r, md)
                      | r <- toList . Relation.lookupRan ns $ Memory.Star3.d1 s,
                        let mdrefs1to2 (_typeR1, valR1) = m2hReference valR1
                            md :: Hashing.MdValues
                            md = Hashing.MdValues . Set.map mdrefs1to2 . Relation.lookupDom r $ Memory.Star3.d3 s
                    ]
        ]

    doPatches ::
      Map Memory.NameSegment.NameSegment (PatchHash, m Memory.Patch.Patch) ->
      Map Hashing.NameSegment Hash
    doPatches = Map.bimap m2hNameSegment (unPatchHash . fst)

    doChildren ::
      Map Memory.NameSegment.NameSegment (Memory.Branch.Branch m) ->
      Map Hashing.NameSegment Hash
    doChildren = Map.bimap m2hNameSegment (unCausalHash . Memory.Branch.headHash)

m2hNameSegment :: Memory.NameSegment.NameSegment -> Hashing.NameSegment
m2hNameSegment (Memory.NameSegment.NameSegment s) = Hashing.NameSegment s
