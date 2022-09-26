{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}

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

import Control.Applicative (liftA3)
import Control.Lens (over, _3)
import qualified Control.Lens as Lens
import Control.Monad.Trans.Writer.CPS (Writer)
import qualified Control.Monad.Trans.Writer.CPS as Writer
import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.Codebase.Branch.Type as Memory.Branch
import qualified Unison.Codebase.Causal.Type as Memory.Causal
import qualified Unison.Codebase.Patch as Memory.Patch
import qualified Unison.Codebase.TermEdit as Memory.TermEdit
import qualified Unison.Codebase.TypeEdit as Memory.TypeEdit
import qualified Unison.ConstructorReference as Memory.ConstructorReference
import qualified Unison.ConstructorType as CT
import qualified Unison.ConstructorType as Memory.ConstructorType
import qualified Unison.DataDeclaration as Memory.DD
import Unison.Hash (Hash)
import qualified Unison.Hashing.V2.Branch as Hashing.Branch
import qualified Unison.Hashing.V2.Causal as Hashing.Causal
import qualified Unison.Hashing.V2.DataDeclaration as Hashing.DD
import Unison.Hashing.V2.Hashable (HashFor (HashFor), Hashable)
import qualified Unison.Hashing.V2.Hashable as Hashable
import qualified Unison.Hashing.V2.Kind as Hashing.Kind
import qualified Unison.Hashing.V2.Patch as Hashing.Patch
import qualified Unison.Hashing.V2.Pattern as Hashing.Pattern
import qualified Unison.Hashing.V2.Reference as Hashing.Reference
import qualified Unison.Hashing.V2.Referent as Hashing.Referent
import qualified Unison.Hashing.V2.Term as Hashing.Term
import qualified Unison.Hashing.V2.TermEdit as Hashing.TermEdit
import qualified Unison.Hashing.V2.Type as Hashing.Type
import qualified Unison.Hashing.V2.TypeEdit as Hashing.TypeEdit
import qualified Unison.Kind as Memory.Kind
import qualified Unison.NameSegment as Memory.NameSegment
import Unison.Names.ResolutionResult (ResolutionResult)
import qualified Unison.Pattern as Memory.Pattern
import qualified Unison.Reference as Memory.Reference
import qualified Unison.Referent as Memory.Referent
import qualified Unison.Term as Memory.Term
import qualified Unison.Type as Memory.Type
import qualified Unison.Util.Map as Map
import qualified Unison.Util.Relation as Relation
import qualified Unison.Util.Star3 as Memory.Star3
import Unison.Var (Var)

typeToReference :: Var v => Memory.Type.Type v a -> Memory.Reference.Reference
typeToReference = h2mReference . Hashing.Type.toReference . m2hType . Memory.Type.removeAllEffectVars

typeToReferenceMentions :: Var v => Memory.Type.Type v a -> Set Memory.Reference.Reference
typeToReferenceMentions = Set.map h2mReference . Hashing.Type.toReferenceMentions . m2hType . Memory.Type.removeAllEffectVars

-- TODO: remove non-prime version
-- include type in hash
hashTermComponents ::
  forall v a.
  Var v =>
  Map v (a, Memory.Term.Term v a, Memory.Type.Type v a) ->
  Map v (a, Memory.Reference.Id, Memory.Term.Term v a, Memory.Type.Type v a)
hashTermComponents mTerms =
  case Writer.runWriter (traverse (\(a, tm, typ) -> liftA3 (,,) (pure a) (m2hTerm tm) (pure $ m2hType typ)) mTerms) of
    (hTerms, constructorTypes) -> h2mTermResult (constructorTypes Map.!) <$> Hashing.Term.hashComponents hTerms
  where
    h2mTermResult ::
      Ord v =>
      ( Memory.Reference.Reference ->
        Memory.ConstructorType.ConstructorType
      ) ->
      (Hashing.Reference.Id, Hashing.Term.Term v a, Hashing.Type.Type v a) ->
      (Memory.Reference.Id, Memory.Term.Term v a, Memory.Type.Type v a)
    h2mTermResult getCtorType (id, tm, typ) = (h2mReferenceId id, h2mTerm getCtorType tm, h2mType typ)

-- | This shouldn't be used when storing terms in the codebase, as it doesn't incorporate the type into the hash.
--   this should only be used in cases where you just need a way to identify some terms that you have, but won't be
--   saving them.
hashTermComponentsWithoutTypes ::
  forall v a.
  Var v =>
  Map v (Memory.Term.Term v a) ->
  Map v (Memory.Reference.Id, Memory.Term.Term v a)
hashTermComponentsWithoutTypes mTerms =
  case Writer.runWriter (traverse m2hTerm mTerms) of
    (hTerms, constructorTypes) -> h2mTermResult (constructorTypes Map.!) <$> Hashing.Term.hashComponentsWithoutTypes hTerms
  where
    h2mTermResult :: Ord v => (Memory.Reference.Reference -> Memory.ConstructorType.ConstructorType) -> (Hashing.Reference.Id, Hashing.Term.Term v a) -> (Memory.Reference.Id, Memory.Term.Term v a)
    h2mTermResult getCtorType (id, tm) = (h2mReferenceId id, h2mTerm getCtorType tm)

hashClosedTerm :: Var v => Memory.Term.Term v a -> Memory.Reference.Id
hashClosedTerm = h2mReferenceId . Hashing.Term.hashClosedTerm . fst . Writer.runWriter . m2hTerm

m2hTerm :: Ord v => Memory.Term.Term v a -> Writer (Map Memory.Reference.Reference Memory.ConstructorType.ConstructorType) (Hashing.Term.Term v a)
m2hTerm = ABT.transformM \case
  Memory.Term.Int i -> pure (Hashing.Term.Int i)
  Memory.Term.Nat n -> pure (Hashing.Term.Nat n)
  Memory.Term.Float d -> pure (Hashing.Term.Float d)
  Memory.Term.Boolean b -> pure (Hashing.Term.Boolean b)
  Memory.Term.Text t -> pure (Hashing.Term.Text t)
  Memory.Term.Char c -> pure (Hashing.Term.Char c)
  Memory.Term.Blank b -> pure (Hashing.Term.Blank b)
  Memory.Term.Ref r -> pure (Hashing.Term.Ref (m2hReference r))
  Memory.Term.Constructor (Memory.ConstructorReference.ConstructorReference r i) -> pure (Hashing.Term.Constructor (m2hReference r) i)
  Memory.Term.Request (Memory.ConstructorReference.ConstructorReference r i) -> pure (Hashing.Term.Request (m2hReference r) i)
  Memory.Term.Handle x y -> pure (Hashing.Term.Handle x y)
  Memory.Term.App f x -> pure (Hashing.Term.App f x)
  Memory.Term.Ann e t -> pure (Hashing.Term.Ann e (m2hType t))
  Memory.Term.List as -> pure (Hashing.Term.List as)
  Memory.Term.And p q -> pure (Hashing.Term.And p q)
  Memory.Term.If c t f -> pure (Hashing.Term.If c t f)
  Memory.Term.Or p q -> pure (Hashing.Term.Or p q)
  Memory.Term.Lam a -> pure (Hashing.Term.Lam a)
  Memory.Term.LetRec _isTop bs body -> pure (Hashing.Term.LetRec bs body)
  Memory.Term.Let _isTop b body -> pure (Hashing.Term.Let b body)
  Memory.Term.Match scr cases -> pure (Hashing.Term.Match scr (fmap m2hMatchCase cases))
  Memory.Term.TermLink r -> Hashing.Term.TermLink <$> m2hReferent r
  Memory.Term.TypeLink r -> pure (Hashing.Term.TypeLink (m2hReference r))

m2hMatchCase :: Memory.Term.MatchCase a a1 -> Hashing.Term.MatchCase a a1
m2hMatchCase (Memory.Term.MatchCase pat m_a1 a1) = Hashing.Term.MatchCase (m2hPattern pat) m_a1 a1

m2hPattern :: Memory.Pattern.Pattern a -> Hashing.Pattern.Pattern a
m2hPattern = \case
  Memory.Pattern.Unbound loc -> Hashing.Pattern.Unbound loc
  Memory.Pattern.Var loc -> Hashing.Pattern.Var loc
  Memory.Pattern.Boolean loc b -> Hashing.Pattern.Boolean loc b
  Memory.Pattern.Int loc i -> Hashing.Pattern.Int loc i
  Memory.Pattern.Nat loc n -> Hashing.Pattern.Nat loc n
  Memory.Pattern.Float loc f -> Hashing.Pattern.Float loc f
  Memory.Pattern.Text loc t -> Hashing.Pattern.Text loc t
  Memory.Pattern.Char loc c -> Hashing.Pattern.Char loc c
  Memory.Pattern.Constructor loc (Memory.ConstructorReference.ConstructorReference r i) ps ->
    Hashing.Pattern.Constructor loc (m2hReference r) i (fmap m2hPattern ps)
  Memory.Pattern.As loc p -> Hashing.Pattern.As loc (m2hPattern p)
  Memory.Pattern.EffectPure loc p -> Hashing.Pattern.EffectPure loc (m2hPattern p)
  Memory.Pattern.EffectBind loc (Memory.ConstructorReference.ConstructorReference r i) ps k ->
    Hashing.Pattern.EffectBind loc (m2hReference r) i (fmap m2hPattern ps) (m2hPattern k)
  Memory.Pattern.SequenceLiteral loc ps -> Hashing.Pattern.SequenceLiteral loc (fmap m2hPattern ps)
  Memory.Pattern.SequenceOp loc l op r -> Hashing.Pattern.SequenceOp loc (m2hPattern l) (m2hSequenceOp op) (m2hPattern r)

m2hSequenceOp :: Memory.Pattern.SeqOp -> Hashing.Pattern.SeqOp
m2hSequenceOp = \case
  Memory.Pattern.Cons -> Hashing.Pattern.Cons
  Memory.Pattern.Snoc -> Hashing.Pattern.Snoc
  Memory.Pattern.Concat -> Hashing.Pattern.Concat

m2hReferent :: Memory.Referent.Referent -> Writer (Map Memory.Reference.Reference Memory.ConstructorType.ConstructorType) Hashing.Referent.Referent
m2hReferent = \case
  Memory.Referent.Ref ref -> pure (Hashing.Referent.Ref (m2hReference ref))
  Memory.Referent.Con (Memory.ConstructorReference.ConstructorReference ref n) ct -> do
    Writer.tell (Map.singleton ref ct)
    pure (Hashing.Referent.Con (m2hReference ref) n)

h2mTerm :: Ord v => (Memory.Reference.Reference -> Memory.ConstructorType.ConstructorType) -> Hashing.Term.Term v a -> Memory.Term.Term v a
h2mTerm getCT = ABT.transform \case
  Hashing.Term.Int i -> Memory.Term.Int i
  Hashing.Term.Nat n -> Memory.Term.Nat n
  Hashing.Term.Float d -> Memory.Term.Float d
  Hashing.Term.Boolean b -> Memory.Term.Boolean b
  Hashing.Term.Text t -> Memory.Term.Text t
  Hashing.Term.Char c -> Memory.Term.Char c
  Hashing.Term.Blank b -> Memory.Term.Blank b
  Hashing.Term.Ref r -> Memory.Term.Ref (h2mReference r)
  Hashing.Term.Constructor r i -> Memory.Term.Constructor (Memory.ConstructorReference.ConstructorReference (h2mReference r) i)
  Hashing.Term.Request r i -> Memory.Term.Request (Memory.ConstructorReference.ConstructorReference (h2mReference r) i)
  Hashing.Term.Handle x y -> Memory.Term.Handle x y
  Hashing.Term.App f x -> Memory.Term.App f x
  Hashing.Term.Ann e t -> Memory.Term.Ann e (h2mType t)
  Hashing.Term.List as -> Memory.Term.List as
  Hashing.Term.If c t f -> Memory.Term.If c t f
  Hashing.Term.And p q -> Memory.Term.And p q
  Hashing.Term.Or p q -> Memory.Term.Or p q
  Hashing.Term.Lam a -> Memory.Term.Lam a
  Hashing.Term.LetRec bs body -> Memory.Term.LetRec False bs body
  Hashing.Term.Let b body -> Memory.Term.Let False b body
  Hashing.Term.Match scr cases -> Memory.Term.Match scr (h2mMatchCase <$> cases)
  Hashing.Term.TermLink r -> Memory.Term.TermLink (h2mReferent getCT r)
  Hashing.Term.TypeLink r -> Memory.Term.TypeLink (h2mReference r)

h2mMatchCase :: Hashing.Term.MatchCase a b -> Memory.Term.MatchCase a b
h2mMatchCase (Hashing.Term.MatchCase pat m_b b) = Memory.Term.MatchCase (h2mPattern pat) m_b b

h2mPattern :: Hashing.Pattern.Pattern a -> Memory.Pattern.Pattern a
h2mPattern = \case
  Hashing.Pattern.Unbound loc -> Memory.Pattern.Unbound loc
  Hashing.Pattern.Var loc -> Memory.Pattern.Var loc
  Hashing.Pattern.Boolean loc b -> Memory.Pattern.Boolean loc b
  Hashing.Pattern.Int loc i -> Memory.Pattern.Int loc i
  Hashing.Pattern.Nat loc n -> Memory.Pattern.Nat loc n
  Hashing.Pattern.Float loc f -> Memory.Pattern.Float loc f
  Hashing.Pattern.Text loc t -> Memory.Pattern.Text loc t
  Hashing.Pattern.Char loc c -> Memory.Pattern.Char loc c
  Hashing.Pattern.Constructor loc r i ps ->
    Memory.Pattern.Constructor loc (Memory.ConstructorReference.ConstructorReference (h2mReference r) i) (h2mPattern <$> ps)
  Hashing.Pattern.As loc p -> Memory.Pattern.As loc (h2mPattern p)
  Hashing.Pattern.EffectPure loc p -> Memory.Pattern.EffectPure loc (h2mPattern p)
  Hashing.Pattern.EffectBind loc r i ps k ->
    Memory.Pattern.EffectBind loc (Memory.ConstructorReference.ConstructorReference (h2mReference r) i) (h2mPattern <$> ps) (h2mPattern k)
  Hashing.Pattern.SequenceLiteral loc ps -> Memory.Pattern.SequenceLiteral loc (h2mPattern <$> ps)
  Hashing.Pattern.SequenceOp loc l op r -> Memory.Pattern.SequenceOp loc (h2mPattern l) (h2mSequenceOp op) (h2mPattern r)

h2mSequenceOp :: Hashing.Pattern.SeqOp -> Memory.Pattern.SeqOp
h2mSequenceOp = \case
  Hashing.Pattern.Cons -> Memory.Pattern.Cons
  Hashing.Pattern.Snoc -> Memory.Pattern.Snoc
  Hashing.Pattern.Concat -> Memory.Pattern.Concat

h2mReferent :: (Memory.Reference.Reference -> Memory.ConstructorType.ConstructorType) -> Hashing.Referent.Referent -> Memory.Referent.Referent
h2mReferent getCT = \case
  Hashing.Referent.Ref ref -> Memory.Referent.Ref (h2mReference ref)
  Hashing.Referent.Con ref n ->
    let mRef = h2mReference ref
     in Memory.Referent.Con (Memory.ConstructorReference.ConstructorReference mRef n) (getCT mRef)

hashDataDecls ::
  Var v =>
  Map v (Memory.DD.DataDeclaration v a) ->
  ResolutionResult v a [(v, Memory.Reference.Id, Memory.DD.DataDeclaration v a)]
hashDataDecls memDecls = do
  let hashingDecls = fmap m2hDecl memDecls
  hashingResult <- Hashing.DD.hashDecls hashingDecls
  pure $ map h2mDeclResult hashingResult
  where
    h2mDeclResult :: Ord v => (v, Hashing.Reference.Id, Hashing.DD.DataDeclaration v a) -> (v, Memory.Reference.Id, Memory.DD.DataDeclaration v a)
    h2mDeclResult (v, id, dd) = (v, h2mReferenceId id, h2mDecl dd)

hashDecls ::
  Var v =>
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

m2hDecl :: Ord v => Memory.DD.DataDeclaration v a -> Hashing.DD.DataDeclaration v a
m2hDecl (Memory.DD.DataDeclaration mod ann bound ctors) =
  Hashing.DD.DataDeclaration (m2hModifier mod) ann bound $ fmap (Lens.over _3 m2hType) ctors

m2hType :: Ord v => Memory.Type.Type v a -> Hashing.Type.Type v a
m2hType = ABT.transform \case
  Memory.Type.Ref ref -> Hashing.Type.Ref (m2hReference ref)
  Memory.Type.Arrow a1 a1' -> Hashing.Type.Arrow a1 a1'
  Memory.Type.Ann a1 ki -> Hashing.Type.Ann a1 (m2hKind ki)
  Memory.Type.App a1 a1' -> Hashing.Type.App a1 a1'
  Memory.Type.Effect a1 a1' -> Hashing.Type.Effect a1 a1'
  Memory.Type.Effects a1s -> Hashing.Type.Effects a1s
  Memory.Type.Forall a1 -> Hashing.Type.Forall a1
  Memory.Type.IntroOuter a1 -> Hashing.Type.IntroOuter a1

m2hKind :: Memory.Kind.Kind -> Hashing.Kind.Kind
m2hKind = \case
  Memory.Kind.Star -> Hashing.Kind.Star
  Memory.Kind.Arrow k1 k2 -> Hashing.Kind.Arrow (m2hKind k1) (m2hKind k2)

m2hReference :: Memory.Reference.Reference -> Hashing.Reference.Reference
m2hReference = \case
  Memory.Reference.Builtin t -> Hashing.Reference.Builtin t
  Memory.Reference.DerivedId d -> Hashing.Reference.DerivedId (m2hReferenceId d)

m2hReferenceId :: Memory.Reference.Id -> Hashing.Reference.Id
m2hReferenceId (Memory.Reference.Id h i) = Hashing.Reference.Id h i

h2mModifier :: Hashing.DD.Modifier -> Memory.DD.Modifier
h2mModifier = \case
  Hashing.DD.Structural -> Memory.DD.Structural
  Hashing.DD.Unique text -> Memory.DD.Unique text

m2hModifier :: Memory.DD.Modifier -> Hashing.DD.Modifier
m2hModifier = \case
  Memory.DD.Structural -> Hashing.DD.Structural
  Memory.DD.Unique text -> Hashing.DD.Unique text

h2mDecl :: Ord v => Hashing.DD.DataDeclaration v a -> Memory.DD.DataDeclaration v a
h2mDecl (Hashing.DD.DataDeclaration mod ann bound ctors) =
  Memory.DD.DataDeclaration (h2mModifier mod) ann bound (over _3 h2mType <$> ctors)

h2mType :: Ord v => Hashing.Type.Type v a -> Memory.Type.Type v a
h2mType = ABT.transform \case
  Hashing.Type.Ref ref -> Memory.Type.Ref (h2mReference ref)
  Hashing.Type.Arrow a1 a1' -> Memory.Type.Arrow a1 a1'
  Hashing.Type.Ann a1 ki -> Memory.Type.Ann a1 (h2mKind ki)
  Hashing.Type.App a1 a1' -> Memory.Type.App a1 a1'
  Hashing.Type.Effect a1 a1' -> Memory.Type.Effect a1 a1'
  Hashing.Type.Effects a1s -> Memory.Type.Effects a1s
  Hashing.Type.Forall a1 -> Memory.Type.Forall a1
  Hashing.Type.IntroOuter a1 -> Memory.Type.IntroOuter a1

h2mKind :: Hashing.Kind.Kind -> Memory.Kind.Kind
h2mKind = \case
  Hashing.Kind.Star -> Memory.Kind.Star
  Hashing.Kind.Arrow k1 k2 -> Memory.Kind.Arrow (h2mKind k1) (h2mKind k2)

h2mReference :: Hashing.Reference.Reference -> Memory.Reference.Reference
h2mReference = \case
  Hashing.Reference.Builtin t -> Memory.Reference.Builtin t
  Hashing.Reference.DerivedId d -> Memory.Reference.DerivedId (h2mReferenceId d)

h2mReferenceId :: Hashing.Reference.Id -> Memory.Reference.Id
h2mReferenceId (Hashing.Reference.Id h i) = Memory.Reference.Id h i

m2hPatch :: Memory.Patch.Patch -> Hashing.Patch.Patch
m2hPatch (Memory.Patch.Patch termEdits typeEdits) =
  Hashing.Patch.Patch termEdits' typeEdits'
  where
    typeEdits' =
      Map.fromList
        . map (bimap m2hReference (Set.map m2hTypeEdit))
        . Map.toList
        $ Relation.toMultimap typeEdits
    termEdits' =
      Map.fromList
        . map (bimap (Hashing.Referent.Ref . m2hReference) (Set.map m2hTermEdit))
        . Map.toList
        $ Relation.toMultimap termEdits
    m2hTermEdit = \case
      Memory.TermEdit.Replace r _ -> Hashing.TermEdit.Replace (Hashing.Referent.Ref $ m2hReference r)
      Memory.TermEdit.Deprecate -> Hashing.TermEdit.Deprecate
    m2hTypeEdit = \case
      Memory.TypeEdit.Replace r -> Hashing.TypeEdit.Replace (m2hReference r)
      Memory.TypeEdit.Deprecate -> Hashing.TypeEdit.Deprecate

hashPatch :: Memory.Patch.Patch -> Hash
hashPatch = Hashing.Patch.hashPatch . m2hPatch

hashBranch0 :: Memory.Branch.Branch0 m -> Hash
hashBranch0 = Hashing.Branch.hashBranch . m2hBranch0

hashCausal :: Hashable e => e -> Set Memory.Causal.CausalHash -> (Memory.Causal.CausalHash, HashFor e)
hashCausal e tails =
  let valueHash@(HashFor vh) = (Hashable.hashFor e)
      causalHash =
        Memory.Causal.CausalHash . Hashing.Causal.hashCausal $
          Hashing.Causal.Causal vh (Set.map Memory.Causal.unCausalHash tails)
   in (causalHash, valueHash)

m2hBranch0 :: Memory.Branch.Branch0 m -> Hashing.Branch.Raw
m2hBranch0 b =
  Hashing.Branch.Raw
    (doTerms (Memory.Branch._terms b))
    (doTypes (Memory.Branch._types b))
    (doPatches (Memory.Branch._edits b))
    (doChildren (Memory.Branch._children b))
  where
    -- is there a more readable way to structure these that's also linear?
    doTerms ::
      Memory.Branch.Star Memory.Referent.Referent Memory.NameSegment.NameSegment ->
      Map Hashing.Branch.NameSegment (Map Hashing.Referent.Referent Hashing.Branch.MdValues)
    doTerms s =
      Map.fromList
        [ (m2hNameSegment ns, m2)
          | ns <- toList . Relation.ran $ Memory.Star3.d1 s,
            let m2 =
                  Map.fromList
                    [ (fst (Writer.runWriter (m2hReferent r)), md)
                      | r <- toList . Relation.lookupRan ns $ Memory.Star3.d1 s,
                        let mdrefs1to2 (_typeR1, valR1) = m2hReference valR1
                            md = Hashing.Branch.MdValues . Set.map mdrefs1to2 . Relation.lookupDom r $ Memory.Star3.d3 s
                    ]
        ]

    doTypes ::
      Memory.Branch.Star Memory.Reference.Reference Memory.NameSegment.NameSegment ->
      Map Hashing.Branch.NameSegment (Map Hashing.Reference.Reference Hashing.Branch.MdValues)
    doTypes s =
      Map.fromList
        [ (m2hNameSegment ns, m2)
          | ns <- toList . Relation.ran $ Memory.Star3.d1 s,
            let m2 =
                  Map.fromList
                    [ (m2hReference r, md)
                      | r <- toList . Relation.lookupRan ns $ Memory.Star3.d1 s,
                        let mdrefs1to2 (_typeR1, valR1) = m2hReference valR1
                            md :: Hashing.Branch.MdValues
                            md = Hashing.Branch.MdValues . Set.map mdrefs1to2 . Relation.lookupDom r $ Memory.Star3.d3 s
                    ]
        ]

    doPatches ::
      Map Memory.NameSegment.NameSegment (Memory.Branch.EditHash, m Memory.Patch.Patch) ->
      Map Hashing.Branch.NameSegment Hash
    doPatches = Map.bimap m2hNameSegment fst

    doChildren ::
      Map Memory.NameSegment.NameSegment (Memory.Branch.Branch m) ->
      Map Hashing.Branch.NameSegment Hash
    doChildren = Map.bimap m2hNameSegment (Memory.Causal.unCausalHash . Memory.Branch.headHash)

m2hNameSegment :: Memory.NameSegment.NameSegment -> Hashing.Branch.NameSegment
m2hNameSegment (Memory.NameSegment.NameSegment s) = Hashing.Branch.NameSegment s
