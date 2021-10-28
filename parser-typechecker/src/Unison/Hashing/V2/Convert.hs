{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Hashing.V2.Convert
  ( ResolutionResult,
    tokensBranch0,
    hashDecls,
    hashDecls',
    hashPatch,
    hashClosedTerm,
    hashTermComponents,
    hashTermComponents',
    hashTypeComponents,
    typeToReference,
    typeToReferenceMentions,
  )
where

import Control.Lens (over, _3)
import qualified Control.Lens as Lens
import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.Codebase.Branch.Type as Memory.Branch
import qualified Unison.Codebase.Causal as Memory.Causal
import qualified Unison.Codebase.Patch as Memory.Patch
import qualified Unison.Codebase.TermEdit as Memory.TermEdit
import qualified Unison.Codebase.TypeEdit as Memory.TypeEdit
import qualified Unison.DataDeclaration as Memory.DD
import Unison.Hash (Hash)
import Unison.Hashable (Accumulate, Token)
import qualified Unison.Hashable as H
import qualified Unison.Hashing.V2.Branch as Hashing.Branch
import qualified Unison.Hashing.V2.Causal as Hashing.Causal
import qualified Unison.Hashing.V2.DataDeclaration as Hashing.DD
import qualified Unison.Hashing.V2.Patch as Hashing.Patch
import qualified Unison.Hashing.V2.Pattern as Hashing.Pattern
import qualified Unison.Hashing.V2.Reference as Hashing.Reference
import qualified Unison.Hashing.V2.Referent as Hashing.Referent
import qualified Unison.Hashing.V2.Term as Hashing.Term
import qualified Unison.Hashing.V2.TermEdit as Hashing.TermEdit
import qualified Unison.Hashing.V2.Type as Hashing.Type
import qualified Unison.Hashing.V2.TypeEdit as Hashing.TypeEdit
import Unison.NameSegment (NameSegment)
import Unison.Names.ResolutionResult (ResolutionResult)
import qualified Unison.Pattern as Memory.Pattern
import qualified Unison.Reference as Memory.Reference
import qualified Unison.Referent as Memory.Referent
import qualified Unison.Term as Memory.Term
import qualified Unison.Type as Memory.Type
import qualified Unison.Util.Relation as Relation
import qualified Unison.Util.Star3 as Memory.Star3
import Unison.Var (Var)

typeToReference :: Var v => Memory.Type.Type v a -> Memory.Reference.Reference
typeToReference = h2mReference . Hashing.Type.toReference . m2hType . Memory.Type.removeAllEffectVars

typeToReferenceMentions :: Var v => Memory.Type.Type v a -> Set Memory.Reference.Reference
typeToReferenceMentions = Set.map h2mReference . Hashing.Type.toReferenceMentions . m2hType . Memory.Type.removeAllEffectVars

hashTypeComponents :: Var v => Map v (Memory.Type.Type v a) -> Map v (Memory.Reference.Id, Memory.Type.Type v a)
hashTypeComponents = fmap h2mTypeResult . Hashing.Type.hashComponents . fmap m2hType
  where
    h2mTypeResult :: Ord v => (Hashing.Reference.Id, Hashing.Type.Type v a) -> (Memory.Reference.Id, Memory.Type.Type v a)
    h2mTypeResult (id, tp) = (h2mReferenceId id, h2mType tp)

hashTermComponents :: Var v => Map v (Memory.Term.Term v a) -> Map v (Memory.Reference.Id, Memory.Term.Term v a)
hashTermComponents = fmap h2mTermResult . Hashing.Term.hashComponents . fmap m2hTerm
  where
    h2mTermResult :: Ord v => (Hashing.Reference.Id, Hashing.Term.Term v a) -> (Memory.Reference.Id, Memory.Term.Term v a)
    h2mTermResult (id, tm) = (h2mReferenceId id, h2mTerm tm)

-- TODO: remove non-prime version
-- include type in hash
hashTermComponents' :: Var v => Map v (Memory.Term.Term v a, Memory.Type.Type v a) -> Map v (Memory.Reference.Id, Memory.Term.Term v a, Memory.Type.Type v a)
hashTermComponents' = undefined

hashClosedTerm :: Var v => Memory.Term.Term v a -> Memory.Reference.Id
hashClosedTerm = h2mReferenceId . Hashing.Term.hashClosedTerm . m2hTerm

m2hTerm :: Ord v => Memory.Term.Term v a -> Hashing.Term.Term v a
m2hTerm = ABT.transform \case
  Memory.Term.Int i -> Hashing.Term.Int i
  Memory.Term.Nat n -> Hashing.Term.Nat n
  Memory.Term.Float d -> Hashing.Term.Float d
  Memory.Term.Boolean b -> Hashing.Term.Boolean b
  Memory.Term.Text t -> Hashing.Term.Text t
  Memory.Term.Char c -> Hashing.Term.Char c
  Memory.Term.Blank b -> Hashing.Term.Blank b
  Memory.Term.Ref r -> Hashing.Term.Ref (m2hReference r)
  Memory.Term.Constructor r i -> Hashing.Term.Constructor (m2hReference r) i
  Memory.Term.Request r i -> Hashing.Term.Request (m2hReference r) i
  Memory.Term.Handle x y -> Hashing.Term.Handle x y
  Memory.Term.App f x -> Hashing.Term.App f x
  Memory.Term.Ann e t -> Hashing.Term.Ann e (m2hType t)
  Memory.Term.List as -> Hashing.Term.List as
  Memory.Term.And p q -> Hashing.Term.And p q
  Memory.Term.If c t f -> Hashing.Term.If c t f
  Memory.Term.Or p q -> Hashing.Term.Or p q
  Memory.Term.Lam a -> Hashing.Term.Lam a
  Memory.Term.LetRec isTop bs body -> Hashing.Term.LetRec isTop bs body
  Memory.Term.Let isTop b body -> Hashing.Term.Let isTop b body
  Memory.Term.Match scr cases -> Hashing.Term.Match scr (fmap m2hMatchCase cases)
  Memory.Term.TermLink r -> Hashing.Term.TermLink (m2hReferent r)
  Memory.Term.TypeLink r -> Hashing.Term.TypeLink (m2hReference r)

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
  Memory.Pattern.Constructor loc r i ps -> Hashing.Pattern.Constructor loc (m2hReference r) i (fmap m2hPattern ps)
  Memory.Pattern.As loc p -> Hashing.Pattern.As loc (m2hPattern p)
  Memory.Pattern.EffectPure loc p -> Hashing.Pattern.EffectPure loc (m2hPattern p)
  Memory.Pattern.EffectBind loc r i ps k -> Hashing.Pattern.EffectBind loc (m2hReference r) i (fmap m2hPattern ps) (m2hPattern k)
  Memory.Pattern.SequenceLiteral loc ps -> Hashing.Pattern.SequenceLiteral loc (fmap m2hPattern ps)
  Memory.Pattern.SequenceOp loc l op r -> Hashing.Pattern.SequenceOp loc (m2hPattern l) (m2hSequenceOp op) (m2hPattern r)

m2hSequenceOp :: Memory.Pattern.SeqOp -> Hashing.Pattern.SeqOp
m2hSequenceOp = \case
  Memory.Pattern.Cons -> Hashing.Pattern.Cons
  Memory.Pattern.Snoc -> Hashing.Pattern.Snoc
  Memory.Pattern.Concat -> Hashing.Pattern.Concat

m2hReferent :: Memory.Referent.Referent -> Hashing.Referent.Referent
m2hReferent = \case
  Memory.Referent.Ref ref -> Hashing.Referent.Ref (m2hReference ref)
  Memory.Referent.Con ref n ct -> Hashing.Referent.Con (m2hReference ref) n ct

h2mTerm :: Ord v => Hashing.Term.Term v a -> Memory.Term.Term v a
h2mTerm = ABT.transform \case
  Hashing.Term.Int i -> Memory.Term.Int i
  Hashing.Term.Nat n -> Memory.Term.Nat n
  Hashing.Term.Float d -> Memory.Term.Float d
  Hashing.Term.Boolean b -> Memory.Term.Boolean b
  Hashing.Term.Text t -> Memory.Term.Text t
  Hashing.Term.Char c -> Memory.Term.Char c
  Hashing.Term.Blank b -> Memory.Term.Blank b
  Hashing.Term.Ref r -> Memory.Term.Ref (h2mReference r)
  Hashing.Term.Constructor r i -> Memory.Term.Constructor (h2mReference r) i
  Hashing.Term.Request r i -> Memory.Term.Request (h2mReference r) i
  Hashing.Term.Handle x y -> Memory.Term.Handle x y
  Hashing.Term.App f x -> Memory.Term.App f x
  Hashing.Term.Ann e t -> Memory.Term.Ann e (h2mType t)
  Hashing.Term.List as -> Memory.Term.List as
  Hashing.Term.If c t f -> Memory.Term.If c t f
  Hashing.Term.And p q -> Memory.Term.And p q
  Hashing.Term.Or p q -> Memory.Term.Or p q
  Hashing.Term.Lam a -> Memory.Term.Lam a
  Hashing.Term.LetRec isTop bs body -> Memory.Term.LetRec isTop bs body
  Hashing.Term.Let isTop b body -> Memory.Term.Let isTop b body
  Hashing.Term.Match scr cases -> Memory.Term.Match scr (h2mMatchCase <$> cases)
  Hashing.Term.TermLink r -> Memory.Term.TermLink (h2mReferent r)
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
  Hashing.Pattern.Constructor loc r i ps -> Memory.Pattern.Constructor loc (h2mReference r) i (h2mPattern <$> ps)
  Hashing.Pattern.As loc p -> Memory.Pattern.As loc (h2mPattern p)
  Hashing.Pattern.EffectPure loc p -> Memory.Pattern.EffectPure loc (h2mPattern p)
  Hashing.Pattern.EffectBind loc r i ps k -> Memory.Pattern.EffectBind loc (h2mReference r) i (h2mPattern <$> ps) (h2mPattern k)
  Hashing.Pattern.SequenceLiteral loc ps -> Memory.Pattern.SequenceLiteral loc (h2mPattern <$> ps)
  Hashing.Pattern.SequenceOp loc l op r -> Memory.Pattern.SequenceOp loc (h2mPattern l) (h2mSequenceOp op) (h2mPattern r)

h2mSequenceOp :: Hashing.Pattern.SeqOp -> Memory.Pattern.SeqOp
h2mSequenceOp = \case
  Hashing.Pattern.Cons -> Memory.Pattern.Cons
  Hashing.Pattern.Snoc -> Memory.Pattern.Snoc
  Hashing.Pattern.Concat -> Memory.Pattern.Concat

h2mReferent :: Hashing.Referent.Referent -> Memory.Referent.Referent
h2mReferent = \case
  Hashing.Referent.Ref ref -> Memory.Referent.Ref (h2mReference ref)
  Hashing.Referent.Con ref n ct -> Memory.Referent.Con (h2mReference ref) n ct

hashDecls ::
  Var v =>
  Map v (Memory.DD.DataDeclaration v a) ->
  ResolutionResult v a [(v, Memory.Reference.Id, Memory.DD.DataDeclaration v a)]
hashDecls memDecls = do
  let hashingDecls = fmap m2hDecl memDecls
  hashingResult <- Hashing.DD.hashDecls hashingDecls
  pure $ map h2mDeclResult hashingResult
  where
    h2mDeclResult :: Ord v => (v, Hashing.Reference.Id, Hashing.DD.DataDeclaration v a) -> (v, Memory.Reference.Id, Memory.DD.DataDeclaration v a)
    h2mDeclResult (v, id, dd) = (v, h2mReferenceId id, h2mDecl dd)

-- TODO: rename hashDecls to hashDataDecls, remove tick from this
hashDecls' ::
  Var v =>
  Map v (Memory.DD.Decl v a) ->
  ResolutionResult v a [(v, Memory.Reference.Id, Memory.DD.Decl v a)]
hashDecls' = undefined

m2hDecl :: Ord v => Memory.DD.DataDeclaration v a -> Hashing.DD.DataDeclaration v a
m2hDecl (Memory.DD.DataDeclaration mod ann bound ctors) =
  Hashing.DD.DataDeclaration (m2hModifier mod) ann bound $ fmap (Lens.over _3 m2hType) ctors

m2hType :: Ord v => Memory.Type.Type v a -> Hashing.Type.Type v a
m2hType = ABT.transform \case
  Memory.Type.Ref ref -> Hashing.Type.Ref (m2hReference ref)
  Memory.Type.Arrow a1 a1' -> Hashing.Type.Arrow a1 a1'
  Memory.Type.Ann a1 ki -> Hashing.Type.Ann a1 ki
  Memory.Type.App a1 a1' -> Hashing.Type.App a1 a1'
  Memory.Type.Effect a1 a1' -> Hashing.Type.Effect a1 a1'
  Memory.Type.Effects a1s -> Hashing.Type.Effects a1s
  Memory.Type.Forall a1 -> Hashing.Type.Forall a1
  Memory.Type.IntroOuter a1 -> Hashing.Type.IntroOuter a1

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
  Hashing.Type.Ann a1 ki -> Memory.Type.Ann a1 ki
  Hashing.Type.App a1 a1' -> Memory.Type.App a1 a1'
  Hashing.Type.Effect a1 a1' -> Memory.Type.Effect a1 a1'
  Hashing.Type.Effects a1s -> Memory.Type.Effects a1s
  Hashing.Type.Forall a1 -> Memory.Type.Forall a1
  Hashing.Type.IntroOuter a1 -> Memory.Type.IntroOuter a1

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
hashPatch = H.accumulate' . m2hPatch

tokensBranch0 :: Accumulate h => Memory.Branch.Branch0 m -> [Token h]
tokensBranch0 = H.tokens . m2hBranch

-- hashing of branches isn't currently delegated here, because it's enmeshed
-- with every `cons` or `step` function.  I think it would be good to do while
-- we're updating the hash function, but I'm also not looking forward to doing it
-- and it's not clearly a problem yet.
_hashBranch :: Memory.Branch.Branch m -> Hash
_hashBranch = H.accumulate . _tokensBranch

_tokensBranch :: Accumulate h => Memory.Branch.Branch m -> [Token h]
_tokensBranch = H.tokens . _m2hCausal . Memory.Branch._history

_m2hCausal :: Memory.Branch.UnwrappedBranch m -> Hashing.Causal.Causal -- Hashing.Branch.Raw
_m2hCausal = undefined -- TODO: re-implement
  --  \case
  --Memory.Causal.One _h e ->
  --  Hashing.Causal.Causal (m2hBranch e) mempty
  --Memory.Causal.Cons _h e (ht, _) ->
  --  Hashing.Causal.Causal (m2hBranch e) $ Set.singleton (Memory.Causal.unRawHash ht)
  --Memory.Causal.Merge _h e ts ->
  --  Hashing.Causal.Causal (m2hBranch e) $ Set.map Memory.Causal.unRawHash (Map.keysSet ts)

m2hBranch :: Memory.Branch.Branch0 m -> Hashing.Branch.Raw
m2hBranch b =
  Hashing.Branch.Raw
    (doTerms (Memory.Branch._terms b))
    (doTypes (Memory.Branch._types b))
    (doPatches (Memory.Branch._edits b))
    (doChildren (Memory.Branch._children b))
  where
    -- is there a more readable way to structure these that's also linear?
    doTerms :: Memory.Branch.Star Memory.Referent.Referent NameSegment -> Map NameSegment (Map Hashing.Referent.Referent Hashing.Branch.MdValues)
    doTerms s =
      Map.fromList
        [ (ns, m2)
          | ns <- toList . Relation.ran $ Memory.Star3.d1 s,
            let m2 =
                  Map.fromList
                    [ (m2hReferent r, md)
                      | r <- toList . Relation.lookupRan ns $ Memory.Star3.d1 s,
                        let mdrefs1to2 (_typeR1, valR1) = m2hReference valR1
                            md = Hashing.Branch.MdValues . Set.map mdrefs1to2 . Relation.lookupDom r $ Memory.Star3.d3 s
                    ]
        ]

    doTypes :: Memory.Branch.Star Memory.Reference.Reference NameSegment -> Map NameSegment (Map Hashing.Reference.Reference Hashing.Branch.MdValues)
    doTypes s =
      Map.fromList
        [ (ns, m2)
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

    doPatches :: Map NameSegment (Memory.Branch.EditHash, m Memory.Patch.Patch) -> Map NameSegment Hash
    doPatches = Map.map fst

    doChildren :: Map NameSegment (Memory.Branch.Branch m) -> Map NameSegment Hash
    doChildren = Map.map (Memory.Causal.unRawHash . Memory.Branch.headHash)
