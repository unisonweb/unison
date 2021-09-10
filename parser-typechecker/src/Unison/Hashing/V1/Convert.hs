{-# LANGUAGE ViewPatterns #-}

module Unison.Hashing.V1.Convert (hashDecls, hashTermComponents, hashTypeComponents) where

import Control.Lens (over, _3)
import qualified Control.Lens as Lens
import Control.Monad.Validate (Validate)
import qualified Control.Monad.Validate as Validate
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Unison.ABT as ABT
import qualified Unison.DataDeclaration as Memory.DD
import Unison.Hash (Hash)
import qualified Unison.Hashing.V1.DataDeclaration as Hashing.DD
import qualified Unison.Hashing.V1.Pattern as Hashing.Pattern
import qualified Unison.Hashing.V1.Reference as Hashing.Reference
import qualified Unison.Hashing.V1.Referent as Hashing.Referent
import qualified Unison.Hashing.V1.Term as Hashing.Term
import qualified Unison.Hashing.V1.Type as Hashing.Type
import qualified Unison.Names.ResolutionResult as Names
import qualified Unison.Pattern as Memory.Pattern
import qualified Unison.Reference as Memory.Reference
import qualified Unison.Referent as Memory.Referent
import qualified Unison.Term as Memory.Term
import qualified Unison.Type as Memory.Type
import Unison.Var (Var)

data ResolutionFailure v a
  = TermResolutionFailure v a (Set Memory.Referent.Referent)
  | TypeResolutionFailure v a (Set Memory.Reference.Reference)
  | CycleResolutionFailure Hash
  deriving (Eq, Ord, Show)

type ResolutionResult v a r = Validate (Seq (ResolutionFailure v a)) r

convertResolutionResult :: Names.ResolutionResult v a r -> ResolutionResult v a r
convertResolutionResult = \case
  Left e -> Validate.refute (fmap f e)
  Right a -> pure a
  where
    f = \case
      Names.TermResolutionFailure v a rs -> TermResolutionFailure v a rs
      Names.TypeResolutionFailure v a rs -> TypeResolutionFailure v a rs

hashTypeComponents ::
  Var v => (Hash -> Maybe Hashing.Reference.Size) -> Map v (Memory.Type.Type v a) -> Validate (Seq Hash) (Map v (Memory.Reference.Id, Memory.Type.Type v a))
hashTypeComponents f memTypes = do
  hashingTypes <- traverse (m2hType f) memTypes
  let hashingResult = Hashing.Type.hashComponents hashingTypes
  pure $ fmap h2mTypeResult hashingResult
  where
    h2mTypeResult :: Ord v => (Hashing.Reference.Id, Hashing.Type.Type v a) -> (Memory.Reference.Id, Memory.Type.Type v a)
    h2mTypeResult (id, tp) = (h2mReferenceId id, h2mType tp)

hashTermComponents :: Var v => (Hash -> Maybe Hashing.Reference.Size) -> Map v (Memory.Term.Term v a) -> Validate (Seq Hash) (Map v (Memory.Reference.Id, Memory.Term.Term v a))
hashTermComponents f memTerms = do
  hashingTerms <- traverse (m2hTerm f) memTerms
  let hashingResult = Hashing.Term.hashComponents hashingTerms
  pure $ fmap h2mTermResult hashingResult
  where
    h2mTermResult :: Ord v => (Hashing.Reference.Id, Hashing.Term.Term v a) -> (Memory.Reference.Id, Memory.Term.Term v a)
    h2mTermResult (id, tm) = (h2mReferenceId id, h2mTerm tm)


m2hTerm :: Ord v => (Hash -> Maybe Hashing.Reference.Size) -> Memory.Term.Term v a -> Validate (Seq Hash) (Hashing.Term.Term v a)
m2hTerm f = ABT.transformM \case
  Memory.Term.Int i -> pure $ Hashing.Term.Int i
  Memory.Term.Nat n -> pure $ Hashing.Term.Nat n
  Memory.Term.Float d -> pure $ Hashing.Term.Float d
  Memory.Term.Boolean b -> pure $ Hashing.Term.Boolean b
  Memory.Term.Text t -> pure $ Hashing.Term.Text t
  Memory.Term.Char c -> pure $ Hashing.Term.Char c
  Memory.Term.Blank b -> pure $ Hashing.Term.Blank b
  Memory.Term.Ref r -> Hashing.Term.Ref <$> m2hReference f r
  Memory.Term.Constructor r i -> Hashing.Term.Constructor <$> m2hReference f r <*> pure i
  Memory.Term.Request r i -> Hashing.Term.Request <$> m2hReference f r <*> pure i
  Memory.Term.Handle x y -> pure $ Hashing.Term.Handle x y
  Memory.Term.App f x -> pure $ Hashing.Term.App f x
  Memory.Term.Ann e t -> Hashing.Term.Ann e <$> m2hType f t
  Memory.Term.List as -> pure $ Hashing.Term.List as
  Memory.Term.If c t f -> pure $ Hashing.Term.If c t f
  Memory.Term.And p q -> pure $ Hashing.Term.And p q
  Memory.Term.Or p q -> pure $ Hashing.Term.Or p q
  Memory.Term.Lam a -> pure $ Hashing.Term.Lam a
  Memory.Term.LetRec isTop bs body -> pure $ Hashing.Term.LetRec isTop bs body
  Memory.Term.Let isTop b body -> pure $ Hashing.Term.Let isTop b body
  Memory.Term.Match scr cases -> Hashing.Term.Match scr <$> traverse (m2hMatchCase f) cases
  Memory.Term.TermLink r -> Hashing.Term.TermLink <$> m2hReferent f r
  Memory.Term.TypeLink r -> Hashing.Term.TypeLink <$> m2hReference f r

m2hMatchCase :: (Hash -> Maybe Hashing.Reference.Size) -> Memory.Term.MatchCase a a1 -> Validate (Seq Hash) (Hashing.Term.MatchCase a a1)
m2hMatchCase f (Memory.Term.MatchCase pat m_a1 a1) = Hashing.Term.MatchCase <$> m2hPattern f pat <*> pure m_a1 <*> pure a1

m2hPattern :: (Hash -> Maybe Hashing.Reference.Size) -> Memory.Pattern.Pattern a -> Validate (Seq Hash) (Hashing.Pattern.Pattern a)
m2hPattern f = \case
  Memory.Pattern.Unbound loc -> pure $ Hashing.Pattern.Unbound loc
  Memory.Pattern.Var loc -> pure $ Hashing.Pattern.Var loc
  Memory.Pattern.Boolean loc b -> pure $ Hashing.Pattern.Boolean loc b
  Memory.Pattern.Int loc i -> pure $ Hashing.Pattern.Int loc i
  Memory.Pattern.Nat loc n -> pure $ Hashing.Pattern.Nat loc n
  Memory.Pattern.Float loc f -> pure $ Hashing.Pattern.Float loc f
  Memory.Pattern.Text loc t -> pure $ Hashing.Pattern.Text loc t
  Memory.Pattern.Char loc c -> pure $ Hashing.Pattern.Char loc c
  Memory.Pattern.Constructor loc r i ps -> Hashing.Pattern.Constructor loc <$> m2hReference f r <*> pure i <*> traverse (m2hPattern f) ps
  Memory.Pattern.As loc p -> Hashing.Pattern.As loc <$> m2hPattern f p
  Memory.Pattern.EffectPure loc p -> Hashing.Pattern.EffectPure loc <$> m2hPattern f p
  Memory.Pattern.EffectBind loc r i ps k -> Hashing.Pattern.EffectBind loc <$> m2hReference f r <*> pure i <*> traverse (m2hPattern f) ps <*> m2hPattern f k
  Memory.Pattern.SequenceLiteral loc ps -> Hashing.Pattern.SequenceLiteral loc <$> traverse (m2hPattern f) ps
  Memory.Pattern.SequenceOp loc l op r -> Hashing.Pattern.SequenceOp loc <$> m2hPattern f l <*> pure (m2hSequenceOp op) <*> m2hPattern f r

m2hSequenceOp :: Memory.Pattern.SeqOp -> Hashing.Pattern.SeqOp
m2hSequenceOp = \case
      Memory.Pattern.Cons -> Hashing.Pattern.Cons
      Memory.Pattern.Snoc -> Hashing.Pattern.Snoc
      Memory.Pattern.Concat -> Hashing.Pattern.Concat

m2hReferent :: (Hash -> Maybe Hashing.Reference.Size) -> Memory.Referent.Referent -> Validate (Seq Hash) Hashing.Referent.Referent
m2hReferent f = \case
  Memory.Referent.Ref ref -> Hashing.Referent.Ref <$> m2hReference f ref
  Memory.Referent.Con ref n ct -> Hashing.Referent.Con <$> m2hReference f ref <*> pure n <*> pure ct

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
  (Hash -> Maybe Hashing.Reference.Size) ->
  Map v (Memory.DD.DataDeclaration v a) ->
  ResolutionResult v a [(v, Memory.Reference.Id, Memory.DD.DataDeclaration v a)]
hashDecls f memDecls = do
  hashingDecls <- Validate.mapErrors (fmap CycleResolutionFailure) $ traverse (m2hDecl f) memDecls
  hashingResult <- convertResolutionResult $ Hashing.DD.hashDecls hashingDecls
  pure $ map h2mDeclResult hashingResult
  where
    h2mDeclResult :: Ord v => (v, Hashing.Reference.Id, Hashing.DD.DataDeclaration v a) -> (v, Memory.Reference.Id, Memory.DD.DataDeclaration v a)
    h2mDeclResult (v, id, dd) = (v, h2mReferenceId id, h2mDecl dd)

m2hDecl ::
  Ord v =>
  (Hash -> Maybe Hashing.Reference.Size) ->
  Memory.DD.DataDeclaration v a ->
  Validate (Seq Hash) (Hashing.DD.DataDeclaration v a)
m2hDecl f (Memory.DD.DataDeclaration mod ann bound ctors) =
  Hashing.DD.DataDeclaration (m2hModifier mod) ann bound
    <$> traverse (Lens.mapMOf _3 (m2hType f)) ctors

lookupHash :: (Hash -> Maybe Hashing.Reference.Size) -> Hash -> Validate (Seq Hash) Hashing.Reference.Size
lookupHash f h = case f h of
  Just size -> pure size
  Nothing -> Validate.refute $ pure h

m2hType ::
  Ord v =>
  (Hash -> Maybe Hashing.Reference.Size) ->
  Memory.Type.Type v a ->
  Validate (Seq Hash) (Hashing.Type.Type v a)
m2hType f = ABT.transformM \case
  Memory.Type.Ref ref -> Hashing.Type.Ref <$> m2hReference f ref
  Memory.Type.Arrow a1 a1' -> pure $ Hashing.Type.Arrow a1 a1'
  Memory.Type.Ann a1 ki -> pure $ Hashing.Type.Ann a1 ki
  Memory.Type.App a1 a1' -> pure $ Hashing.Type.App a1 a1'
  Memory.Type.Effect a1 a1' -> pure $ Hashing.Type.Effect a1 a1'
  Memory.Type.Effects a1s -> pure $ Hashing.Type.Effects a1s
  Memory.Type.Forall a1 -> pure $ Hashing.Type.Forall a1
  Memory.Type.IntroOuter a1 -> pure $ Hashing.Type.IntroOuter a1

m2hReference ::
  (Hash -> Maybe Hashing.Reference.Size) ->
  Memory.Reference.Reference ->
  Validate (Seq Hash) Hashing.Reference.Reference
m2hReference f = \case
  Memory.Reference.Builtin t -> pure $ Hashing.Reference.Builtin t
  Memory.Reference.DerivedId d -> Hashing.Reference.DerivedId <$> m2hReferenceId f d

m2hReferenceId ::
  (Hash -> Maybe Hashing.Reference.Size) ->
  Memory.Reference.Id ->
  Validate (Seq Hash) Hashing.Reference.Id
m2hReferenceId f (Memory.Reference.Id h i _n) = Hashing.Reference.Id h i <$> lookupHash f h

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
h2mReferenceId (Hashing.Reference.Id h i n) = Memory.Reference.Id h i n
