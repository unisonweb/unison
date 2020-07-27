{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase2a.Serialization.V2 where

import Prelude hiding (getChar, putChar)

import Data.Bytes.Get
import Data.Bytes.Put
import qualified Data.Sequence as Sequence

import Unison.Term (TermH)
import qualified Unison.Term as Term
import Unison.Type (TypeH)
--import qualified Unison.Type as Type
import qualified Unison.Pattern as Pattern

import Unison.Codebase.Serialization.V1
  ( getInt, getNat, getFloat, getBoolean, getText
  , putInt, putNat, putFloat, putBoolean, putText
  , {-getEither,-} getMaybe
  , putEither, putMaybe
  , getABT, getChar, getList,     getLength, getKind, {-getModifier,-} getPatternH, getReferenceH, getReferentH
  , putABT, putChar, putFoldable, putLength, putKind, putModifier, putSeqOp,    putReferenceH, putReferentH
  , putTuple3
  , unknownTag )

import qualified Unison.DataDeclaration as DD
import qualified Unison.Referent as Referent
import qualified Unison.Reference as Reference
import qualified Unison.Type as Type
import Data.Maybe (fromMaybe, fromJust)
import Unison.Pattern (PatternH)

putType :: (MonadPut m, Eq h, Ord v)
        => (h -> m ()) -> (v -> m ()) -> (a -> m ())
        -> TypeH (Maybe h) v a
        -> m ()
putType putHash putVar putA = putABT putVar putA go where
  go putChild t = case t of
    Type.Ref r       -> case r of
     Reference.Derived Nothing i _n
                     -> putWord8 8 *> putLength i
     (Reference.hmap fromJust -> r)
                     -> putWord8 0 *> putReferenceH putHash r
    Type.Arrow i o   -> putWord8 1 *> putChild i *> putChild o
    Type.Ann t k     -> putWord8 2 *> putChild t *> putKind k
    Type.App f x     -> putWord8 3 *> putChild f *> putChild x
    Type.Effect e t  -> putWord8 4 *> putChild e *> putChild t
    Type.Effects es  -> putWord8 5 *> putFoldable putChild es
    Type.Forall body -> putWord8 6 *> putChild body
    Type.IntroOuter body -> putWord8 7 *> putChild body

-- we assume a term is always part of a component, and thus has a known
-- component hash, whereas a type only has a one as part of a decl, not as part
-- of a Term.F.Ann
getType :: (MonadGet m, Ord v)
        => m h -> m v -> m a -> Maybe h -> m (TypeH h v a)
getType getHash getVar getA selfHash = getABT getVar getA go where
  go getChild = getWord8 >>= \tag -> case tag of
    8 -> let
      err = error "V2.getType encountered a self-reference where no self-hash was provided"
      err2 = error "todo: removed reference length field"
      h = fromMaybe err selfHash
      in Type.Ref <$> (Reference.Derived h <$> getLength <*> pure err2)
    0 -> Type.Ref <$> getReferenceH getHash
    1 -> Type.Arrow <$> getChild <*> getChild
    2 -> Type.Ann <$> getChild <*> getKind
    3 -> Type.App <$> getChild <*> getChild
    4 -> Type.Effect <$> getChild <*> getChild
    5 -> Type.Effects <$> getList getChild
    6 -> Type.Forall <$> getChild
    7 -> Type.IntroOuter <$> getChild
    _ -> unknownTag "getType" tag

putTerm :: (MonadPut m, Eq h, Ord v)
        => (h -> m ()) -> (v -> m ()) -> (a -> m ())
        -> TermH (Maybe h) v a
        -> m ()
putTerm putH putVar putA = putABT putVar putA go where
  err = error "V2.putTerm got an unexpected self-reference"
  collapse = Reference.hmap (fromMaybe err)
  collapset = Referent.hmap (fromMaybe err)
  go putChild t = case t of
    Term.Int n
      -> putWord8 0 *> putInt n
    Term.Nat n
      -> putWord8 1 *> putNat n
    Term.Float n
      -> putWord8 2 *> putFloat n
    Term.Boolean b
      -> putWord8 3 *> putBoolean b
    Term.Text t
      -> putWord8 4 *> putText t
    Term.Blank _
      -> error "can't serialize term with blanks"
    Term.Ref (Reference.Derived Nothing i _n)
      -> putWord8 22 *> putLength i
    Term.Ref (collapse -> r)
      -> putWord8 5 *> putReferenceH putH r
    Term.Constructor (collapse -> r) cid
      -> putWord8 6 *> putReferenceH putH r *> putLength cid
    Term.Request (collapse -> r) cid
      -> putWord8 7 *> putReferenceH putH r *> putLength cid
    Term.Handle h a
      -> putWord8 8 *> putChild h *> putChild a
    Term.App f arg
      -> putWord8 9 *> putChild f *> putChild arg
    Term.Ann e t
      -> putWord8 10 *> putChild e *> putType putH putVar putA t
    Term.Sequence vs
      -> putWord8 11 *> putFoldable putChild vs
    Term.If cond t f
      -> putWord8 12 *> putChild cond *> putChild t *> putChild f
    Term.And x y
      -> putWord8 13 *> putChild x *> putChild y
    Term.Or x y
      -> putWord8 14 *> putChild x *> putChild y
    Term.Lam body
      -> putWord8 15 *> putChild body
    Term.LetRec _ bs body
      -> putWord8 16 *> putFoldable putChild bs *> putChild body
    Term.Let _ b body
      -> putWord8 17 *> putChild b *> putChild body
    Term.Match s cases
      -> putWord8 18 *> putChild s *> putFoldable (putMatchCase putH putA putChild) cases
    Term.Char c
      -> putWord8 19 *> putChar c
    Term.TermLink (collapset -> r)
      -> putWord8 20 *> putReferentH putH r
    Term.TypeLink (collapse -> r)
      -> putWord8 21 *> putReferenceH putH r

  putMatchCase :: MonadPut m => (h -> m ()) -> (a -> m ()) -> (x -> m ()) -> Term.MatchCaseH (Maybe h) a x -> m ()
  putMatchCase putH putA putChild (Term.MatchCase pat guard body) =
    putPatternH putH putA pat *> putMaybe guard putChild *> putChild body

  putPatternH :: MonadPut m => (h -> m ()) -> (a -> m ()) -> PatternH (Maybe h) a -> m ()
  putPatternH putH putA p = case p of
    Pattern.UnboundP a   -> putWord8 0 *> putA a
    Pattern.VarP     a   -> putWord8 1 *> putA a
    Pattern.BooleanP a b -> putWord8 2 *> putA a *> putBoolean b
    Pattern.IntP     a n -> putWord8 3 *> putA a *> putInt n
    Pattern.NatP     a n -> putWord8 4 *> putA a *> putNat n
    Pattern.FloatP   a n -> putWord8 5 *> putA a *> putFloat n
    Pattern.ConstructorP a (collapse -> r) cid ps ->
      putWord8 6
        *> putA a
        *> putReferenceH putH r
        *> putLength cid
        *> putFoldable (putPatternH putH putA) ps
    Pattern.AsP         a p -> putWord8 7 *> putA a *> putPatternH putH putA p
    Pattern.EffectPureP a p -> putWord8 8 *> putA a *> putPatternH putH putA p
    Pattern.EffectBindP a (collapse -> r) cid args k ->
      putWord8 9
        *> putA a
        *> putReferenceH putH r
        *> putLength cid
        *> putFoldable (putPatternH putH putA) args
        *> putPatternH putH putA k
    Pattern.SequenceLiteralP a ps ->
      putWord8 10 *> putA a *> putFoldable (putPatternH putH putA) ps
    Pattern.SequenceOpP a l op r ->
      putWord8 11
        *> putA a
        *> putPatternH putH putA l
        *> putSeqOp op
        *> putPatternH putH putA r
    Pattern.TextP a t -> putWord8 12 *> putA a *> putText t
    Pattern.CharP a c -> putWord8 13 *> putA a *> putChar c


getTerm :: (MonadGet m, Ord v)
        => m h -> m v -> m a -> h -> m (TermH h v a)
getTerm getHash getVar getA selfHash = getABT getVar getA go where
  go getChild = getWord8 >>= \tag -> case tag of
    0 -> Term.Int <$> getInt
    1 -> Term.Nat <$> getNat
    2 -> Term.Float <$> getFloat
    3 -> Term.Boolean <$> getBoolean
    4 -> Term.Text <$> getText
    5 -> Term.Ref <$> getReferenceH getHash
    -- if we don't store the cycle length, what will break?
    22 -> Term.Ref <$> (Reference.Derived selfHash <$> getLength <*> pure undefined)
    6 -> Term.Constructor <$> getReferenceH getHash <*> getLength
    7 -> Term.Request <$> getReferenceH getHash <*> getLength
    8 -> Term.Handle <$> getChild <*> getChild
    9 -> Term.App <$> getChild <*> getChild
    10 -> Term.Ann <$> getChild <*> getType getHash getVar getA Nothing
    11 -> Term.Sequence . Sequence.fromList <$> getList getChild
    12 -> Term.If <$> getChild <*> getChild <*> getChild
    13 -> Term.And <$> getChild <*> getChild
    14 -> Term.Or <$> getChild <*> getChild
    15 -> Term.Lam <$> getChild
    16 -> Term.LetRec False <$> getList getChild <*> getChild
    17 -> Term.Let False <$> getChild <*> getChild
    18 -> Term.Match <$> getChild
                     <*> getList (Term.MatchCase <$> getPatternH getHash getA <*> getMaybe getChild <*> getChild)
    19 -> Term.Char <$> getChar
    20 -> Term.TermLink <$> getReferentH getHash
    21 -> Term.TypeLink <$> getReferenceH getHash
    _ -> unknownTag "getTerm" tag

putDecl :: (MonadPut m, Ord v, Eq h)
        => (h -> m ())
        -> (v -> m ())
        -> (a -> m ())
        -> DD.DeclH (Maybe h) v a
        -> m ()
putDecl putH putV putA = putEither (go . DD.toDataDecl) go
  where
  go decl = do
    putModifier $ DD.modifier decl
    putA $ DD.annotation decl
    putFoldable putV (DD.bound decl)
    putFoldable (putTuple3 putA putV (putType putH putV putA)) (DD.constructors' decl)
