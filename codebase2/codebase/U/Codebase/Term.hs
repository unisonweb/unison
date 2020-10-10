{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}


module U.Codebase.Term where

import Data.Int (Int64)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic, Generic1)
import U.Codebase.Reference (Reference, Reference'(ReferenceBuiltin, ReferenceDerived))
import qualified U.Codebase.Reference as Reference
import U.Codebase.Referent (Referent')
import U.Codebase.Type (TypeR)
import U.Util.Hash (Hash)
import qualified U.Core.ABT as ABT
import qualified U.Util.Hashable as H
import qualified U.Codebase.Type as Type
import qualified U.Util.Hash as Hash
import qualified Data.Foldable as Foldable

type ConstructorId = Word64

type Term v = ABT.Term (F v) v ()

-- | Base functor for terms in the Unison codebase
type F vt =
  F'
    Text -- text
    (Reference' Text (Maybe Hash)) -- termRef
    Reference -- typeRef
    (Referent' (Reference' Text (Maybe Hash)) (Reference' Text Hash)) -- termLink
    Reference -- typeLink
    vt

-- | Generalized version.  We could generalize further to allow sharing within
--  terms.
data F' text termRef typeRef termLink typeLink vt a
  = Int Int64
  | Nat Word64
  | Float Double
  | Boolean Bool
  | Text text
  | Char Char
  | Ref termRef
  | -- First argument identifies the data type,
    -- second argument identifies the constructor
    Constructor typeRef ConstructorId
  | Request typeRef ConstructorId
  | Handle a a
  | App a a
  | Ann a (TypeR typeRef vt)
  | Sequence (Seq a)
  | If a a a
  | And a a
  | Or a a
  | Lam a
  | -- Note: let rec blocks have an outer ABT.Cycle which introduces as many
    -- variables as there are bindings
    LetRec [a] a
  | -- Note: first parameter is the binding, second is the expression which may refer
    -- to this let bound variable. Constructed as `Let b (abs v e)`
    Let a a
  | -- Pattern matching / eliminating data types, example:
    --  case x of
    --    Just n -> rhs1
    --    Nothing -> rhs2
    --
    -- translates to
    --
    --   Match x
    --     [ (Constructor 0 [Var], ABT.abs n rhs1)
    --     , (Constructor 1 [], rhs2) ]
    Match a [MatchCase text typeRef a]
  | TermLink termLink
  | TypeLink typeLink
  deriving (Foldable, Functor, Traversable)

data MatchCase t r a = MatchCase (Pattern t r) (Maybe a) a
  deriving (Foldable, Functor, Generic, Generic1, Traversable)

data Pattern t r
  = PUnbound
  | PVar
  | PBoolean !Bool
  | PInt !Int64
  | PNat !Word64
  | PFloat !Double
  | PText !t
  | PChar !Char
  | PConstructor !r !Int [Pattern t r]
  | PAs (Pattern t r)
  | PEffectPure (Pattern t r)
  | PEffectBind !r !Int [Pattern t r] (Pattern t r)
  | PSequenceLiteral [Pattern t r]
  | PSequenceOp (Pattern t r) !SeqOp (Pattern t r)
  deriving (Generic, Functor, Foldable, Traversable)

data SeqOp
  = PCons
  | PSnoc
  | PConcat
  deriving (Eq, Show)

-- getHashesAndTextF ::

-- rmap ::
--   (termRef -> termRef') ->
--   (typeRef -> typeRef') ->
--   (termLink -> termLink') ->
--   TermR termRef typeRef termLink typeRef (TypeR typeRef vt at) blankRepr ap v a ->
--   TermR termRef' typeRef' termLink' typeRef' (TypeR typeRef' vt at) blankRepr ap v a
-- rmap fTermRef fTypeRef fTermLink t =
--   extraMap fTermRef fTypeRef fTermLink fTypeRef (Type.rmap fTypeRef) undefined id t

extraMap :: forall text termRef typeRef termLink typeLink vt
                   text' termRef' typeRef' termLink' typeLink' vt' v a
                   . (Ord v, Ord vt')
         => (text -> text') -> (termRef -> termRef') -> (typeRef -> typeRef')
         -> (termLink -> termLink') -> (typeLink -> typeLink') -> (vt -> vt')
         -> ABT.Term (F' text termRef typeRef termLink typeLink vt) v a
         -> ABT.Term (F' text' termRef' typeRef' termLink' typeLink' vt') v a
extraMap ftext ftermRef ftypeRef ftermLink ftypeLink fvt = go' where
 go' = ABT.transform go
 go :: forall x. F' text termRef typeRef termLink typeLink vt x -> F' text' termRef' typeRef' termLink' typeLink' vt' x
 go = \case
  Int i -> Int i
  Nat n -> Nat n
  Float d -> Float d
  Boolean b -> Boolean b
  Text t -> Text (ftext t)
  Char c -> Char c
  Ref r -> Ref (ftermRef r)
  Constructor r cid -> Constructor (ftypeRef r) cid
  Request r cid -> Request (ftypeRef r) cid
  Handle e h -> Handle e h
  App f a -> App f a
  Ann a typ -> Ann a (Type.rmap ftypeRef $ ABT.vmap fvt typ)
  Sequence s -> Sequence s
  If c t f -> If c t f
  And p q -> And p q
  Or p q -> Or p q
  Lam b -> Lam b
  LetRec bs b -> LetRec bs b
  Let a b -> Let a b
  Match s cs -> Match s (goCase <$> cs)
  TermLink r -> TermLink (ftermLink r)
  TypeLink r -> TypeLink (ftypeLink r)
 goCase :: MatchCase text typeRef x -> MatchCase text' typeRef' x
 goCase (MatchCase p g b) = MatchCase (goPat p) g b
 goPat = rmapPattern ftext ftypeRef

rmapPattern :: (t -> t') -> (r -> r') -> Pattern t r -> Pattern t' r'
rmapPattern ft fr = go where
 go = \case
  PUnbound -> PUnbound
  PVar -> PVar
  PBoolean b -> PBoolean b
  PInt i -> PInt i
  PNat n -> PNat n
  PFloat d -> PFloat d
  PText t -> PText (ft t)
  PChar c -> PChar c
  PConstructor r i ps -> PConstructor (fr r) i (go <$> ps)
  PAs p -> PAs (go p)
  PEffectPure p -> PEffectPure (go p)
  PEffectBind r i ps p -> PEffectBind (fr r) i (go <$> ps) (go p)
  PSequenceLiteral ps -> PSequenceLiteral (go <$> ps)
  PSequenceOp p1 op p2 -> PSequenceOp (go p1) op (go p2)

-- * Instances

instance H.Hashable SeqOp where
  tokens PCons = [H.Tag 0]
  tokens PSnoc = [H.Tag 1]
  tokens PConcat = [H.Tag 2]

instance H.Hashable (Pattern Text Reference) where
  tokens (PUnbound) = [H.Tag 0]
  tokens (PVar) = [H.Tag 1]
  tokens (PBoolean b) = H.Tag 2 : [H.Tag $ if b then 1 else 0]
  tokens (PInt n) = H.Tag 3 : [H.Int n]
  tokens (PNat n) = H.Tag 4 : [H.Nat n]
  tokens (PFloat f) = H.Tag 5 : H.tokens f
  tokens (PConstructor r n args) =
    [H.Tag 6, H.accumulateToken r, H.Nat $ fromIntegral n, H.accumulateToken args]
  tokens (PEffectPure p) = H.Tag 7 : H.tokens p
  tokens (PEffectBind r n args k) =
    [H.Tag 8, H.accumulateToken r, H.Nat $ fromIntegral n, H.accumulateToken args, H.accumulateToken k]
  tokens (PAs p) = H.Tag 9 : H.tokens p
  tokens (PText t) = H.Tag 10 : H.tokens t
  tokens (PSequenceLiteral ps) = H.Tag 11 : concatMap H.tokens ps
  tokens (PSequenceOp l op r) = H.Tag 12 : H.tokens op ++ H.tokens l ++ H.tokens r
  tokens (PChar c) = H.Tag 13 : H.tokens c

instance (Eq v, Show v) => H.Hashable1 (F v) where
  hash1 hashCycle hash e
    = let (tag, hashed, varint) =
            (H.Tag, H.Hashed, H.Nat . fromIntegral)
      in
        case e of
        -- So long as `Reference.Derived` ctors are created using the same
        -- hashing function as is used here, this case ensures that references
        -- are 'transparent' wrt hash and hashing is unaffected by whether
        -- expressions are linked. So for example `x = 1 + 1` and `y = x` hash
        -- the same.
          Ref (Reference.Derived (Just h) 0) -> H.fromBytes (Hash.toBytes h)
          Ref (Reference.Derived h i) -> H.accumulate
            [ tag 1 -- it's a term
            , tag 1 -- it's a derived reference
            , H.accumulateToken (Hash.toBytes <$> h)
            , H.Nat i
            ]
          -- Note: start each layer with leading `1` byte, to avoid collisions
          -- with types, which start each layer with leading `0`.
          -- See `Hashable1 Type.F`
          _ ->
            H.accumulate
              $ tag 1 -- it's a term
              : case e of
                  Nat     n -> tag 64 : H.tokens n
                  Int     i -> tag 65 : H.tokens i
                  Float   d -> tag 66 : H.tokens d
                  Boolean b -> tag 67 : H.tokens b
                  Text    t -> tag 68 : H.tokens t
                  Char    c -> tag 69 : H.tokens c
                  Ref (ReferenceBuiltin name) -> [tag 2, H.accumulateToken name]
                  Ref ReferenceDerived {} ->
                    error "handled above, but GHC can't figure this out"
                  App a a2  -> [tag 3, hashed (hash a), hashed (hash a2)]
                  Ann a t   -> [tag 4, hashed (hash a), hashed (ABT.hash t)]
                  Sequence as -> tag 5 : varint (fromIntegral (length as)) : map
                    (hashed . hash)
                    (Foldable.toList as)
                  Lam a         -> [tag 6, hashed (hash a)]
                  -- note: we use `hashCycle` to ensure result is independent of
                  -- let binding order
                  LetRec as a -> case hashCycle as of
                    (hs, hash) -> tag 7 : hashed (hash a) : map hashed hs
                  -- here, order is significant, so don't use hashCycle
                  Let b a -> [tag 8, hashed $ hash b, hashed $ hash a]
                  If b t f ->
                    [tag 9, hashed $ hash b, hashed $ hash t, hashed $ hash f]
                  Request     r n -> [tag 10, H.accumulateToken r, varint n]
                  Constructor r n -> [tag 12, H.accumulateToken r, varint n]
                  Match e branches ->
                    tag 13 : hashed (hash e) : concatMap h branches
                   where
                    h (MatchCase pat guard branch) = concat
                      [ [H.accumulateToken pat]
                      , Foldable.toList @Maybe (hashed . hash <$> guard)
                      , [hashed (hash branch)]
                      ]
                  Handle h b -> [tag 15, hashed $ hash h, hashed $ hash b]
                  And    x y -> [tag 16, hashed $ hash x, hashed $ hash y]
                  Or     x y -> [tag 17, hashed $ hash x, hashed $ hash y]
                  TermLink r -> [tag 18, H.accumulateToken r]
                  TypeLink r -> [tag 19, H.accumulateToken r]
