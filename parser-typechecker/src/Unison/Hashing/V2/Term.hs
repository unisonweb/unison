{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Hashing.V2.Term
  ( Term,
    F (..),
    MatchCase (..),
    hashClosedTerm,
    hashComponents,
    hashComponentsWithoutTypes,
  )
where

import qualified Data.Sequence as Sequence
import qualified Data.Text as Text
import qualified Unison.ABT as ABT
import qualified Unison.Blank as B
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import qualified Unison.Hash as Hash
import Unison.Hashing.V2.Tokenizable (Hashable1, accumulateToken)
import qualified Unison.Hashing.V2.Tokenizable as Hashable
import qualified Unison.Hashing.V2.ABT as ABT
import Unison.Hashing.V2.Pattern (Pattern)
import Unison.Hashing.V2.Reference (Reference)
import qualified Unison.Hashing.V2.Reference as Reference
import qualified Unison.Hashing.V2.Reference.Util as ReferenceUtil
import Unison.Hashing.V2.Referent (Referent)
import Unison.Hashing.V2.Type (Type)
import Unison.Prelude
import Unison.Var (Var)
import Prelude hiding (and, or)
import qualified Data.Zip as Zip

data MatchCase loc a = MatchCase (Pattern loc) (Maybe a) a
  deriving (Show, Eq, Foldable, Functor, Generic, Generic1, Traversable)

-- | Base functor for terms in the Unison language
-- We need `typeVar` because the term and type variables may differ.
data F typeVar typeAnn patternAnn a
  = Int Int64
  | Nat Word64
  | Float Double
  | Boolean Bool
  | Text Text
  | Char Char
  | Blank (B.Blank typeAnn)
  | Ref Reference
  | -- First argument identifies the data type,
    -- second argument identifies the constructor
    Constructor Reference ConstructorId
  | Request Reference ConstructorId
  | Handle a a
  | App a a
  | Ann a (Type typeVar typeAnn)
  | List (Seq a)
  | If a a a
  | And a a
  | Or a a
  | Lam a
  | -- Note: let rec blocks have an outer ABT.Cycle which introduces as many
    -- variables as there are bindings
    LetRec IsTop [a] a
  | -- Note: first parameter is the binding, second is the expression which may refer
    -- to this let bound variable. Constructed as `Let b (abs v e)`
    Let IsTop a a
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
    Match a [MatchCase patternAnn a]
  | TermLink Referent
  | TypeLink Reference
  deriving (Foldable, Functor, Generic, Generic1, Traversable)

type IsTop = Bool

-- | Like `Term v`, but with an annotation of type `a` at every level in the tree
type Term v a = Term2 v a a v a

-- | Allow type variables, term variables, type annotations and term annotations
-- to all differ
type Term2 vt at ap v a = ABT.Term (F vt at ap) v a

-- some smart constructors
ref :: Ord v => a -> Reference -> Term2 vt at ap v a
ref a r = ABT.tm' a (Ref r)

refId :: Ord v => a -> Reference.Id -> Term2 vt at ap v a
refId a = ref a . Reference.DerivedId

hashComponents ::
  forall v a.
  Var v => Map v (Term v a, Type v a) -> Map v (Reference.Id, Term v a, Type v a)
hashComponents terms =
  Zip.zipWith keepType terms (ReferenceUtil.hashComponents (refId ()) terms')
   where
    terms' :: Map v (Term v a)
    terms' = uncurry incorporateType <$> terms

    keepType :: ((Term v a, Type v a) -> (Reference.Id, Term v a) -> (Reference.Id, Term v a, Type v a))
    keepType (_oldTrm, typ) (refId, trm) = (refId, trm, typ)

    incorporateType :: Term v a -> Type v a -> Term v a
    incorporateType a@(ABT.out -> ABT.Tm (Ann e _tp)) typ = ABT.tm' (ABT.annotation a) (Ann e typ)
    incorporateType e typ = ABT.tm' (ABT.annotation e) (Ann e typ)

    -- keep these until we decide if we want to add the appropriate smart constructors back into this module
    -- incorporateType (Term.Ann' e _) typ = Term.ann () e typ
    -- incorporateType e typ = Term.ann () e typ

      -- Need to insert an "Ann" node inside the 'Tm' ABT wrapper
      -- iff there isn't already a top-level annotation.
      -- What if there's a top-level Annotation but it doesn't match
      -- the type that was provided?

hashComponentsWithoutTypes :: Var v => Map v (Term v a) -> Map v (Reference.Id, Term v a)
hashComponentsWithoutTypes = ReferenceUtil.hashComponents $ refId ()

hashClosedTerm :: Var v => Term v a -> Reference.Id
hashClosedTerm tm = Reference.Id (ABT.hash tm) 0

instance Var v => Hashable1 (F v a p) where
  hash1 :: forall h x. (Ord h, Hashable.Accumulate h) => ([x] -> ([h], x -> h)) -> (x -> h) -> (F v a p) x -> h
  hash1 hashCycle hash e =
    let varint :: Integral i => i -> Hashable.Token h
        varint = Hashable.Nat . fromIntegral
        tag = Hashable.Tag
        hashed = Hashable.Hashed
     in case e of
          -- So long as `Reference.Derived` ctors are created using the same
          -- hashing function as is used here, this case ensures that references
          -- are 'transparent' wrt hash and hashing is unaffected by whether
          -- expressions are linked. So for example `x = 1 + 1` and `y = x` hash
          -- the same.
          Ref (Reference.Derived h 0) -> Hashable.fromBytes (Hash.toByteString h)
          Ref (Reference.Derived h i) ->
            Hashable.accumulate
              [ tag 1,
                hashed $ Hashable.fromBytes (Hash.toByteString h),
                Hashable.Nat i
              ]
          -- Note: start each layer with leading `1` byte, to avoid collisions
          -- with types, which start each layer with leading `0`.
          -- See `Hashable1 Type.F`
          _ ->
            Hashable.accumulate $
              tag 1 :
              case e of
                Nat i -> [tag 64, accumulateToken i]
                Int i -> [tag 65, accumulateToken i]
                Float n -> [tag 66, Hashable.Double n]
                Boolean b -> [tag 67, accumulateToken b]
                Text t -> [tag 68, accumulateToken t]
                Char c -> [tag 69, accumulateToken c]
                Blank b ->
                  tag 1 : case b of
                    B.Blank -> [tag 0]
                    B.Recorded (B.Placeholder _ s) ->
                      [tag 1, Hashable.Text (Text.pack s)]
                    B.Recorded (B.Resolve _ s) ->
                      [tag 2, Hashable.Text (Text.pack s)]
                Ref (Reference.Builtin name) -> [tag 2, accumulateToken name]
                Ref Reference.Derived {} ->
                  error "handled above, but GHC can't figure this out"
                App a a2 -> [tag 3, hashed (hash a), hashed (hash a2)]
                Ann a t -> [tag 4, hashed (hash a), hashed (ABT.hash t)]
                List as ->
                  tag 5 :
                  varint (Sequence.length as) :
                  map
                    (hashed . hash)
                    (toList as)
                Lam a -> [tag 6, hashed (hash a)]
                -- note: we use `hashCycle` to ensure result is independent of
                -- let binding order
                LetRec _ as a -> case hashCycle as of
                  (hs, hash) -> tag 7 : hashed (hash a) : map hashed hs
                -- here, order is significant, so don't use hashCycle
                Let _ b a -> [tag 8, hashed $ hash b, hashed $ hash a]
                If b t f ->
                  [tag 9, hashed $ hash b, hashed $ hash t, hashed $ hash f]
                Request r n -> [tag 10, accumulateToken r, varint n]
                Constructor r n -> [tag 12, accumulateToken r, varint n]
                Match e branches ->
                  tag 13 : hashed (hash e) : concatMap h branches
                  where
                    h (MatchCase pat guard branch) =
                      concat
                        [ [accumulateToken pat],
                          toList (hashed . hash <$> guard),
                          [hashed (hash branch)]
                        ]
                Handle h b -> [tag 15, hashed $ hash h, hashed $ hash b]
                And x y -> [tag 16, hashed $ hash x, hashed $ hash y]
                Or x y -> [tag 17, hashed $ hash x, hashed $ hash y]
                TermLink r -> [tag 18, accumulateToken r]
                TypeLink r -> [tag 19, accumulateToken r]
