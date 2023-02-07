module Unison.Hashing.V2.Term
  ( Term,
    TermF (..),
    MatchCase (..),
    hashClosedTerm,
    hashTermComponents,
    hashTermComponentsWithoutTypes,
  )
where

import qualified Data.Sequence as Sequence
import qualified Data.Text as Text
import qualified Data.Zip as Zip
import qualified Unison.ABT as ABT
import qualified Unison.Blank as B
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.Hash (Hash)
import qualified Unison.Hash as Hash
import qualified Unison.Hashing.V2.ABT as ABT
import Unison.Hashing.V2.Pattern (Pattern)
import Unison.Hashing.V2.Reference (Reference (..), ReferenceId (..), pattern ReferenceDerived)
import qualified Unison.Hashing.V2.Reference.Util as ReferenceUtil
import Unison.Hashing.V2.Referent (Referent)
import Unison.Hashing.V2.Tokenizable (Hashable1, accumulateToken)
import qualified Unison.Hashing.V2.Tokenizable as Hashable
import Unison.Hashing.V2.Type (Type)
import Unison.Prelude
import Unison.Var (Var)
import Prelude hiding (and, or)

data MatchCase loc a = MatchCase (Pattern loc) (Maybe a) a
  deriving stock (Show, Eq, Foldable, Functor, Generic, Generic1, Traversable)

-- | Base functor for terms in the Unison language
-- We need `typeVar` because the term and type variables may differ.
data TermF typeVar typeAnn patternAnn a
  = TermInt Int64
  | TermNat Word64
  | TermFloat Double
  | TermBoolean Bool
  | TermText Text
  | TermChar Char
  | TermBlank (B.Blank typeAnn)
  | TermRef Reference
  | -- First argument identifies the data type,
    -- second argument identifies the constructor
    TermConstructor Reference ConstructorId
  | TermRequest Reference ConstructorId
  | TermHandle a a
  | TermApp a a
  | TermAnn a (Type typeVar typeAnn)
  | TermList (Seq a)
  | TermIf a a a
  | TermAnd a a
  | TermOr a a
  | TermLam a
  | -- Note: let rec blocks have an outer ABT.Cycle which introduces as many
    -- variables as there are bindings
    TermLetRec [a] a
  | -- Note: first parameter is the binding, second is the expression which may refer
    -- to this let bound variable. Constructed as `Let b (abs v e)`
    TermLet a a
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
    TermMatch a [MatchCase patternAnn a]
  | TermTermLink Referent
  | TermTypeLink Reference
  deriving (Foldable, Functor, Generic, Generic1, Traversable)

-- | Like `Term v`, but with an annotation of type `a` at every level in the tree
type Term v a = Term2 v a a v a

-- | Allow type variables, term variables, type annotations and term annotations
-- to all differ
type Term2 vt at ap v a = ABT.Term (TermF vt at ap) v a

-- some smart constructors
ref :: Ord v => a -> Reference -> Term2 vt at ap v a
ref a r = ABT.tm' a (TermRef r)

refId :: Ord v => a -> ReferenceId -> Term2 vt at ap v a
refId a = ref a . ReferenceDerivedId

hashTermComponents ::
  forall v a.
  Var v =>
  Map v (Term v a, Type v a) ->
  Map v (ReferenceId, Term v a, Type v a)
hashTermComponents terms =
  Zip.zipWith keepType terms (ReferenceUtil.hashComponents (refId ()) terms')
  where
    terms' :: Map v (Term v a)
    terms' = uncurry incorporateType <$> terms

    keepType :: ((Term v a, Type v a) -> (ReferenceId, Term v a) -> (ReferenceId, Term v a, Type v a))
    keepType (_oldTrm, typ) (refId, trm) = (refId, trm, typ)

    incorporateType :: Term v a -> Type v a -> Term v a
    incorporateType a@(ABT.out -> ABT.Tm (TermAnn e _tp)) typ = ABT.tm' (ABT.annotation a) (TermAnn e typ)
    incorporateType e typ = ABT.tm' (ABT.annotation e) (TermAnn e typ)

-- keep these until we decide if we want to add the appropriate smart constructors back into this module
-- incorporateType (Term.Ann' e _) typ = Term.ann () e typ
-- incorporateType e typ = Term.ann () e typ

-- Need to insert an "Ann" node inside the 'Tm' ABT wrapper
-- iff there isn't already a top-level annotation.
-- What if there's a top-level Annotation but it doesn't match
-- the type that was provided?

hashTermComponentsWithoutTypes :: Var v => Map v (Term v a) -> Map v (ReferenceId, Term v a)
hashTermComponentsWithoutTypes = ReferenceUtil.hashComponents $ refId ()

hashClosedTerm :: Var v => Term v a -> ReferenceId
hashClosedTerm tm = ReferenceId (ABT.hash tm) 0

instance Var v => Hashable1 (TermF v a p) where
  hash1 :: forall x. ([x] -> ([Hash], x -> Hash)) -> (x -> Hash) -> (TermF v a p) x -> Hash
  hash1 hashCycle hash e =
    let varint :: Integral i => i -> Hashable.Token
        varint = Hashable.Nat . fromIntegral
        tag = Hashable.Tag
        hashed = Hashable.Hashed
     in case e of
          -- So long as `Reference.Derived` ctors are created using the same
          -- hashing function as is used here, this case ensures that references
          -- are 'transparent' wrt hash and hashing is unaffected by whether
          -- expressions are linked. So for example `x = 1 + 1` and `y = x` hash
          -- the same.
          TermRef (ReferenceDerived h 0) -> Hash.fromByteString (Hash.toByteString h)
          TermRef (ReferenceDerived h i) ->
            Hashable.accumulate
              [ tag 1,
                hashed $ Hash.fromByteString (Hash.toByteString h),
                Hashable.Nat i
              ]
          -- Note: start each layer with leading `1` byte, to avoid collisions
          -- with types, which start each layer with leading `0`.
          -- See `Hashable1 Type.F`
          _ ->
            Hashable.accumulate $
              tag 1 :
              case e of
                TermNat i -> [tag 64, accumulateToken i]
                TermInt i -> [tag 65, accumulateToken i]
                TermFloat n -> [tag 66, Hashable.Double n]
                TermBoolean b -> [tag 67, accumulateToken b]
                TermText t -> [tag 68, accumulateToken t]
                TermChar c -> [tag 69, accumulateToken c]
                TermBlank b ->
                  tag 1 : case b of
                    B.Blank -> [tag 0]
                    B.Recorded (B.Placeholder _ s) ->
                      [tag 1, Hashable.Text (Text.pack s)]
                    B.Recorded (B.Resolve _ s) ->
                      [tag 2, Hashable.Text (Text.pack s)]
                TermRef (ReferenceBuiltin name) -> [tag 2, accumulateToken name]
                TermRef ReferenceDerived {} ->
                  error "handled above, but GHC can't figure this out"
                TermApp a a2 -> [tag 3, hashed (hash a), hashed (hash a2)]
                TermAnn a t -> [tag 4, hashed (hash a), hashed (ABT.hash t)]
                TermList as ->
                  tag 5 :
                  varint (Sequence.length as) :
                  map
                    (hashed . hash)
                    (toList as)
                TermLam a -> [tag 6, hashed (hash a)]
                -- note: we use `hashCycle` to ensure result is independent of
                -- let binding order
                TermLetRec as a -> case hashCycle as of
                  (hs, hash) -> tag 7 : hashed (hash a) : map hashed hs
                -- here, order is significant, so don't use hashCycle
                TermLet b a -> [tag 8, hashed $ hash b, hashed $ hash a]
                TermIf b t f ->
                  [tag 9, hashed $ hash b, hashed $ hash t, hashed $ hash f]
                TermRequest r n -> [tag 10, accumulateToken r, varint n]
                TermConstructor r n -> [tag 12, accumulateToken r, varint n]
                TermMatch e branches ->
                  tag 13 : hashed (hash e) : concatMap h branches
                  where
                    h (MatchCase pat guard branch) =
                      concat
                        [ [accumulateToken pat],
                          toList (hashed . hash <$> guard),
                          [hashed (hash branch)]
                        ]
                TermHandle h b -> [tag 15, hashed $ hash h, hashed $ hash b]
                TermAnd x y -> [tag 16, hashed $ hash x, hashed $ hash y]
                TermOr x y -> [tag 17, hashed $ hash x, hashed $ hash y]
                TermTermLink r -> [tag 18, accumulateToken r]
                TermTypeLink r -> [tag 19, accumulateToken r]
