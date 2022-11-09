{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Term where

import Control.Lens (Lens', Prism', lens)
import Control.Monad.State (evalState)
import qualified Control.Monad.Writer.Strict as Writer
import Data.Bifunctor (second)
import Data.Generics.Sum (_Ctor)
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NES
import qualified Data.Text as Text
import Prelude.Extras (Eq1 (..), Show1 (..))
import Text.Show
import qualified Unison.ABT as ABT
import qualified Unison.Blank as B
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import qualified Unison.ConstructorType as CT
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.LabeledDependency (LabeledDependency)
import qualified Unison.LabeledDependency as LD
import qualified Unison.Name as Name
import Unison.Names (Names)
import qualified Unison.Names as Names
import qualified Unison.Names.ResolutionResult as Names
import qualified Unison.NamesWithHistory as Names
import Unison.Pattern (Pattern)
import qualified Unison.Pattern as Pattern
import Unison.Prelude
import Unison.Reference (Reference, TermReference, pattern Builtin)
import qualified Unison.Reference as Reference
import Unison.Referent (Referent)
import qualified Unison.Referent as Referent
import Unison.Type (Type)
import qualified Unison.Type as Type
import Unison.Util.List (multimap, validate)
import Unison.Var (Var)
import qualified Unison.Var as Var
import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (and, or)

data MatchCase loc a = MatchCase
  { matchPattern :: Pattern loc,
    matchGuard :: Maybe a,
    matchBody :: a
  }
  deriving (Show, Eq, Foldable, Functor, Generic, Generic1, Traversable)

matchPattern_ :: Lens' (MatchCase loc a) (Pattern loc)
matchPattern_ = lens matchPattern setter
  where
    setter m p = m {matchPattern = p}

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
  | Constructor ConstructorReference
  | Request ConstructorReference
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

_Ref :: Prism' (F tv ta pa a) Reference
_Ref = _Ctor @"Ref"

_Match :: Prism' (F tv ta pa a) (a, [MatchCase pa a])
_Match = _Ctor @"Match"

_Constructor :: Prism' (F tv ta pa a) ConstructorReference
_Constructor = _Ctor @"Constructor"

_Request :: Prism' (F tv ta pa a) ConstructorReference
_Request = _Ctor @"Request"

_Ann :: Prism' (F tv ta pa a) (a, ABT.Term Type.F tv ta)
_Ann = _Ctor @"Ann"

_TermLink :: Prism' (F tv ta pa a) Referent
_TermLink = _Ctor @"TermLink"

_TypeLink :: Prism' (F tv ta pa a) Reference
_TypeLink = _Ctor @"TypeLink"

type IsTop = Bool

-- | Like `Term v`, but with an annotation of type `a` at every level in the tree
type Term v a = Term2 v a a v a

-- | Allow type variables and term variables to differ
type Term' vt v a = Term2 vt a a v a

-- | Allow type variables, term variables, type annotations and term annotations
-- to all differ
type Term2 vt at ap v a = ABT.Term (F vt at ap) v a

-- | Like `Term v a`, but with only () for type and pattern annotations.
type Term3 v a = Term2 v () () v a

-- | Terms are represented as ABTs over the base functor F, with variables in `v`
type Term0 v = Term v ()

-- | Terms with type variables in `vt`, and term variables in `v`
type Term0' vt v = Term' vt v ()

bindNames ::
  forall v a.
  Var v =>
  Set v ->
  Names ->
  Term v a ->
  Names.ResolutionResult v a (Term v a)
bindNames keepFreeTerms ns0 e = do
  let freeTmVars = [(v, a) | (v, a) <- ABT.freeVarOccurrences keepFreeTerms e]
      -- !_ = trace "bindNames.free term vars: " ()
      -- !_ = traceShow $ fst <$> freeTmVars
      freeTyVars =
        [ (v, a) | (v, as) <- Map.toList (freeTypeVarAnnotations e), a <- as
        ]
      ns = Names.NamesWithHistory ns0 mempty
      -- !_ = trace "bindNames.free type vars: " ()
      -- !_ = traceShow $ fst <$> freeTyVars
      okTm :: (v, a) -> Names.ResolutionResult v a (v, Term v a)
      okTm (v, a) = case Names.lookupHQTerm (Name.convert $ Name.unsafeFromVar v) ns of
        rs
          | Set.size rs == 1 ->
              pure (v, fromReferent a $ Set.findMin rs)
          | otherwise -> case NES.nonEmptySet rs of
              Nothing -> Left (pure (Names.TermResolutionFailure v a Names.NotFound))
              Just refs -> Left (pure (Names.TermResolutionFailure v a (Names.Ambiguous ns0 refs)))
      okTy (v, a) = case Names.lookupHQType (Name.convert $ Name.unsafeFromVar v) ns of
        rs
          | Set.size rs == 1 -> pure (v, Type.ref a $ Set.findMin rs)
          | otherwise -> case NES.nonEmptySet rs of
              Nothing -> Left (pure (Names.TypeResolutionFailure v a Names.NotFound))
              Just refs -> Left (pure (Names.TypeResolutionFailure v a (Names.Ambiguous ns0 refs)))
  termSubsts <- validate okTm freeTmVars
  typeSubsts <- validate okTy freeTyVars
  pure . substTypeVars typeSubsts . ABT.substsInheritAnnotation termSubsts $ e

-- This function replaces free term and type variables with
-- hashes found in the provided `Names`, using suffix-based
-- lookup. Any terms not found in the `Names` are kept free.
bindSomeNames ::
  forall v a.
  Var v =>
  Set v ->
  Names ->
  Term v a ->
  Names.ResolutionResult v a (Term v a)
-- bindSomeNames ns e | trace "Term.bindSome" False
--                   || trace "Names =" False
--                   || traceShow ns False
--                   || trace "Free type vars:" False
--                   || traceShow (freeTypeVars e) False
--                   || trace "Free term vars:" False
--                   || traceShow (freeVars e) False
--                   || traceShow e False
--                   = undefined
bindSomeNames avoid ns e = bindNames (avoid <> varsToTDNR) ns e
  where
    -- `Term.bindNames` takes a set of variables that are not substituted.
    -- These should be the variables that will be subject to TDNR, which
    -- we compute as the set of variables whose names cannot be found in `ns`.
    --
    -- This allows TDNR to disambiguate those names (if multiple definitions
    -- share the same suffix) or to report the type expected for that name
    -- (if a free variable is being used as a typed hole).
    varsToTDNR = Set.filter notFound (freeVars e)
    notFound var =
      Set.size (Name.searchByRankedSuffix (Name.unsafeFromVar var) (Names.terms ns)) /= 1

-- Prepare a term for type-directed name resolution by replacing
-- any remaining free variables with blanks to be resolved by TDNR
prepareTDNR :: Var v => ABT.Term (F vt b ap) v b -> ABT.Term (F vt b ap) v b
prepareTDNR t = fmap fst . ABT.visitPure f $ ABT.annotateBound t
  where
    f (ABT.Term _ (a, bound) (ABT.Var v))
      | Set.notMember v bound =
          Just $ resolve (a, bound) a (Text.unpack $ Var.name v)
    f _ = Nothing

amap :: Ord v => (a -> a2) -> Term v a -> Term v a2
amap f = fmap f . patternMap (fmap f) . typeMap (fmap f)

patternMap :: (Pattern ap -> Pattern ap2) -> Term2 vt at ap v a -> Term2 vt at ap2 v a
patternMap f = go
  where
    go (ABT.Term fvs a t) = ABT.Term fvs a $ case t of
      ABT.Abs v t -> ABT.Abs v (go t)
      ABT.Var v -> ABT.Var v
      ABT.Cycle t -> ABT.Cycle (go t)
      ABT.Tm (Match e cases) ->
        ABT.Tm
          ( Match
              (go e)
              [ MatchCase (f p) (go <$> g) (go a) | MatchCase p g a <- cases
              ]
          )
      -- Safe since `Match` is only ctor that has embedded `Pattern ap` arg
      ABT.Tm ts -> unsafeCoerce $ ABT.Tm (fmap go ts)

vmap :: Ord v2 => (v -> v2) -> Term v a -> Term v2 a
vmap f = ABT.vmap f . typeMap (ABT.vmap f)

vtmap :: Ord vt2 => (vt -> vt2) -> Term' vt v a -> Term' vt2 v a
vtmap f = typeMap (ABT.vmap f)

typeMap ::
  Ord vt2 =>
  (Type vt at -> Type vt2 at2) ->
  Term2 vt at ap v a ->
  Term2 vt2 at2 ap v a
typeMap f = go
  where
    go (ABT.Term fvs a t) = ABT.Term fvs a $ case t of
      ABT.Abs v t -> ABT.Abs v (go t)
      ABT.Var v -> ABT.Var v
      ABT.Cycle t -> ABT.Cycle (go t)
      ABT.Tm (Ann e t) -> ABT.Tm (Ann (go e) (f t))
      -- Safe since `Ann` is only ctor that has embedded `Type v` arg
      -- otherwise we'd have to manually match on every non-`Ann` ctor
      ABT.Tm ts -> unsafeCoerce $ ABT.Tm (fmap go ts)

extraMap' ::
  (Ord vt, Ord vt') =>
  (vt -> vt') ->
  (at -> at') ->
  (ap -> ap') ->
  Term2 vt at ap v a ->
  Term2 vt' at' ap' v a
extraMap' vtf atf apf = ABT.extraMap (extraMap vtf atf apf)

extraMap ::
  (Ord vt, Ord vt') =>
  (vt -> vt') ->
  (at -> at') ->
  (ap -> ap') ->
  F vt at ap a ->
  F vt' at' ap' a
extraMap vtf atf apf = \case
  Int x -> Int x
  Nat x -> Nat x
  Float x -> Float x
  Boolean x -> Boolean x
  Text x -> Text x
  Char x -> Char x
  Blank x -> Blank (fmap atf x)
  Ref x -> Ref x
  Constructor x -> Constructor x
  Request x -> Request x
  Handle x y -> Handle x y
  App x y -> App x y
  Ann tm x -> Ann tm (ABT.amap atf (ABT.vmap vtf x))
  List x -> List x
  If x y z -> If x y z
  And x y -> And x y
  Or x y -> Or x y
  Lam x -> Lam x
  LetRec x y z -> LetRec x y z
  Let x y z -> Let x y z
  Match tm l -> Match tm (map (matchCaseExtraMap apf) l)
  TermLink r -> TermLink r
  TypeLink r -> TypeLink r

matchCaseExtraMap :: (loc -> loc') -> MatchCase loc a -> MatchCase loc' a
matchCaseExtraMap f (MatchCase p x y) = MatchCase (fmap f p) x y

unannotate ::
  forall vt at ap v a. Ord v => Term2 vt at ap v a -> Term0' vt v
unannotate = go
  where
    go :: Term2 vt at ap v a -> Term0' vt v
    go (ABT.out -> ABT.Abs v body) = ABT.abs v (go body)
    go (ABT.out -> ABT.Cycle body) = ABT.cycle (go body)
    go (ABT.Var' v) = ABT.var v
    go (ABT.Tm' f) = case go <$> f of
      Ann e t -> ABT.tm (Ann e (void t))
      Match scrutinee branches ->
        let unann (MatchCase pat guard body) = MatchCase (void pat) guard body
         in ABT.tm (Match scrutinee (unann <$> branches))
      f' -> ABT.tm (unsafeCoerce f')
    go _ = error "unpossible"

wrapV :: Ord v => Term v a -> Term (ABT.V v) a
wrapV = vmap ABT.Bound

-- | All variables mentioned in the given term.
-- Includes both term and type variables, both free and bound.
allVars :: Ord v => Term v a -> Set v
allVars tm =
  Set.fromList $
    ABT.allVars tm ++ [v | tp <- allTypes tm, v <- ABT.allVars tp]
  where
    allTypes tm = case tm of
      Ann' e tp -> tp : allTypes e
      _ -> foldMap allTypes $ ABT.out tm

freeVars :: Term' vt v a -> Set v
freeVars = ABT.freeVars

freeTypeVars :: Ord vt => Term' vt v a -> Set vt
freeTypeVars t = Map.keysSet $ freeTypeVarAnnotations t

freeTypeVarAnnotations :: Ord vt => Term' vt v a -> Map vt [a]
freeTypeVarAnnotations e = multimap $ go Set.empty e
  where
    go bound tm = case tm of
      Var' _ -> mempty
      Ann' e (Type.stripIntroOuters -> t1) ->
        let bound' = case t1 of
              Type.ForallsNamed' vs _ -> bound <> Set.fromList vs
              _ -> bound
         in go bound' e <> ABT.freeVarOccurrences bound t1
      ABT.Tm' f -> foldMap (go bound) f
      (ABT.out -> ABT.Abs _ body) -> go bound body
      (ABT.out -> ABT.Cycle body) -> go bound body
      _ -> error "unpossible"

substTypeVars ::
  (Ord v, Var vt) =>
  [(vt, Type vt b)] ->
  Term' vt v a ->
  Term' vt v a
substTypeVars subs e = foldl' go e subs
  where
    go e (vt, t) = substTypeVar vt t e

-- Capture-avoiding substitution of a type variable inside a term. This
-- will replace that type variable wherever it appears in type signatures of
-- the term, avoiding capture by renaming ∀-binders.
substTypeVar ::
  (Ord v, ABT.Var vt) =>
  vt ->
  Type vt b ->
  Term' vt v a ->
  Term' vt v a
substTypeVar vt ty = go Set.empty
  where
    go bound tm | Set.member vt bound = tm
    go bound tm =
      let loc = ABT.annotation tm
       in case tm of
            Var' _ -> tm
            Ann' e t -> uncapture [] e (Type.stripIntroOuters t)
              where
                fvs = ABT.freeVars ty
                -- if the ∀ introduces a variable, v, which is free in `ty`, we pick a new
                -- variable name for v which is unique, v', and rename v to v' in e.
                uncapture vs e t@(Type.Forall' body)
                  | Set.member (ABT.variable body) fvs =
                      let v = ABT.variable body
                          v2 = Var.freshIn (ABT.freeVars t) . Var.freshIn (Set.insert vt fvs) $ v
                          t2 = ABT.bindInheritAnnotation body (Type.var () v2)
                       in uncapture ((ABT.annotation t, v2) : vs) (renameTypeVar v v2 e) t2
                uncapture vs e t0 =
                  let t = foldl (\body (loc, v) -> Type.forall loc v body) t0 vs
                      bound' = case Type.unForalls (Type.stripIntroOuters t) of
                        Nothing -> bound
                        Just (vs, _) -> bound <> Set.fromList vs
                      t' = ABT.substInheritAnnotation vt ty (Type.stripIntroOuters t)
                   in ann loc (go bound' e) (Type.freeVarsToOuters bound t')
            ABT.Tm' f -> ABT.tm' loc (go bound <$> f)
            (ABT.out -> ABT.Abs v body) -> ABT.abs' loc v (go bound body)
            (ABT.out -> ABT.Cycle body) -> ABT.cycle' loc (go bound body)
            _ -> error "unpossible"

renameTypeVar :: (Ord v, ABT.Var vt) => vt -> vt -> Term' vt v a -> Term' vt v a
renameTypeVar old new = go Set.empty
  where
    go bound tm | Set.member old bound = tm
    go bound tm =
      let loc = ABT.annotation tm
       in case tm of
            Var' _ -> tm
            Ann' e t ->
              let bound' = case Type.unForalls (Type.stripIntroOuters t) of
                    Nothing -> bound
                    Just (vs, _) -> bound <> Set.fromList vs
                  t' = ABT.rename old new (Type.stripIntroOuters t)
               in ann loc (go bound' e) (Type.freeVarsToOuters bound t')
            ABT.Tm' f -> ABT.tm' loc (go bound <$> f)
            (ABT.out -> ABT.Abs v body) -> ABT.abs' loc v (go bound body)
            (ABT.out -> ABT.Cycle body) -> ABT.cycle' loc (go bound body)
            _ -> error "unpossible"

-- Converts free variables to bound variables using forall or introOuter. Example:
--
-- foo : x -> x
-- foo a =
--   r : x
--   r = a
--   r
--
-- This becomes:
--
-- foo : ∀ x . x -> x
-- foo a =
--   r : outer x . x -- FYI, not valid syntax
--   r = a
--   r
--
-- More specifically: in the expression `e : t`, unbound lowercase variables in `t`
-- are bound with foralls, and any ∀-quantified type variables are made bound in
-- `e` and its subexpressions. The result is a term with no lowercase free
-- variables in any of its type signatures, with outer references represented
-- with explicit `introOuter` binders. The resulting term may have uppercase
-- free variables that are still unbound.
generalizeTypeSignatures :: (Var vt, Var v) => Term' vt v a -> Term' vt v a
generalizeTypeSignatures = go Set.empty
  where
    go bound tm =
      let loc = ABT.annotation tm
       in case tm of
            Var' _ -> tm
            Ann' e (Type.generalizeLowercase bound -> t) ->
              let bound' = case Type.unForalls t of
                    Nothing -> bound
                    Just (vs, _) -> bound <> Set.fromList vs
               in ann loc (go bound' e) (Type.freeVarsToOuters bound t)
            ABT.Tm' f -> ABT.tm' loc (go bound <$> f)
            (ABT.out -> ABT.Abs v body) -> ABT.abs' loc v (go bound body)
            (ABT.out -> ABT.Cycle body) -> ABT.cycle' loc (go bound body)
            _ -> error "unpossible"

-- nicer pattern syntax

pattern Var' :: v -> ABT.Term f v a
pattern Var' v <- ABT.Var' v

pattern Cycle' :: [v] -> f (ABT.Term f v a) -> ABT.Term f v a
pattern Cycle' xs t <- ABT.Cycle' xs t

pattern Abs' ::
  (Foldable f, Functor f, ABT.Var v) =>
  ABT.Subst f v a ->
  ABT.Term f v a
pattern Abs' subst <- ABT.Abs' subst

pattern Int' :: Int64 -> ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Int' n <- (ABT.out -> ABT.Tm (Int n))

pattern Nat' :: Word64 -> ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Nat' n <- (ABT.out -> ABT.Tm (Nat n))

pattern Float' :: Double -> ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Float' n <- (ABT.out -> ABT.Tm (Float n))

pattern Boolean' :: Bool -> ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Boolean' b <- (ABT.out -> ABT.Tm (Boolean b))

pattern Text' :: Text -> ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Text' s <- (ABT.out -> ABT.Tm (Text s))

pattern Char' :: Char -> ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Char' c <- (ABT.out -> ABT.Tm (Char c))

pattern Blank' :: B.Blank typeAnn -> ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Blank' b <- (ABT.out -> ABT.Tm (Blank b))

pattern Ref' :: Reference -> ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Ref' r <- (ABT.out -> ABT.Tm (Ref r))

pattern TermLink' :: Referent -> ABT.Term (F typeVar typeAnn patternAnn) v a
pattern TermLink' r <- (ABT.out -> ABT.Tm (TermLink r))

pattern TypeLink' :: Reference -> ABT.Term (F typeVar typeAnn patternAnn) v a
pattern TypeLink' r <- (ABT.out -> ABT.Tm (TypeLink r))

pattern Builtin' :: Text -> ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Builtin' r <- (ABT.out -> ABT.Tm (Ref (Builtin r)))

pattern App' ::
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  ABT.Term (F typeVar typeAnn patternAnn) v a
pattern App' f x <- (ABT.out -> ABT.Tm (App f x))

pattern Match' ::
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  [ MatchCase
      patternAnn
      (ABT.Term (F typeVar typeAnn patternAnn) v a)
  ] ->
  ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Match' scrutinee branches <- (ABT.out -> ABT.Tm (Match scrutinee branches))

pattern Constructor' :: ConstructorReference -> ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Constructor' ref <- (ABT.out -> ABT.Tm (Constructor ref))

pattern Request' :: ConstructorReference -> ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Request' ref <- (ABT.out -> ABT.Tm (Request ref))

pattern RequestOrCtor' :: ConstructorReference -> Term2 vt at ap v a
pattern RequestOrCtor' ref <- (unReqOrCtor -> Just ref)

pattern If' ::
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  ABT.Term (F typeVar typeAnn patternAnn) v a
pattern If' cond t f <- (ABT.out -> ABT.Tm (If cond t f))

pattern And' ::
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  ABT.Term (F typeVar typeAnn patternAnn) v a
pattern And' x y <- (ABT.out -> ABT.Tm (And x y))

pattern Or' ::
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Or' x y <- (ABT.out -> ABT.Tm (Or x y))

pattern Handle' ::
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Handle' h body <- (ABT.out -> ABT.Tm (Handle h body))

pattern Apps' :: Term2 vt at ap v a -> [Term2 vt at ap v a] -> Term2 vt at ap v a
pattern Apps' f args <- (unApps -> Just (f, args))

-- begin pretty-printer helper patterns
pattern Ands' :: [Term2 vt at ap v a] -> Term2 vt at ap v a -> Term2 vt at ap v a
pattern Ands' ands lastArg <- (unAnds -> Just (ands, lastArg))

pattern Ors' :: [Term2 vt at ap v a] -> Term2 vt at ap v a -> Term2 vt at ap v a
pattern Ors' ors lastArg <- (unOrs -> Just (ors, lastArg))

pattern AppsPred' ::
  Term2 vt at ap v a ->
  [Term2 vt at ap v a] ->
  (Term2 vt at ap v a, Term2 vt at ap v a -> Bool)
pattern AppsPred' f args <- (unAppsPred -> Just (f, args))

pattern BinaryApp' ::
  Term2 vt at ap v a ->
  Term2 vt at ap v a ->
  Term2 vt at ap v a ->
  Term2 vt at ap v a

pattern BinaryApps' ::
  [(Term2 vt at ap v a, Term2 vt at ap v a)] ->
  Term2 vt at ap v a ->
  Term2 vt at ap v a

pattern BinaryApp' f arg1 arg2 <- (unBinaryApp -> Just (f, arg1, arg2))

pattern BinaryApps' apps lastArg <- (unBinaryApps -> Just (apps, lastArg))

pattern BinaryAppsPred' ::
  [(Term2 vt at ap v a, Term2 vt at ap v a)] ->
  Term2 vt at ap v a ->
  (Term2 vt at ap v a, Term2 vt at ap v a -> Bool)
pattern BinaryAppsPred' apps lastArg <- (unBinaryAppsPred -> Just (apps, lastArg))

pattern OverappliedBinaryAppPred' ::
  Term2 vt at ap v a ->
  Term2 vt at ap v a ->
  Term2 vt at ap v a ->
  [Term2 vt at ap v a] ->
  (Term2 vt at ap v a, Term2 vt at ap v a -> Bool)
pattern OverappliedBinaryAppPred' f arg1 arg2 rest <-
  (unOverappliedBinaryAppPred -> Just (f, arg1, arg2, rest))

-- end pretty-printer helper patterns
pattern Ann' ::
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  Type typeVar typeAnn ->
  ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Ann' x t <- (ABT.out -> ABT.Tm (Ann x t))

pattern List' ::
  Seq (ABT.Term (F typeVar typeAnn patternAnn) v a) ->
  ABT.Term (F typeVar typeAnn patternAnn) v a
pattern List' xs <- (ABT.out -> ABT.Tm (List xs))

pattern Lam' ::
  ABT.Var v =>
  ABT.Subst (F typeVar typeAnn patternAnn) v a ->
  ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Lam' subst <- ABT.Tm' (Lam (ABT.Abs' subst))

pattern Delay' :: Ord v => Term2 vt at ap v a -> Term2 vt at ap v a
pattern Delay' body <- (unDelay -> Just body)

unDelay :: Ord v => Term2 vt at ap v a -> Maybe (Term2 vt at ap v a)
unDelay tm = case ABT.out tm of
  ABT.Tm (Lam (ABT.Term _ _ (ABT.Abs v body)))
    | Set.notMember v (ABT.freeVars body) ->
        Just body
  _ -> Nothing

pattern LamNamed' ::
  v ->
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  ABT.Term (F typeVar typeAnn patternAnn) v a
pattern LamNamed' v body <- (ABT.out -> ABT.Tm (Lam (ABT.Term _ _ (ABT.Abs v body))))

pattern LamsNamed' :: [v] -> Term2 vt at ap v a -> Term2 vt at ap v a
pattern LamsNamed' vs body <- (unLams' -> Just (vs, body))

pattern LamsNamedOpt' :: [v] -> Term2 vt at ap v a -> Term2 vt at ap v a
pattern LamsNamedOpt' vs body <- (unLamsOpt' -> Just (vs, body))

pattern LamsNamedPred' :: [v] -> Term2 vt at ap v a -> (Term2 vt at ap v a, v -> Bool)
pattern LamsNamedPred' vs body <- (unLamsPred' -> Just (vs, body))

pattern LamsNamedOrDelay' ::
  Var v =>
  [v] ->
  Term2 vt at ap v a ->
  Term2 vt at ap v a
pattern LamsNamedOrDelay' vs body <- (unLamsUntilDelay' -> Just (vs, body))

pattern Let1' ::
  Var v =>
  Term' vt v a ->
  ABT.Subst (F vt a a) v a ->
  Term' vt v a
pattern Let1' b subst <- (unLet1 -> Just (_, b, subst))

pattern Let1Top' ::
  Var v =>
  IsTop ->
  Term' vt v a ->
  ABT.Subst (F vt a a) v a ->
  Term' vt v a
pattern Let1Top' top b subst <- (unLet1 -> Just (top, b, subst))

pattern Let1Named' ::
  v ->
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Let1Named' v b e <- (ABT.Tm' (Let _ b (ABT.out -> ABT.Abs v e)))

pattern Let1NamedTop' ::
  IsTop ->
  v ->
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  ABT.Term (F typeVar typeAnn patternAnn) v a ->
  ABT.Term (F typeVar typeAnn patternAnn) v a
pattern Let1NamedTop' top v b e <- (ABT.Tm' (Let top b (ABT.out -> ABT.Abs v e)))

pattern Lets' ::
  [(IsTop, v, Term2 vt at ap v a)] ->
  Term2 vt at ap v a ->
  Term2 vt at ap v a
pattern Lets' bs e <- (unLet -> Just (bs, e))

pattern LetRecNamed' ::
  [(v, Term2 vt at ap v a)] ->
  Term2 vt at ap v a ->
  Term2 vt at ap v a
pattern LetRecNamed' bs e <- (unLetRecNamed -> Just (_, bs, e))

pattern LetRecNamedTop' ::
  IsTop ->
  [(v, Term2 vt at ap v a)] ->
  Term2 vt at ap v a ->
  Term2 vt at ap v a
pattern LetRecNamedTop' top bs e <- (unLetRecNamed -> Just (top, bs, e))

pattern LetRec' ::
  (Monad m, Var v) =>
  ((v -> m v) -> m ([(v, Term2 vt at ap v a)], Term2 vt at ap v a)) ->
  Term2 vt at ap v a
pattern LetRec' subst <- (unLetRec -> Just (_, subst))

pattern LetRecTop' ::
  (Monad m, Var v) =>
  IsTop ->
  ( (v -> m v) ->
    m ([(v, Term2 vt at ap v a)], Term2 vt at ap v a)
  ) ->
  Term2 vt at ap v a
pattern LetRecTop' top subst <- (unLetRec -> Just (top, subst))

pattern LetRecNamedAnnotated' :: a -> [((a, v), Term' vt v a)] -> Term' vt v a -> Term' vt v a
pattern LetRecNamedAnnotated' ann bs e <- (unLetRecNamedAnnotated -> Just (_, ann, bs, e))

pattern LetRecNamedAnnotatedTop' ::
  IsTop ->
  a ->
  [((a, v), Term' vt v a)] ->
  Term' vt v a ->
  Term' vt v a
pattern LetRecNamedAnnotatedTop' top ann bs e <-
  (unLetRecNamedAnnotated -> Just (top, ann, bs, e))

fresh :: Var v => Term0 v -> v -> v
fresh = ABT.fresh

-- some smart constructors

var :: a -> v -> Term2 vt at ap v a
var = ABT.annotatedVar

var' :: Var v => Text -> Term0' vt v
var' = var () . Var.named

ref :: Ord v => a -> Reference -> Term2 vt at ap v a
ref a r = ABT.tm' a (Ref r)

pattern Referent' :: Referent -> Term2 vt at ap v a
pattern Referent' r <- (unReferent -> Just r)

unReferent :: Term2 vt at ap v a -> Maybe Referent
unReferent (Ref' r) = Just $ Referent.Ref r
unReferent (Constructor' r) = Just $ Referent.Con r CT.Data
unReferent (Request' r) = Just $ Referent.Con r CT.Effect
unReferent _ = Nothing

refId :: Ord v => a -> Reference.Id -> Term2 vt at ap v a
refId a = ref a . Reference.DerivedId

termLink :: Ord v => a -> Referent -> Term2 vt at ap v a
termLink a r = ABT.tm' a (TermLink r)

typeLink :: Ord v => a -> Reference -> Term2 vt at ap v a
typeLink a r = ABT.tm' a (TypeLink r)

builtin :: Ord v => a -> Text -> Term2 vt at ap v a
builtin a n = ref a (Reference.Builtin n)

float :: Ord v => a -> Double -> Term2 vt at ap v a
float a d = ABT.tm' a (Float d)

boolean :: Ord v => a -> Bool -> Term2 vt at ap v a
boolean a b = ABT.tm' a (Boolean b)

int :: Ord v => a -> Int64 -> Term2 vt at ap v a
int a d = ABT.tm' a (Int d)

nat :: Ord v => a -> Word64 -> Term2 vt at ap v a
nat a d = ABT.tm' a (Nat d)

text :: Ord v => a -> Text -> Term2 vt at ap v a
text a = ABT.tm' a . Text

char :: Ord v => a -> Char -> Term2 vt at ap v a
char a = ABT.tm' a . Char

watch :: (Var v, Semigroup a) => a -> String -> Term v a -> Term v a
watch a note e =
  apps' (builtin a "Debug.watch") [text a (Text.pack note), e]

watchMaybe :: (Var v, Semigroup a) => Maybe String -> Term v a -> Term v a
watchMaybe Nothing e = e
watchMaybe (Just note) e = watch (ABT.annotation e) note e

blank :: Ord v => a -> Term2 vt at ap v a
blank a = ABT.tm' a (Blank B.Blank)

placeholder :: Ord v => a -> String -> Term2 vt a ap v a
placeholder a s = ABT.tm' a . Blank $ B.Recorded (B.Placeholder a s)

resolve :: Ord v => at -> ab -> String -> Term2 vt ab ap v at
resolve at ab s = ABT.tm' at . Blank $ B.Recorded (B.Resolve ab s)

constructor :: Ord v => a -> ConstructorReference -> Term2 vt at ap v a
constructor a ref = ABT.tm' a (Constructor ref)

request :: Ord v => a -> ConstructorReference -> Term2 vt at ap v a
request a ref = ABT.tm' a (Request ref)

-- todo: delete and rename app' to app
app_ :: Ord v => Term0' vt v -> Term0' vt v -> Term0' vt v
app_ f arg = ABT.tm (App f arg)

app :: Ord v => a -> Term2 vt at ap v a -> Term2 vt at ap v a -> Term2 vt at ap v a
app a f arg = ABT.tm' a (App f arg)

match :: Ord v => a -> Term2 vt at a v a -> [MatchCase a (Term2 vt at a v a)] -> Term2 vt at a v a
match a scrutinee branches = ABT.tm' a (Match scrutinee branches)

handle :: Ord v => a -> Term2 vt at ap v a -> Term2 vt at ap v a -> Term2 vt at ap v a
handle a h block = ABT.tm' a (Handle h block)

and :: Ord v => a -> Term2 vt at ap v a -> Term2 vt at ap v a -> Term2 vt at ap v a
and a x y = ABT.tm' a (And x y)

or :: Ord v => a -> Term2 vt at ap v a -> Term2 vt at ap v a -> Term2 vt at ap v a
or a x y = ABT.tm' a (Or x y)

list :: Ord v => a -> [Term2 vt at ap v a] -> Term2 vt at ap v a
list a es = list' a (Sequence.fromList es)

list' :: Ord v => a -> Seq (Term2 vt at ap v a) -> Term2 vt at ap v a
list' a es = ABT.tm' a (List es)

apps ::
  Ord v =>
  Term2 vt at ap v a ->
  [(a, Term2 vt at ap v a)] ->
  Term2 vt at ap v a
apps = foldl' (\f (a, t) -> app a f t)

apps' ::
  (Ord v, Semigroup a) =>
  Term2 vt at ap v a ->
  [Term2 vt at ap v a] ->
  Term2 vt at ap v a
apps' = foldl' (\f t -> app (ABT.annotation f <> ABT.annotation t) f t)

iff :: Ord v => a -> Term2 vt at ap v a -> Term2 vt at ap v a -> Term2 vt at ap v a -> Term2 vt at ap v a
iff a cond t f = ABT.tm' a (If cond t f)

ann_ :: Ord v => Term0' vt v -> Type vt () -> Term0' vt v
ann_ e t = ABT.tm (Ann e t)

ann ::
  Ord v =>
  a ->
  Term2 vt at ap v a ->
  Type vt at ->
  Term2 vt at ap v a
ann a e t = ABT.tm' a (Ann e t)

-- arya: are we sure we want the two annotations to be the same?
lam :: Ord v => a -> v -> Term2 vt at ap v a -> Term2 vt at ap v a
lam a v body = ABT.tm' a (Lam (ABT.abs' a v body))

delay :: Var v => a -> Term2 vt at ap v a -> Term2 vt at ap v a
delay a body =
  ABT.tm' a (Lam (ABT.abs' a (ABT.freshIn (ABT.freeVars body) (Var.named "_")) body))

lam' :: Ord v => a -> [v] -> Term2 vt at ap v a -> Term2 vt at ap v a
lam' a vs body = foldr (lam a) body vs

lam'' :: Ord v => [(a, v)] -> Term2 vt at ap v a -> Term2 vt at ap v a
lam'' vs body = foldr (uncurry lam) body vs

isLam :: Term2 vt at ap v a -> Bool
isLam t = arity t > 0

arity :: Term2 vt at ap v a -> Int
arity (LamNamed' _ body) = 1 + arity body
arity (Ann' e _) = arity e
arity _ = 0

unLetRecNamedAnnotated ::
  Term' vt v a ->
  Maybe
    (IsTop, a, [((a, v), Term' vt v a)], Term' vt v a)
unLetRecNamedAnnotated (ABT.CycleA' ann avs (ABT.Tm' (LetRec isTop bs e))) =
  Just (isTop, ann, avs `zip` bs, e)
unLetRecNamedAnnotated _ = Nothing

letRec' ::
  (Ord v, Monoid a) =>
  Bool ->
  [(v, Term' vt v a)] ->
  Term' vt v a ->
  Term' vt v a
letRec' isTop bindings body =
  letRec
    isTop
    (foldMap (ABT.annotation . snd) bindings <> ABT.annotation body)
    [((ABT.annotation b, v), b) | (v, b) <- bindings]
    body

-- Prepend a binding to form a (bigger) let rec. Useful when
-- building up a block incrementally using a right fold.
--
-- For example:
--   consLetRec (x = 42) "hi"
--   =>
--   let rec x = 42 in "hi"
--
--   consLetRec (x = 42) (let rec y = "hi" in (x,y))
--   =>
--   let rec x = 42; y = "hi" in (x,y)
consLetRec ::
  Ord v =>
  Bool -> -- isTop parameter
  a -> -- annotation for overall let rec
  (a, v, Term' vt v a) -> -- the binding
  Term' vt v a -> -- the body
  Term' vt v a
consLetRec isTop a (ab, vb, b) body = case body of
  LetRecNamedAnnotated' _ bs body -> letRec isTop a (((ab, vb), b) : bs) body
  _ -> letRec isTop a [((ab, vb), b)] body

letRec ::
  Ord v =>
  Bool ->
  a ->
  [((a, v), Term' vt v a)] ->
  Term' vt v a ->
  Term' vt v a
letRec _ _ [] e = e
letRec isTop a bindings e =
  ABT.cycle'
    a
    (foldr (uncurry ABT.abs' . fst) z bindings)
  where
    z = ABT.tm' a (LetRec isTop (map snd bindings) e)

-- | Smart constructor for let rec blocks. Each binding in the block may
-- reference any other binding in the block in its body (including itself),
-- and the output expression may also reference any binding in the block.
letRec_ :: Ord v => IsTop -> [(v, Term0' vt v)] -> Term0' vt v -> Term0' vt v
letRec_ _ [] e = e
letRec_ isTop bindings e = ABT.cycle (foldr (ABT.abs . fst) z bindings)
  where
    z = ABT.tm (LetRec isTop (map snd bindings) e)

-- | Smart constructor for let blocks. Each binding in the block may
-- reference only previous bindings in the block, not including itself.
-- The output expression may reference any binding in the block.
-- todo: delete me
let1_ :: Ord v => IsTop -> [(v, Term0' vt v)] -> Term0' vt v -> Term0' vt v
let1_ isTop bindings e = foldr f e bindings
  where
    f (v, b) body = ABT.tm (Let isTop b (ABT.abs v body))

-- | annotations are applied to each nested Let expression
let1 ::
  Ord v =>
  IsTop ->
  [((a, v), Term2 vt at ap v a)] ->
  Term2 vt at ap v a ->
  Term2 vt at ap v a
let1 isTop bindings e = foldr f e bindings
  where
    f ((ann, v), b) body = ABT.tm' ann (Let isTop b (ABT.abs' ann v body))

let1' ::
  (Semigroup a, Ord v) =>
  IsTop ->
  [(v, Term2 vt at ap v a)] ->
  Term2 vt at ap v a ->
  Term2 vt at ap v a
let1' isTop bindings e = foldr f e bindings
  where
    ann = ABT.annotation
    f (v, b) body = ABT.tm' a (Let isTop b (ABT.abs' a v body))
      where
        a = ann b <> ann body

-- let1' :: Var v => [(Text, Term0 vt v)] -> Term0 vt v -> Term0 vt v
-- let1' bs e = let1 [(ABT.v' name, b) | (name,b) <- bs ] e

unLet1 ::
  Var v =>
  Term' vt v a ->
  Maybe (IsTop, Term' vt v a, ABT.Subst (F vt a a) v a)
unLet1 (ABT.Tm' (Let isTop b (ABT.Abs' subst))) = Just (isTop, b, subst)
unLet1 _ = Nothing

-- | Satisfies `unLet (let' bs e) == Just (bs, e)`
unLet ::
  Term2 vt at ap v a ->
  Maybe ([(IsTop, v, Term2 vt at ap v a)], Term2 vt at ap v a)
unLet t = fixup (go t)
  where
    go (ABT.Tm' (Let isTop b (ABT.out -> ABT.Abs v t))) = case go t of
      (env, t) -> ((isTop, v, b) : env, t)
    go t = ([], t)
    fixup ([], _) = Nothing
    fixup bst = Just bst

-- | Satisfies `unLetRec (letRec bs e) == Just (bs, e)`
unLetRecNamed ::
  Term2 vt at ap v a ->
  Maybe
    ( IsTop,
      [(v, Term2 vt at ap v a)],
      Term2 vt at ap v a
    )
unLetRecNamed (ABT.Cycle' vs (LetRec isTop bs e))
  | length vs == length bs = Just (isTop, zip vs bs, e)
unLetRecNamed _ = Nothing

unLetRec ::
  (Monad m, Var v) =>
  Term2 vt at ap v a ->
  Maybe
    ( IsTop,
      (v -> m v) ->
      m
        ( [(v, Term2 vt at ap v a)],
          Term2 vt at ap v a
        )
    )
unLetRec (unLetRecNamed -> Just (isTop, bs, e)) =
  Just
    ( isTop,
      \freshen -> do
        vs <- sequence [freshen v | (v, _) <- bs]
        let sub = ABT.substsInheritAnnotation (map fst bs `zip` map ABT.var vs)
        pure (vs `zip` [sub b | (_, b) <- bs], sub e)
    )
unLetRec _ = Nothing

unAnds ::
  Term2 vt at ap v a ->
  Maybe
    ( [Term2 vt at ap v a],
      Term2 vt at ap v a
    )
unAnds t = case t of
  And' i o -> case unAnds i of
    Just (as, xLast) -> Just (xLast : as, o)
    Nothing -> Just ([i], o)
  _ -> Nothing

unOrs ::
  Term2 vt at ap v a ->
  Maybe
    ( [Term2 vt at ap v a],
      Term2 vt at ap v a
    )
unOrs t = case t of
  Or' i o -> case unOrs i of
    Just (as, xLast) -> Just (xLast : as, o)
    Nothing -> Just ([i], o)
  _ -> Nothing

unApps ::
  Term2 vt at ap v a ->
  Maybe (Term2 vt at ap v a, [Term2 vt at ap v a])
unApps t = unAppsPred (t, const True)

-- Same as unApps but taking a predicate controlling whether we match on a given function argument.
unAppsPred ::
  (Term2 vt at ap v a, Term2 vt at ap v a -> Bool) ->
  Maybe (Term2 vt at ap v a, [Term2 vt at ap v a])
unAppsPred (t, pred) = case go t [] of [] -> Nothing; f : args -> Just (f, args)
  where
    go (App' i o) acc | pred o = go i (o : acc)
    go _ [] = []
    go fn args = fn : args

unBinaryApp ::
  Term2 vt at ap v a ->
  Maybe
    ( Term2 vt at ap v a,
      Term2 vt at ap v a,
      Term2 vt at ap v a
    )
unBinaryApp t = case unApps t of
  Just (f, [arg1, arg2]) -> Just (f, arg1, arg2)
  _ -> Nothing

-- Special case for overapplied binary operators
unOverappliedBinaryAppPred ::
  (Term2 vt at ap v a, Term2 vt at ap v a -> Bool) ->
  Maybe
    ( Term2 vt at ap v a,
      Term2 vt at ap v a,
      Term2 vt at ap v a,
      [Term2 vt at ap v a]
    )
unOverappliedBinaryAppPred (t, pred) = case unApps t of
  Just (f, arg1 : arg2 : rest) | pred f -> Just (f, arg1, arg2, rest)
  _ -> Nothing

-- "((a1 `f1` a2) `f2` a3)" becomes "Just ([(a2, f2), (a1, f1)], a3)"
unBinaryApps ::
  Term2 vt at ap v a ->
  Maybe
    ( [(Term2 vt at ap v a, Term2 vt at ap v a)],
      Term2 vt at ap v a
    )
unBinaryApps t = unBinaryAppsPred (t, const True)

-- Same as unBinaryApps but taking a predicate controlling whether we match on a given binary function.
unBinaryAppsPred ::
  ( Term2 vt at ap v a,
    Term2 vt at ap v a -> Bool
  ) ->
  Maybe
    ( [ ( Term2 vt at ap v a,
          Term2 vt at ap v a
        )
      ],
      Term2 vt at ap v a
    )
unBinaryAppsPred (t, pred) = case unBinaryApp t of
  Just (f, x, y) | pred f -> case unBinaryAppsPred (x, pred) of
    Just (as, xLast) -> Just ((xLast, f) : as, y)
    Nothing -> Just ([(x, f)], y)
  _ -> Nothing

unLams' ::
  Term2 vt at ap v a -> Maybe ([v], Term2 vt at ap v a)
unLams' t = unLamsPred' (t, const True)

-- Same as unLams', but always matches.  Returns an empty [v] if the term doesn't start with a
-- lambda extraction.
unLamsOpt' :: Term2 vt at ap v a -> Maybe ([v], Term2 vt at ap v a)
unLamsOpt' t = case unLams' t of
  r@(Just _) -> r
  Nothing -> Just ([], t)

-- Same as unLams', but stops at any variable named `()`, which indicates a
-- delay (`'`) annotation which we want to preserve.
unLamsUntilDelay' ::
  Var v =>
  Term2 vt at ap v a ->
  Maybe ([v], Term2 vt at ap v a)
unLamsUntilDelay' t = case unLamsPred' (t, (/=) $ Var.named "()") of
  r@(Just _) -> r
  Nothing -> Just ([], t)

-- Same as unLams' but taking a predicate controlling whether we match on a given binary function.
unLamsPred' ::
  (Term2 vt at ap v a, v -> Bool) ->
  Maybe ([v], Term2 vt at ap v a)
unLamsPred' (LamNamed' v body, pred) | pred v = case unLamsPred' (body, pred) of
  Nothing -> Just ([v], body)
  Just (vs, body) -> Just (v : vs, body)
unLamsPred' _ = Nothing

unReqOrCtor :: Term2 vt at ap v a -> Maybe ConstructorReference
unReqOrCtor (Constructor' r) = Just r
unReqOrCtor (Request' r) = Just r
unReqOrCtor _ = Nothing

-- Dependencies including referenced data and effect decls
dependencies :: (Ord v, Ord vt) => Term2 vt at ap v a -> Set Reference
dependencies t = Set.map (LD.fold id Referent.toReference) (labeledDependencies t)

termDependencies :: (Ord v, Ord vt) => Term2 vt at ap v a -> Set TermReference
termDependencies =
  Set.fromList
    . mapMaybe
      ( LD.fold
          (\_typeRef -> Nothing)
          ( Referent.fold
              (\termRef -> Just termRef)
              (\_typeConRef _i _ct -> Nothing)
          )
      )
    . toList
    . labeledDependencies

-- gets types from annotations and constructors
typeDependencies :: (Ord v, Ord vt) => Term2 vt at ap v a -> Set Reference
typeDependencies =
  Set.fromList . mapMaybe (LD.fold Just (const Nothing)) . toList . labeledDependencies

-- Gets the types to which this term contains references via patterns and
-- data constructors.
constructorDependencies ::
  (Ord v, Ord vt) => Term2 vt at ap v a -> Set Reference
constructorDependencies =
  Set.unions
    . generalizedDependencies
      (const mempty)
      (const mempty)
      Set.singleton
      (const . Set.singleton)
      Set.singleton
      (const . Set.singleton)
      Set.singleton

generalizedDependencies ::
  (Ord v, Ord vt, Ord r) =>
  (Reference -> r) ->
  (Reference -> r) ->
  (Reference -> r) ->
  (Reference -> ConstructorId -> r) ->
  (Reference -> r) ->
  (Reference -> ConstructorId -> r) ->
  (Reference -> r) ->
  Term2 vt at ap v a ->
  Set r
generalizedDependencies termRef typeRef literalType dataConstructor dataType effectConstructor effectType =
  Set.fromList . Writer.execWriter . ABT.visit' f
  where
    f t@(Ref r) = Writer.tell [termRef r] $> t
    f t@(TermLink r) = case r of
      Referent.Ref r -> Writer.tell [termRef r] $> t
      Referent.Con (ConstructorReference r id) CT.Data -> Writer.tell [dataConstructor r id] $> t
      Referent.Con (ConstructorReference r id) CT.Effect -> Writer.tell [effectConstructor r id] $> t
    f t@(TypeLink r) = Writer.tell [typeRef r] $> t
    f t@(Ann _ typ) =
      Writer.tell (map typeRef . toList $ Type.dependencies typ) $> t
    f t@(Nat _) = Writer.tell [literalType Type.natRef] $> t
    f t@(Int _) = Writer.tell [literalType Type.intRef] $> t
    f t@(Float _) = Writer.tell [literalType Type.floatRef] $> t
    f t@(Boolean _) = Writer.tell [literalType Type.booleanRef] $> t
    f t@(Text _) = Writer.tell [literalType Type.textRef] $> t
    f t@(List _) = Writer.tell [literalType Type.listRef] $> t
    f t@(Constructor (ConstructorReference r cid)) =
      Writer.tell [dataType r, dataConstructor r cid] $> t
    f t@(Request (ConstructorReference r cid)) =
      Writer.tell [effectType r, effectConstructor r cid] $> t
    f t@(Match _ cases) = traverse_ goPat cases $> t
    f t = pure t
    goPat (MatchCase pat _ _) =
      Writer.tell . toList $
        Pattern.generalizedDependencies
          literalType
          dataConstructor
          dataType
          effectConstructor
          effectType
          pat

labeledDependencies ::
  (Ord v, Ord vt) => Term2 vt at ap v a -> Set LabeledDependency
labeledDependencies =
  generalizedDependencies
    LD.termRef
    LD.typeRef
    LD.typeRef
    (\r i -> LD.dataConstructor (ConstructorReference r i))
    LD.typeRef
    (\r i -> LD.effectConstructor (ConstructorReference r i))
    LD.typeRef

updateDependencies ::
  Ord v =>
  Map Referent Referent ->
  Map Reference Reference ->
  Term v a ->
  Term v a
updateDependencies termUpdates typeUpdates = ABT.rebuildUp go
  where
    referent (Referent.Ref r) = Ref r
    referent (Referent.Con r CT.Data) = Constructor r
    referent (Referent.Con r CT.Effect) = Request r
    go (Ref r) = case Map.lookup (Referent.Ref r) termUpdates of
      Nothing -> Ref r
      Just r -> referent r
    go ct@(Constructor r) = case Map.lookup (Referent.Con r CT.Data) termUpdates of
      Nothing -> ct
      Just r -> referent r
    go req@(Request r) = case Map.lookup (Referent.Con r CT.Effect) termUpdates of
      Nothing -> req
      Just r -> referent r
    go (TermLink r) = TermLink (Map.findWithDefault r r termUpdates)
    go (TypeLink r) = TypeLink (Map.findWithDefault r r typeUpdates)
    go (Ann tm tp) = Ann tm $ Type.updateDependencies typeUpdates tp
    go (Match tm cases) = Match tm (u <$> cases)
      where
        u (MatchCase pat g b) = MatchCase (Pattern.updateDependencies termUpdates pat) g b
    go f = f

-- | If the outermost term is a function application,
-- perform substitution of the argument into the body
betaReduce :: Var v => Term0 v -> Term0 v
betaReduce (App' (Lam' f) arg) = ABT.bind f arg
betaReduce e = e

betaNormalForm :: Var v => Term0 v -> Term0 v
betaNormalForm (App' f a) = betaNormalForm (betaReduce (app () (betaNormalForm f) a))
betaNormalForm e = e

-- x -> f x => f
etaNormalForm :: Ord v => Term0 v -> Term0 v
etaNormalForm tm = case tm of
  LamNamed' v body -> step . lam (ABT.annotation tm) v $ etaNormalForm body
    where
      step (LamNamed' v (App' f (Var' v')))
        | v == v', v `Set.notMember` freeVars f = f
      step tm = tm
  _ -> tm

-- x -> f x => f as long as `x` is a variable of type `Var.Eta`
etaReduceEtaVars :: Var v => Term0 v -> Term0 v
etaReduceEtaVars tm = case tm of
  LamNamed' v body -> step . lam (ABT.annotation tm) v $ etaReduceEtaVars body
    where
      ok v v' f =
        v == v' && Var.typeOf v == Var.Eta
          && v `Set.notMember` freeVars f
      step (LamNamed' v (App' f (Var' v'))) | ok v v' f = f
      step tm = tm
  _ -> tm

-- This converts `Reference`s it finds that are in the input `Map`
-- back to free variables
unhashComponent ::
  forall v a.
  Var v =>
  Map Reference.Id (Term v a) ->
  Map Reference.Id (v, Term v a)
unhashComponent m =
  let usedVars = foldMap (Set.fromList . ABT.allVars) m
      m' :: Map Reference.Id (v, Term v a)
      m' = evalState (Map.traverseWithKey assignVar m) usedVars
        where
          assignVar r t = (,t) <$> ABT.freshenS (Var.unnamedRef r)
      unhash1 :: Term v a -> Term v a
      unhash1 = ABT.rebuildUp' go
        where
          go e@(Ref' (Reference.DerivedId r)) = case Map.lookup r m' of
            Nothing -> e
            Just (v, _) -> var (ABT.annotation e) v
          go e = e
   in second unhash1 <$> m'

fromReferent ::
  Ord v =>
  a ->
  Referent ->
  Term2 vt at ap v a
fromReferent a = \case
  Referent.Ref r -> ref a r
  Referent.Con r ct -> case ct of
    CT.Data -> constructor a r
    CT.Effect -> request a r

-- mostly boring serialization code below ...

instance (Eq a, ABT.Var v) => Eq1 (F v a p) where (==#) = (==)

instance (Show v) => Show1 (F v a p) where showsPrec1 = showsPrec

instance (ABT.Var vt, Eq at, Eq a) => Eq (F vt at p a) where
  Int x == Int y = x == y
  Nat x == Nat y = x == y
  Float x == Float y = x == y
  Boolean x == Boolean y = x == y
  Text x == Text y = x == y
  Char x == Char y = x == y
  Blank b == Blank q = b == q
  Ref x == Ref y = x == y
  TermLink x == TermLink y = x == y
  TypeLink x == TypeLink y = x == y
  Constructor r == Constructor r2 = r == r2
  Request r == Request r2 = r == r2
  Handle h b == Handle h2 b2 = h == h2 && b == b2
  App f a == App f2 a2 = f == f2 && a == a2
  Ann e t == Ann e2 t2 = e == e2 && t == t2
  List v == List v2 = v == v2
  If a b c == If a2 b2 c2 = a == a2 && b == b2 && c == c2
  And a b == And a2 b2 = a == a2 && b == b2
  Or a b == Or a2 b2 = a == a2 && b == b2
  Lam a == Lam b = a == b
  LetRec _ bs body == LetRec _ bs2 body2 = bs == bs2 && body == body2
  Let _ binding body == Let _ binding2 body2 =
    binding == binding2 && body == body2
  Match scrutinee cases == Match s2 cs2 = scrutinee == s2 && cases == cs2
  _ == _ = False

instance (Show v, Show a) => Show (F v a0 p a) where
  showsPrec = go
    where
      go _ (Int n) = (if n >= 0 then s "+" else s "") <> shows n
      go _ (Nat n) = shows n
      go _ (Float n) = shows n
      go _ (Boolean True) = s "true"
      go _ (Boolean False) = s "false"
      go p (Ann t k) = showParen (p > 1) $ shows t <> s ":" <> shows k
      go p (App f x) = showParen (p > 9) $ showsPrec 9 f <> s " " <> showsPrec 10 x
      go _ (Lam body) = showParen True (s "λ " <> shows body)
      go _ (List vs) = showListWith shows (toList vs)
      go _ (Blank b) = case b of
        B.Blank -> s "_"
        B.Recorded (B.Placeholder _ r) -> s ("_" ++ r)
        B.Recorded (B.Resolve _ r) -> s r
      go _ (Ref r) = s "Ref(" <> shows r <> s ")"
      go _ (TermLink r) = s "TermLink(" <> shows r <> s ")"
      go _ (TypeLink r) = s "TypeLink(" <> shows r <> s ")"
      go _ (Let _ b body) =
        showParen True (s "let " <> shows b <> s " in " <> shows body)
      go _ (LetRec _ bs body) =
        showParen
          True
          (s "let rec" <> shows bs <> s " in " <> shows body)
      go _ (Handle b body) =
        showParen
          True
          (s "handle " <> shows b <> s " in " <> shows body)
      go _ (Constructor (ConstructorReference r n)) = s "Con" <> shows r <> s "#" <> shows n
      go _ (Match scrutinee cases) =
        showParen
          True
          (s "case " <> shows scrutinee <> s " of " <> shows cases)
      go _ (Text s) = shows s
      go _ (Char c) = shows c
      go _ (Request (ConstructorReference r n)) = s "Req" <> shows r <> s "#" <> shows n
      go p (If c t f) =
        showParen (p > 0) $
          s "if "
            <> shows c
            <> s " then "
            <> shows t
            <> s " else "
            <> shows f
      go p (And x y) =
        showParen (p > 0) $ s "and " <> shows x <> s " " <> shows y
      go p (Or x y) =
        showParen (p > 0) $ s "or " <> shows x <> s " " <> shows y
      (<>) = (.)
      s = showString
