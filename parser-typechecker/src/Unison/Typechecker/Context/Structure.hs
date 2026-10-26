-- | The typechecker's ordered context, indexed so that the operations the
-- algorithm performs cost logarithmic time in the size of the context.
--
-- The context is a sequence of 'Element's. The algorithm appends to it,
-- retracts a suffix of it (everything after a marker), replaces one element
-- with a few (an existential solved to an arrow, with the arrow's fresh
-- existentials inserted before it), and asks scoping questions: is this
-- variable bound, as what, and before which position?
--
-- An earlier version answered the scoping questions from a finger-tree
-- measure that summarized every subtree with sets of its bound and free
-- variables. Every split and concatenation then recomputed those set unions
-- for every node it touched, on the order of a hundred per solve, and a
-- profile of a definition with 500 overloaded operators put two thirds of the
-- typechecking time in those unions and the variable comparisons under them.
--
-- Here the facts live in maps keyed by variable, and the sequence only orders
-- them. Each element carries a 'Label' giving its position; the tree's measure
-- is the maximum label of a subtree (so a position is found by searching for
-- its label) and a count of 'Inconsistent' markers, both O(1) to combine. The
-- map from a variable to its binding element, with that element's label,
-- answers "bound before position L" by one lookup and one label comparison,
-- and a solution or annotation is read from the same map. Replacing or
-- retracting elements deletes their entries, so the map always describes
-- exactly the elements in the sequence, and scoping is the sequence's.
--
-- A retraction walks the elements it discards, so the typechecker can report
-- on them, and deletes their entries. Every element is appended once and
-- discarded at most once, so that walk is amortized against the appends.
--
-- Segments of a context that have been retracted are plain lists
-- ('CtxSegment'); the typechecker reads them whole (to generalize over their
-- unsolved existentials, apply their solutions, or push some of them back).
module Unison.Typechecker.Context.Structure
  ( -- * Elements
    Element (..),
    variable,
    binds,

    -- * Contexts
    Context,
    context0,
    elements,
    CtxSegment,

    -- * Scope
    Scope (..),
    scopeOf,
    existentialIn,
    universalIn,
    ordered,

    -- * Queries
    lookupElement,
    lookupSolved,
    lookupAnn,
    annotations,
    inconsistent,
    apply,
    applySegment,
    unsolvedExistentials,
    boundExistentials,
    recorded,

    -- * Updates
    BadContext (..),
    Rejected (..),
    extend,
    extends,
    replace,
    retract,
    markRetained,
  )
where

import Data.FingerTree (FingerTree, Measured (..), SearchResult (..), search, (<|), (><), (|>))
import Data.FingerTree qualified as FT
import Data.Foldable qualified as FO
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Word (Word64)
import Unison.ABT qualified as ABT
import Unison.Blank qualified as B
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Syntax.TypePrinter qualified as TP
import Unison.Type qualified as Type
import Unison.Typechecker.TypeVar qualified as TypeVar
import Unison.Var (Var)
import Unison.Var qualified as Var

type Monotype v loc = Type.Monotype (TypeVar v loc) loc

type TypeVar v loc = TypeVar.TypeVar (B.Blank loc) v

type Type v loc = Type.Type (TypeVar v loc) loc

-- | Elements of an ordered algorithmic context. Every element but 'Refined'
-- binds its variable; a context is valid when no variable is bound twice and
-- every type in it mentions only variables bound before it.
data Element v loc
  = -- | A variable declaration
    Var (TypeVar v loc)
  | -- | `v` is solved to some monotype
    Solved (B.Blank loc) v (Monotype v loc)
  | -- | `v` has type `a`, maybe quantified
    -- loc contains the span of the name of the bound 'v'
    Ann v loc (Type v loc)
  | -- | The (already-bound) universal variable `v` is locally known to
    -- equal the given type. This is how GADT pattern matching refines a
    -- rigid type index inside a branch: matching e.g. @NatLit : Expr Nat@
    -- against a scrutinee of type @Expr a@ records @a ~ Nat@ for the
    -- duration of that branch. Like other branch-local context, these are
    -- introduced after a 'Marker' and dropped on retraction.
    Refined v (Type v loc)
  | -- | Marks the context as inconsistent (the DK indexed-types paper's `⊥`):
    -- the branch-local equalities are contradictory, so the current pattern-match
    -- branch is unreachable and its body is vacuously well-typed. Carries a
    -- (fresh) variable only so it scopes like any other element and is dropped on
    -- retraction. See 'inconsistent'.
    Inconsistent v
  | -- | used for scoping
    Marker v

variable :: Element v loc -> v
variable (Var v) = TypeVar.underlying v
variable (Solved _ v _) = v
variable (Ann v _ _) = v
variable (Refined v _) = v
variable (Inconsistent v) = v
variable (Marker v) = v

-- | The variable an element binds, if it binds one. A 'Refined' element
-- constrains a universal bound earlier rather than binding it.
binds :: Element v loc -> Maybe v
binds (Refined _ _) = Nothing
binds e = Just (variable e)

instance (Var v) => Eq (Element v loc) where
  Var v == Var v2 = v == v2
  Solved _ v t == Solved _ v2 t2 = v == v2 && t == t2
  Ann v _loc t == Ann v2 _loc2 t2 = v == v2 && t == t2
  Refined v t == Refined v2 t2 = v == v2 && t == t2
  Inconsistent v == Inconsistent v2 = v == v2
  Marker v == Marker v2 = v == v2
  _ == _ = False

isExistential :: Element v loc -> Bool
isExistential (Var (TypeVar.Existential _ _)) = True
isExistential (Solved {}) = True
isExistential _ = False

isUniversal :: Element v loc -> Bool
isUniversal (Var (TypeVar.Universal _)) = True
isUniversal _ = False

-- | The variables occurring in an element's type, which must all be bound
-- before the element.
freeVarsOf :: (Ord v) => Element v loc -> Set (TypeVar v loc)
freeVarsOf (Solved _ _ t) = Type.freeVars (Type.getPolytype t)
freeVarsOf (Ann _ _ t) = Type.freeVars t
freeVarsOf (Refined _ t) = Type.freeVars t
freeVarsOf _ = Set.empty

-- | The position of an element: a path of counter values ending in
-- 'maxBound', compared lexicographically.
--
-- An appended element gets @[n, maxBound]@ for the next counter value @n@,
-- which sorts after every label already in the context. An element inserted
-- before the element labelled @p ++ [maxBound]@ gets @p ++ [c, maxBound]@ for
-- the next counter value @c@: that sorts before @p ++ [maxBound]@ (as
-- @c < maxBound@), after anything inserted before the same element earlier
-- (a smaller @c@), and otherwise exactly where @p@ sorts. Counter values are
-- never reused, so a label names one element for the life of the context.
newtype Label = Label [Word64] deriving (Eq, Ord, Show)

appended :: Word64 -> Label
appended n = Label [n, maxBound]

insertedBefore :: Label -> Word64 -> Label
insertedBefore (Label ws) c = Label (List.init ws ++ [c, maxBound])

-- | What the sequence summarizes about a subtree: enough to find a position
-- by its label, and whether an 'Inconsistent' element is present.
data Measure = Measure
  { maxLabel :: !Label,
    inconsistentCount :: !Int
  }

instance Semigroup Measure where
  Measure l i <> Measure l' i' = Measure (max l l') (i + i')

instance Monoid Measure where
  mempty = Measure (Label []) 0

data Entry v loc = Entry
  { entryLabel :: !Label,
    entryElement :: Element v loc
  }

instance Measured Measure (Entry v loc) where
  measure (Entry l e) = Measure l (case e of Inconsistent _ -> 1; _ -> 0)

-- | An ordered context. The sequence orders the elements; 'bound' holds the
-- binding element of every bound variable together with its position, and
-- 'refinements' the 'Refined' elements per universal, latest first. The maps
-- describe exactly the elements in the sequence.
data Context v loc = Context
  { entries :: !(FingerTree Measure (Entry v loc)),
    -- | Source of label components; only ever increases.
    counter :: !Word64,
    bound :: !(Map v (Entry v loc)),
    -- | The 'Ann' elements again, so that a walk of the annotations (which
    -- the ability checks make per existential) costs the annotations, not
    -- the context.
    anns :: !(Map v (loc, Type v loc)),
    refinements :: !(Map v [(Label, Type v loc)])
  }

-- | The empty context
context0 :: Context v loc
context0 = Context FT.empty 0 Map.empty Map.empty Map.empty

-- | The elements, in order.
elements :: Context v loc -> [Element v loc]
elements = map entryElement . FO.toList . entries

-- | A run of elements taken out of a context, in order.
type CtxSegment v loc = [Element v loc]

-- | A place in the context at which scoping questions are asked: at the end,
-- or just before a given element.
data Scope = AtEnd | Before !Label

inScope :: Scope -> Label -> Bool
inScope AtEnd _ = True
inScope (Before l) l' = l' < l

-- | The scope just before the given variable's element.
scopeOf :: (Ord v) => v -> Context v loc -> Maybe Scope
scopeOf v ctx = Before . entryLabel <$> Map.lookup v (bound ctx)

-- | Whether the variable is bound as an existential, solved or not, within
-- the scope.
existentialIn :: (Ord v) => Scope -> v -> Context v loc -> Bool
existentialIn sc v ctx = case Map.lookup v (bound ctx) of
  Just (Entry l e) | isExistential e -> inScope sc l
  _ -> False

-- | Whether the variable is bound as a universal within the scope.
universalIn :: (Ord v) => Scope -> v -> Context v loc -> Bool
universalIn sc v ctx = case Map.lookup v (bound ctx) of
  Just (Entry l e) | isUniversal e -> inScope sc l
  _ -> False

-- | @ordered ctx v v2@ holds when @v@ is an existential bound before @v2@'s
-- element: Γ[v^][v2^] in the paper's notation.
ordered :: (Ord v) => Context v loc -> v -> v -> Bool
ordered ctx v v2 = case Map.lookup v2 (bound ctx) of
  Just (Entry l _) -> existentialIn (Before l) v ctx
  Nothing -> False

-- | The element binding the variable, if any.
lookupElement :: (Ord v) => v -> Context v loc -> Maybe (Element v loc)
lookupElement v ctx = entryElement <$> Map.lookup v (bound ctx)

lookupSolved :: (Ord v) => Context v loc -> v -> Maybe (Monotype v loc)
lookupSolved ctx v = case lookupElement v ctx of
  Just (Solved _ _ t) -> Just t
  _ -> Nothing

lookupAnn :: (Ord v) => Context v loc -> v -> Maybe (Type v loc)
lookupAnn ctx v = snd <$> Map.lookup v (anns ctx)

-- | Every term variable annotation in the context.
annotations :: Context v loc -> [(v, loc, Type v loc)]
annotations ctx = [(v, l, t) | (v, (l, t)) <- Map.toList (anns ctx)]

-- | Whether the context holds an 'Inconsistent' element.
inconsistent :: Context v loc -> Bool
inconsistent ctx = inconsistentCount (measure (entries ctx)) > 0

-- | Replace any existentials with their solution in the context, and any
-- refined universals with their refinement.
apply :: (Var v) => Context v loc -> Type v loc -> Type v loc
apply ctx = apply' (lookupSolved ctx) refined
  where
    refined v = case Map.lookup v (refinements ctx) of
      Just ((_, t) : _) -> Just t
      _ -> Nothing

-- | 'apply' with the solutions and refinements found in a segment. The maps
-- are built once per segment, so a partial application to a segment can be
-- reused over many types.
applySegment :: (Var v) => CtxSegment v loc -> Type v loc -> Type v loc
applySegment es = apply' (`Map.lookup` solved) (`Map.lookup` refined)
  where
    -- A later element wins, as the rightmost one does in a context.
    solved = Map.fromList [(v, t) | Solved _ v t <- es]
    refined = Map.fromList [(v, t) | Refined v t <- es]

apply' ::
  (Var v) =>
  (v -> Maybe (Monotype v loc)) ->
  (v -> Maybe (Type v loc)) ->
  Type v loc ->
  Type v loc
apply' solved refined t0
  | Set.null (Type.freeVars t0) = t0
  | otherwise = go t0
  where
    go t = case t of
      Type.Var' (TypeVar.Universal v) ->
        maybe t go (refined v)
      Type.Ref' _ -> t
      Type.Var' (TypeVar.Existential _ v) ->
        maybe t (\(Type.Monotype t') -> go t') (solved v)
      Type.Arrow' i o -> Type.arrow a (go i) (go o)
      Type.App' x y -> Type.app a (go x) (go y)
      Type.Ann' v k -> Type.ann a (go v) k
      Type.Effect1' e t -> Type.effect1 a (go e) (go t)
      Type.Effects' es -> Type.effects a (fmap go es)
      Type.ForallNamed' v t' -> Type.forAll a v (go t')
      Type.IntroOuterNamed' v t' -> Type.introOuter a v (go t')
      Type.Record' fb fields ->
        -- Substituting a solution into a row can nest records, so re-flatten:
        -- `{x: Nat | {y: Text}}` and `{x: Nat, y: Text}` are the same type and
        -- have to compare equal.
        Type.normalize (Type.record a (go <$> fb) (go <$> fields))
      _ -> error $ "Match error in Context.apply': " ++ show t
      where
        a = ABT.annotation t

-- | The existentials a segment declares and does not solve.
unsolvedExistentials :: (Ord v) => CtxSegment v loc -> Set v
unsolvedExistentials es =
  Set.fromList [v | Var (TypeVar.Existential _ v) <- es]
    `Set.difference` Set.fromList [v | Solved _ v _ <- es]

-- | The existentials a segment binds, solved or not.
boundExistentials :: (Ord v) => CtxSegment v loc -> Set v
boundExistentials es = Set.fromList [variable e | e <- es, isExistential e]

-- | The existentials of a segment that carry a 'B.Recorded' blank, with the
-- type each stands for: its solution, or itself when unsolved.
recorded :: (Ord v) => CtxSegment v loc -> Map v (B.Recorded loc, Type v loc)
recorded = Map.fromList . mapMaybe f
  where
    f (Var tv@(TypeVar.Existential (B.Recorded b) v)) =
      Just (v, (b, ABT.annotatedVar (B.loc b) tv))
    f (Solved (B.Recorded b) v ty) = Just (v, (b, Type.getPolytype ty))
    f _ = Nothing

-- | Why an element cannot go where it was put.
data BadContext
  = -- | Its variable is already bound.
    Shadowing
  | -- | Its type mentions an existential not bound before it.
    UnboundEx
  | -- | Its type mentions a universal not bound before it.
    UnboundUn
  | -- | A replacement for a variable's element does not bind the variable
    -- again, which would unbind every later element that mentions it.
    NotRebound
  deriving (Eq, Show)

-- | A refused update: the context as it stood (for 'replace', the elements
-- before the replaced one plus the replacements accepted so far), the element
-- refused, and why.
data Rejected v loc = Rejected (Context v loc) (Element v loc) BadContext

-- Checks an element against the context at the given scope: its variable must
-- not already be bound, and its type's variables must be bound within the
-- scope, each as the kind of variable it occurs as.
check :: (Ord v) => Scope -> Element v loc -> Context v loc -> Maybe BadContext
check sc e ctx
  | Just v <- binds e, Map.member v (bound ctx) = Just Shadowing
  | Refined v _ <- e, not (universalIn sc v ctx) = Just UnboundUn
  | otherwise = listToMaybe (mapMaybe unbound (Set.toList (freeVarsOf e)))
  where
    unbound (TypeVar.Existential _ v)
      | existentialIn sc v ctx = Nothing
      | otherwise = Just UnboundEx
    unbound (TypeVar.Universal v)
      | universalIn sc v ctx = Nothing
      | otherwise = Just UnboundUn

-- Enters an element that is in the sequence into the maps.
record :: (Ord v) => Entry v loc -> Context v loc -> Context v loc
record en@(Entry l e) ctx = case e of
  Refined v t -> ctx {refinements = Map.insertWith (++) v [(l, t)] (refinements ctx)}
  Ann v loc t -> ctx {bound = Map.insert v en (bound ctx), anns = Map.insert v (loc, t) (anns ctx)}
  _ -> ctx {bound = Map.insert (variable e) en (bound ctx)}

-- Removes an element that has left the sequence from the maps. Only the entry
-- made for this element (by label) is removed, so an element that has already
-- been superseded in the maps is left alone.
forget :: (Ord v) => Entry v loc -> Context v loc -> Context v loc
forget (Entry l e) ctx = case e of
  Refined v _ ->
    ctx {refinements = Map.update (nonEmpty . List.filter ((/= l) . fst)) v (refinements ctx)}
  _
    | Just en <- Map.lookup v (bound ctx),
      entryLabel en == l ->
        ctx
          { bound = Map.delete v (bound ctx),
            anns = case e of
              Ann {} -> Map.delete v (anns ctx)
              _ -> anns ctx
          }
    | otherwise -> ctx
  where
    v = variable e
    nonEmpty [] = Nothing
    nonEmpty xs = Just xs

-- Appends an element with a fresh label, without checking it.
append :: (Ord v) => Label -> Element v loc -> Context v loc -> Context v loc
append l e ctx = record en ctx {entries = entries ctx |> en, counter = counter ctx + 1}
  where
    en = Entry l e

-- | Adds an element to the end of the context.
extend :: (Ord v) => Element v loc -> Context v loc -> Either BadContext (Context v loc)
extend e ctx = case check AtEnd e ctx of
  Just bad -> Left bad
  Nothing -> Right (append (appended (counter ctx)) e ctx)

-- | Adds elements to the end of the context, in order.
extends :: (Ord v) => CtxSegment v loc -> Context v loc -> Either (Rejected v loc) (Context v loc)
extends es ctx0 = go ctx0 es
  where
    go ctx [] = Right ctx
    go ctx (e : rest) = case extend e ctx of
      Left bad -> Left (Rejected ctx e bad)
      Right ctx' -> go ctx' rest

-- The position of the element with the given label: the first element at
-- which the running maximum label reaches it.
atLabel :: Label -> Measure -> Measure -> Bool
atLabel l pre _ = maxLabel pre >= l

-- | Replaces the element binding a variable with a run of elements, which
-- must bind the variable again (typically last, after fresh existentials its
-- solution mentions). Each replacement is checked in the scope of the
-- replaced element. A variable that is not bound leaves the context as it is.
replace :: (Ord v) => v -> CtxSegment v loc -> Context v loc -> Either (Rejected v loc) (Context v loc)
replace v es ctx0 = case Map.lookup v (bound ctx0) of
  Nothing -> Right ctx0
  Just old@(Entry l oldE)
    | not (any ((== Just v) . binds) es) -> Left (Rejected ctx0 oldE NotRebound)
    | Position left _ right <- search (atLabel l) (entries ctx0) ->
        let go ctx [] = Right ctx {entries = entries ctx >< right}
            go ctx (e : rest) = case check (Before l) e ctx of
              Just bad -> Left (Rejected ctx e bad)
              Nothing -> go (append (insertedBefore l (counter ctx)) e ctx) rest
         in go (forget old ctx0) {entries = left} es
    | otherwise -> error "Context.Structure.replace: a bound variable's label is not in the sequence"

-- | Removes the element binding the variable and everything after it. Returns
-- the context that remains and the removed elements after the binding one.
retract :: (Ord v) => v -> Context v loc -> Maybe (Context v loc, CtxSegment v loc)
retract v ctx = case Map.lookup v (bound ctx) of
  Nothing -> Nothing
  Just (Entry l _) -> case search (atLabel l) (entries ctx) of
    Position left en right ->
      let gone = en : FO.toList right
          ctx' = List.foldl' (flip forget) ctx gone
       in Just (ctx' {entries = left}, map entryElement (FO.toList right))
    _ -> error "Context.Structure.retract: a bound variable's label is not in the sequence"

-- | Marks the given existentials, where they are bound with a plain blank, as
-- retained ('B.Retain'), so that later generalization leaves them in place.
markRetained :: (Ord v) => Set v -> Context v loc -> Context v loc
markRetained keep ctx0 = FO.foldl' mark ctx0 keep
  where
    mark ctx v = case Map.lookup v (bound ctx) of
      Just (Entry l e) | Just e' <- retain e -> replaceAt l e' ctx
      _ -> ctx
    retain (Var (TypeVar.Existential B.Blank v)) = Just (Var (TypeVar.Existential B.Retain v))
    retain (Solved B.Blank v t) = Just (Solved B.Retain v t)
    retain _ = Nothing

-- Replaces the element at a label with another binding the same variable.
replaceAt :: (Ord v) => Label -> Element v loc -> Context v loc -> Context v loc
replaceAt l e ctx = case search (atLabel l) (entries ctx) of
  Position left _ right -> record en ctx {entries = left >< (en <| right)}
  _ -> ctx
  where
    en = Entry l e

instance (Var v) => Show (Element v loc) where
  show (Var v) = case v of
    TypeVar.Universal x -> "@" <> show x
    e -> show e
  show (Solved _ v t) = "'" ++ Text.unpack (Var.name v) ++ " = " ++ Text.unpack (TP.prettyStr 0 PPE.empty (Type.getPolytype t))
  show (Ann v _loc t) =
    Text.unpack (Var.name v)
      ++ " : "
      ++ Text.unpack (TP.prettyStr 0 PPE.empty t)
  show (Refined v t) =
    Text.unpack (Var.name v)
      ++ " ~ "
      ++ Text.unpack (TP.prettyStr 0 PPE.empty t)
  show (Inconsistent _) = "⊥"
  show (Marker v) = "|" ++ Text.unpack (Var.name v) ++ "|"

renderType :: (Ord loc, Var v) => Context v loc -> Type v loc -> String
renderType ctx ty = Text.unpack . TP.prettyStr 0 PPE.empty $ apply ctx ty

renderElement ::
  (Ord loc, Var v) => Context v loc -> Element v loc -> String
renderElement ctx = \case
  Var v
    | TypeVar.Universal x <- v -> "@" <> show x
    | otherwise -> show v
  Solved _ v (Type.Monotype t) ->
    mconcat
      [ "'",
        Text.unpack $ Var.name v,
        " = ",
        renderType ctx t
      ]
  Ann v _loc t ->
    mconcat
      [ Text.unpack $ Var.name v,
        " : ",
        renderType ctx t
      ]
  Refined v t ->
    mconcat
      [ Text.unpack $ Var.name v,
        " ~ ",
        renderType ctx t
      ]
  Inconsistent _ -> "⊥"
  Marker v -> "|" <> Text.unpack (Var.name v) <> "|"

renderElements ::
  (Ord loc, Var v) => Context v loc -> [Element v loc] -> String
renderElements ctx = List.intercalate "\n  " . fmap (renderElement ctx)

instance (Ord loc, Var v) => Show (Context v loc) where
  show ctx =
    "Γ\n  " <> renderElements ctx (reverse (elements ctx))
