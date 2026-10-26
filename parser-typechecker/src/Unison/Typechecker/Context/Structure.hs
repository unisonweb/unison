-- | This module defines the ordered context type and related types used by
-- the typechecking algorithm.
module Unison.Typechecker.Context.Structure
  ( -- * Elements
    Element (..),
    variable,
    declares,

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
    existentialVars,
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

-- | The ordered context of the typechecking algorithm.
--
-- The context is a sequence of 'Element's. The typechecking algorithm appends
-- to it, retracts a suffix of it (everything after a marker), replaces one
-- element with a few (e.g. an existential solved to an arrow, with the arrow's
-- fresh existentials inserted before it), and asks scoping questions: is this
-- variable declared, as an existential or a universal, and at what position?
-- Is it solved, and to what?
--
-- The sequence only orders the elements. What is known about each variable
-- is kept in indexes keyed by variable name, beside the sequence, and each of
-- those maps a variable to a position in the sequence ('CtxPosition'): every
-- element in the sequence has one, and 'vars' maps each variable to
-- its element together with that position.
--
-- So for instance, a solution proposed for an existential @β@ may only
-- mention variables declared before @β@ in the context, existentials and
-- universals alike (the universals are the rigid type variables of enclosing
-- quantifiers). `solve` therefore looks up each free variable of the solution
-- in 'vars', checks that it is declared as the kind of variable it occurs
-- as, and compares its position with @β@'s. Once @β@ is solved, its solution
-- is read from the same entry. 'anns' and 'refinements' are the same idea for
-- term variable annotations and for the branch-local refinements of a
-- universal.
--
-- The sequence is a finger tree whose measure includes the greatest position in a
-- subtree (so that the element at a position can be found in logarithmic time),
-- and a count of 'Inconsistent' elements (so that 'inconsistent' can tell in
-- constant time whether the pattern-match branch being checked has been
-- marked unreachable by contradictory GADT refinements, in which case its
-- body is skipped).
--
-- Every change keeps the indexes describing exactly the elements in the
-- sequence: an append or replacement records the new elements, and a
-- retraction walks the elements it discards and forgets them. Each element
-- is appended once and discarded at most once, so a retraction costs no more
-- than the appends it undoes.
--
-- Segments of a context that have been retracted are plain lists
-- ('CtxSegment'); the typechecker reads them whole (to generalize over their
-- unsolved existentials, apply their solutions, or push some of them back).
data Context v loc = Context
  { entries :: !(FingerTree Measure (Entry v loc)),
    -- | Source of position components; only ever increases.
    counter :: !Word64,
    -- | The element that declares each variable, with its position. Term and
    -- type variables share this one namespace, and a valid context declares a
    -- variable once ('check' refuses a second declaration of any kind, an
    -- 'Ann' after a 'Var' included, as 'DuplicateVariable'), so one entry per
    -- variable is exact; the one kind of element that may recur for a
    -- variable, 'Refined', declares nothing and is kept in 'refinements'.
    vars :: !(Map v (Entry v loc)),
    -- | The 'Ann' elements again, so that a walk of the annotations (which
    -- the ability checks make per existential) costs the annotations, not
    -- the context.
    anns :: !(Map v (loc, Type v loc)),
    -- | The 'Refined' elements per universal, latest first.
    refinements :: !(Map v [(CtxPosition, Type v loc)])
  }

-- | Elements of an ordered algorithmic context. Every element but 'Refined'
-- declares its variable; a context is valid when no variable is declared twice and
-- every type in it mentions only variables declared before it.
data Element v loc
  = -- | A variable declaration
    Var (TypeVar v loc)
  | -- | `v` is solved to some monotype
    Solved (B.Blank loc) v (Monotype v loc)
  | -- | `v` has type `a`, maybe quantified
    -- loc contains the span of the name of the annotated 'v'
    Ann v loc (Type v loc)
  | -- | The (already-declared) universal variable `v` is locally known to
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

-- | The variable an element declares, if it declares one. A 'Refined' element
-- constrains a universal declared earlier rather than declaring it.
declares :: Element v loc -> Maybe v
declares (Refined _ _) = Nothing
declares e = Just (variable e)

-- | The position of an element: a path of values of the context's own
-- 'counter', ending in 'maxBound', compared lexicographically.
--
-- An appended element gets @[n, maxBound]@ for the next counter value @n@,
-- which sorts after every position already in the context. An element inserted
-- before the element at position @p ++ [maxBound]@ gets @p ++ [c, maxBound]@ for
-- the next counter value @c@: that sorts before @p ++ [maxBound]@ (as
-- @c < maxBound@), after anything inserted before the same element earlier
-- (a smaller @c@), and otherwise exactly where @p@ sorts.
--
-- The counter is a field of the 'Context' value and every append or insertion
-- takes the next value from it, so within one context value no two elements
-- share a position, which is all the indexes need. The typechecker never sees
-- positions: it asks its questions by variable, and keeps its own counter
-- ('freshId') for naming variables. When the typechecker restores an earlier
-- context value (to retry something), that value's counter comes back with
-- it, and the positions it hands out again belong to that value's elements,
-- not to the elements of the abandoned one.
newtype CtxPosition = CtxPosition [Word64] deriving (Eq, Ord, Show)

appended :: Word64 -> CtxPosition
appended n = CtxPosition [n, maxBound]

insertedBefore :: CtxPosition -> Word64 -> CtxPosition
insertedBefore (CtxPosition ws) c = CtxPosition (List.init ws ++ [c, maxBound])

-- | What the sequence summarizes about a subtree: enough to find an element
-- by its position, and whether an 'Inconsistent' element is present.
data Measure = Measure
  { maxPosition :: !CtxPosition,
    inconsistentCount :: !Int
  }

data Entry v loc = Entry
  { entryPosition :: !CtxPosition,
    entryElement :: Element v loc
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
data Scope = AtEnd | Before !CtxPosition

-- | The scope just before the given variable's element.
scopeOf :: (Ord v) => v -> Context v loc -> Maybe Scope
scopeOf v ctx = Before . entryPosition <$> Map.lookup v (vars ctx)

-- | Whether the variable is declared as an existential, solved or not, within
-- the scope.
existentialIn :: (Ord v) => Scope -> v -> Context v loc -> Bool
existentialIn sc v ctx = case Map.lookup v (vars ctx) of
  Just (Entry l e) | isExistential e -> inScope sc l
  _ -> False

-- | Whether the variable is declared as a universal within the scope.
universalIn :: (Ord v) => Scope -> v -> Context v loc -> Bool
universalIn sc v ctx = case Map.lookup v (vars ctx) of
  Just (Entry l e) | isUniversal e -> inScope sc l
  _ -> False

-- | @ordered ctx v v2@ holds when @v@ is an existential declared before @v2@'s
-- element: Γ[v^][v2^] in the paper's notation.
ordered :: (Ord v) => Context v loc -> v -> v -> Bool
ordered ctx v v2 = case Map.lookup v2 (vars ctx) of
  Just (Entry l _) -> existentialIn (Before l) v ctx
  Nothing -> False

-- | The element declaring the variable, if any.
lookupElement :: (Ord v) => v -> Context v loc -> Maybe (Element v loc)
lookupElement v ctx = entryElement <$> Map.lookup v (vars ctx)

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

-- | The existentials a segment declares, solved or not.
existentialVars :: (Ord v) => CtxSegment v loc -> Set v
existentialVars es = Set.fromList [variable e | e <- es, isExistential e]

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
  = -- | Its variable is already declared by an element of the context.
    DuplicateVariable
  | -- | Its type mentions an existential not declared before it.
    UndeclaredExistential
  | -- | Its type mentions a universal not declared before it.
    UndeclaredUniversal
  | -- | A replacement for a variable's element does not bind the variable
    -- again, which would make every later element that mentions it ill-formed.
    NotRedeclared
  deriving (Eq, Show)

-- | A refused update: the context as it stood (for 'replace', the elements
-- before the replaced one plus the replacements accepted so far), the element
-- refused, and why.
data Rejected v loc = Rejected (Context v loc) (Element v loc) BadContext

-- Checks an element against the context at the given scope: its variable must
-- not already be declared, and its type's variables must be declared within the
-- scope, each as the kind of variable it occurs as.
check :: (Ord v) => Scope -> Element v loc -> Context v loc -> Maybe BadContext
check sc e ctx
  | Just v <- declares e, Map.member v (vars ctx) = Just DuplicateVariable
  | Refined v _ <- e, not (universalIn sc v ctx) = Just UndeclaredUniversal
  | otherwise = listToMaybe (mapMaybe undeclared (Set.toList (freeVarsOf e)))
  where
    undeclared (TypeVar.Existential _ v)
      | existentialIn sc v ctx = Nothing
      | otherwise = Just UndeclaredExistential
    undeclared (TypeVar.Universal v)
      | universalIn sc v ctx = Nothing
      | otherwise = Just UndeclaredUniversal

-- Enters an element that is in the sequence into the maps.
record :: (Ord v) => Entry v loc -> Context v loc -> Context v loc
record en@(Entry l e) ctx = case e of
  Refined v t -> ctx {refinements = Map.insertWith (++) v [(l, t)] (refinements ctx)}
  Ann v loc t -> ctx {vars = Map.insert v en (vars ctx), anns = Map.insert v (loc, t) (anns ctx)}
  _ -> ctx {vars = Map.insert (variable e) en (vars ctx)}

-- Removes an element that has left the sequence from the maps. Only the entry
-- made for this element (by position) is removed, so an element that has already
-- been superseded in the maps is left alone.
forget :: (Ord v) => Entry v loc -> Context v loc -> Context v loc
forget (Entry l e) ctx = case e of
  Refined v _ ->
    ctx {refinements = Map.update (nonEmpty . List.filter ((/= l) . fst)) v (refinements ctx)}
  _
    | Just en <- Map.lookup v (vars ctx),
      entryPosition en == l ->
        ctx
          { vars = Map.delete v (vars ctx),
            anns = case e of
              Ann {} -> Map.delete v (anns ctx)
              _ -> anns ctx
          }
    | otherwise -> ctx
  where
    v = variable e
    nonEmpty [] = Nothing
    nonEmpty xs = Just xs

-- Appends an element with a fresh position, without checking it.
append :: (Ord v) => CtxPosition -> Element v loc -> Context v loc -> Context v loc
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

-- The position of the element with the given position: the first element at
-- which the running maximum position reaches it.
atPosition :: CtxPosition -> Measure -> Measure -> Bool
atPosition l pre _ = maxPosition pre >= l

-- | Replaces the element declaring a variable with a run of elements, which
-- must bind the variable again (typically last, after fresh existentials its
-- solution mentions). Each replacement is checked in the scope of the
-- replaced element. A variable that is not declared leaves the context as it is.
replace :: (Ord v) => v -> CtxSegment v loc -> Context v loc -> Either (Rejected v loc) (Context v loc)
replace v es ctx0 = case Map.lookup v (vars ctx0) of
  Nothing -> Right ctx0
  Just old@(Entry l oldE)
    | not (any ((== Just v) . declares) es) -> Left (Rejected ctx0 oldE NotRedeclared)
    | Position left _ right <- search (atPosition l) (entries ctx0) ->
        let go ctx [] = Right ctx {entries = entries ctx >< right}
            go ctx (e : rest) = case check (Before l) e ctx of
              Just bad -> Left (Rejected ctx e bad)
              Nothing -> go (append (insertedBefore l (counter ctx)) e ctx) rest
         in go (forget old ctx0) {entries = left} es
    | otherwise -> error "Context.Structure.replace: a declared variable's position is not in the sequence"

-- | Removes the element declaring the variable and everything after it. Returns
-- the context that remains and the removed elements after the declaring one.
retract :: (Ord v) => v -> Context v loc -> Maybe (Context v loc, CtxSegment v loc)
retract v ctx = case Map.lookup v (vars ctx) of
  Nothing -> Nothing
  Just (Entry l _) -> case search (atPosition l) (entries ctx) of
    Position left en right ->
      let gone = en : FO.toList right
          ctx' = List.foldl' (flip forget) ctx gone
       in Just (ctx' {entries = left}, map entryElement (FO.toList right))
    _ -> error "Context.Structure.retract: a declared variable's position is not in the sequence"

-- | Marks the given existentials, where they are declared with a plain blank, as
-- retained ('B.Retain'), so that later generalization leaves them in place.
markRetained :: (Ord v) => Set v -> Context v loc -> Context v loc
markRetained keep ctx0 = FO.foldl' mark ctx0 keep
  where
    mark ctx v = case Map.lookup v (vars ctx) of
      Just (Entry l e) | Just e' <- retain e -> replaceAt l e' ctx
      _ -> ctx
    retain (Var (TypeVar.Existential B.Blank v)) = Just (Var (TypeVar.Existential B.Retain v))
    retain (Solved B.Blank v t) = Just (Solved B.Retain v t)
    retain _ = Nothing

-- Replaces the element at a position with another declaring the same variable.
replaceAt :: (Ord v) => CtxPosition -> Element v loc -> Context v loc -> Context v loc
replaceAt l e ctx = case search (atPosition l) (entries ctx) of
  Position left _ right -> record en ctx {entries = left >< (en <| right)}
  _ -> ctx
  where
    en = Entry l e

-- Helpers ---------------------------------------------------------------------

isExistential :: Element v loc -> Bool
isExistential (Var (TypeVar.Existential _ _)) = True
isExistential (Solved {}) = True
isExistential _ = False

isUniversal :: Element v loc -> Bool
isUniversal (Var (TypeVar.Universal _)) = True
isUniversal _ = False

-- The variables occurring in an element's type, which must all be declared
-- before the element.
freeVarsOf :: (Ord v) => Element v loc -> Set (TypeVar v loc)
freeVarsOf (Solved _ _ t) = Type.freeVars (Type.getPolytype t)
freeVarsOf (Ann _ _ t) = Type.freeVars t
freeVarsOf (Refined _ t) = Type.freeVars t
freeVarsOf _ = Set.empty

inScope :: Scope -> CtxPosition -> Bool
inScope AtEnd _ = True
inScope (Before l) l' = l' < l

-- Instances -------------------------------------------------------------------

instance Semigroup Measure where
  Measure l i <> Measure l' i' = Measure (max l l') (i + i')

instance Monoid Measure where
  mempty = Measure (CtxPosition []) 0

instance Measured Measure (Entry v loc) where
  measure (Entry l e) = Measure l (case e of Inconsistent _ -> 1; _ -> 0)

instance (Var v) => Eq (Element v loc) where
  Var v == Var v2 = v == v2
  Solved _ v t == Solved _ v2 t2 = v == v2 && t == t2
  Ann v _loc t == Ann v2 _loc2 t2 = v == v2 && t == t2
  Refined v t == Refined v2 t2 = v == v2 && t == t2
  Inconsistent v == Inconsistent v2 = v == v2
  Marker v == Marker v2 = v == v2
  _ == _ = False

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
