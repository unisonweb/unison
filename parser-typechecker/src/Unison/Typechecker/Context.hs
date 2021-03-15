{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Typechecker.Context
  ( synthesizeClosed
  , ErrorNote(..)
  , CompilerBug (..)
  , InfoNote(..)
  , Cause(..)
  , Context(..)
  , ActualArgCount
  , ExpectedArgCount
  , ConstructorId
  , Element(..)
  , PathElement(..)
  , Term
  , Type
  , TypeVar
  , Result(..)
  , errorTerms
  , innermostErrorTerm
  , lookupAnn
  , lookupSolved
  , apply
  , isEqual
  , isSubtype
  , isRedundant
  , Suggestion(..)
  , SuggestionMatch(..)
  , isExact
  , typeErrors
  , infoNotes
  , Unknown(..)
  )
where

import Unison.Prelude

import           Control.Lens                   (over, _2)
import qualified Control.Monad.Fail            as MonadFail
import           Control.Monad.Reader.Class
import           Control.Monad.State            ( get
                                                , put
                                                , StateT
                                                , runStateT
                                                , evalState
                                                )
import           Data.Bifunctor                 ( first
                                                , second
                                                )
import qualified Data.Foldable                 as Foldable
import           Data.Functor.Compose           ( Compose(..) )
import           Data.List
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.Map                      as Map
import qualified Data.Sequence                 as Seq
import           Data.Sequence.NonEmpty         ( NESeq )
import qualified Data.Sequence.NonEmpty        as NESeq
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import qualified Unison.ABT                    as ABT
import qualified Unison.Blank                  as B
import           Unison.DataDeclaration         ( DataDeclaration
                                                , EffectDeclaration
                                                )
import qualified Unison.DataDeclaration        as DD
import           Unison.Pattern                 ( Pattern )
import qualified Unison.Pattern                as Pattern
import           Unison.Reference               ( Reference )
import           Unison.Referent                ( Referent )
import qualified Unison.Term                   as Term
import qualified Unison.Type                   as Type
import           Unison.Typechecker.Components  ( minimize' )
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.Typechecker.TypeVar    as TypeVar
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var
import qualified Unison.TypePrinter            as TP

type TypeVar v loc = TypeVar.TypeVar (B.Blank loc) v
type Type v loc = Type.Type (TypeVar v loc) loc
type Term v loc = Term.Term' (TypeVar v loc) v loc
type Monotype v loc = Type.Monotype (TypeVar v loc) loc
type RedundantTypeAnnotation = Bool

pattern Universal v = Var (TypeVar.Universal v)
pattern Existential b v = Var (TypeVar.Existential b v)

existential :: v -> Element v loc
existential = Existential B.Blank

existential' :: Ord v => a -> B.Blank loc -> v -> Type.Type (TypeVar v loc) a
existential' a blank v = ABT.annotatedVar a (TypeVar.Existential blank v)

existentialp :: Ord v => a -> v -> Type v a
existentialp a = existential' a B.Blank

universal' :: Ord v => a -> v -> Type.Type (TypeVar v loc) a
universal' a v = ABT.annotatedVar a (TypeVar.Universal v)

-- | Elements of an ordered algorithmic context
data Element v loc
  = Var (TypeVar v loc)                    -- A variable declaration
  | Solved (B.Blank loc) v (Monotype v loc)  -- `v` is solved to some monotype
  | Ann v (Type v loc)                     -- `v` has type `a`, maybe quantified
  | Marker v                               -- used for scoping

instance (Ord loc, Var v) => Eq (Element v loc) where
  Var v == Var v2                = v == v2
  Solved _ v t == Solved _ v2 t2 = v == v2 && t == t2
  Ann v t == Ann v2 t2           = v == v2 && t == t2
  Marker v == Marker v2          = v == v2
  _ == _ = False

data Env v loc = Env { freshId :: Word64, ctx :: Context v loc }

type DataDeclarations v loc = Map Reference (DataDeclaration v loc)
type EffectDeclarations v loc = Map Reference (EffectDeclaration v loc)

data Result v loc a = Success (Seq (InfoNote v loc)) a
                    | TypeError (NESeq (ErrorNote v loc)) (Seq (InfoNote v loc))
                    | CompilerBug (CompilerBug v loc)
                                  (Seq (ErrorNote v loc)) -- type errors before hitting the bug
                                  (Seq (InfoNote v loc))  -- info notes before hitting the bug
                    deriving (Functor)

instance Applicative (Result v loc) where
  pure = Success mempty
  CompilerBug bug es is <*> _                       = CompilerBug bug es is
  r                     <*> CompilerBug bug es' is' = CompilerBug bug (typeErrors r <> es') (infoNotes r <> is')
  TypeError es is       <*> r'                      = TypeError (es NESeq.|>< (typeErrors r')) (is <> infoNotes r')
  Success is _          <*> TypeError es' is'       = TypeError es' (is <> is')
  Success is f          <*> Success is' a           = Success (is <> is') (f a)

instance Monad (Result v loc) where
  s@(Success _ a)       >>= f = s *> f a
  TypeError es is       >>= _ = TypeError es is
  CompilerBug bug es is >>= _ = CompilerBug bug es is

btw' :: InfoNote v loc -> Result v loc ()
btw' note = Success (Seq.singleton note) ()

typeError :: Cause v loc -> Result v loc a
typeError cause = TypeError (pure $ ErrorNote cause mempty) mempty

compilerBug :: CompilerBug v loc -> Result v loc a
compilerBug bug = CompilerBug bug mempty mempty

typeErrors :: Result v loc a -> Seq (ErrorNote v loc)
typeErrors = \case
  TypeError es _     -> NESeq.toSeq es
  CompilerBug _ es _ -> es
  Success _ _        -> mempty

infoNotes :: Result v loc a -> Seq (InfoNote v loc)
infoNotes = \case
  TypeError _ is     -> is
  CompilerBug _ _ is -> is
  Success is _       -> is

mapErrors :: (ErrorNote v loc -> ErrorNote v loc) -> Result v loc a -> Result v loc a
mapErrors f r = case r of
  TypeError es is -> TypeError (f <$> es) is
  CompilerBug bug es is -> CompilerBug bug (f <$> es) is
  s@(Success _ _) -> s

newtype MT v loc f a = MT {
  runM :: MEnv v loc -> f (a, Env v loc)
}

-- | Typechecking monad
type M v loc = MT v loc (Result v loc)

-- | Typechecking computation that, unless it crashes
-- with a compiler bug, always produces a value.
type TotalM v loc = MT v loc (Either (CompilerBug v loc))

liftResult :: Result v loc a -> M v loc a
liftResult r = MT (\m -> (, env m) <$> r)

liftTotalM :: TotalM v loc a -> M v loc a
liftTotalM (MT m) = MT $ \menv -> case m menv of
  Left bug -> CompilerBug bug mempty mempty
  Right a  -> Success mempty a

-- errorNote :: Cause v loc -> M v loc ()
-- errorNote = liftResult . errorNote

btw :: InfoNote v loc -> M v loc ()
btw = liftResult . btw'

modEnv :: (Env v loc -> Env v loc) -> M v loc ()
modEnv f = modEnv' $ ((), ) . f

modEnv' :: (Env v loc -> (a, Env v loc)) -> M v loc a
modEnv' f = MT (\menv -> pure . f $ env menv)

data Unknown = Data | Effect deriving Show

data CompilerBug v loc
  = UnknownDecl Unknown Reference (Map Reference (DataDeclaration v loc))
  | UnknownConstructor Unknown Reference Int (DataDeclaration v loc)
  | UndeclaredTermVariable v (Context v loc)
  | RetractFailure (Element v loc) (Context v loc)
  | EmptyLetRec (Term v loc) -- the body of the empty let rec
  | PatternMatchFailure
  | EffectConstructorHadMultipleEffects (Type v loc)
  | FreeVarsInTypeAnnotation (Set (TypeVar v loc))
  | UnannotatedReference Reference
  | MalformedPattern (Pattern loc)
  | UnknownTermReference Reference
  | UnknownExistentialVariable v (Context v loc)
  -- `IllegalContextExtension ctx elem msg`
  --     extending `ctx` with `elem` would make `ctx` ill-formed, as explained by `msg`
  | IllegalContextExtension (Context v loc) (Element v loc) String
  | OtherBug String
  deriving Show

data PathElement v loc
  = InSynthesize (Term v loc)
  | InSubtype (Type v loc) (Type v loc)
  | InCheck (Term v loc) (Type v loc)
  | InInstantiateL v (Type v loc)
  | InInstantiateR (Type v loc) v
  | InSynthesizeApp (Type v loc) (Term v loc) Int
  | InFunctionCall [v] (Term v loc) (Type v loc) [Term v loc]
  | InAndApp
  | InOrApp
  | InIfCond
  | InIfBody loc -- location of `then` expression
  | InVectorApp loc -- location of 1st vector element
  | InMatch loc -- location of 1st case body
  | InMatchGuard
  | InMatchBody
  deriving Show

type ExpectedArgCount = Int
type ActualArgCount = Int
type ConstructorId = Int

data SuggestionMatch = Exact | WrongType | WrongName
  deriving (Ord, Eq, Show)

data Suggestion v loc =
  Suggestion { suggestionName :: Text
             , suggestionType :: Type v loc
             , suggestionReplacement :: Either v Referent
             , suggestionMatch :: SuggestionMatch
             }
  deriving (Eq, Show)

isExact :: Suggestion v loc -> Bool
isExact Suggestion {..} = suggestionMatch == Exact

data ErrorNote v loc = ErrorNote {
  cause :: Cause v loc,
  path :: Seq (PathElement v loc)
} deriving Show

-- `Decision v loc fqn` is a decision to replace the name v at location loc
-- with the fully qualified name fqn.
data InfoNote v loc
  = SolvedBlank (B.Recorded loc) v (Type v loc)
  | Decision v loc (Term.Term v loc)
  | TopLevelComponent [(v, Type.Type v loc, RedundantTypeAnnotation)]
  deriving (Show)

topLevelComponent :: Var v => [(v, Type.Type v loc, RedundantTypeAnnotation)] -> InfoNote v loc
topLevelComponent = TopLevelComponent . fmap (over _2 removeSyntheticTypeVars)

-- The typechecker generates synthetic type variables as part of type inference.
-- This function converts these synthetic type variables to regular named type
-- variables guaranteed to not collide with any other type variables.
--
-- It also attempts to pick "nice" type variable names, based on what sort of
-- synthetic type variable it is and what type variable names are not already
-- being used.
removeSyntheticTypeVars :: Var v => Type.Type v loc -> Type.Type v loc
removeSyntheticTypeVars typ =
  flip evalState (Set.fromList (ABT.allVars typ), mempty) $ ABT.vmapM go typ
  where
  go v | Var.User _ <- Var.typeOf v = pure v -- user-provided type variables left alone
       | otherwise                  = do
         (used,curMappings) <- get
         case Map.lookup v curMappings of
           Nothing -> do
             let v' = pickName used (Var.typeOf v)
             put (Set.insert v' used, Map.insert v v' curMappings)
             pure v'
           Just v' -> pure v'
  pickName used vt = ABT.freshIn used . Var.named $ case vt of
    -- for each type of variable, we have some preferred variable
    -- names that we like, if they aren't already being used
    Var.Inference Var.Ability -> pick ["g","h","m","p"]
    Var.Inference Var.Input -> pick ["a","b","c","i","j"]
    Var.Inference Var.Output -> pick ["r","o"]
    Var.Inference Var.Other -> pick ["t","u","w"]
    Var.Inference Var.TypeConstructor -> pick ["f","k","d"]
    Var.Inference Var.TypeConstructorArg -> pick ["v","w","y"]
    Var.User n -> n
    _ -> defaultName
    where
      used1CharVars = Set.fromList $ ABT.allVars typ >>= \v ->
        case Text.unpack (Var.name . Var.reset $ v) of
          [ch] -> [Text.singleton ch]
          _ -> []
      pick ns@(n:_) = fromMaybe n $ find (`Set.notMember` used1CharVars) ns
      pick [] = error "impossible"
      defaultName = "x"

data Cause v loc
  = TypeMismatch (Context v loc)
  | IllFormedType (Context v loc)
  | UnknownSymbol loc v
  | UnknownTerm loc v [Suggestion v loc] (Type v loc)
  | AbilityCheckFailure [Type v loc] [Type v loc] (Context v loc) -- ambient, requested
  | EffectConstructorWrongArgCount ExpectedArgCount ActualArgCount Reference ConstructorId
  | MalformedEffectBind (Type v loc) (Type v loc) [Type v loc] -- type of ctor, type of ctor result
  -- Type of ctor, number of arguments we got
  | PatternArityMismatch loc (Type v loc) Int
  -- A variable is defined twice in the same block
  | DuplicateDefinitions (NonEmpty (v, [loc]))
  -- A let rec where things that aren't guarded cyclicly depend on each other
  | UnguardedLetRecCycle [v] [(v, Term v loc)]
  | ConcatPatternWithoutConstantLength loc (Type v loc)
  | HandlerOfUnexpectedType loc (Type v loc)
  | DataEffectMismatch Unknown Reference (DataDeclaration v loc)
  deriving Show

errorTerms :: ErrorNote v loc -> [Term v loc]
errorTerms n = Foldable.toList (path n) >>= \e -> case e of
  InCheck e _           -> [e]
  InSynthesizeApp _ e _ -> [e]
  InSynthesize e        -> [e]
  _                     -> [ ]

innermostErrorTerm :: ErrorNote v loc -> Maybe (Term v loc)
innermostErrorTerm n = listToMaybe $ errorTerms n

solveBlank :: B.Recorded loc -> v -> Type v loc -> M v loc ()
solveBlank blank v typ = btw $ SolvedBlank blank v typ

-- Add `p` onto the end of the `path` of this `ErrorNote`
scope' :: PathElement v loc -> ErrorNote v loc -> ErrorNote v loc
scope' p (ErrorNote cause path) = ErrorNote cause (path `mappend` pure p)

-- Add `p` onto the end of the `path` of any `ErrorNote`s emitted by the action
scope :: PathElement v loc -> M v loc a -> M v loc a
scope p (MT m) = MT (mapErrors (scope' p) . m)

-- | The typechecking environment
data MEnv v loc = MEnv {
  env :: Env v loc,                    -- The typechecking state
  abilities :: [Type v loc],           -- Allowed ambient abilities
  dataDecls :: DataDeclarations v loc, -- Data declarations in scope
  effectDecls :: EffectDeclarations v loc, -- Effect declarations in scope
  -- Types for which ability check should be skipped.
  -- See abilityCheck function for how this is used.
  skipAbilityCheck :: [Type v loc]
}

newtype Context v loc = Context [(Element v loc, Info v loc)]

data Info v loc =
  Info { existentialVars :: Set v -- set of existentials seen so far
       , solvedExistentials :: Map v (Monotype v loc) -- `v` is solved to some monotype
       , universalVars :: Set v -- set of universals seen so far
       , termVarAnnotations :: Map v (Type v loc)
       , allVars :: Set v -- all variables seen so far
       , previouslyTypecheckedVars :: Set v -- term vars already typechecked
       }

-- | The empty context
context0 :: Context v loc
context0 = Context []

-- | Focuses on the first element in the list that satisfies the predicate.
-- Returns `(prefix, focusedElem, suffix)`, where `prefix` is in reverse order.
focusAt :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
focusAt p xs = go [] xs where
  go _ [] = Nothing
  go l (h:t) = if p h then Just (l, h, t) else go (h:l) t

-- | Delete from the end of this context up to and including
-- the given `Element`. Returns `Nothing` if the element is not found.
retract0 :: (Var v, Ord loc) => Element v loc -> Context v loc -> Maybe (Context v loc, [Element v loc])
retract0 e (Context ctx) = case focusAt (\(e',_) -> e' == e) ctx of
  Just (discarded, _, remaining) ->
    -- note: no need to recompute used variables; any suffix of the
    -- context snoc list is also a valid context
    Just (Context remaining, map fst discarded)
  Nothing -> Nothing

-- | Adds a marker to the end of the context, runs the `body` and then discards
-- from the end of the context up to and including the marker. Returns the result
-- of `body` and the discarded context (not including the marker), respectively.
-- Freshened `markerHint` is used to create the marker.
markThenRetract :: (Var v, Ord loc) => v -> M v loc a -> M v loc (a, [Element v loc])
markThenRetract markerHint body = do
  v <- freshenVar markerHint
  extendContext (Marker v)
  a <- body
  (a,) <$> doRetract (Marker v)
 where
  doRetract :: (Var v, Ord loc) => Element v loc -> M v loc [Element v loc]
  doRetract e = do
    ctx <- getContext
    case retract0 e ctx of
      Nothing             -> compilerCrash (RetractFailure e ctx)
      Just (t, discarded) -> do
        let solved =
              [ (b, v, inst $ Type.getPolytype sa)
              | Solved (B.Recorded b) v sa <- discarded
              ]
            unsolved =
              [ (b, v, inst $ existential' (B.loc b) b' v)
              | Existential b'@(B.Recorded b) v <- discarded
              ]
            go (b, v, sa) = solveBlank b v sa
            inst = apply ctx
        Foldable.traverse_ go (solved ++ unsolved)
        setContext t
        pure discarded

markThenRetract0 :: (Var v, Ord loc) => v -> M v loc a -> M v loc ()
markThenRetract0 markerHint body = () <$ markThenRetract markerHint body

-- unsolved' :: Context v loc -> [(B.Blank loc, v)]
-- unsolved' (Context ctx) = [(b,v) | (Existential b v, _) <- ctx]

replace :: (Var v, Ord loc) => Element v loc -> [Element v loc] -> Context v loc -> M v loc (Context v loc)
replace e focus ctx =
  case breakAt e ctx of
    Just (l, _, r) -> l `extendN` (focus <> r)
    Nothing -> pure ctx

breakAt :: (Var v, Ord loc)
        => Element v loc
        -> Context v loc
        -> Maybe (Context v loc, Element v loc, [Element v loc])
breakAt m (Context xs) =
  case focusAt (\(e,_) -> e === m) xs of
    Just (r, m, l) ->
      -- l is a suffix of xs and is already a valid context
      Just (Context l, fst m, map fst r)
    Nothing -> Nothing
  where
    Existential _ v === Existential _ v2 | v == v2 = True
    Universal v     === Universal v2 | v == v2 = True
    Marker v        === Marker v2 | v == v2 = True
    _ === _ = False


-- | ordered Γ α β = True <=> Γ[α^][β^]
ordered :: (Var v, Ord loc) => Context v loc -> v -> v -> Bool
ordered ctx v v2 = Set.member v (existentials (retract' (existential v2) ctx))
 where
  -- Like `retract`, but returns the empty context if retracting would remove
  -- all elements.
  retract'
    :: (Var v, Ord loc) => Element v loc -> Context v loc -> Context v loc
  retract' e ctx = maybe context0 fst $ retract0 e ctx

-- env0 :: Env v loc
-- env0 = Env 0 context0

debugEnabled :: Bool
debugEnabled = False

debugPatternsEnabled :: Bool
debugPatternsEnabled = False

_logContext :: (Ord loc, Var v) => String -> M v loc ()
_logContext msg = when debugEnabled $ do
  ctx <- getContext
  let !_ = trace ("\n"++msg ++ ": " ++ show ctx) ()
  setContext ctx

usedVars :: Ord v => Context v loc -> Set v
usedVars = allVars . info

fromMEnv :: (MEnv v loc -> a) -> M v loc a
fromMEnv f = f <$> ask

getContext :: M v loc (Context v loc)
getContext = fromMEnv $ ctx . env

setContext :: Context v loc -> M v loc ()
setContext ctx = modEnv (\e -> e { ctx = ctx })

modifyContext :: (Context v loc -> M v loc (Context v loc)) -> M v loc ()
modifyContext f = do
  c <- getContext
  c <- f c
  setContext c

appendContext :: (Var v, Ord loc) => [Element v loc] -> M v loc ()
appendContext = traverse_ extendContext

extendContext :: Var v => Element v loc -> M v loc ()
extendContext e = isReserved (varOf e) >>= \case
  True -> modifyContext (extend e)
  False -> getContext >>= \ctx -> compilerCrash $
    IllegalContextExtension ctx e $
      "Extending context with a variable that is not reserved by the typechecking environment." <>
      " That means `freshenVar` is allowed to return it as a fresh variable, which would be wrong."

replaceContext :: (Var v, Ord loc) => Element v loc -> [Element v loc] -> M v loc ()
replaceContext elem replacement =
  fromMEnv (\menv -> find (not . (`isReservedIn` env menv) . varOf) replacement) >>= \case
    Nothing -> modifyContext (replace elem replacement)
    Just e -> getContext >>= \ctx -> compilerCrash $
      IllegalContextExtension ctx e $
        "Extending context with a variable that is not reserved by the typechecking environment." <>
        " That means `freshenVar` is allowed to return it as a fresh variable, which would be wrong."

varOf :: Element v loc -> v
varOf (Var tv) = TypeVar.underlying tv
varOf (Solved _ v _) = v
varOf (Ann v _) = v
varOf (Marker v) = v

isReserved :: Var v => v -> M v loc Bool
isReserved v = fromMEnv $ (v `isReservedIn`) . env

isReservedIn :: Var v => v -> Env v loc -> Bool
isReservedIn v e = freshId e > Var.freshId v

universals :: Ord v => Context v loc -> Set v
universals = universalVars . info

existentials :: Ord v => Context v loc -> Set v
existentials = existentialVars . info

-- | "Reserves" the given variables in this typechecking environment,
-- i.e. ensures that they won't be returned from `freshenVar` as fresh.
reserveAll :: (Var v, Foldable t) => t v -> M v loc ()
reserveAll vs =
  let maxId = foldr (max . Var.freshId) 0 vs
  in modEnv (\e -> e { freshId = freshId e `max` maxId + 1})

freshenVar :: Var v => v -> M v0 loc v
freshenVar v = modEnv'
  (\e ->
    let id = freshId e in (Var.freshenId id v, e { freshId = freshId e + 1 })
  )

freshenTypeVar :: Var v => TypeVar v loc -> M v loc v
freshenTypeVar v = modEnv'
  (\e ->
    let id = freshId e
    in  (Var.freshenId id (TypeVar.underlying v), e { freshId = id + 1 })
  )

isClosed :: Var v => Term v loc -> M v loc Bool
isClosed e = Set.null <$> freeVars e

freeVars :: Var v => Term v loc -> M v loc (Set v)
freeVars e = do
  ctx <- getContext
  pure $ ABT.freeVars e `Set.difference` previouslyTypecheckedVars (info ctx)

-- todo: do we want this to return a location for the aspect of the type that was not well formed
-- todo: or maybe a note / list of notes, or an M
-- | Check that the type is well formed wrt the given `Context`, see Figure 7 of paper
wellformedType :: Var v => Context v loc -> Type v loc -> Bool
wellformedType c t = case t of
  Type.Var' (TypeVar.Existential _ v) -> Set.member v (existentials c)
  Type.Var' (TypeVar.Universal v) -> Set.member v (universals c)
  Type.Ref' _ -> True
  Type.Arrow' i o -> wellformedType c i && wellformedType c o
  Type.Ann' t' _ -> wellformedType c t'
  Type.App' x y -> wellformedType c x && wellformedType c y
  Type.Effect1' e a -> wellformedType c e && wellformedType c a
  Type.Effects' es -> all (wellformedType c) es
  Type.IntroOuterNamed' _ t -> wellformedType c t
  Type.Forall' t' ->
    let (v,ctx2) = extendUniversal c
    in wellformedType ctx2 (ABT.bind t' (universal' (ABT.annotation t) v))
  _ -> error $ "Match failure in wellformedType: " ++ show t
  where
  -- | Extend this `Context` with a single variable, guaranteed fresh
  extendUniversal ctx =
    let v          = Var.freshIn (usedVars ctx) (Var.named "var")
        Right ctx' = extend' (Universal v) ctx
    in (v, ctx')

-- | Return the `Info` associated with the last element of the context, or the zero `Info`.
info :: Ord v => Context v loc -> Info v loc
info (Context []) = Info mempty mempty mempty mempty mempty mempty
info (Context ((_,i):_)) = i

-- | Add an element onto the end of this `Context`. Takes `O(log N)` time,
-- including updates to the accumulated `Info` value.
-- Fail if the new context is not well formed (see Figure 7 of paper).
extend' :: Var v => Element v loc -> Context v loc -> Either (CompilerBug v loc) (Context v loc)
extend' e c@(Context ctx) = Context . (:ctx) . (e,) <$> i' where
  Info es ses us uas vs pvs = info c
  -- see figure 7
  i' = case e of
    Var v -> case v of
      -- UvarCtx - ensure no duplicates
      TypeVar.Universal v -> if Set.notMember v vs
        then pure $ Info es ses (Set.insert v us) uas (Set.insert v vs) pvs
        else crash $ "variable " <> show v <> " already defined in the context"
      -- EvarCtx - ensure no duplicates, and that this existential is not solved earlier in context
      TypeVar.Existential _ v -> if Set.notMember v vs
        then pure $ Info (Set.insert v es) ses us uas (Set.insert v vs) pvs
        else crash $ "variable " <> show v <> " already defined in the context"
    -- SolvedEvarCtx - ensure `v` is fresh, and the solution is well-formed wrt the context
    Solved _ v sa@(Type.getPolytype -> t)
      | Set.member v vs          -> crash $ "variable " <> show v <> " already defined in the context"
      | not (wellformedType c t) -> crash $ "type " <> show t <> " is not well-formed wrt the context"
      | otherwise                -> pure $
          Info (Set.insert v es) (Map.insert v sa ses) us uas (Set.insert v vs) pvs
    -- VarCtx - ensure `v` is fresh, and annotation is well-formed wrt the context
    Ann v t
      | Set.member v vs          -> crash $ "variable " <> show v <> " already defined in the context"
      | not (wellformedType c t) -> crash $ "type " <> show t <> " is not well-formed wrt the context"
      | otherwise                -> pure $
          Info es ses us (Map.insert v t uas) (Set.insert v vs)
                ((if Set.null (Type.freeVars t) then Set.insert v else id) pvs)
    -- MarkerCtx - note that since a Marker is always the first mention of a variable, suffices to
    -- just check that `v` is not previously mentioned
    Marker v -> if Set.notMember v vs
      then pure $ Info es ses us uas (Set.insert v vs) pvs
      else crash $ "marker variable " <> show v <> " already defined in the context"
  crash reason = Left $ IllegalContextExtension c e reason

extend :: Var v => Element v loc -> Context v loc -> M v loc (Context v loc)
extend e c = either compilerCrash pure $ extend' e c

-- | Add the given elements onto the end of the given `Context`.
-- Fail if the new context is not well-formed.
extendN :: Var v => Context v loc -> [Element v loc] -> M v loc (Context v loc)
extendN ctx es = foldM (flip extend) ctx es

-- | doesn't combine notes
orElse :: M v loc a -> M v loc a -> M v loc a
orElse m1 m2 = MT go where
  go menv = runM m1 menv <|> runM m2 menv
  s@(Success _ _)         <|> _ = s
  TypeError _ _           <|> r = r
  CompilerBug _ _ _       <|> r = r -- swallowing bugs for now: when checking whether a type annotation
                                    -- is redundant, typechecking without that annotation might result in
                                    -- a CompilerBug that we want `orElse` to recover from

-- getMaybe :: Result v loc a -> Result v loc (Maybe a)
-- getMaybe = hoistMaybe Just

-- hoistMaybe :: (Maybe a -> Maybe b) -> Result v loc a -> Result v loc b
-- hoistMaybe f (Result es is a) = Result es is (f a)

getDataDeclarations :: M v loc (DataDeclarations v loc)
getDataDeclarations = fromMEnv dataDecls

getEffectDeclarations :: M v loc (EffectDeclarations v loc)
getEffectDeclarations = fromMEnv effectDecls

getAbilities :: M v loc [Type v loc]
getAbilities = fromMEnv abilities

shouldPerformAbilityCheck :: (Ord loc, Var v) => Type v loc -> M v loc Bool
shouldPerformAbilityCheck t = do
  skip <- fromMEnv skipAbilityCheck
  skip <- traverse applyM skip
  t <- applyM t
  pure $ all (/= t) skip

compilerCrash :: CompilerBug v loc -> M v loc a
compilerCrash bug = liftResult $ compilerBug bug

failWith :: Cause v loc -> M v loc a
failWith cause = liftResult $ typeError cause

compilerCrashResult :: CompilerBug v loc -> Result v loc a
compilerCrashResult bug = CompilerBug bug mempty mempty

getDataDeclaration :: Reference -> M v loc (DataDeclaration v loc)
getDataDeclaration r = do
  ddecls <- getDataDeclarations
  case Map.lookup r ddecls of
    Nothing -> getEffectDeclarations >>= \edecls ->
      case Map.lookup r edecls of
        Nothing -> compilerCrash (UnknownDecl Data r ddecls)
        Just decl ->
          liftResult . typeError
            $ DataEffectMismatch Effect r (DD.toDataDecl decl)
    Just decl -> pure decl

getEffectDeclaration :: Reference -> M v loc (EffectDeclaration v loc)
getEffectDeclaration r = do
  edecls <- getEffectDeclarations
  case Map.lookup r edecls of
    Nothing -> getDataDeclarations >>= \ddecls ->
      case Map.lookup r ddecls of
        Nothing -> compilerCrash
          $ UnknownDecl Effect r (DD.toDataDecl <$> edecls)
        Just decl ->
          liftResult . typeError $ DataEffectMismatch Data r decl
    Just decl -> pure decl

getDataConstructorType :: (Var v, Ord loc) => Reference -> Int -> M v loc (Type v loc)
getDataConstructorType = getConstructorType' Data getDataDeclaration

getEffectConstructorType :: (Var v, Ord loc) => Reference -> Int -> M v loc (Type v loc)
getEffectConstructorType = getConstructorType' Effect go where
  go r = DD.toDataDecl <$> getEffectDeclaration r

-- Encountered an unknown constructor in the typechecker; unknown constructors
-- should have been detected earlier though.
getConstructorType' :: Var v
                    => Unknown
                    -> (Reference -> M v loc (DataDeclaration v loc))
                    -> Reference
                    -> Int
                    -> M v loc (Type v loc)
getConstructorType' kind get r cid = do
  decl <- get r
  case drop cid (DD.constructors decl) of
    [] -> compilerCrash $ UnknownConstructor kind r cid decl
    (_v, typ) : _ -> pure $ ABT.vmap TypeVar.Universal typ

extendUniversal :: (Var v) => v -> M v loc v
extendUniversal v = do
  v' <- freshenVar v
  extendContext (Universal v')
  pure v'

extendExistential :: (Var v) => v -> M v loc v
extendExistential v = do
  v' <- freshenVar v
  extendContext (Existential B.Blank v')
  pure v'

extendExistentialTV :: Var v => v -> M v loc (TypeVar v loc)
extendExistentialTV v =
  TypeVar.Existential B.Blank <$> extendExistential v

notMember :: (Var v, Ord loc) => v -> Set (TypeVar v loc) -> Bool
notMember v s =
  Set.notMember (TypeVar.Universal v) s &&
  Set.notMember (TypeVar.Existential B.Blank v) s

-- | Replace any existentials with their solution in the context
apply :: (Var v, Ord loc) => Context v loc -> Type v loc -> Type v loc
apply ctx = apply' (solvedExistentials . info $ ctx)

-- | Replace any existentials with their solution in the context (given as a list of elements)
applyCtx :: (Var v, Ord loc) => [Element v loc] -> Type v loc -> Type v loc
applyCtx elems = apply' $ Map.fromList [ (v, sa) | Solved _ v sa <- elems ]

apply' :: (Var v, Ord loc) => Map v (Monotype v loc) -> Type v loc -> Type v loc
apply' _ t | Set.null (Type.freeVars t) = t
apply' solvedExistentials t = go t where
  go t = case t of
    Type.Var' (TypeVar.Universal _) -> t
    Type.Ref' _ -> t
    Type.Var' (TypeVar.Existential _ v) ->
      maybe t (\(Type.Monotype t') -> go t') (Map.lookup v solvedExistentials)
    Type.Arrow' i o -> Type.arrow a (go i) (go o)
    Type.App' x y -> Type.app a (go x) (go y)
    Type.Ann' v k -> Type.ann a (go v) k
    Type.Effect1' e t -> Type.effect1 a (go e) (go t)
    Type.Effects' es -> Type.effects a (map go es)
    Type.ForallNamed' v t' -> Type.forall a v (go t')
    Type.IntroOuterNamed' v t' -> Type.introOuter a v (go t')
    _ -> error $ "Match error in Context.apply': " ++ show t
    where a = ABT.annotation t

loc :: ABT.Term f v loc -> loc
loc = ABT.annotation

-- Prepends the provided abilities onto the existing ambient for duration of `m`
withEffects :: [Type v loc] -> M v loc a -> M v loc a
withEffects abilities' m =
  MT (\menv -> runM m (menv { abilities = abilities' ++ abilities menv }))

-- Replaces the ambient abilities with the provided for duration of `m`
withEffects0 :: [Type v loc] -> M v loc a -> M v loc a
withEffects0 abilities' m =
  MT (\menv -> runM m (menv { abilities = abilities' }))


synthesizeApps :: (Foldable f, Var v, Ord loc) => Type v loc -> f (Term v loc) -> M v loc (Type v loc)
synthesizeApps ft args =
  foldM go ft $ Foldable.toList args `zip` [1..]
  where go ft arg = do
          ctx <- getContext
          synthesizeApp (apply ctx ft) arg

-- | Synthesize the type of the given term, `arg` given that a function of
-- the given type `ft` is being applied to `arg`. Update the context in
-- the process.
-- e.g. in `(f:t) x` -- finds the type of (f x) given t and x.
synthesizeApp :: (Var v, Ord loc) => Type v loc -> (Term v loc, Int) -> M v loc (Type v loc)
synthesizeApp ft arg | debugEnabled && traceShow ("synthesizeApp"::String, ft, arg) False = undefined
synthesizeApp (Type.stripIntroOuters -> Type.Effect'' es ft) argp@(arg, argNum) =
  scope (InSynthesizeApp ft arg argNum) $ abilityCheck es >> go ft
  where
  go (Type.Forall' body) = do -- Forall1App
    v <- ABT.freshen body freshenTypeVar
    appendContext [existential v]
    let ft2 = ABT.bindInheritAnnotation body (existential' () B.Blank v)
    synthesizeApp ft2 argp
  go (Type.Arrow' i o) = do -- ->App
    let (es, _) = Type.stripEffect o
    abilityCheck es
    o <$ check arg i
  go (Type.Var' (TypeVar.Existential b a)) = do -- a^App
    [i,e,o] <- traverse freshenVar [Var.named "i", Var.named "synthsizeApp-refined-effect", Var.named "o"]
    let it = existential' (loc ft) B.Blank i
        ot = existential' (loc ft) B.Blank o
        et = existential' (loc ft) B.Blank e
        soln = Type.Monotype (Type.arrow (loc ft)
                                         it
                                         (Type.effect (loc ft) [et] ot))
        ctxMid = [existential o, existential e,
                  existential i, Solved b a soln]
    replaceContext (existential a) ctxMid
    synthesizeApp (Type.getPolytype soln) argp
  go _ = getContext >>= \ctx -> failWith $ TypeMismatch ctx
synthesizeApp _ _ = error "unpossible - Type.Effect'' pattern always succeeds"

-- For arity 3, creates the type `∀ a . a -> a -> a -> Sequence a`
-- For arity 2, creates the type `∀ a . a -> a -> Sequence a`
vectorConstructorOfArity :: (Var v, Ord loc) => loc -> Int -> M v loc (Type v loc)
vectorConstructorOfArity loc arity = do
  let elementVar = Var.named "elem"
      args = replicate arity (loc, Type.var loc elementVar)
      resultType = Type.app loc (Type.list loc) (Type.var loc elementVar)
      vt = Type.forall loc elementVar (Type.arrows args resultType)
  pure vt

generalizeAndUnTypeVar :: Var v => Type v a -> Type.Type v a
generalizeAndUnTypeVar t =
  Type.cleanup . ABT.vmap TypeVar.underlying . Type.generalize (Set.toList $ ABT.freeVars t) $ t

generalizeExistentials'
  :: Var v => Type v a -> Type v a
generalizeExistentials' t =
  Type.generalize (filter isExistential . Set.toList $ ABT.freeVars t) t
  where
  isExistential (TypeVar.Existential _ _) = True
  isExistential _ = False

noteTopLevelType
  :: (Ord loc, Var v)
  => ABT.Subst f v a
  -> Term v loc
  -> Type v loc
  -> M v loc ()
noteTopLevelType e binding typ = case binding of
  Term.Ann' strippedBinding _ -> do
    inferred <- (Just <$> synthesize strippedBinding) `orElse` pure Nothing
    case inferred of
      Nothing -> btw $ topLevelComponent
        [(Var.reset (ABT.variable e), generalizeAndUnTypeVar typ, False)]
      Just inferred -> do
        redundant <- isRedundant typ inferred
        btw $ topLevelComponent
          [(Var.reset (ABT.variable e), generalizeAndUnTypeVar typ, redundant)]
  -- The signature didn't exist, so was definitely redundant
  _ -> btw $ topLevelComponent
    [(Var.reset (ABT.variable e), generalizeAndUnTypeVar typ, True)]

-- | Synthesize the type of the given term, updating the context in the process.
-- | Figure 11 from the paper
synthesize :: forall v loc . (Var v, Ord loc) => Term v loc -> M v loc (Type v loc)
synthesize e | debugEnabled && traceShow ("synthesize"::String, e) False = undefined
synthesize e = scope (InSynthesize e) $
  case minimize' e of
    Left es -> failWith (DuplicateDefinitions es)
    Right e -> do
      Type.Effect'' es t <- go e
      abilityCheck es
      pure t
  where
  l = loc e
  go :: (Var v, Ord loc) => Term v loc -> M v loc (Type v loc)
  go (Term.Var' v) = getContext >>= \ctx -> case lookupAnn ctx v of -- Var
    Nothing -> compilerCrash $ UndeclaredTermVariable v ctx
    Just t -> pure t
  go (Term.Blank' blank) = do
    v <- freshenVar Var.blank
    appendContext [Existential blank v]
    pure $ existential' l blank v -- forall (TypeVar.Universal v) (Type.universal v)
  go (Term.Ann' (Term.Ref' _) t) = case ABT.freeVars t of
    s | Set.null s ->
      -- innermost Ref annotation assumed to be correctly provided by `synthesizeClosed`
      existentializeArrows t
    s -> compilerCrash $ FreeVarsInTypeAnnotation s
  go (Term.Ref' h) = compilerCrash $ UnannotatedReference h
  go (Term.Constructor' r cid) =
    Type.purifyArrows <$> getDataConstructorType r cid
  go (Term.Request' r cid) =
    ungeneralize . Type.purifyArrows =<< getEffectConstructorType r cid
  go (Term.Ann' e t) = checkScoped e t
  go (Term.Float' _) = pure $ Type.float l -- 1I=>
  go (Term.Int' _) = pure $ Type.int l -- 1I=>
  go (Term.Nat' _) = pure $ Type.nat l -- 1I=>
  go (Term.Boolean' _) = pure $ Type.boolean l
  go (Term.Text' _) = pure $ Type.text l
  go (Term.Char' _) = pure $ Type.char l
  go (Term.TermLink' _) = pure $ Type.termLink l
  go (Term.TypeLink' _) = pure $ Type.typeLink l
  go (Term.Apps' f args) = do -- ->EEEEE
    ft <- synthesize f
    ctx <- getContext
    (vs, ft) <- ungeneralize' ft
    scope (InFunctionCall vs f ft args) $ synthesizeApps (apply ctx ft) args
  go (Term.List' v) = do
    ft <- vectorConstructorOfArity (loc e) (Foldable.length v)
    case Foldable.toList v of
      [] -> pure ft
      v1 : _ ->
        scope (InVectorApp (ABT.annotation v1)) $ synthesizeApps ft v
  go (Term.Let1Top' top binding e) = do
    isClosed <- isClosed binding
    -- note: no need to freshen binding, it can't refer to v
    (t, ctx2) <- markThenRetract Var.inferOther $ do
      _ <- extendExistential Var.inferOther
      synthesize binding
      -- If the binding has no free variables, we generalize over its existentials
    tbinding <-
      if isClosed then pure $ generalizeExistentials ctx2 t
      else applyM . applyCtx ctx2 $ t
    v' <- ABT.freshen e freshenVar
    appendContext [Ann v' tbinding]
    t <- applyM =<< synthesize (ABT.bindInheritAnnotation e (Term.var() v'))
    when top $ noteTopLevelType e binding tbinding
    -- doRetract $ Ann v' tbinding
    pure t
  go (Term.Lam' body) = do -- ->I=> (Full Damas Milner rule)
    -- arya: are there more meaningful locations we could put into and pull out of the abschain?)
    [arg, i, e, o] <- sequence [ ABT.freshen body freshenVar
                               , freshenVar (ABT.variable body)
                               , freshenVar Var.inferAbility
                               , freshenVar Var.inferOutput ]
    let it = existential' l B.Blank i
        ot = existential' l B.Blank o
        et = existential' l B.Blank e
    appendContext $
      [existential i, existential e, existential o, Ann arg it]
    body' <- pure $ ABT.bindInheritAnnotation body (Term.var() arg)
    if Term.isLam body' then withEffects0 [] $ check body' ot
    else                     withEffects0 [et] $ check body' ot
    ctx <- getContext
    let t = Type.arrow l it (Type.effect l (apply ctx <$> [et]) ot)
    pure t
  go (Term.LetRecNamed' [] body) = synthesize body
  go (Term.LetRecTop' isTop letrec) = do
    (t, ctx2) <- markThenRetract (Var.named "let-rec-marker") $ do
      e <- annotateLetRecBindings isTop letrec
      synthesize e
    pure $ generalizeExistentials ctx2 t
  go (Term.If' cond t f) = do
    scope InIfCond $ check cond (Type.boolean l)
    scope (InIfBody $ ABT.annotation t) $ synthesizeApps (Type.iff2 l) [t, f]
  go (Term.And' a b) =
    scope InAndApp $ synthesizeApps (Type.andor' l) [a, b]
  go (Term.Or' a b) =
    scope InOrApp $ synthesizeApps (Type.andor' l) [a, b]
  go (Term.Match' scrutinee cases) = do
    scrutineeType <- synthesize scrutinee
    outputTypev <- freshenVar (Var.named "match-output")
    let outputType = existential' l B.Blank outputTypev
    appendContext [existential outputTypev]
    checkCases scrutineeType outputType cases
    ctx <- getContext
    pure $ apply ctx outputType
  go (Term.Handle' h body) = do
    -- To synthesize a handle block, we first synthesize the handler h,
    -- then push its allowed abilities onto the current ambient set when
    -- checking the body. Assuming that works, we also verify that the
    -- handler only uses abilities in the current ambient set.
    ht <- synthesize h >>= applyM >>= ungeneralize
    ctx <- getContext
    case ht of
      -- common case, like `h : Request {Remote} a -> b`, brings
      -- `Remote` into ambient when checking `body`
      Type.Arrow' (Type.Apps' (Type.Ref' ref) [et,i]) o | ref == Type.effectRef -> do
        let es = Type.flattenEffects et
        withEffects es $ check body i
        o <- applyM o
        let (oes, o') = Type.stripEffect o
        abilityCheck oes
        pure o'
      -- degenerate case, like `handle x -> 10 in ...`
      Type.Arrow' (i@(Type.Var' (TypeVar.Existential _ v@(lookupSolved ctx -> Nothing)))) o -> do
        e <- extendExistential v
        withEffects [existentialp (loc i) e] $ check body i
        o <- applyM o
        let (oes, o') = Type.stripEffect o
        abilityCheck oes
        pure o'
      _ -> failWith $ HandlerOfUnexpectedType (loc h) ht
  go _e = compilerCrash PatternMatchFailure

checkCases
  :: Var v
  => Ord loc
  => Type v loc
  -> Type v loc
  -> [Term.MatchCase loc (Term v loc)]
  -> M v loc ()
checkCases _ _ [] = pure ()
checkCases scrutType outType cases@(Term.MatchCase _ _ t : _)
  = scope (InMatch (ABT.annotation t)) $ do
      mes <- requestType (cases <&> \(Term.MatchCase p _ _) -> p)
      for_ mes $ \es -> applyM scrutType >>= \sty -> do
        v <- freshenVar Var.inferPatternPureV
        let lo = loc scrutType
            vt = existentialp lo v
        appendContext [existential v]
        subtype (Type.effectV lo (lo, Type.effects lo es) (lo, vt)) sty
      traverse_ (checkCase scrutType outType) cases

getEffect
  :: Var v => Ord loc => Reference -> Int -> M v loc (Type v loc)
getEffect ref cid = do
  ect <- getEffectConstructorType ref cid
  uect <- ungeneralize ect
  let final (Type.Arrow' _ o) = final o
      final t = t
  case final uect of
    Type.Effect'' [et] _ -> pure et
    t@(Type.Effect'' _ _) ->
      compilerCrash $ EffectConstructorHadMultipleEffects t
    _ -> compilerCrash PatternMatchFailure

requestType
  :: Var v => Ord loc => [Pattern loc] -> M v loc (Maybe [Type v loc])
requestType ps = getCompose . fmap fold $ traverse single ps
  where
  single (Pattern.As _ p) = single p
  single Pattern.EffectPure{} = Compose . pure . Just $ []
  single (Pattern.EffectBind _ ref cid _ _)
    = Compose $ Just . pure <$> getEffect ref cid
  single _ = Compose $ pure Nothing

checkCase :: forall v loc . (Var v, Ord loc)
          => Type v loc
          -> Type v loc
          -> Term.MatchCase loc (Term v loc)
          -> M v loc ()
checkCase scrutineeType outputType (Term.MatchCase pat guard rhs) = do
  scrutineeType <- applyM scrutineeType
  outputType <- applyM outputType
  markThenRetract0 Var.inferOther $ do
    let peel t = case t of
                  ABT.AbsN' vars bod -> (vars, bod)
                  _ -> ([], t)
        (rhsvs, rhsbod) = peel rhs
        mayGuard = snd . peel <$> guard
    (substs, remains) <- runStateT (checkPattern scrutineeType pat) rhsvs
    unless (null remains) $ compilerCrash (MalformedPattern pat)
    let subst = ABT.substsInheritAnnotation (second (Term.var ()) <$> substs)
        rhs' = subst rhsbod
        guard' = subst <$> mayGuard
    for_ guard' $ \g -> scope InMatchGuard $ check g (Type.boolean (loc g))
    outputType <- applyM outputType
    scope InMatchBody $ check rhs' outputType

-- For example:
--   match scrute with
--     (x, [42,y,Foo z]) -> blah x y z
--
-- scrutineeType will just be the type of `scrute`
-- The starting state will be the variables [x,y,z] (extracted from the Abs-chain on the RHS of the ->)
-- The output (assuming no type errors) is [(x,x'), (y,y'), (z,z')]
-- where x', y', z' are freshened versions of x, y, z. These will be substituted
-- into `blah x y z` to produce `blah x' y' z'` before typechecking it.
checkPattern
  :: (Var v, Ord loc)
  => Type v loc
  -> Pattern loc
  -> StateT [v] (M v loc) [(v, v)]
checkPattern tx ty | (debugEnabled || debugPatternsEnabled) && traceShow ("checkPattern"::String, tx, ty) False = undefined
checkPattern scrutineeType0 p =
  lift (ungeneralize scrutineeType0) >>= \scrutineeType -> case p of
    Pattern.Unbound _    -> pure []
    Pattern.Var     _loc -> do
      v  <- getAdvance p
      v' <- lift $ freshenVar v
      lift . appendContext $ [Ann v' scrutineeType]
      pure [(v, v')]
    -- Ex: [42, y, Foo z]
    Pattern.SequenceLiteral loc ps -> do
      vt <- lift $ do
        v <- freshenVar Var.inferOther
        let vt = existentialp loc v
        appendContext [existential v]
        -- ['a] <: scrutineeType, where 'a is fresh existential
        subtype (Type.app loc (Type.list loc) vt) scrutineeType
        applyM vt
      join <$> traverse (checkPattern vt) ps
    Pattern.SequenceOp loc l op r -> do
      let (locL, locR) = (Pattern.loc l, Pattern.loc r)
      vt <- lift $ do
        v <- freshenVar Var.inferOther
        let vt = existentialp loc v
        appendContext [existential v]
        -- todo: `Type.list loc` is super-probably wrong;
        -- I'm thinking it should be Ann.Intrinsic, but we don't
        -- have access to that here.
        subtype (Type.app loc (Type.list loc) vt) scrutineeType
        applyM vt
      case op of
        Pattern.Cons -> do
          lvs <- checkPattern vt l
          -- todo: same `Type.list loc` thing
          rvs <- checkPattern (Type.app locR (Type.list locR) vt) r
          pure $ lvs ++ rvs
        Pattern.Snoc -> do
          -- todo: same `Type.list loc` thing
          lvs <- checkPattern (Type.app locL (Type.list locL) vt) l
          rvs <- checkPattern vt r
          pure $ lvs ++ rvs
        Pattern.Concat ->
          case (l, r) of
            (p, _) | isConstLen p -> f
            (_, p) | isConstLen p -> f
            (_, _) -> lift . failWith $
              ConcatPatternWithoutConstantLength loc (Type.app loc (Type.list loc) vt)
          where
            f = liftA2 (++) (g locL l) (g locR r)
            -- todo: same `Type.list loc` thing
            g l p = checkPattern (Type.app l (Type.list l) vt) p

            -- Only pertains to sequences, returns False if not a sequence
            isConstLen :: Pattern loc -> Bool
            isConstLen p = case p of
              Pattern.SequenceLiteral _ _ -> True
              Pattern.SequenceOp _ l op r -> case op of
                Pattern.Snoc -> isConstLen l
                Pattern.Cons -> isConstLen r
                Pattern.Concat -> isConstLen l && isConstLen r
              Pattern.As _ p -> isConstLen p
              _ -> False
    -- TODO: provide a scope here for giving a good error message
    Pattern.Boolean loc _ ->
      lift $ subtype (Type.boolean loc) scrutineeType $> mempty
    Pattern.Int loc _ ->
      lift $ subtype (Type.int loc) scrutineeType $> mempty
    Pattern.Nat loc _ ->
      lift $ subtype (Type.nat loc) scrutineeType $> mempty
    Pattern.Float loc _ ->
      lift $ subtype (Type.float loc) scrutineeType $> mempty
    Pattern.Text loc _ ->
      lift $ subtype (Type.text loc) scrutineeType $> mempty
    Pattern.Char loc _  ->
      lift $ subtype (Type.char loc) scrutineeType $> mempty
    Pattern.Constructor loc ref cid args -> do
      dct  <- lift $ getDataConstructorType ref cid
      udct <- lift $ skolemize forcedData dct
      unless (Type.arity udct == length args)
        . lift
        . failWith
        $ PatternArityMismatch loc dct (length args)
      let step (Type.Arrow' i o, vso) pat =
            (\vso' -> (o, vso ++ vso')) <$> checkPattern i pat
          step _ _ =
            lift . failWith $ PatternArityMismatch loc dct (length args)
      (overall, vs) <- foldM step (udct, []) args
      st            <- lift $ applyM scrutineeType
      lift $ subtype overall st
      pure vs
    Pattern.As _loc p' -> do
      v  <- getAdvance p
      v' <- lift $ freshenVar v
      lift . appendContext $ [Ann v' scrutineeType]
      ((v, v') :) <$> checkPattern scrutineeType p'
    -- ex: { a } -> a
    -- ex: { (x, 42) } -> a
    Pattern.EffectPure loc p -> do
      vt <- lift $ do
        v <- freshenVar Var.inferPatternPureV
        e <- freshenVar Var.inferPatternPureE
        let vt = existentialp loc v
        let et = existentialp loc e
        appendContext [existential v, existential e]
        subtype (Type.effectV loc (loc, et) (loc, vt)) scrutineeType
        applyM vt
      checkPattern vt p
    -- ex: { Stream.emit x -> k } -> ...
    Pattern.EffectBind loc ref cid args k -> do
      -- scrutineeType should be a supertype of `Effect e vt`
      -- for fresh existentials `e` and `vt`
      e <- lift $ extendExistential Var.inferPatternBindE
      v <- lift $ extendExistential Var.inferPatternBindV
      let evt = Type.effectV loc (loc, existentialp loc e)
                                 (loc, existentialp loc v)
      lift $ subtype evt scrutineeType
      ect  <- lift $ getEffectConstructorType ref cid
      uect <- lift $ skolemize forcedEffect ect
      unless (Type.arity uect == length args)
        . lift
        . failWith
        . PatternArityMismatch loc ect
        $ length args
      let step (Type.Arrow' i o, vso) pat =
            (\vso' -> (o, vso ++ vso')) <$> checkPattern i pat
          step _ _ =
            lift . failWith $ PatternArityMismatch loc ect (length args)
      (ctorOutputType, vs) <- foldM step (uect, []) args
      case ctorOutputType of
        -- an effect ctor should have exactly 1 effect!
        Type.Effect'' [et] it -> do
          -- expecting scrutineeType to be `Effect et vt`

          -- ensure that the variables in `et` unify with those from
          -- the scrutinee.
          lift $ do
            res <- Type.flattenEffects <$> applyM (existentialp loc e)
            abilityCheck' res [et]

          st <- lift $ applyM scrutineeType
          case st of
            Type.App' _ vt ->
              let kt = Type.arrow (Pattern.loc k)
                                  it
                                  (Type.effect (Pattern.loc k) [et] vt)
              in (vs ++) <$> checkPattern kt k
            _ -> lift . compilerCrash $ PatternMatchFailure
        _ -> lift . compilerCrash $ EffectConstructorHadMultipleEffects
          ctorOutputType
 where

  getAdvance :: Pattern loc -> StateT [v] (M v loc) v
  getAdvance p = do
    vs <- get
    case vs of
      []       -> lift $ compilerCrash (MalformedPattern p)
      (v : vs) -> do
        put vs
        pure v

applyM :: (Var v, Ord loc) => Type v loc -> M v loc (Type v loc)
applyM t = (`apply` t) <$> getContext

lookupAnn :: Ord v => Context v loc -> v -> Maybe (Type v loc)
lookupAnn ctx v = Map.lookup v (termVarAnnotations . info $ ctx)

lookupSolved :: Ord v => Context v loc -> v -> Maybe (Monotype v loc)
lookupSolved ctx v = Map.lookup v (solvedExistentials . info $ ctx)

resetContextAfter :: a -> M v loc a -> M v loc a
resetContextAfter x a = do
  ctx <- getContext
  a <- a `orElse` pure x
  setContext ctx
  pure a

-- | Synthesize and generalize the type of each binding in a let rec.
-- Updates the context so that all bindings are annotated with
-- their type. Also returns the freshened version of `body`.
-- See usage in `synthesize` and `check` for `LetRec'` case.
annotateLetRecBindings
  :: (Var v, Ord loc)
  => Term.IsTop
  -> ((v -> M v loc v) -> M v loc ([(v, Term v loc)], Term v loc))
  -> M v loc (Term v loc)
annotateLetRecBindings isTop letrec =
  -- If this is a top-level letrec, then emit a TopLevelComponent note,
  -- which asks if the user-provided type annotations were needed.
  if isTop
  then do
    -- First, typecheck (using annotateLetRecBindings') the bindings with any
    -- user-provided annotations.
    (body, vts) <- annotateLetRecBindings' True
    -- Then, try typechecking again, but ignoring any user-provided annotations.
    -- This will infer whatever type.  If it altogether fails to typecheck here
    -- then, ...(1)
    withoutAnnotations  <-
      resetContextAfter Nothing $ Just <$> annotateLetRecBindings' False
    -- convert from typechecker TypeVar back to regular `v` vars
    let unTypeVar (v, t) = (v, generalizeAndUnTypeVar t)
    case withoutAnnotations of
      Just (_, vts') -> do
        r <- and <$> zipWithM isRedundant (fmap snd vts) (fmap snd vts')
        btw $ topLevelComponent ((\(v,b) -> (Var.reset v, b,r)) . unTypeVar <$> vts)
      -- ...(1) we'll assume all the user-provided annotations were needed
      Nothing -> btw
        $ topLevelComponent ((\(v, b) -> (Var.reset v, b, False)) . unTypeVar <$> vts)
    pure body
  -- If this isn't a top-level letrec, then we don't have to do anything special
  else fst <$> annotateLetRecBindings' True
 where
  annotateLetRecBindings' useUserAnnotations = do
    (bindings, body) <- letrec freshenVar
    let vs = map fst bindings
    ((bindings, bindingTypes), ctx2) <- markThenRetract Var.inferOther $ do
      let f (v, binding) = case binding of
            -- If user has provided an annotation, we use that
            Term.Ann' e t | useUserAnnotations -> do
              -- Arrows in `t` with no ability lists get an attached fresh
              -- existential to allow inference of required abilities
              t2 <- existentializeArrows =<< applyM t
              pure (Term.ann (loc binding) e t2, t2)
            -- If we're not using an annotation, we make one up. There's 2 cases:

            lam@(Term.Lam' _) ->
              -- If `e` is a lambda of arity K, we immediately refine the
              -- existential to `a1 ->{e1} a2 ... ->{eK} r`. This gives better
              -- inference of the lambda's ability variables in conjunction with
              -- handling of lambdas in `check` judgement.
              (lam,) <$> existentialFunctionTypeFor lam
            e -> do
              -- Anything else, just make up a fresh existential
              -- which will be refined during typechecking of the binding
              vt <- extendExistential v
              pure $ (e, existential' (loc binding) B.Blank vt)
      (bindings, bindingTypes) <- unzip <$> traverse f bindings
      appendContext (zipWith Ann vs bindingTypes)
      -- check each `bi` against its type
      Foldable.for_ (zip bindings bindingTypes) $ \(b, t) ->
        -- note: elements of a cycle have to be pure, otherwise order of effects
        -- is unclear and chaos ensues
        withEffects0 [] (checkScoped b t)
      ensureGuardedCycle (vs `zip` bindings)
      pure (bindings, bindingTypes)
    -- compute generalized types `gt1, gt2 ...` for each binding `b1, b2...`;
    -- add annotations `v1 : gt1, v2 : gt2 ...` to the context
    let bindingArities = Term.arity <$> bindings
        gen bindingType _arity = generalizeExistentials ctx2 bindingType
        bindingTypesGeneralized = zipWith gen bindingTypes bindingArities
        annotations             = zipWith Ann vs bindingTypesGeneralized
    appendContext annotations
    pure (body, vs `zip` bindingTypesGeneralized)

ensureGuardedCycle :: Var v => [(v, Term v loc)] -> M v loc ()
ensureGuardedCycle bindings = let
  -- We make sure that nonLambdas can depend only on lambdas, not on each other
  nonLambdas = Set.fromList [ v | (v, b) <- bindings, Term.arity b == 0 ]
  (notok, ok) = partition f bindings
  f (v, b) =
    if Set.member v nonLambdas then
      not $ Set.null (ABT.freeVars b `Set.intersection` nonLambdas)
    else False
  in if length ok == length bindings then pure ()
     else failWith $ UnguardedLetRecCycle (fst <$> notok) bindings

existentialFunctionTypeFor :: Var v => Term v loc -> M v loc (Type v loc)
existentialFunctionTypeFor lam@(Term.LamNamed' v body) = do
  v <- extendExistential v
  e <- extendExistential Var.inferAbility
  o <- existentialFunctionTypeFor body
  pure $ Type.arrow (loc lam)
                    (existentialp (loc lam) v)
                    (Type.effect (loc lam) [existentialp (loc lam) e] o)
existentialFunctionTypeFor e = do
  v <- extendExistential Var.inferOutput
  pure $ existentialp (loc e) v

existentializeArrows :: Var v => Type v loc -> M v loc (Type v loc)
existentializeArrows t = do
  t <- Type.existentializeArrows (extendExistentialTV Var.inferAbility) t
  pure t

ungeneralize :: (Var v, Ord loc) => Type v loc -> M v loc (Type v loc)
ungeneralize t = snd <$> ungeneralize' t

ungeneralize' :: (Var v, Ord loc) => Type v loc -> M v loc ([v], Type v loc)
ungeneralize' (Type.Forall' t) = do
  v <- ABT.freshen t freshenTypeVar
  appendContext [existential v]
  t <- pure $ ABT.bindInheritAnnotation t (existential' () B.Blank v)
  first (v:) <$> ungeneralize' t
ungeneralize' t = pure ([], t)

skolemize
  :: Var v
  => Ord loc
  => (Type v loc -> Set (TypeVar v loc))
  -> Type v loc
  -> M v loc (Type v loc)
skolemize forced (Type.ForallsNamed' vs ty) = do
  urn <- for uvs $ \u -> (,) u <$> freshenTypeVar u
  srn <- for svs $ \u -> (,) u <$> freshenTypeVar u
  let uctx = existential . snd <$> urn
      sctx = Universal . snd <$> srn
      rn = (fmap (existential' () B.Blank) <$> urn)
        ++ (fmap (universal' ()) <$> srn)
  appendContext $ uctx ++ sctx
  pure $ foldl (flip $ uncurry ABT.substInheritAnnotation) ty rn
  where
  fovs = forced ty
  (uvs, svs) = partition (`Set.member` fovs) vs
skolemize _ ty = pure ty

forcedEffect :: Type v loc -> Set (TypeVar v loc)
forcedEffect (Type.Arrow' _ o) = forcedEffect o
forcedEffect (Type.Effect1' es _) = Type.freeVars es
forcedEffect _ = Set.empty

forcedData :: Type v loc -> Set (TypeVar v loc)
forcedData (Type.Arrow' _ o) = forcedData o
forcedData ty = Type.freeVars ty

-- | Apply the context to the input type, then convert any unsolved existentials
-- to universals.
generalizeExistentials :: (Var v, Ord loc) => [Element v loc] -> Type v loc -> Type v loc
generalizeExistentials = generalizeP existentialP

generalizeP
  :: Var v
  => Ord loc
  => (Element v loc -> Maybe (TypeVar v loc, v))
  -> [Element v loc]
  -> Type v loc
  -> Type v loc
generalizeP p ctx0 ty = foldr gen (applyCtx ctx0 ty) ctx
  where
  ctx = mapMaybe p ctx0

  gen (tv, v) t
    | tv `ABT.isFreeIn` t
    -- location of the forall is just the location of the input type
    -- and the location of each quantified variable is just inherited
    -- from its source location
    = Type.forall (loc t) (TypeVar.Universal v)
        (ABT.substInheritAnnotation tv (universal' () v) t)
   -- don't bother introducing a forall if type variable is unused
    | otherwise = t

existentialP :: Element v loc -> Maybe (TypeVar v loc, v)
existentialP (Var (TypeVar.Existential _ v))
  = Just (TypeVar.Existential B.Blank v, v)
existentialP _ = Nothing

variableP :: Element v loc -> Maybe (TypeVar v loc, v)
variableP (Var (TypeVar.Existential _ v))
  = Just (TypeVar.Existential B.Blank v, v)
variableP (Var tv@(TypeVar.Universal v)) = Just (tv, v)
variableP _ = Nothing

-- This checks `e` against the type `t`, but if `t` is a `∀`, any ∀-quantified
-- variables are freshened and substituted into `e`. This should be called whenever
-- a term is being checked against a type due to a user-provided signature on `e`.
-- See its usage in `synthesize` and `annotateLetRecBindings`.
checkScoped
  :: forall v loc
   . (Var v, Ord loc)
  => Term v loc -> Type v loc -> M v loc (Type v loc)
checkScoped e (Type.Forall' body) = do
  v <- ABT.freshen body freshenTypeVar
  (ty, pop) <- markThenRetract v $ do
    x <- extendUniversal v
    let e' = Term.substTypeVar (ABT.variable body) (universal' () x) e
    checkScoped e' (ABT.bindInheritAnnotation body (universal' () x))
  pure $ generalizeP variableP pop ty
checkScoped e t = do
  t <- existentializeArrows t
  t <$ check e t

-- | Check that under the given context, `e` has type `t`,
-- updating the context in the process.
check :: forall v loc . (Var v, Ord loc) => Term v loc -> Type v loc -> M v loc ()
check e t | debugEnabled && traceShow ("check" :: String, e, t) False = undefined
check e0 t0 = scope (InCheck e0 t0) $ do
  ctx <- getContext
  let Type.Effect'' es t = t0
  let e                  = minimize' e0
  case e of
    Left e -> failWith $ DuplicateDefinitions e
    Right e ->
      if wellformedType ctx t0
        then case t of
             -- expand existentials before checking
          t@(Type.Var' (TypeVar.Existential _ _)) -> abilityCheck es >> go e (apply ctx t)
          t                         -> go e (Type.stripIntroOuters t)
        else failWith $ IllFormedType ctx
 where
  go :: Term v loc -> Type v loc -> M v loc ()
  go e (Type.Forall' body) = do -- ForallI
    v <- ABT.freshen body freshenTypeVar
    markThenRetract0 v $ do
      x <- extendUniversal v
      check e (ABT.bindInheritAnnotation body (universal' () x))
  go (Term.Lam' body) (Type.Arrow' i o) = do -- =>I
    x <- ABT.freshen body freshenVar
    markThenRetract0 x $ do
      extendContext (Ann x i)
      let Type.Effect'' es ot = o
      body' <- pure $ ABT.bindInheritAnnotation body (Term.var() x)
      withEffects0 es $ check body' ot
  go (Term.Let1' binding e) t = do
    v        <- ABT.freshen e freshenVar
    tbinding <- synthesize binding
    markThenRetract0 v $ do
      extendContext (Ann v tbinding)
      check (ABT.bindInheritAnnotation e (Term.var () v)) t
  go (Term.LetRecNamed' [] e) t = check e t
  go (Term.LetRecTop' isTop letrec) t =
    markThenRetract0 (Var.named "let-rec-marker") $ do
      e <- annotateLetRecBindings isTop letrec
      check e t
  go e t = do -- Sub
    a   <- synthesize e
    ctx <- getContext
    subtype (apply ctx a) (apply ctx t)

-- | `subtype ctx t1 t2` returns successfully if `t1` is a subtype of `t2`.
-- This may have the effect of altering the context.
subtype :: forall v loc . (Var v, Ord loc) => Type v loc -> Type v loc -> M v loc ()
subtype tx ty | debugEnabled && traceShow ("subtype"::String, tx, ty) False = undefined
subtype tx ty = scope (InSubtype tx ty) $ do
  ctx <- getContext
  go (ctx :: Context v loc) (Type.stripIntroOuters tx) (Type.stripIntroOuters ty)
  where -- Rules from figure 9
  go :: Context v loc -> Type v loc -> Type v loc -> M v loc ()
  go _ (Type.Ref' r) (Type.Ref' r2) | r == r2 = pure () -- `Unit`
  go ctx t1@(Type.Var' (TypeVar.Universal v1)) t2@(Type.Var' (TypeVar.Universal v2)) -- `Var`
    | v1 == v2 && wellformedType ctx t1 && wellformedType ctx t2
    = pure ()
  go ctx t1@(Type.Var' (TypeVar.Existential _ v1)) t2@(Type.Var' (TypeVar.Existential _ v2)) -- `Exvar`
    | v1 == v2 && wellformedType ctx t1 && wellformedType ctx t2
    = pure ()
  go _ (Type.Arrow' i1 o1) (Type.Arrow' i2 o2) = do -- `-->`
    subtype i2 i1; ctx' <- getContext
    subtype (apply ctx' o1) (apply ctx' o2)
  go _ (Type.App' x1 y1) (Type.App' x2 y2) = do -- analogue of `-->`
    subtype x1 x2
    -- We don't know the variance of the type argument, so we assume
    -- (conservatively) that it's invariant, see
    -- discussion https://github.com/unisonweb/unison/issues/512
    y1 <- applyM y1; y2 <- applyM y2
    subtype y1 y2
    y1 <- applyM y1; y2 <- applyM y2
    -- performing the subtype check in both directions means the types must be equal
    subtype y2 y1
  go _ t (Type.Forall' t2) = do
    v <- ABT.freshen t2 freshenTypeVar
    markThenRetract0 v $ do
      v' <- extendUniversal v
      t2 <- pure $ ABT.bindInheritAnnotation t2 (universal' () v')
      subtype t t2
  go _ (Type.Forall' t) t2 = do
    v0 <- ABT.freshen t freshenTypeVar
    markThenRetract0 v0 $ do
      v <- extendExistential v0
      t <- pure $ ABT.bindInheritAnnotation t (existential' () B.Blank v)
      t1 <- applyM t
      subtype t1 t2
  go _ (Type.Effect1' e1 a1) (Type.Effect1' e2 a2) = do
    subtype e1 e2
    ctx <- getContext
    subtype (apply ctx a1) (apply ctx a2)
  go _ a (Type.Effect1' _e2 a2) = subtype a a2
  go _ (Type.Effect1' es a) a2 = do
     subtype es (Type.effects (loc es) [])
     subtype a a2
  go ctx (Type.Var' (TypeVar.Existential b v)) t -- `InstantiateL`
    | Set.member v (existentials ctx) && notMember v (Type.freeVars t) =
    instantiateL b v t
  go ctx t (Type.Var' (TypeVar.Existential b v)) -- `InstantiateR`
    | Set.member v (existentials ctx) && notMember v (Type.freeVars t) =
    instantiateR t b v
  go _ (Type.Effects' es1) (Type.Effects' es2) = do
     ctx <- getContext
     let es1' = map (apply ctx) es1
         es2' = map (apply ctx) es2
     if all (`elem` es2') es1' then pure () else abilityCheck' es2' es1'
  go _ t t2@(Type.Effects' _) | expand t  = subtype (Type.effects (loc t) [t]) t2
  go _ t@(Type.Effects' _) t2 | expand t2 = subtype t (Type.effects (loc t2) [t2])
  go ctx _ _ = failWith $ TypeMismatch ctx

  expand :: Type v loc -> Bool
  expand t = case t of
    Type.Var' (TypeVar.Existential _ _) -> True
    Type.App' _ _ -> True
    Type.Ref' _ -> True
    _ -> False


-- | Instantiate the given existential such that it is
-- a subtype of the given type, updating the context
-- in the process.
instantiateL :: (Var v, Ord loc) => B.Blank loc -> v -> Type v loc -> M v loc ()
instantiateL _ v t | debugEnabled && traceShow ("instantiateL"::String, v, t) False = undefined
instantiateL blank v (Type.stripIntroOuters -> t) = scope (InInstantiateL v t) $
  getContext >>= \ctx -> case Type.monotype t of
    Just t -> solve ctx v t >>= \case
      Just ctx -> setContext ctx -- InstLSolve
      Nothing -> go ctx
    Nothing -> go ctx
  where
    go ctx = case t of
      Type.Var' (TypeVar.Existential _ v2) | ordered ctx v v2 -> -- InstLReach (both are existential, set v2 = v)
        solve ctx v2 (Type.Monotype (existentialp (loc t) v)) >>=
          maybe (failWith $ TypeMismatch ctx) setContext
      Type.Arrow' i o -> do -- InstLArr
        [i',o'] <- traverse freshenVar [nameFrom Var.inferInput i, nameFrom Var.inferOutput o]
        let s = Solved blank v (Type.Monotype (Type.arrow (loc t)
                                                 (existentialp (loc i) i')
                                                 (existentialp (loc o) o')))
        replaceContext (existential v)
                       [existential o', existential i', s]
        instantiateR i B.Blank i' -- todo: not sure about this, could also be `blank`
        applyM o >>= instantiateL B.Blank o'
      Type.App' x y -> do -- analogue of InstLArr
        [x', y'] <- traverse freshenVar [nameFrom Var.inferTypeConstructor x, nameFrom Var.inferTypeConstructorArg y]
        let s = Solved blank v (Type.Monotype (Type.app (loc t)
                                                  (existentialp (loc x) x')
                                                  (existentialp (loc y) y')))
        replaceContext (existential v)
                       [existential y', existential x', s]
        applyM x >>= instantiateL B.Blank x'
        applyM y >>= instantiateL B.Blank y'
      Type.Effect1' es vt -> do
        es' <- freshenVar Var.inferAbility
        vt' <- freshenVar Var.inferOther
        let t' = Type.effect1 (loc t) (existentialp (loc es) es')
                                      (existentialp (loc vt) vt')
            s = Solved blank v (Type.Monotype t')
        replaceContext (existential v)
                       [existential es', existential vt', s]
        applyM es >>= instantiateL B.Blank es'
        applyM vt >>= instantiateL B.Blank vt'
      Type.Effects' es -> do
        es' <- traverse (\e -> freshenVar (nameFrom Var.inferAbility e)) es
        let locs = loc <$> es
            t' = Type.effects (loc t) (uncurry existentialp <$> locs `zip` es')
            s = Solved blank v $ Type.Monotype t'
        replaceContext (existential v)
                       ((existential <$> es') ++ [s])
        Foldable.for_ (es' `zip` es) $ \(e',e) ->
          applyM e >>= instantiateL B.Blank e'
      Type.Forall' body -> do -- InstLIIL
        v0 <- ABT.freshen body freshenTypeVar
        markThenRetract0 v0 $ do
          v <- extendUniversal v0
          instantiateL B.Blank v (ABT.bindInheritAnnotation body (universal' () v))
      _ -> failWith $ TypeMismatch ctx

nameFrom :: Var v => v -> Type v loc -> v
nameFrom _ (Type.Var' v) = TypeVar.underlying (Var.reset v)
nameFrom ifNotVar _ = ifNotVar

-- | Instantiate the given existential such that it is
-- a supertype of the given type, updating the context
-- in the process.
instantiateR :: (Var v, Ord loc) => Type v loc -> B.Blank loc -> v -> M v loc ()
instantiateR t _ v | debugEnabled && traceShow ("instantiateR"::String, t, v) False = undefined
instantiateR (Type.stripIntroOuters -> t) blank v = scope (InInstantiateR t v) $
  getContext >>= \ctx -> case Type.monotype t of
    Just t -> solve ctx v t >>= \case
      Just ctx -> setContext ctx -- InstRSolve
      Nothing -> go ctx
    Nothing -> go ctx
  where
    go ctx = case t of
      Type.Var' (TypeVar.Existential _ v2) | ordered ctx v v2 -> -- InstRReach (both are existential, set v2 = v)
        solve ctx v2 (Type.Monotype (existentialp (loc t) v)) >>=
          maybe (failWith $ TypeMismatch ctx) setContext
      Type.Arrow' i o -> do -- InstRArrow
        [i', o'] <- traverse freshenVar [nameFrom Var.inferInput i, nameFrom Var.inferOutput o]
        let s = Solved blank v (Type.Monotype
                          (Type.arrow (loc t)
                            (existentialp (loc i) i')
                            (existentialp (loc o) o')))
        replaceContext (existential v)
                       [existential o', existential i', s]
        ctx <- instantiateL B.Blank i' i >> getContext
        instantiateR (apply ctx o) B.Blank o'
      Type.App' x y -> do -- analogue of InstRArr
        -- example foo a <: v' will
        -- 1. create foo', a', add these to the context
        -- 2. add v' = foo' a' to the context
        -- 3. recurse to refine the types of foo' and a'
        [x', y'] <- traverse freshenVar [nameFrom Var.inferTypeConstructor x, nameFrom Var.inferTypeConstructorArg y]
        let s = Solved blank v (Type.Monotype (Type.app (loc t) (existentialp (loc x) x') (existentialp (loc y) y')))
        replaceContext (existential v) [existential y', existential x', s]
        applyM x >>= \x -> instantiateR x B.Blank x'
        applyM y >>= \y -> instantiateR y B.Blank y'
      Type.Effect1' es vt -> do
        es' <- freshenVar (nameFrom Var.inferAbility es)
        vt' <- freshenVar (nameFrom Var.inferTypeConstructorArg vt)
        let t' = Type.effect1 (loc t) (existentialp (loc es) es')
                                      (existentialp (loc vt) vt')
            s = Solved blank v (Type.Monotype t')
        replaceContext (existential v)
                       [existential es', existential vt', s]
        applyM es >>= \es -> instantiateR es B.Blank es'
        applyM vt >>= \vt -> instantiateR vt B.Blank vt'
      Type.Effects' es -> do
        es' <- traverse (\e -> freshenVar (nameFrom Var.inferAbility e)) es
        let locs = loc <$> es
            t' = Type.effects (loc t) (uncurry existentialp <$> locs `zip` es')
            s = Solved blank v $ Type.Monotype t'
        replaceContext (existential v)
                       ((existential <$> es') ++ [s])
        Foldable.for_ (es `zip` es') $ \(e, e') -> do
          ctx <- getContext
          instantiateR (apply ctx e) B.Blank e'
      Type.Forall' body -> do -- InstRAIIL
        x' <- ABT.freshen body freshenTypeVar
        markThenRetract0 x' $ do
          appendContext [existential x']
          instantiateR (ABT.bindInheritAnnotation body (existential' () B.Blank x')) B.Blank v
      _ -> failWith $ TypeMismatch ctx

-- | solve (ΓL,α^,ΓR) α τ = (ΓL,α^ = τ,ΓR)
-- Solve the given existential variable to the given monotype.
-- If the given monotype is not well-formed at the context location
-- where the existential variable is introduced, return `Nothing`.
-- Fail with type mismatch if the existential is already solved to something else.
-- Fail with a compiler bug if the existential does not appear in the context at all.
solve :: (Var v, Ord loc) => Context v loc -> v -> Monotype v loc -> M v loc (Maybe (Context v loc))
solve ctx v t = case lookupSolved ctx v of
  Just t2 ->
    -- okay to solve something again if it's to an identical type
    if same t t2 then pure (Just ctx)
    else failWith $ TypeMismatch ctx
   where same t1 t2 = apply ctx (Type.getPolytype t1) == apply ctx (Type.getPolytype t2)
  Nothing -> case breakAt (existential v) ctx of
    Just (ctxL, Existential blank v, ctxR) ->
      if wellformedType ctxL (Type.getPolytype t)
      then Just <$> ctxL `extendN` ((Solved blank v t) : ctxR)
      else pure Nothing
    _ -> compilerCrash $ UnknownExistentialVariable v ctx

abilityCheck' :: forall v loc . (Var v, Ord loc) => [Type v loc] -> [Type v loc] -> M v loc ()
abilityCheck' [] [] = pure ()
abilityCheck' ambient0 requested0 = go ambient0 requested0 where
  go _ambient [] = pure ()
  go ambient0 (r:rs) = do
    -- Note: if applyM returns an existential, it's unsolved
    ambient <- traverse applyM ambient0
    r <- applyM r
    -- 1. Look in ambient for exact match of head of `r`
    case find (headMatch r) ambient of
      -- 2a. If yes for `a` in ambient, do `subtype amb r` and done.
      Just amb -> do
        subtype amb r `orElse` die r
        go ambient rs
      -- Corner case where a unification caused `r` to expand to a
      -- list of effects. This whole function should be restructured
      -- such that this can go in a better spot.
      Nothing | Type.Effects' es <- r -> go ambient (es ++ rs)
      -- 2b. If no:
      Nothing -> case r of
        -- It's an unsolved existential, instantiate it to all of ambient
        Type.Var' tv@(TypeVar.Existential b v) -> do
          let et2 = Type.effects (loc r) ambient
              acyclic
                | Set.member tv (Type.freeVars et2)
                -- just need to trigger `orElse` in this case
                = getContext >>= failWith . TypeMismatch
                | otherwise = instantiateR et2 b v
          -- instantiate it to `{}` if can't cover all of ambient
          acyclic
            `orElse` instantiateR (Type.effects (loc r) []) b v
            `orElse` die1
          go ambient rs
        _ -> -- find unsolved existential, 'e, that appears in ambient
          let unsolveds = (ambient >>= Type.flattenEffects >>= vars)
              vars (Type.Var' (TypeVar.Existential b v)) = [(b,v)]
              vars _ = []
          in case listToMaybe unsolveds of
            Just (b, e') -> do
              -- introduce fresh existential 'e2 to context
              e2' <- extendExistential e'
              let et2 = Type.effects (loc r) [r, existentialp (loc r) e2']
              instantiateR et2 b e' `orElse` die r
              go ambient rs
            _ -> die r

  headMatch :: Type v loc -> Type v loc -> Bool
  headMatch (Type.App' f _) (Type.App' f2 _) = headMatch f f2
  headMatch r r2 = r == r2

  -- as a last ditch effort, if the request is an existential and there are
  -- no remaining unbound existentials left in ambient, we try to instantiate
  -- the request to the ambient effect list
  die r = case r of
    Type.Var' (TypeVar.Existential b v) ->
      instantiateL b v (Type.effects (loc r) ambient0) `orElse` die1
      -- instantiateL b v (Type.effects (loc r) []) `orElse` die1
    _ -> die1 -- and if that doesn't work, then we're really toast

  die1 = do
    ctx <- getContext
    failWith $ AbilityCheckFailure (apply ctx <$> ambient0)
                                   (apply ctx <$> requested0)
                                   ctx

abilityCheck :: (Var v, Ord loc) => [Type v loc] -> M v loc ()
abilityCheck requested = do
  ambient <- getAbilities
  requested' <- filterM shouldPerformAbilityCheck requested
  ctx <- getContext
  abilityCheck' (apply ctx <$> ambient >>= Type.flattenEffects)
                (apply ctx <$> requested' >>= Type.flattenEffects)

verifyDataDeclarations :: (Var v, Ord loc) => DataDeclarations v loc -> Result v loc ()
verifyDataDeclarations decls = forM_ (Map.toList decls) $ \(_ref, decl) -> do
  let ctors = DD.constructors decl
  forM_ ctors $ \(_ctorName,typ) -> verifyClosed typ id

-- | public interface to the typechecker
synthesizeClosed
  :: (Var v, Ord loc)
  => [Type v loc]
  -> TL.TypeLookup v loc
  -> Term v loc
  -> Result v loc (Type v loc)
synthesizeClosed abilities lookupType term0 = let
  datas = TL.dataDecls lookupType
  effects = TL.effectDecls lookupType
  term = annotateRefs (TL.typeOfTerm' lookupType) term0
  in case term of
    Left missingRef ->
      compilerCrashResult (UnknownTermReference missingRef)
    Right term -> run [] datas effects $ do
      liftResult $  verifyDataDeclarations datas
                 *> verifyDataDeclarations (DD.toDataDecl <$> effects)
                 *> verifyClosedTerm term
      synthesizeClosed' abilities term

verifyClosedTerm :: forall v loc . Ord v => Term v loc -> Result v loc ()
verifyClosedTerm t = do
  ok1 <- verifyClosed t id
  let freeTypeVars = Map.toList $ Term.freeTypeVarAnnotations t
      reportError (v, locs) = for_ locs $ \loc ->
        typeError (UnknownSymbol loc (TypeVar.underlying v))
  for_ freeTypeVars reportError
  when (not ok1 || (not . null) freeTypeVars) $ compilerBug (OtherBug "impossible")

verifyClosed :: (Traversable f, Ord v) => ABT.Term f v a -> (v -> v2) -> Result v2 a Bool
verifyClosed t toV2 =
  let isBoundIn v t = Set.member v (snd (ABT.annotation t))
      loc t = fst (ABT.annotation t)
      go t@(ABT.Var' v) | not (isBoundIn v t) = typeError (UnknownSymbol (loc t) $ toV2 v)
      go _ = pure True
  in all id <$> ABT.foreachSubterm go (ABT.annotateBound t)

annotateRefs :: (Applicative f, Var v)
             => (Reference -> f (Type.Type v loc))
             -> Term v loc
             -> f (Term v loc)
annotateRefs synth = ABT.visit f where
  f r@(Term.Ref' h) = Just (Term.ann ra (Term.ref ra h) <$> (ge <$> synth h))
    where ra = ABT.annotation r
          ge t = ABT.vmap TypeVar.Universal $ t
  f _ = Nothing

run
  :: (Var v, Ord loc, Functor f)
  => [Type v loc]
  -> DataDeclarations v loc
  -> EffectDeclarations v loc
  -> MT v loc f a
  -> f a
run ambient datas effects m =
  fmap fst
    . runM m
    $ MEnv (Env 1 context0) ambient datas effects []

synthesizeClosed' :: (Var v, Ord loc)
                  => [Type v loc]
                  -> Term v loc
                  -> M v loc (Type v loc)
synthesizeClosed' abilities term = do
  -- save current context, for restoration when done
  ctx0 <- getContext
  setContext context0
  (t, ctx) <- markThenRetract (Var.named "start") $ do
    -- retract will cause notes to be written out for
    -- any `Blank`-tagged existentials passing out of scope
    withEffects0 abilities (synthesize term)
  setContext ctx0 -- restore the initial context
  pure $ generalizeExistentials ctx t

-- Check if the given typechecking action succeeds.
succeeds :: M v loc a -> TotalM v loc Bool
succeeds m = do
  e <- ask
  case runM m e of
    Success _ _ -> pure True
    TypeError _ _ -> pure False
    CompilerBug bug _ _ -> MT (\_ -> Left bug)

-- Check if `t1` is a subtype of `t2`. Doesn't update the typechecking context.
isSubtype' :: (Var v, Ord loc) => Type v loc -> Type v loc -> TotalM v loc Bool
isSubtype' type1 type2 = succeeds $ do
  let vars = Set.toList $ Set.union (ABT.freeVars type1) (ABT.freeVars type2)
  reserveAll (TypeVar.underlying <$> vars)
  appendContext (Var <$> vars)
  subtype type1 type2

-- `isRedundant userType inferredType` returns `True` if the `userType`
-- is equal "up to inferred abilities" to `inferredType`.
--
-- Example: `userType` is `Nat -> Nat`, `inferredType` is `∀ a . a ->{IO} a`.
--           In this case, the signature isn't redundant, and we return
--           `False`.
-- Example: `userType` is (`∀ a . a -> a`) and inferred is `∀ z e . z ->{e} z`.
--           In this case, the signature IS redundant, and we return `True`.
isRedundant
  :: (Var v, Ord loc)
  => Type v loc
  -> Type v loc
  -> M v loc Bool
isRedundant userType0 inferredType0 = do
  ctx0 <- getContext
  -- the inferred type may have some unsolved existentials, which we generalize over
  -- before doing the comparison, otherwise it will just test equal to any
  -- concrete instantiation of those existentials. For instance, the
  -- inferred type `a -> a` for a existential `a` should get generalized
  -- to `∀ a . a -> a` before comparison to `Nat -> Nat`, otherwise the
  -- typechecker will solve `a = Nat` and call the types equal!
  userType <- existentializeArrows userType0
  inferredType <- generalizeExistentials' <$> applyM inferredType0
  -- We already know `inferred <: userType`, otherwise the user's given
  -- type would have caused the program not to typecheck! Ex: if user writes
  -- `: Nat -> Nat` when it has an inferred type of `a -> a`. So we only
  -- need to check the other direction to determine redundancy.
  (liftTotalM $ isSubtype' userType inferredType) <* setContext ctx0

-- Public interface to `isSubtype`
isSubtype
  :: (Var v, Ord loc) => Type v loc -> Type v loc -> Either (CompilerBug v loc) Bool
isSubtype t1 t2 =
  run [] Map.empty Map.empty (isSubtype' t1 t2)

isEqual
  :: (Var v, Ord loc) => Type v loc -> Type v loc -> Either (CompilerBug v loc) Bool
isEqual t1 t2 =
  (&&) <$> isSubtype t1 t2 <*> isSubtype t2 t1

instance (Var v) => Show (Element v loc) where
  show (Var v) = case v of
    TypeVar.Universal x -> "@" <> show x
    TypeVar.Existential _ x -> "'" ++ show x
  show (Solved _ v t) = "'"++Text.unpack (Var.name v)++" = "++TP.pretty' Nothing mempty (Type.getPolytype t)
  show (Ann v t) = Text.unpack (Var.name v) ++ " : " ++
                   TP.pretty' Nothing mempty t
  show (Marker v) = "|"++Text.unpack (Var.name v)++"|"

instance (Ord loc, Var v) => Show (Context v loc) where
  show ctx@(Context es) = "Γ\n  " ++ (intercalate "\n  " . map (showElem ctx . fst)) (reverse es)
    where
    showElem _ctx (Var v) = case v of
      TypeVar.Universal x -> "@" <> show x
      TypeVar.Existential _ x -> "'" ++ show x
    showElem ctx (Solved _ v (Type.Monotype t)) = "'"++Text.unpack (Var.name v)++" = "++ TP.pretty' Nothing mempty (apply ctx t)
    showElem ctx (Ann v t) = Text.unpack (Var.name v) ++ " : " ++ TP.pretty' Nothing mempty (apply ctx t)
    showElem _ (Marker v) = "|"++Text.unpack (Var.name v)++"|"

-- MEnv v loc -> (Seq (ErrorNote v loc), (a, Env v loc))
instance Monad f => Monad (MT v loc f) where
  return a = MT (\menv -> pure (a, env menv))
  m >>= f = MT go where
    go menv = do
      (a, env1) <- runM m menv
      runM (f a) (menv { env = env1 })

instance Monad f => MonadFail.MonadFail (MT v loc f) where
  fail = error

instance Monad f => Applicative (MT v loc f) where
  pure a = MT (\menv -> pure (a, env menv))
  (<*>) = ap

instance Functor f => Functor (MT v loc f) where
  fmap f (MT m) = MT (\menv -> fmap (first f) (m menv))

instance Monad f => MonadReader (MEnv v loc) (MT v loc f) where
  ask = MT (\e -> pure (e, env e))
  local f m = MT $ runM m . f
