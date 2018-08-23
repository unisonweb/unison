{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Unison.Typechecker.Context
  ( synthesizeClosed
  , Note(..)
  , Cause(..)
  , Context(..)
  , Element(..)
  , PathElement(..)
  , Term
  , Type
  , TypeVar
  , errorTerms
  , innermostErrorTerm
  , lookupAnn
  , lookupSolved
  , apply
  , isSubtype
  , Suggestion(..)
  )
where

import           Control.Monad
import           Control.Monad.Loops (anyM, allM)
import           Control.Monad.Reader.Class
import           Control.Monad.State (get, put, StateT, runStateT)
import           Control.Monad.Trans (lift)
import           Data.Bifunctor (second)
import           Data.Foldable (for_)
import qualified Data.Foldable as Foldable
import           Data.Functor
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Sequence (Seq)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Debug.Trace
import qualified Unison.ABT as ABT
import qualified Unison.Blank as B
import           Unison.DataDeclaration (DataDeclaration', EffectDeclaration')
import qualified Unison.DataDeclaration as DD
import           Unison.PatternP (Pattern)
import qualified Unison.PatternP as Pattern
import           Unison.Reference (Reference)
import           Unison.Term (AnnotatedTerm')
import qualified Unison.Term as Term
import           Unison.Type (AnnotatedType)
import qualified Unison.Type as Type
import qualified Unison.TypeVar as TypeVar
import           Unison.Typechecker.Components (minimize')
import           Unison.Var (Var)
import qualified Unison.Var as Var

type TypeVar v loc = TypeVar.TypeVar (B.Blank loc) v
type Type v loc = AnnotatedType (TypeVar v loc) loc
type Term v loc = AnnotatedTerm' (TypeVar v loc) v loc
type Monotype v loc = Type.Monotype (TypeVar v loc) loc

pattern Universal v = Var (TypeVar.Universal v)
pattern Existential b v = Var (TypeVar.Existential b v)

existential :: v -> Element v loc
existential = Existential B.Blank

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

data Env v loc = Env { freshId :: Word, ctx :: Context v loc }

type DataDeclarations v loc = Map Reference (DataDeclaration' v loc)
type EffectDeclarations v loc = Map Reference (EffectDeclaration' v loc)

type Result v loc a = (Seq (Note v loc), Maybe a)

-- | Typechecking monad
newtype M v loc a = M {
  runM :: MEnv v loc -> Result v loc (a, Env v loc)
}

data Unknown = Data | Effect deriving Show

data CompilerBug v loc
  = UnknownDecl Unknown Reference (Map Reference (DataDeclaration' v loc))
  | UnknownConstructor Unknown Reference Int (DataDeclaration' v loc)
  | UndeclaredTermVariable v (Context v loc)
  | RetractFailure (Element v loc) (Context v loc)
  | EmptyLetRec (Term v loc) -- the body of the empty let rec
  | PatternMatchFailure
  | EffectConstructorHadMultipleEffects (Type v loc)
  | FreeVarsInTypeAnnotation (Set (TypeVar v loc))
  | UnannotatedReference Reference
  | MalformedPattern (Pattern loc)
  deriving Show

data PathElement v loc
  = InSynthesize (Term v loc)
  | InSubtype (Type v loc) (Type v loc)
  | InCheck (Term v loc) (Type v loc)
  | InInstantiateL v (Type v loc)
  | InInstantiateR (Type v loc) v
  | InSynthesizeApp (Type v loc) (Term v loc) Int
  | InSynthesizeApps (Term v loc) (Type v loc) [Term v loc]
  | InAndApp
  | InOrApp
  | InIfCond
  | InIfBody loc -- location of `then` expression
  | InVectorApp loc -- location of 1st vector element
  | InMatch loc -- location of 1st case body
  deriving Show

type ExpectedArgCount = Int
type ActualArgCount = Int
type ConstructorId = Int

data Suggestion v loc =
  Suggestion { suggestionName :: Text
             , suggestionType :: Type v loc
             }
  deriving Show

data Cause v loc
  = TypeMismatch (Context v loc)
  | IllFormedType (Context v loc)
  | UnknownSymbol loc v
  | UnknownTerm loc v [Suggestion v loc] (Type v loc)
  | CompilerBug (CompilerBug v loc)
  | AbilityCheckFailure [Type v loc] [Type v loc] (Context v loc) -- ambient, requested
  | EffectConstructorWrongArgCount ExpectedArgCount ActualArgCount Reference ConstructorId
  | MalformedEffectBind (Type v loc) (Type v loc) [Type v loc] -- type of ctor, type of ctor result
  | SolvedBlank (B.Recorded loc) v (Type v loc)
  | PatternArityMismatch loc (Type v loc) Int -- Type of ctor, number of arguments we got
  deriving Show

errorTerms :: Note v loc -> [Term v loc]
errorTerms n = Foldable.toList (path n) >>= \e -> case e of
  InCheck e _           -> [e]
  InSynthesizeApp _ e _ -> [e]
  InSynthesize e        -> [e]
  _                     -> [ ]

innermostErrorTerm :: Note v loc -> Maybe (Term v loc)
innermostErrorTerm n = listToMaybe $ errorTerms n

data Note v loc = Note { cause :: Cause v loc, path :: Seq (PathElement v loc) } deriving Show

solveBlank :: B.Recorded loc -> v -> Type v loc -> M v loc ()
solveBlank blank v typ =
  M (\menv -> (pure $ Note (SolvedBlank blank v typ) mempty, Just ((), env menv)))

-- Add `p` onto the end of the `path` of this `Note`
scope' :: PathElement v loc -> Note v loc -> Note v loc
scope' p (Note cause path) = Note cause (path `mappend` pure p)

-- Add `p` onto the end of the `path` of any `Note`s emitted by the action
scope :: PathElement v loc -> M v loc a -> M v loc a
scope p (M m) = M go where
  go menv =
    let (notes, r) = m menv
    in (scope' p <$> notes, r)

-- | The typechecking environment
data MEnv v loc = MEnv {
  env :: Env v loc,                    -- The typechecking state
  abilities :: [Type v loc],           -- Allowed ambient abilities
  builtinLocation :: loc,              -- The location of builtins
  dataDecls :: DataDeclarations v loc, -- Data declarations in scope
  effectDecls :: EffectDeclarations v loc, -- Effect declarations in scope

  -- Returns `True` if ability checks should be performed on the
  -- input type. See abilityCheck function for how this is used.
  abilityCheckMask :: Type v loc -> M v loc Bool
}

newtype Context v loc = Context [(Element v loc, Info v)]

data Info v =
  Info { existentialVars :: Set v -- set of existentials seen so far
       , universalVars :: Set v -- set of universals seen so far
       , allVars :: Set v -- all variables seen so far
       , isWellformed :: Bool -- whether the context so far is well-formed
       }

-- | The empty context
context0 :: Context v loc
context0 = Context []

-- | Build a context from a list of elements.
context :: Var v => [Element v loc] -> Context v loc
context = foldl' (flip extend) context0

-- | Delete from the end of this context up to and including
-- the given `Element`. Returns `Nothing` if the element is not found.
retract0 :: (Var v, Ord loc) => Element v loc -> Context v loc -> Maybe (Context v loc, [Element v loc])
retract0 e (Context ctx) =
  let maybeTail [] = Nothing
      maybeTail (_:t) = pure t
      (discarded, ctx2) = span (\(e',_) -> e' /= e) ctx
      -- note: no need to recompute used variables; any suffix of the
      -- context snoc list is also a valid context
      go ctx2 = (Context ctx2, fst <$> discarded)
  in go <$> maybeTail ctx2

-- | Delete from the end of this context up to and including
-- the given `Element`.
-- Example, if input context is `[a, b, c, d, ...]`
-- Retract `d` returns `[a, b, c]`
doRetract :: (Var v, Ord loc) => Element v loc -> M v loc ()
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
            [ (b, v, inst $ Type.existential' (B.loc b) b' v)
            | Existential b'@(B.Recorded b) v <- discarded
            ]
          go (b, v, sa) = solveBlank b v sa
          inst = apply ctx
      Foldable.traverse_ go (solved ++ unsolved)
      setContext t

solved :: Context v loc -> [(v, Monotype v loc)]
solved (Context ctx) = [(v, sa) | (Solved _ v sa, _) <- ctx]

unsolved :: Context v loc -> [v]
unsolved (Context ctx) = [v | (Existential _ v, _) <- ctx]

replace :: (Var v, Ord loc) => Element v loc -> Context v loc -> Context v loc -> Context v loc
replace e focus ctx =
  let (l,mid,r) = breakAt e ctx
  in if null mid then ctx
     else l `mappend` focus `mappend` r

breakAt :: (Var v, Ord loc)
        => Element v loc
        -> Context v loc
        -> (Context v loc, [Element v loc], Context v loc)
breakAt m (Context xs) =
  let
    (r, l) = break (\(e,_) -> e === m) xs
  -- l is a suffix of xs and is already a valid context;
  -- r needs to be rebuilt
    Existential _ v === Existential _ v2 | v == v2 = True
    Universal v     === Universal v2 | v == v2 = True
    Marker v        === Marker v2 | v == v2 = True
    _ === _ = False
  in (Context (drop 1 l), fst <$> take 1 l, context . map fst $ reverse r)


-- | ordered Γ α β = True <=> Γ[α^][β^]
ordered :: (Var v, Ord loc) => Context v loc -> v -> v -> Bool
ordered ctx v v2 = Set.member v (existentials (retract' (existential v2) ctx))
  where
  -- | Like `retract`, but returns the empty context if retracting would remove all elements.
  retract' :: (Var v, Ord loc) => Element v loc -> Context v loc -> Context v loc
  retract' e ctx = maybe mempty fst $ retract0 e ctx

-- env0 :: Env v loc
-- env0 = Env 0 context0

debugEnabled :: Bool
debugEnabled = False

_logContext :: (Ord loc, Var v) => String -> M v loc ()
_logContext msg = when debugEnabled $ do
  ctx <- getContext
  let !_ = trace ("\n"++msg ++ ": " ++ show ctx) ()
  setContext ctx

usedVars :: Context v loc -> Set v
usedVars = allVars . info

fromMEnv :: (MEnv v loc -> a)
         -> MEnv v loc
         -> (Seq (Note v loc), Maybe (a, Env v loc))
fromMEnv f m = (mempty, pure (f m, env m))

getBuiltinLocation :: M v loc loc
getBuiltinLocation = M . fromMEnv $ builtinLocation

getAbilityCheckMask :: M v loc (Type v loc -> M v loc Bool)
getAbilityCheckMask = M . fromMEnv $ abilityCheckMask

getContext :: M v loc (Context v loc)
getContext = M . fromMEnv $ ctx . env

setContext :: Context v loc -> M v loc ()
setContext ctx = M (\menv -> let e = env menv in (mempty, pure ((), e {ctx = ctx})))

modifyContext :: (Context v loc -> M v loc (Context v loc)) -> M v loc ()
modifyContext f = do c <- getContext; c <- f c; setContext c

modifyContext' :: (Context v loc -> Context v loc) -> M v loc ()
modifyContext' f = modifyContext (pure . f)

appendContext :: (Var v, Ord loc) => Context v loc -> M v loc ()
appendContext tl = modifyContext' (<> tl)

universals :: Context v loc -> Set v
universals = universalVars . info

existentials :: Context v loc -> Set v
existentials = existentialVars . info

freshenVar :: Var v => v -> M v loc v
freshenVar v =
  M (\menv ->
       let e = env menv
           id = freshId e
       in (mempty, pure (Var.freshenId id v, e {freshId = id+1})))

freshenTypeVar :: Var v => TypeVar v loc -> M v loc v
freshenTypeVar v =
  M (\menv ->
       let e = env menv
           id = freshId e
       in (mempty, pure (Var.freshenId id (TypeVar.underlying v), e {freshId = id+1})))

freshNamed :: (Var v, Ord loc) => Text -> M v loc v
freshNamed = freshenVar . Var.named

-- | Check that the context is well formed, see Figure 7 of paper
-- Since contexts are 'monotonic', we can compute an cache this efficiently
-- as the context is built up, see implementation of `extend`.
wellformed :: Context v loc -> Bool
wellformed ctx = isWellformed (info ctx)

-- todo: do we want this to return a location for the aspect of the type that was not well formed
-- todo: or maybe a note / list of notes, or an M
-- | Check that the type is well formed wrt the given `Context`, see Figure 7 of paper
wellformedType :: Var v => Context v loc -> Type v loc -> Bool
wellformedType c t = wellformed c && case t of
  Type.Existential' _ v -> Set.member v (existentials c)
  Type.Universal' v -> Set.member v (universals c)
  Type.Ref' _ -> True
  Type.Arrow' i o -> wellformedType c i && wellformedType c o
  Type.Ann' t' _ -> wellformedType c t'
  Type.App' x y -> wellformedType c x && wellformedType c y
  Type.Effect1' e a -> wellformedType c e && wellformedType c a
  Type.Effects' es -> all (wellformedType c) es
  Type.Forall' t' ->
    let (v,ctx2) = extendUniversal c
    in wellformedType ctx2 (ABT.bind t' (Type.universal' (ABT.annotation t) v))
  _ -> error $ "Match failure in wellformedType: " ++ show t
  where
  -- | Extend this `Context` with a single variable, guaranteed fresh
  extendUniversal ctx = case Var.freshIn (usedVars ctx) (Var.named "var") of
    v -> (v, extend (Universal v) ctx)

-- | Return the `Info` associated with the last element of the context, or the zero `Info`.
info :: Context v loc -> Info v
info (Context []) = Info Set.empty Set.empty Set.empty True
info (Context ((_,i):_)) = i

-- | Add an element onto the end of this `Context`. Takes `O(log N)` time,
-- including updates to the accumulated `Info` value.
extend :: Var v => Element v loc -> Context v loc -> Context v loc
extend e c@(Context ctx) = Context ((e,i'):ctx) where
  i' = addInfo e (info c)
  -- see figure 7
  addInfo e (Info es us vs ok) = case e of
    Var v -> case v of
      -- UvarCtx - ensure no duplicates
      TypeVar.Universal v ->
        Info es (Set.insert v us) (Set.insert v vs) (ok && Set.notMember v us)
      -- EvarCtx - ensure no duplicates, and that this existential is not solved earlier in context
      TypeVar.Existential _ v ->
        Info (Set.insert v es) us (Set.insert v vs) (ok && Set.notMember v es)
    -- SolvedEvarCtx - ensure `v` is fresh, and the solution is well-formed wrt the context
    Solved _ v sa ->
      Info (Set.insert v es) us (Set.insert v vs)
           (ok && Set.notMember v es && wellformedType c (Type.getPolytype sa))
    -- VarCtx - ensure `v` is fresh, and annotation is well-formed wrt the context
    Ann v t ->
      Info es us (Set.insert v vs) (ok && Set.notMember v vs && wellformedType c t)
    -- MarkerCtx - note that since a Marker is always the first mention of a variable, suffices to
    -- just check that `v` is not previously mentioned
    Marker v -> Info es us (Set.insert v vs) (ok && Set.notMember v vs)

-- | doesn't combine notes
orElse :: M v loc a -> M v loc a -> M v loc a
orElse m1 m2 = M go where
  go menv = case runM m1 menv of
    r @ (_, Just (_, _)) -> r
    _ -> runM m2 menv

-- If the action fails, preserve all the notes it emitted and then return the
-- provided `a`; if the action succeeds, we're good, just use its result
recover :: a -> M v loc a -> M v loc a
recover ifFail (M action) = M go where
  go menv = case action menv of
    (notes, Nothing) -> (notes, Just (ifFail, env menv))
    r -> r

getDataDeclarations :: M v loc (DataDeclarations v loc)
getDataDeclarations = M $ fromMEnv dataDecls

getEffectDeclarations :: M v loc (EffectDeclarations v loc)
getEffectDeclarations = M $ fromMEnv effectDecls

getAbilities :: M v loc [Type v loc]
getAbilities = M $ fromMEnv abilities

-- run `m` without doing ability checks on requests which match `ambient0`
-- are a subtype of `ambient0`.
withoutAbilityCheckFor :: (Ord loc, Var v) => Type v loc -> M v loc a -> M v loc a
withoutAbilityCheckFor ambient0 m = do
  abilities <- filterM wouldNotCollide =<< getAbilities
  withEffects0 abilities m2
  where
    m2 = M (\menv -> runM m $ menv { abilityCheckMask = go (abilityCheckMask menv) })
    go mask t = (False <$ subtype ambient0 t) `orElse` mask t
    wouldNotCollide t = do
      ctx <- getContext
      ok <- (False <$ subtype ambient0 t) `orElse` pure True
      setContext ctx
      pure ok

compilerCrash :: CompilerBug v loc -> M v loc a
compilerCrash bug = failWith $ CompilerBug bug

failWith :: Cause v loc -> M v loc a
failWith cause = M (const (pure (Note cause mempty), Nothing))

failSecretlyAndDangerously :: M v loc a
failSecretlyAndDangerously = M (const (mempty, Nothing))

getDataDeclaration :: Reference -> M v loc (DataDeclaration' v loc)
getDataDeclaration r = do
  decls <- getDataDeclarations
  case Map.lookup r decls of
    Nothing -> compilerCrash (UnknownDecl Data r decls)
    Just decl -> pure decl

getEffectDeclaration :: Reference -> M v loc (EffectDeclaration' v loc)
getEffectDeclaration r = do
  decls <- getEffectDeclarations
  case Map.lookup r decls of
    Nothing -> compilerCrash (UnknownDecl Effect r (DD.toDataDecl <$> decls))
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
                    -> (Reference -> M v loc (DataDeclaration' v loc))
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
  modifyContext (pure . extend (Universal v'))
  pure v'

extendExistential :: (Var v) => v -> M v loc v
extendExistential v = do
  v' <- freshenVar v
  modifyContext (pure . extend (Existential B.Blank v'))
  pure v'

extendMarker :: (Var v, Ord loc) => v -> M v loc v
extendMarker v = do
  v' <- freshenVar v
  modifyContext (\ctx -> pure $ ctx <> context [Marker v', existential v'])
  pure v'

notMember :: (Var v, Ord loc) => v -> Set (TypeVar v loc) -> Bool
notMember v s =
  Set.notMember (TypeVar.Universal v) s &&
  Set.notMember (TypeVar.Existential B.Blank v) s

-- | Replace any existentials with their solution in the context
apply :: (Var v, Ord loc) => Context v loc -> Type v loc -> Type v loc
apply ctx t = case t of
  Type.Universal' _ -> t
  Type.Ref' _ -> t
  Type.Existential' _ v ->
    maybe t (\(Type.Monotype t') -> apply ctx t') (lookup v (solved ctx))
  Type.Arrow' i o -> Type.arrow a (apply ctx i) (apply ctx o)
  Type.App' x y -> Type.app a (apply ctx x) (apply ctx y)
  Type.Ann' v k -> Type.ann a (apply ctx v) k
  Type.Effect1' e t -> Type.effect1 a (apply ctx e) (apply ctx t)
  Type.Effects' es -> Type.effects a (map (apply ctx) es)
  Type.ForallNamed' v t' -> Type.forall a v (apply ctx t')
  _ -> error $ "Match error in Context.apply: " ++ show t
  where a = ABT.annotation t

loc :: ABT.Term f v loc -> loc
loc = ABT.annotation

-- Prepends the provided abilities onto the existing ambient for duration of `m`
_withEffects :: [Type v loc] -> M v loc a -> M v loc a
_withEffects abilities' m =
  M (\menv -> runM m (menv { abilities = abilities' ++ abilities menv }))

-- Replaces the ambient abilities with the provided for duration of `m`
withEffects0 :: [Type v loc] -> M v loc a -> M v loc a
withEffects0 abilities' m =
  M (\menv -> runM m (menv { abilities = abilities' }))


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
-- synthesizeApp ft arg | debugEnabled && traceShow ("synthesizeApp"::String, ft, arg) False = undefined
synthesizeApp (Type.Effect'' es ft) argp@(arg, argNum) =
  scope (InSynthesizeApp ft arg argNum) $ abilityCheck es >> go ft
  where
  go (Type.Forall' body) = do -- Forall1App
    v <- ABT.freshen body freshenTypeVar
    appendContext (context [existential v])
    let ft2 = ABT.bindInheritAnnotation body (Type.existential B.Blank v)
        vs es = [TypeVar.underlying v | Type.Var' v <- es ]
    -- todo: should do something different here if underapplied
    case Type.unArrows ft2 of
      Nothing -> synthesizeApp ft2 argp
      Just spine -> case reverse spine of
        Type.Effect' es _ : _ | v `elem` vs es -> do
          amb <- getAbilities
          instantiateL B.Blank v (Type.effects (loc ft) amb)
          synthesizeApp ft2 argp
        _ -> synthesizeApp ft2 argp
  go (Type.Arrow' i o) = do -- ->App
    let (es, _) = Type.stripEffect o
    abilityCheck es
    o <$ check arg i
  go (Type.Existential' b a) = do -- a^App
    [i,o] <- traverse freshenVar [ABT.v' "i", ABT.v' "o"]
    let it = Type.existential' (loc ft) B.Blank i
        ot = Type.existential' (loc ft) B.Blank o
        soln = Type.Monotype (Type.arrow (loc ft) it ot)
        ctxMid = context [existential o, existential i, Solved b a soln]
    modifyContext' $ replace (existential a) ctxMid
    synthesizeApp (Type.getPolytype soln) argp
  go _ = getContext >>= \ctx -> failWith $ TypeMismatch ctx
synthesizeApp _ _ = error "unpossible - Type.Effect'' pattern always succeeds"

-- For arity 3, creates the type `∀ a . a -> a -> a -> Sequence a`
-- For arity 2, creates the type `∀ a . a -> a -> Sequence a`
vectorConstructorOfArity :: (Var v, Ord loc) => Int -> M v loc (Type v loc)
vectorConstructorOfArity arity = do
  bl <- getBuiltinLocation
  let elementVar = Var.named "elem"
      args = replicate arity (bl, Type.var bl elementVar)
      resultType = Type.app bl (Type.vector bl) (Type.var bl elementVar)
      vt = Type.forall bl elementVar (Type.arrows args resultType)
  pure vt

-- | Synthesize the type of the given term, updating the context in the process.
-- | Figure 11 from the paper
synthesize :: forall v loc . (Var v, Ord loc) => Term v loc -> M v loc (Type v loc)
synthesize e = scope (InSynthesize e) $ do
  Type.Effect'' es t <- go (minimize' e)
  abilityCheck es
  pure t
  where
  l = loc e
  go :: (Var v, Ord loc) => Term v loc -> M v loc (Type v loc)
  go (Term.Var' v) = getContext >>= \ctx -> case lookupAnn ctx v of -- Var
    Nothing -> compilerCrash $ UndeclaredTermVariable v ctx
    Just t -> pure t
  go (Term.Blank' blank) = do
    v <- freshNamed "_"
    appendContext $ context [Existential blank v]
    pure $ Type.existential' l blank v -- forall (TypeVar.Universal v) (Type.universal v)
  go (Term.Ann' (Term.Ref' _) t) = case ABT.freeVars t of
    s | Set.null s ->
      -- innermost Ref annotation assumed to be correctly provided by `synthesizeClosed`
      pure t
    s -> compilerCrash $ FreeVarsInTypeAnnotation s
  go (Term.Ref' h) = compilerCrash $ UnannotatedReference h
  go (Term.Constructor' r cid) = getDataConstructorType r cid
  go (Term.Request' r cid) = ungeneralize =<< getEffectConstructorType r cid
  go (Term.Ann' e' t) = t <$ check e' t
  go (Term.Float' _) = pure $ Type.float l -- 1I=>
  go (Term.Int64' _) = pure $ Type.int64 l -- 1I=>
  go (Term.UInt64' _) = pure $ Type.uint64 l -- 1I=>
  go (Term.Boolean' _) = pure $ Type.boolean l
  go (Term.Text' _) = pure $ Type.text l
  go (Term.Apps' f args) = do -- ->EEEEE
    ft <- synthesize f
    ctx <- getContext
    scope (InSynthesizeApps f ft args) $ synthesizeApps (apply ctx ft) args
  go (Term.Vector' v) = do
    ft <- vectorConstructorOfArity (Foldable.length v)
    case Foldable.toList v of
      [] -> pure ft
      v1 : _ ->
        scope (InVectorApp (ABT.annotation v1)) $ synthesizeApps ft v
  go (Term.Let1' binding e) | Set.null (ABT.freeVars binding) = do
    -- special case when it is definitely safe to generalize - binding contains
    -- no free variables, i.e. `let id x = x in ...`
    abilities <- getAbilities
    t  <- synthesizeClosed' abilities binding
    v' <- ABT.freshen e freshenVar
    -- note: `Ann' (Ref'  _) t` synthesizes to `t`
    e  <- pure $ ABT.bindInheritAnnotation e (Term.ann () (Term.builtin() (Var.name v')) t)
    synthesize e
  go (Term.Let1' binding e) = do
    -- note: no need to freshen binding, it can't refer to v
    tbinding <- synthesize binding
    v' <- ABT.freshen e freshenVar
    appendContext (context [Ann v' tbinding])
    t <- synthesize (ABT.bindInheritAnnotation e (Term.var() v'))
    doRetract $ Ann v' tbinding
    pure t
  go (Term.Lam' body) = do -- ->I=> (Full Damas Milner rule)
    -- arya: are there more meaningful locations we could put into and pull out of the abschain?)
    -- TODO: slightly hacky, won't infer more than 2 effects
    [arg, i, e, e2, o] <- sequence [ ABT.freshen body freshenVar
                               , freshenVar (ABT.variable body)
                               , freshNamed "inferred-effect"
                               , freshNamed "inferred-effect"
                               , freshNamed "inferred-output" ]
    let it = Type.existential' l B.Blank i
        ot = Type.existential' l B.Blank o
        et = Type.existential' l B.Blank e
        et2 = Type.existential' l B.Blank e2
    appendContext $
      context [Marker i, existential i, existential e, existential e2, existential o, Ann arg it]
    body <- pure $ ABT.bindInheritAnnotation body (Term.var() arg)
    withEffects0 [et, et2] $ check body ot
    (_, _, ctx2) <- breakAt (Marker i) <$> getContext
    ctx <- getContext
    -- unsolved existentials get generalized to universals
    doRetract (Marker i)
    let es2 = [ et | (e,et) <- [(e,et),(e2,et2)], e `notElem` unsolved ctx]
    pure $ if null es2
      then generalizeExistentials ctx2 (Type.arrow l it ot)
      else generalizeExistentials ctx2 (Type.arrow l it (Type.effect l (apply ctx <$> es2) ot))
  go (Term.LetRecNamed' [] body) = synthesize body
  go (Term.LetRec' letrec) = do
    (marker, e) <- annotateLetRecBindings letrec
    t <- synthesize e
    (_, _, ctx2) <- breakAt marker <$> getContext
    generalizeExistentials ctx2 t <$ doRetract marker
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
    let outputType = Type.existential' l B.Blank outputTypev
    appendContext $ context [existential outputTypev]
    case cases of -- only relevant with 2 or more cases, but 1 is safe too.
      [] -> pure ()
      Term.MatchCase _ _ t : _ -> scope (InMatch (ABT.annotation t)) $
        Foldable.traverse_ (checkCase scrutineeType outputType) cases
    ctx <- getContext
    pure $ apply ctx outputType
  go h@(Term.Handle' _ _) = do
    o <- freshNamed "o"
    appendContext $ context [existential o]
    let ot = Type.existential' l B.Blank o
    check h ot
    ctx <- getContext
    pure (apply ctx ot)
  go _e = compilerCrash PatternMatchFailure

checkCase :: forall v loc . (Var v, Ord loc)
          => Type v loc
          -> Type v loc
          -> Term.MatchCase loc (Term v loc)
          -> M v loc ()
checkCase scrutineeType outputType (Term.MatchCase pat guard rhs) = do
  scrutineeType <- applyM scrutineeType
  outputType <- applyM outputType
  m <- freshNamed "check-case"
  appendContext $ context [Marker m]
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
  for_ guard' $ \g -> check g (Type.boolean (loc g))
  outputType <- applyM outputType
  check rhs' outputType
  doRetract $ Marker m

checkPattern
  :: (Var v, Ord loc)
  => Type v loc
  -> Pattern loc
  -> StateT [v] (M v loc) [(v, v)]
checkPattern scrutineeType0 p =
  lift (ungeneralize scrutineeType0) >>= \scrutineeType -> case p of
    Pattern.Unbound _    -> pure []
    Pattern.Var     _loc -> do
      v  <- getAdvance p
      v' <- lift $ freshenVar v
      lift . appendContext $ context [Ann v' scrutineeType]
      pure [(v, v')]
    -- TODO: provide a scope here for giving a good error message
    Pattern.Boolean loc _ ->
      lift $ subtype (Type.boolean loc) scrutineeType $> mempty
    Pattern.Int64 loc _ ->
      lift $ subtype (Type.int64 loc) scrutineeType $> mempty
    Pattern.UInt64 loc _ ->
      lift $ subtype (Type.uint64 loc) scrutineeType $> mempty
    Pattern.Float loc _ ->
      lift $ subtype (Type.float loc) scrutineeType $> mempty
    Pattern.Constructor loc ref cid args -> do
      dct  <- lift $ getDataConstructorType ref cid
      udct <- lift $ ungeneralize dct
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
      lift . appendContext $ context [Ann v' scrutineeType]
      ((v, v') :) <$> checkPattern scrutineeType p'
    Pattern.EffectPure loc p -> do
      vt <- lift $ do
        v <- freshNamed "v"
        e <- freshNamed "e"
        let vt = Type.existentialp loc v
        let et = Type.existentialp loc e
        appendContext $ context [existential v, existential e]
        subtype (Type.effectV loc (loc, et) (loc, vt)) scrutineeType
        applyM vt
      checkPattern vt p
    Pattern.EffectBind loc ref cid args k -> do
      -- scrutineeType should be a supertype of `Effect e vt`
      -- for fresh existentials `e` and `vt`
      e <- lift $ extendExistential (Var.named "ebind-e")
      v <- lift $ extendExistential (Var.named "ebind-v")
      let evt = Type.effectV loc (loc, Type.existentialp loc e)
                                 (loc, Type.existentialp loc v)
      lift $ subtype evt scrutineeType
      ect  <- lift $ getEffectConstructorType ref cid
      uect <- lift $ ungeneralize ect
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
    _ -> lift . compilerCrash $ MalformedPattern p
 where
  getAdvance p = do
    vs <- get
    case vs of
      []       -> lift $ compilerCrash (MalformedPattern p)
      (v : vs) -> do
        put vs
        pure v

applyM :: (Var v, Ord loc) => Type v loc -> M v loc (Type v loc)
applyM t = (`apply` t) <$> getContext

bindings :: Context v loc -> [(v, Type v loc)]
bindings (Context ctx) = [(v,a) | (Ann v a,_) <- ctx]

lookupAnn :: Eq v => Context v loc -> v -> Maybe (Type v loc)
lookupAnn ctx v = lookup v (bindings ctx)

lookupSolved :: Eq v => Context v loc -> v -> Maybe (Monotype v loc)
lookupSolved ctx v = lookup v (solved ctx)

-- | Synthesize and generalize the type of each binding in a let rec
-- and return the new context in which all bindings are annotated with
-- their type. Also returns the freshened version of `body` and a marker
-- which should be used to retract the context after checking/synthesis
-- of `body` is complete. See usage in `synthesize` and `check` for `LetRec'` case.
annotateLetRecBindings
  :: (Var v, Ord loc)
  => ((v -> M v loc v) -> M v loc ([(v, Term v loc)], Term v loc))
  -> M v loc (Element v loc, Term v loc)
annotateLetRecBindings letrec = do
  (bindings, body) <- letrec freshenVar
  let vs = map fst bindings
  -- generate a fresh existential variable `e1, e2 ...` for each binding
  es <- traverse freshenVar vs
  e1 <- freshNamed "bindings-start"
  ctx <- getContext
  -- Introduce these existentials into the context and
  -- annotate each term variable w/ corresponding existential
  -- [marker e1, 'e1, 'e2, ... v1 : 'e1, v2 : 'e2 ...]
  let f e (_,binding) = case binding of
        -- TODO: Think about whether `apply` here is always correct
        --       Used to have a guard that would only do this if t had no free vars
        Term.Ann' _ t -> apply ctx t
        _ -> Type.existential' (loc binding) B.Blank e
  let bindingTypes = zipWith f es bindings
  appendContext $ context (Marker e1 : map existential es ++ zipWith Ann vs bindingTypes)
  -- check each `bi` against `ei`; sequencing resulting contexts
  Foldable.for_ (zip bindings bindingTypes) $ \((_,b), t) -> check b t
  -- compute generalized types `gt1, gt2 ...` for each binding `b1, b2...`;
  -- add annotations `v1 : gt1, v2 : gt2 ...` to the context
  (_, _, ctx2) <- breakAt (Marker e1) <$> getContext
  let gen bindingType = generalizeExistentials ctx2 bindingType
      annotations = zipWith Ann vs (map gen bindingTypes)
  marker <- Marker <$> freshenVar (ABT.v' "let-rec-marker")
  doRetract (Marker e1)
  appendContext . context $ marker : annotations
  pure (marker, body)

ungeneralize :: (Var v, Ord loc) => Type v loc -> M v loc (Type v loc)
ungeneralize (Type.Forall' t) = do
  v <- ABT.freshen t freshenTypeVar
  appendContext $ context [existential v]
  t <- pure $ ABT.bindInheritAnnotation t (Type.existential B.Blank v)
  ungeneralize t
ungeneralize t = pure t

-- | Apply the context to the input type, then convert any unsolved existentials
-- to universals.
generalizeExistentials :: (Var v, Ord loc) => Context v loc -> Type v loc -> Type v loc
generalizeExistentials ctx t =
  foldr gen (apply ctx t) (unsolved ctx)
  where
    gen e t =
      if TypeVar.Existential B.Blank e `ABT.isFreeIn` t
      -- location of the forall is just the location of the input type
      -- and the location of each quantified variable is just inherited from
      -- its source location
      then Type.forall (loc t)
                       (TypeVar.Universal e)
                       (ABT.substInheritAnnotation
                           (TypeVar.Existential B.Blank e)
                           (Type.universal e)
                           t)
      else t -- don't bother introducing a forall if type variable is unused

-- | Check that under the given context, `e` has type `t`,
-- updating the context in the process.
check :: forall v loc . (Var v, Ord loc) => Term v loc -> Type v loc -> M v loc ()
-- check e t | debugEnabled && traceShow ("check"::String, e, t) False = undefined
check e0 t0 = scope (InCheck e0 t0) $ do
  ctx <- getContext
  let Type.Effect'' es t = t0
  let e = minimize' e0
  if wellformedType ctx t0
    then case t of
         -- expand existentials before checking
         t@(Type.Existential' _ _) -> abilityCheck es >> go e (apply ctx t)
         t -> go e t
    else failWith $ IllFormedType ctx
  where
    go :: Term v loc -> Type v loc -> M v loc ()
    go e (Type.Forall' body) = do -- ForallI
      x <- extendUniversal =<< ABT.freshen body freshenTypeVar
      check e (ABT.bindInheritAnnotation body (Type.universal x))
      doRetract $ Universal x
    go (Term.Lam' body) (Type.Arrow' i o) = do -- =>I
      x <- ABT.freshen body freshenVar
      modifyContext' (extend (Ann x i))
      let Type.Effect'' es ot = o
      withEffects0 es $ check (ABT.bindInheritAnnotation body (Term.var() x)) ot
      doRetract $ Ann x i
    go (Term.Let1' binding e) t = do
      v <- ABT.freshen e freshenVar
      tbinding <- synthesize binding
      modifyContext' (extend (Ann v tbinding))
      check (ABT.bindInheritAnnotation e (Term.var() v)) t
      doRetract $ Ann v tbinding
    go (Term.LetRecNamed' [] e) t = check e t
    go (Term.LetRec' letrec) t = do
      (marker, e) <- annotateLetRecBindings letrec
      check e t
      doRetract marker
    go block@(Term.Handle' h body) t = do
      -- `h` should check against `Effect e i -> t` (for new existentials `e` and `i`)
      -- `body` should check against `i`
      [e, i] <- sequence [freshNamed "e", freshNamed "i"]
      appendContext $ context [existential e, existential i]
      let l = loc block
      check h $ Type.arrow l (Type.effectV l (l, Type.existentialp l e) (l, Type.existentialp l i)) t
      ctx <- getContext
      let et = apply ctx (Type.existentialp l e)
      withoutAbilityCheckFor et $
        check body (apply ctx $ Type.existentialp l i)
    go e t = do -- Sub
      a <- synthesize e; ctx <- getContext
      subtype (apply ctx a) (apply ctx t)

-- | `subtype ctx t1 t2` returns successfully if `t1` is a subtype of `t2`.
-- This may have the effect of altering the context.
subtype :: forall v loc . (Var v, Ord loc) => Type v loc -> Type v loc -> M v loc ()
subtype tx ty | debugEnabled && traceShow ("subtype"::String, tx, ty) False = undefined
subtype tx ty = scope (InSubtype tx ty) $
  do ctx <- getContext; go (ctx :: Context v loc) tx ty
  where -- Rules from figure 9
  go :: Context v loc -> Type v loc -> Type v loc -> M v loc ()
  go _ (Type.Ref' r) (Type.Ref' r2) | r == r2 = pure () -- `Unit`
  go ctx t1@(Type.Universal' v1) t2@(Type.Universal' v2) -- `Var`
    | v1 == v2 && wellformedType ctx t1 && wellformedType ctx t2
    = pure ()
  go ctx t1@(Type.Existential' _ v1) t2@(Type.Existential' _ v2) -- `Exvar`
    | v1 == v2 && wellformedType ctx t1 && wellformedType ctx t2
    = pure ()
  go _ (Type.Arrow' i1 o1) (Type.Arrow' i2 o2) = do -- `-->`
    subtype i1 i2; ctx' <- getContext
    subtype (apply ctx' o1) (apply ctx' o2)
  go _ (Type.App' x1 y1) (Type.App' x2 y2) = do -- analogue of `-->`
    subtype x1 x2; ctx' <- getContext
    subtype (apply ctx' y1) (apply ctx' y2)
  go _ t (Type.Forall' t2) = do
    v' <- extendUniversal =<< ABT.freshen t2 freshenTypeVar
    t2 <- pure $ ABT.bindInheritAnnotation t2 (Type.universal v')
    subtype t t2
    doRetract (Universal v')
  go _ (Type.Forall' t) t2 = do
    v <- extendMarker =<< ABT.freshen t freshenTypeVar
    t <- pure $ ABT.bindInheritAnnotation t (Type.existential B.Blank v)
    ctx' <- getContext
    subtype (apply ctx' t) t2
    doRetract (Marker v)
  go _ (Type.Effect1' e1 a1) (Type.Effect1' e2 a2) = do
    subtype e1 e2
    ctx <- getContext
    subtype (apply ctx a1) (apply ctx a2)
  go _ a (Type.Effect1' _e2 a2) = subtype a a2
  go ctx (Type.Existential' b v) t -- `InstantiateL`
    | Set.member v (existentials ctx) && notMember v (Type.freeVars t) =
    instantiateL b v t
  go ctx t (Type.Existential' b v) -- `InstantiateR`
    | Set.member v (existentials ctx) && notMember v (Type.freeVars t) =
    instantiateR t b v
  go _ (Type.Effects' es1) (Type.Effects' es2) = do
     ctx <- getContext
     let es1' = map (apply ctx) es1
         es2' = map (apply ctx) es2
     abilityCheck' es2' es1'
  go _ t t2@(Type.Effects' _) | expand t  = subtype (Type.effects (loc t) [t]) t2
  go _ t@(Type.Effects' _) t2 | expand t2 = subtype t (Type.effects (loc t2) [t2])
  go ctx _ _ = failWith $ TypeMismatch ctx

  expand :: Type v loc -> Bool
  expand t = case t of
    Type.Existential' _ _ -> True
    Type.App' _ _ -> True
    Type.Ref' _ -> True
    _ -> False


-- | Instantiate the given existential such that it is
-- a subtype of the given type, updating the context
-- in the process.
instantiateL :: (Var v, Ord loc) => B.Blank loc -> v -> Type v loc -> M v loc ()
instantiateL _ v t | debugEnabled && traceShow ("instantiateL"::String, v, t) False = undefined
instantiateL blank v t = scope (InInstantiateL v t) $ do
  getContext >>= \ctx -> case Type.monotype t >>= solve ctx v of
    Just ctx -> setContext ctx -- InstLSolve
    Nothing | not (v `elem` unsolved ctx) -> failWith $ TypeMismatch ctx
    Nothing -> case t of
      Type.Existential' _ v2 | ordered ctx v v2 -> -- InstLReach (both are existential, set v2 = v)
        maybe (failWith $ TypeMismatch ctx) setContext $
          solve ctx v2 (Type.Monotype (Type.existentialp (loc t) v))
      Type.Arrow' i o -> do -- InstLArr
        [i',o'] <- traverse freshenVar [ABT.v' "i", ABT.v' "o"]
        let s = Solved blank v (Type.Monotype (Type.arrow (loc t)
                                                 (Type.existentialp (loc i) i')
                                                 (Type.existentialp (loc o) o')))
        modifyContext' $ replace (existential v)
                                 (context [existential o', existential i', s])
        instantiateR i B.Blank i' -- todo: not sure about this, could also be `blank`
        ctx <- getContext
        instantiateL B.Blank o' (apply ctx o)
      Type.App' x y -> do -- analogue of InstLArr
        [x', y'] <- traverse freshenVar [ABT.v' "x", ABT.v' "y"]
        let s = Solved blank v (Type.Monotype (Type.app (loc t)
                                                  (Type.existentialp (loc x) x')
                                                  (Type.existentialp (loc y) y')))
        modifyContext' $ replace (existential v)
                                 (context [existential y', existential x', s])
        ctx0 <- getContext
        ctx' <- instantiateL B.Blank x' (apply ctx0 x) >> getContext
        instantiateL B.Blank y' (apply ctx' y)
      Type.Effect1' es vt -> do
        es' <- freshNamed "effect1-e"
        vt' <- freshNamed "vt"
        let t = Type.effect1 (loc t) (Type.existentialp (loc es) es')
                                     (Type.existentialp (loc vt) vt')
            s = Solved blank v (Type.Monotype t)
        modifyContext' $ replace (existential v)
                         (context [existential es', existential vt', s])
        ctx <- getContext
        instantiateL B.Blank vt' (apply ctx vt)
      Type.Effects' es -> do
        es' <- replicateM (length es) (freshNamed "e")
        let locs = loc <$> es
            t = Type.effects (loc t) (uncurry Type.existentialp <$> locs `zip` es')
            s = Solved blank v $ Type.Monotype t
        modifyContext' $ replace (existential v)
                                 (context $ (existential <$> es') ++ [s])
        Foldable.for_ (es' `zip` es) $ \(e',e) -> do
          ctx <- getContext
          instantiateL B.Blank e' (apply ctx e)
      Type.Forall' body -> do -- InstLIIL
        v <- extendUniversal =<< ABT.freshen body freshenTypeVar
        instantiateL B.Blank v (ABT.bindInheritAnnotation body (Type.universal v))
        doRetract (Universal v)
      _ -> failWith $ TypeMismatch ctx

-- | Instantiate the given existential such that it is
-- a supertype of the given type, updating the context
-- in the process.
instantiateR :: (Var v, Ord loc) => Type v loc -> B.Blank loc -> v -> M v loc ()
instantiateR t _ v | debugEnabled && traceShow ("instantiateR"::String, t, v) False = undefined
instantiateR t blank v = scope (InInstantiateR t v) $
  getContext >>= \ctx -> case Type.monotype t >>= solve ctx v of
    Just ctx -> setContext ctx -- InstRSolve
    Nothing | not (v `elem` unsolved ctx) -> failWith $ TypeMismatch ctx
    Nothing -> case t of
      Type.Existential' _ v2 | ordered ctx v v2 -> -- InstRReach (both are existential, set v2 = v)
        maybe (failWith $ TypeMismatch ctx) setContext $
          solve ctx v2 (Type.Monotype (Type.existentialp (loc t) v))
      Type.Arrow' i o -> do -- InstRArrow
        [i', o'] <- traverse freshenVar [ABT.v' "i", ABT.v' "o"]
        let s = Solved blank v (Type.Monotype
                          (Type.arrow (loc t)
                            (Type.existentialp (loc i) i')
                            (Type.existentialp (loc o) o')))
        modifyContext' $ replace (existential v)
                                 (context [existential o', existential i', s])
        ctx <- instantiateL B.Blank i' i >> getContext
        instantiateR (apply ctx o) B.Blank o'
      Type.App' x y -> do -- analogue of InstRArr
        -- example foo a <: v' will
        -- 1. create foo', a', add these to the context
        -- 2. add v' = foo' a' to the context
        -- 3. recurse to refine the types of foo' and a'
        [x', y'] <- traverse freshenVar [ABT.v' "instR-x", ABT.v' "instR-y"]
        let s = Solved blank v (Type.Monotype (Type.app (loc t) (Type.existentialp (loc x) x') (Type.existentialp (loc y) y')))
        modifyContext' $ replace (existential v) (context [existential y', existential x', s])
        ctx <- getContext
        instantiateR (apply ctx x) B.Blank x'
        ctx <- getContext
        instantiateR (apply ctx y) B.Blank y'
      Type.Effect1' es vt -> do
        es' <- freshNamed "e"
        vt' <- freshNamed "vt"
        let t = Type.effect1 (loc t) (Type.existentialp (loc es) es')
                                     (Type.existentialp (loc vt) vt')
            s = Solved blank v (Type.Monotype t)
        modifyContext' $ replace (existential v)
                         (context [existential es', existential vt', s])
        ctx <- getContext
        instantiateR (apply ctx vt) B.Blank vt'
      Type.Effects' es -> do
        es' <- replicateM (length es) (freshNamed "e")
        let locs = loc <$> es
            t = Type.effects (loc t) (uncurry Type.existentialp <$> locs `zip` es')
            s = Solved blank v $ Type.Monotype t
        modifyContext' $ replace (existential v)
                                 (context $ (existential <$> es') ++ [s])
        Foldable.for_ (es `zip` es') $ \(e, e') -> do
          ctx <- getContext
          instantiateR (apply ctx e) B.Blank e'
      Type.Forall' body -> do -- InstRAIIL
        x' <- ABT.freshen body freshenTypeVar
        setContext $ ctx `mappend` context [Marker x', existential x']
        instantiateR (ABT.bindInheritAnnotation body (Type.existential B.Blank x')) B.Blank v
        doRetract (Marker x')
      _ -> failWith $ TypeMismatch ctx

-- | solve (ΓL,α^,ΓR) α τ = (ΓL,α^ = τ,ΓR)
-- If the given existential variable exists in the context,
-- we solve it to the given monotype, otherwise return `Nothing`
solve :: (Var v, Ord loc) => Context v loc -> v -> Monotype v loc -> Maybe (Context v loc)
solve ctx v t
  -- okay to solve something again if it's to an identical type
  | v `elem` (map fst (solved ctx)) = same =<< lookup v (solved ctx)
  where same t2 | apply ctx (Type.getPolytype t) == apply ctx (Type.getPolytype t2) = Just ctx
                | otherwise = Nothing
solve ctx v t
  | wellformedType ctxL (Type.getPolytype t) = Just ctx'
  | otherwise                                = Nothing
  where (ctxL, focus, ctxR) = breakAt (existential v) ctx
        mid = [ Solved blank v t | Existential blank v <- focus ]
        ctx' = ctxL `mappend` context mid `mappend` ctxR

abilityCheck' :: (Var v, Ord loc) => [Type v loc] -> [Type v loc] -> M v loc ()
abilityCheck' [] [] = pure ()
abilityCheck' ambient requested = do
  -- let !_ = traceShow ("ambient" :: String, ambient, "requested" :: String, requested) ()
  -- if requested is an existential that is unsolved, go ahead and unify that w/ all of ambient
  ctx <- getContext
  let es = [ Type.existential' (loc t) b v
           | t@(Type.Existential' b v) <- apply ctx <$> requested ]
  case es of
    h : _t -> subtype h (Type.effects (loc h) ambient)
    [] -> do
      success <- flip allM requested $ \req -> do
        -- NB - if there's an exact match, use that
        let toCheck = maybe ambient pure $ find (== req) ambient
        ok <- flip anyM toCheck $ \amb -> (True <$ subtype amb req) `orElse` pure False
        pure ok
      when (not success) $ do
        ctx <- getContext
        failWith $ AbilityCheckFailure (apply ctx <$> ambient)
                                       (apply ctx <$> requested)
                                       ctx

abilityCheck :: (Var v, Ord loc) => [Type v loc] -> M v loc ()
abilityCheck requested = do
  enabled <- getAbilityCheckMask
  ambient <- getAbilities
  requested' <- filterM enabled requested
  ctx <- getContext
  abilityCheck' (apply ctx <$> ambient >>= Type.flattenEffects)
                (apply ctx <$> requested' >>= Type.flattenEffects)

verifyDataDeclarations :: (Var v, Ord loc) => DataDeclarations v loc -> M v loc ()
verifyDataDeclarations decls = forM_ (Map.toList decls) $ \(_ref, decl) -> do
  let ctors = DD.constructors decl
  forM_ ctors $ \(_ctorName,typ) -> verifyClosed typ id

-- | public interface to the typechecker
synthesizeClosed
  :: (Monad f, Var v, Ord loc)
  => loc
  -> [Type v loc]
  -> (Reference -> f (Type.AnnotatedType v loc))
  -> (Reference -> f (DataDeclaration' v loc))
  -> (Reference -> f (EffectDeclaration' v loc))
  -> Term v loc
  -> f (Result v loc (Type v loc))
synthesizeClosed builtinLoc abilities synthRef lookupData lookupEffect term = do
  let dataRefs = Set.toList $ Term.referencedDataDeclarations term
      effectRefs = Set.toList $ Term.referencedEffectDeclarations term
  term <- annotateRefs synthRef term
  datas <- Map.fromList <$> traverse (\r -> (r,) <$> lookupData r) dataRefs
  effects <- Map.fromList <$> traverse (\r -> (r,) <$> lookupEffect r) effectRefs
  pure . run builtinLoc [] datas effects $ do
    verifyDataDeclarations datas
    verifyDataDeclarations (DD.toDataDecl <$> effects)
    verifyClosedTerm term
    synthesizeClosed' abilities term

verifyClosedTerm :: forall v loc . Ord v => Term v loc -> M v loc ()
verifyClosedTerm t = do
  ok1 <- verifyClosed t id
  ok2 <- all id <$> ABT.foreachSubterm go t
  if ok1 && ok2
     then pure ()
     else failSecretlyAndDangerously
  where
    go :: Term v loc -> M v loc Bool
    go (Term.Ann' _ t) = verifyClosed t TypeVar.underlying
    go _ = pure True

verifyClosed :: (Traversable f, Ord v) => ABT.Term f v a -> (v -> v2) -> M v2 a Bool
verifyClosed t toV2 =
  let isBoundIn v t = Set.member v (snd (ABT.annotation t))
      loc t = fst (ABT.annotation t)
      go t@(ABT.Var' v) | not (isBoundIn v t) = recover False $ failWith (UnknownSymbol (loc t) $ toV2 v)
      go _ = pure True
  in all id <$> ABT.foreachSubterm go (ABT.annotateBound t)

annotateRefs :: (Applicative f, Ord v)
             => (Reference -> f (Type.AnnotatedType v loc))
             -> Term v loc
             -> f (Term v loc)
annotateRefs synth = ABT.visit f where
  -- already annotated; skip this subtree
  f r@(Term.Ann' (Term.Ref' _) _) = Just (pure r)
  f r@(Term.Ref' h) = Just (Term.ann ra (Term.ref ra h) <$> (ABT.vmap TypeVar.Universal <$> synth h))
    where ra = ABT.annotation r
  f _ = Nothing

run :: (Var v, Ord loc)
    => loc
    -> [Type v loc]
    -> DataDeclarations v loc
    -> EffectDeclarations v loc
    -> M v loc a
    -> Result v loc a
run builtinLoc ambient datas effects m =
  let (notes, o) = runM m (MEnv (Env 0 mempty) ambient builtinLoc datas effects (const $ pure True))
  in (notes, fst <$> o)

synthesizeClosed' :: (Var v, Ord loc)
                  => [Type v loc]
                  -> Term v loc
                  -> M v loc (Type v loc)
synthesizeClosed' abilities term = do
  -- save current context, for restoration when done
  ctx0 <- getContext
  setContext $ context []
  v <- extendMarker $ Var.named "start"
  t <- withEffects0 abilities (synthesize term)
  ctx <- getContext
  -- retract will cause notes to be written out for
  -- any `Blank`-tagged existentials passing out of scope
  doRetract (Marker v)
  setContext ctx0 -- restore the initial context
  pure $ generalizeExistentials ctx t

-- Check if `t1` is a subtype of `t2`. Doesn't update the typechecking context.
isSubtype' :: (Var v, Ord loc) => Type v loc -> Type v loc -> M v loc Bool
isSubtype' type1 type2 = do
  let vars = Set.union (ABT.freeVars type1) (ABT.freeVars type2)
  appendContext $ context (Var <$> Set.toList vars)
  succeeds $ subtype type1 type2
  where
    succeeds :: M v loc a -> M v loc Bool
    succeeds m = do
      e <- ask
      let (_, r) = runM m e
      pure $ isJust r

-- Public interface to `isSubtype`
isSubtype
  :: (Var v, Ord loc)
  => loc
  -> Type v loc
  -> Type v loc
  -> Result v loc Bool
isSubtype builtinLoc t1 t2 = fmap fst <$> runM
  (isSubtype' t1 t2)
  (MEnv (Env 0 mempty) [] builtinLoc Map.empty Map.empty (const $ pure True))

instance (Var v) => Show (Element v loc) where
  show (Var v) = case v of
    TypeVar.Universal x -> "@" <> show x
    TypeVar.Existential _ x -> "'" ++ show x
  show (Solved _ v t) = "'"++Text.unpack (Var.shortName v)++" = "++show t
  show (Ann v t) = Text.unpack (Var.shortName v) ++ " : " ++ show t
  show (Marker v) = "|"++Text.unpack (Var.shortName v)++"|"

instance (Ord loc, Var v) => Show (Context v loc) where
  show ctx@(Context es) = "Γ\n  " ++ (intercalate "\n  " . map (showElem ctx . fst)) (reverse es)
    where
    showElem _ctx (Var v) = case v of
      TypeVar.Universal x -> "@" <> show x
      TypeVar.Existential _ x -> "'" ++ show x
    showElem ctx (Solved _ v (Type.Monotype t)) = "'"++Text.unpack (Var.shortName v)++" = "++ show (apply ctx t)
    showElem ctx (Ann v t) = Text.unpack (Var.shortName v) ++ " : " ++ show (apply ctx t)
    showElem _ (Marker v) = "|"++Text.unpack (Var.shortName v)++"|"

-- MEnv v loc -> (Seq (Note v loc), (a, Env v loc))
instance Monad (M v loc) where
  return a = M (\menv -> (mempty, pure (a, env menv)))
  m >>= f = M go where
    go menv = let
      (notes1, aenv1) = runM m menv
      in case aenv1 of
        Nothing -> (notes1, Nothing)
        Just (a, env1) ->
          let (notes2, x) = runM (f a) (menv { env = env1 })
          in (notes1 `mappend` notes2, x)

instance Applicative (M v loc) where
  pure = return
  (<*>) = ap

instance Functor (M v loc) where
  fmap = liftM

instance (Var v, Ord loc) => Monoid (Context v loc) where
  mempty = context0
  mappend ctxL (Context es) =
    -- since `es` is a snoc list, we add it to `ctxL` in reverse order
    foldl' f ctxL (reverse es) where
      f ctx (e,_) = extend e ctx

instance (Var v, Ord loc) => Semigroup (Context v loc) where
  (<>) = mappend

instance MonadReader (MEnv v loc) (M v loc) where
  ask = M (\e -> (mempty, Just (e, env e)))
  local f m = M $ runM m . f
