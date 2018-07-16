{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Typechecker.Context2 where

import Data.Sequence (Seq)

import           Control.Monad
import           Control.Monad.Loops (anyM, allM)
-- import           Control.Monad.State
import qualified Data.Foldable as Foldable
import Data.Maybe
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Debug.Trace
import qualified Unison.ABT as ABT
import           Unison.DataDeclaration (DataDeclaration', EffectDeclaration')
import qualified Unison.DataDeclaration as DD
-- import           Unison.Note (Note,Noted(..))
-- import qualified Unison.Note as Note
-- import           Unison.Pattern (Pattern)
-- import qualified Unison.Pattern as Pattern
import           Unison.Reference (Reference)
import qualified Unison.Term as Term
import           Unison.Term (AnnotatedTerm')
import           Unison.Type (AnnotatedType)
import qualified Unison.Type as Type
import           Unison.TypeVar (TypeVar)
import qualified Unison.TypeVar as TypeVar
-- import           Unison.Typechecker.Components (minimize')
import           Unison.Var (Var)
import qualified Unison.Var as Var

type Type v loc = AnnotatedType (TypeVar v) loc
type Term v loc = AnnotatedTerm' (TypeVar v) v loc
type Monotype v loc = Type.Monotype (TypeVar v) loc

pattern Universal v <- Var (TypeVar.Universal v) where
  Universal v = Var (TypeVar.Universal v)

pattern Existential v <- Var (TypeVar.Existential v) where
  Existential v = Var (TypeVar.Existential v)

-- | Elements of an ordered algorithmic context
data Element v loc
  = Var (TypeVar v)        -- A variable declaration
  | Solved v (Monotype v loc)  -- `v` is solved to some monotype
  | Ann v (Type v loc)         -- `v` has type `a`, which may be quantified
  | Marker v deriving (Eq) -- used for scoping

data Env v loc = Env { freshId :: Word, ctx :: Context v loc }

type DataDeclarations v loc = Map Reference (DataDeclaration' v loc)
type EffectDeclarations v loc = Map Reference (EffectDeclaration' v loc)

-- | Typechecking monad
newtype M v loc a = M {
  runM :: MEnv v loc -> (Seq (Note v loc), Maybe (a, Env v loc))
}

data Unknown = Data | Effect

data CompilerBug v loc
  = UnknownDecl Unknown Reference (Map Reference (DataDeclaration' v loc))
  | UnknownConstructor Unknown Reference Int (DataDeclaration' v loc)
  | RetractFailure (Element v loc) (Context v loc)

data Note v loc
  = WithinSynthesize (Term v loc) (Note v loc)
  | WithinSubtype (Type v loc) (Type v loc) (Note v loc)
  | WithinCheck (Term v loc) (Type v loc) (Note v loc)
  | TypeMismatch
  | CompilerBug (CompilerBug v loc)
  | AbilityCheckFailure [Type v loc] [Type v loc] -- ambient, requested

withinSynthesize :: Term v loc -> M v loc a -> M v loc a
withinSynthesize t (M m) = M go where
  go menv =
    let (notes, r) = m menv
    in (WithinSynthesize t <$> notes, r)

withinSubtype :: Type v loc -> Type v loc -> M v loc a -> M v loc a
withinSubtype t1 t2 (M m) = M go where
  go menv =
    let (notes, r) = m menv
    in (WithinSubtype t1 t2 <$> notes, r)

withinCheck :: Term v loc -> Type v loc -> M v loc a -> M v loc a
withinCheck e t2 (M m) = M go where
  go menv =
    let (notes, r) = m menv
    in (WithinCheck e t2 <$> notes, r)

-- | The typechecking environment
data MEnv v loc = MEnv {
  env :: Env v loc,                    -- The typechecking state
  abilities :: [Type v loc],           -- Allowed ambient abilities
  dataDecls :: DataDeclarations v loc, -- Data declarations in scope
  effectDecls :: EffectDeclarations v loc, -- Effect declarations in scope
  abilityChecks :: Bool            -- Whether to perform ability checks.
                                   --   It's here so we can disable it during
                                   --   effect inference.
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
context xs = foldl' (flip extend) context0 xs

-- | Delete from the end of this context up to and including
-- the given `Element`. Returns `Nothing` if the element is not found.
retract :: (Eq loc, Var v) => Element v loc -> Context v loc -> Maybe (Context v loc)
retract e (Context ctx) =
  let maybeTail [] = Nothing
      maybeTail (_:t) = pure t
  -- note: no need to recompute used variables; any suffix of the
  -- context snoc list is also a valid context
  in Context <$> maybeTail (dropWhile (\(e',_) -> e' /= e) ctx)

-- | Delete from the end of this context up to and including
-- the given `Element`.
doRetract :: (Eq loc, Var v) => Element v loc -> M v loc ()
doRetract e = do
  ctx <- getContext
  case retract e ctx of
    Nothing -> compilerCrash (RetractFailure e ctx)
    Just t -> setContext t

-- | Like `retract`, but returns the empty context if retracting would remove all elements.
retract' :: (Eq loc, Var v) => Element v loc -> Context v loc -> Context v loc
retract' e ctx = fromMaybe mempty $ retract e ctx

solved :: Context v loc -> [(v, Monotype v loc)]
solved (Context ctx) = [(v, sa) | (Solved v sa, _) <- ctx]

unsolved :: Context v loc -> [v]
unsolved (Context ctx) = [v | (Existential v, _) <- ctx]

replace :: Var v => Element v loc -> Context v loc -> Context v loc -> Context v loc
replace e focus ctx =
  let (l,r) = breakAt e ctx
  in l `mappend` focus `mappend` r

breakAt :: Var v => Element v loc -> Context v loc -> (Context v loc, Context v loc)
breakAt m (Context xs) =
  let
    (r, l) = break (\(e,_) -> e === m) xs
  -- l is a suffix of xs and is already a valid context;
  -- r needs to be rebuilt
    Existential v === Existential v2 | v == v2 = True
    Universal v   === Universal v2 | v == v2 = True
    Marker v      === Marker v2 | v == v2 = True
    _ === _ = False
  in (Context (drop 1 l), context . map fst $ reverse r)


-- | ordered Γ α β = True <=> Γ[α^][β^]
ordered :: (Eq loc, Var v) => Context v loc -> v -> v -> Bool
ordered ctx v v2 = Set.member v (existentials (retract' (Existential v2) ctx))

env0 :: Env v loc
env0 = Env 0 context0

debugEnabled :: Bool
debugEnabled = False

logContext :: (Show loc, Var v) => String -> M v loc ()
logContext msg = when debugEnabled $ do
  ctx <- getContext
  let !_ = trace ("\n"++msg ++ ": " ++ show ctx) ()
  setContext ctx

usedVars :: Context v loc -> Set v
usedVars = allVars . info

fromMEnv :: (MEnv v loc -> a)
         -> MEnv v loc
         -> (Seq (Note v loc), Maybe (a, Env v loc))
fromMEnv f m = (mempty, pure (f m, env m))

getContext :: M v loc (Context v loc)
getContext = M . fromMEnv $ ctx . env

setContext :: Context v loc -> M v loc ()
setContext ctx = M (\menv -> let e = env menv in (mempty, pure ((), e {ctx = ctx})))

modifyContext :: (Context v loc -> M v loc (Context v loc)) -> M v loc ()
modifyContext f = do c <- getContext; c <- f c; setContext c

modifyContext' :: (Context v loc -> Context v loc) -> M v loc ()
modifyContext' f = modifyContext (pure . f)

appendContext :: Var v => Context v loc -> M v loc ()
appendContext tl = modifyContext' (\ctx -> ctx `mappend` tl)

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

freshenTypeVar :: Var v => TypeVar v -> M v loc v
freshenTypeVar v =
  M (\menv ->
       let e = env menv
           id = freshId e
       in (mempty, pure (Var.freshenId id (TypeVar.underlying v), e {freshId = id+1})))

freshNamed :: Var v => Text -> M v loc v
freshNamed = freshenVar . Var.named

freshVar :: Var v => M v loc v
freshVar = freshNamed "v"

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
  Type.Existential' v -> Set.member v (existentials c)
  Type.Universal' v -> Set.member v (universals c)
  Type.Ref' _ -> True
  Type.Arrow' i o -> wellformedType c i && wellformedType c o
  Type.Ann' t' _ -> wellformedType c t'
  Type.App' x y -> wellformedType c x && wellformedType c y
  Type.Effect' es a -> all (wellformedType c) es && wellformedType c a
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
      TypeVar.Universal v -> Info es (Set.insert v us) (Set.insert v vs) (ok && Set.notMember v us)
      -- EvarCtx - ensure no duplicates, and that this existential is not solved earlier in context
      TypeVar.Existential v -> Info (Set.insert v es) us (Set.insert v vs) (ok && Set.notMember v es)
    -- SolvedEvarCtx - ensure `v` is fresh, and the solution is well-formed wrt the context
    Solved v sa -> Info (Set.insert v es) us (Set.insert v vs) (ok && Set.notMember v es
                                                                      && wellformedType c (Type.getPolytype sa))
    -- VarCtx - ensure `v` is fresh, and annotation is well-formed wrt the context
    Ann v t -> Info es us (Set.insert v vs) (ok && Set.notMember v vs && wellformedType c t)
    -- MarkerCtx - note that since a Marker is always the first mention of a variable, suffices to
    -- just check that `v` is not previously mentioned
    Marker v -> Info es us (Set.insert v vs) (ok && Set.notMember v vs)

-- | doesn't combine notes
orElse :: M v loc a -> M v loc a -> M v loc a
orElse m1 m2 = M go where
  go menv = case runM m1 menv of
    r @ (_, Just (_, _)) -> r
    _ -> runM m2 menv

getDataDeclarations :: M v loc (DataDeclarations v loc)
getDataDeclarations = M $ fromMEnv dataDecls

getEffectDeclarations :: M v loc (EffectDeclarations v loc)
getEffectDeclarations = M $ fromMEnv effectDecls

getAbilities :: M v loc [Type v loc]
getAbilities = M $ fromMEnv abilities

abilityCheckEnabled :: M v loc Bool
abilityCheckEnabled = M $ fromMEnv abilityChecks

withoutAbilityCheck :: M v loc a -> M v loc a
withoutAbilityCheck m = M (\menv -> runM m $ menv { abilityChecks = False })

compilerCrash :: CompilerBug v loc -> M v loc a
compilerCrash bug = failNote $ CompilerBug bug

failNote :: Note v loc -> M v loc a
failNote note = M (\_ -> (pure note, Nothing))

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

getDataConstructorType :: Var v => Reference -> Int -> M v loc (Type v loc)
getDataConstructorType = getConstructorType' Data getDataDeclaration

getEffectConstructorType :: Var v => Reference -> Int -> M v loc (Type v loc)
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

extendUniversal :: Var v => v -> M v loc v
extendUniversal v = do
  v' <- freshenVar v
  modifyContext (pure . extend (Universal v'))
  pure v'

extendMarker :: Var v => v -> M v loc v
extendMarker v = do
  v' <- freshenVar v
  modifyContext (\ctx -> pure $ ctx `mappend`
    (context [Marker v', Existential v']))
  pure v'

notMember :: Var v => v -> Set (TypeVar v) -> Bool
notMember v s = Set.notMember (TypeVar.Universal v) s && Set.notMember (TypeVar.Existential v) s

-- | Replace any existentials with their solution in the context
apply :: Var v => Context v loc -> Type v loc -> Type v loc
apply ctx t = case t of
  Type.Universal' _ -> t
  Type.Ref' _ -> t
  Type.Existential' v ->
    maybe t (\(Type.Monotype t') -> apply ctx t') (lookup v (solved ctx))
  Type.Arrow' i o -> Type.arrow a (apply ctx i) (apply ctx o)
  Type.App' x y -> Type.app a (apply ctx x) (apply ctx y)
  Type.Ann' v k -> Type.ann a (apply ctx v) k
  Type.Effect' es t -> Type.effect a (map (apply ctx) es) (apply ctx t)
  Type.ForallNamed' v t' -> Type.forall a v (apply ctx t')
  _ -> error $ "Match error in Context.apply: " ++ show t
  where a = ABT.annotation t

loc :: ABT.Term f v loc -> loc
loc = ABT.annotation

-- Prepends the provided abilities onto the existing ambient for duration of `m`
withEffects :: [Type v loc] -> M v loc a -> M v loc a
withEffects abilities' m =
  M (\menv -> runM m (menv { abilities = abilities' ++ abilities menv }))

-- Replaces the ambient abilities with the provided for duration of `m`
withEffects0 :: [Type v loc] -> M v loc a -> M v loc a
withEffects0 abilities' m =
  M (\menv -> runM m (menv { abilities = abilities' }))

synthesize :: Var v => Term v loc -> M v loc (Type v loc)
synthesize = error "synthesize todo"

-- | Synthesize and generalize the type of each binding in a let rec
-- and return the new context in which all bindings are annotated with
-- their type. Also returns the freshened version of `body` and a marker
-- which should be used to retract the context after checking/synthesis
-- of `body` is complete. See usage in `synthesize` and `check` for `LetRec'` case.
annotateLetRecBindings
  :: Var v => ((v -> M v loc v) -> M v loc ([(v, Term v loc)], Term v loc))
           -> M v loc (Element v loc, Term v loc)
annotateLetRecBindings letrec =
  error "todo"
  --do
  --(bindings, body) <- letrec freshenVar
  --let vs = map fst bindings
  ---- generate a fresh existential variable `e1, e2 ...` for each binding
  --es <- traverse freshenVar vs
  --ctx <- getContext
  --e1 <- if null vs then fail "impossible" else pure $ head es
  ---- Introduce these existentials into the context and
  ---- annotate each term variable w/ corresponding existential
  ---- [marker e1, 'e1, 'e2, ... v1 : 'e1, v2 : 'e2 ...]
  --let f e (_,binding) = case binding of
  --      -- TODO: Think about whether `apply` here is always correct
  --      --       Used to have a guard that would only do this if t had no free vars
  --      Term.Ann' _ t -> apply ctx t
  --      _ -> Type.existential' (loc binding) e
  --let bindingTypes = zipWith f es bindings
  --appendContext $ context (Marker e1 : map Existential es ++ zipWith Ann vs bindingTypes)
  ---- check each `bi` against `ei`; sequencing resulting contexts
  --Foldable.for_ (zip bindings bindingTypes) $ \((_,b), t) -> check b t
  ---- compute generalized types `gt1, gt2 ...` for each binding `b1, b2...`;
  ---- add annotations `v1 : gt1, v2 : gt2 ...` to the context
  --(ctx1, ctx2) <- breakAt (Marker e1) <$> getContext
  --let gen e = generalizeExistentials ctx2 (Type.existential e)
  --let annotations = zipWith Ann vs (map gen es)
  --marker <- Marker <$> freshenVar (ABT.v' "let-rec-marker")
  --setContext (ctx1 `mappend` context (marker : annotations))
  --pure $ (marker, body)

-- | Apply the context to the input type, then convert any unsolved existentials
-- to universals.
generalizeExistentials :: Var v => Context v loc -> Type v loc -> Type v loc
generalizeExistentials ctx t = error "todo"
  -- foldr gen (apply ctx t) (unsolved ctx)
  --where
  --  gen e t =
  --    if TypeVar.Existential e `ABT.isFreeIn` t
  --    then Type.forall (TypeVar.Universal e) (ABT.subst (TypeVar.Existential e) (Type.universal e) t)
  --    else t -- don't bother introducing a forall if type variable is unused

-- | Check that under the given context, `e` has type `t`,
-- updating the context in the process.
check :: Var v => Term v loc -> Type v loc -> M v loc ()
-- check e t | debugEnabled && traceShow ("check"::String, e, t) False = undefined
check e t = withinCheck e t $ error "todo"
  --getContext >>= \ctx ->
  --if wellformedType ctx t then
  --  let
  --    go Term.Blank' _ = pure () -- somewhat hacky short circuit; blank checks successfully against all types
  --    go _ (Type.Forall' body) = do -- ForallI
  --      x <- extendUniversal =<< ABT.freshen body freshenTypeVar
  --      check e (ABT.bindInheritAnnotation body (Type.universal x))
  --      doRetract $ Universal x
  --    go (Term.Lam' body) (Type.Arrow' i o) = do -- =>I
  --      x <- ABT.freshen body freshenVar
  --      modifyContext' (extend (Ann x i))
  --      let Type.Effect'' es _ = o
  --      withEffects0 es $ check (ABT.bindInheritAnnotation body (Term.var x)) o
  --      doRetract $ Ann x i
  --    go (Term.Let1' binding e) t = do
  --      v <- ABT.freshen e freshenVar
  --      tbinding <- synthesize binding
  --      modifyContext' (extend (Ann v tbinding))
  --      check (ABT.bindInheritAnnotation e (Term.var v)) t
  --      doRetract $ Ann v tbinding
  --    go (Term.LetRecNamed' [] e) t = check e t
  --    go (Term.LetRec' letrec) t = do
  --      (marker, e) <- annotateLetRecBindings letrec
  --      check e t
  --      doRetract marker
  --    go (Term.Handle' h body) t = do
  --      -- `h` should check against `Effect e i -> t` (for new existentials `e` and `i`)
  --      -- `body` should check against `i`
  --      [e, i] <- sequence [freshNamed "e", freshNamed "i"]
  --      appendContext $ context [Existential e, Existential i]
  --      check h $ Type.effectV (Type.existential e) (Type.existential i) `Type.arrow` t
  --      ctx <- getContext
  --      let Type.Effect'' requested _ = apply ctx t
  --      abilityCheck requested
  --      withEffects [apply ctx $ Type.existential e] $ do
  --        ambient <- getAbilities
  --        let (_, i') = Type.stripEffect (apply ctx (Type.existential i))
  --        check body (Type.effect ambient i')
  --        pure ()
  --    go _ _ = do -- Sub
  --      a <- synthesize e; ctx <- getContext
  --      subtype (apply ctx a) (apply ctx t)
  --    e' = minimize' e
  --  in scope ("check: " ++ show e' ++ ":   " ++ show t) $ case t of
  --       -- expand existentials before checking
  --       t@(Type.Existential' _) -> go e' (apply ctx t)
  --       t -> go e' t
  --else
  --  scope ("context: " ++ show ctx) .
  --  scope ("term: " ++ show e) .
  --  scope ("type: " ++ show t) .
  --  scope ("context well formed: " ++ show (wellformed ctx)) .
  --  scope ("type well formed wrt context: " ++ show (wellformedType ctx t))
  --  $ fail "check failed"

-- | `subtype ctx t1 t2` returns successfully if `t1` is a subtype of `t2`.
-- This may have the effect of altering the context.
subtype :: forall v loc . (Eq loc, Var v) => loc -> Type v loc -> Type v loc -> M v loc ()
subtype _ tx ty | debugEnabled && traceShow ("subtype"::String, tx, ty) False = undefined
subtype loc tx ty = withinSubtype tx ty $
  do ctx <- getContext; go (ctx :: Context v loc) tx ty
  where -- Rules from figure 9
  go :: Context v loc -> Type v loc -> Type v loc -> M v loc ()
  go _ (Type.Ref' r) (Type.Ref' r2) | r == r2 = pure () -- `Unit`
  go ctx t1@(Type.Universal' v1) t2@(Type.Universal' v2) -- `Var`
    | v1 == v2 && wellformedType ctx t1 && wellformedType ctx t2
    = pure ()
  go ctx t1@(Type.Existential' v1) t2@(Type.Existential' v2) -- `Exvar`
    | v1 == v2 && wellformedType ctx t1 && wellformedType ctx t2
    = pure ()
  go _ (Type.Arrow' i1 o1) (Type.Arrow' i2 o2) = do -- `-->`
    subtype loc i1 i2; ctx' <- getContext
    subtype loc (apply ctx' o1) (apply ctx' o2)
  go _ (Type.App' x1 y1) (Type.App' x2 y2) = do -- analogue of `-->`
    subtype loc x1 x2; ctx' <- getContext
    subtype loc (apply ctx' y1) (apply ctx' y2)
  go _ t (Type.Forall' t2) = do
    v' <- extendUniversal =<< ABT.freshen t2 freshenTypeVar
    t2 <- pure $ ABT.bindInheritAnnotation t2 (Type.universal v')
    subtype loc t t2
    doRetract (Universal v')
  go _ (Type.Forall' t) t2 = do
    v <- extendMarker =<< ABT.freshen t freshenTypeVar
    t <- pure $ ABT.bindInheritAnnotation t (Type.existential v)
    ctx' <- getContext
    subtype loc (apply ctx' t) t2
    doRetract (Marker v)
  go _ (Type.Effect' [] a1) a2 = subtype loc a1 a2
  go _ a1 (Type.Effect' [] a2) = subtype loc a1 a2
  go ctx (Type.Existential' v) t -- `InstantiateL`
    | Set.member v (existentials ctx) && notMember v (Type.freeVars t) =
    instantiateL v t
  go ctx t (Type.Existential' v) -- `InstantiateR`
    | Set.member v (existentials ctx) && notMember v (Type.freeVars t) =
    instantiateR t v
  go _ (Type.Effect'' es1 a1) (Type.Effect' es2 a2) = do
     subtype loc a1 a2
     ctx <- getContext
     let es1' = map (apply ctx) es1
         es2' = map (apply ctx) es2
     abilityCheck' loc es2' es1'
  go _ _ _ = fail "not a subtype"

instantiateL :: Var v => v -> Type v loc -> M v loc ()
instantiateL = error "todo" -- may need a loc parameter?
instantiateR :: Var v => Type v loc -> v -> M v loc ()
instantiateR = error "todo" -- may need a loc parameter?

abilityCheck' :: (Var v, Eq loc) => loc -> [Type v loc] -> [Type v loc] -> M v loc ()
abilityCheck' loc ambient requested = do
  success <- flip allM requested $ \req ->
    flip anyM ambient $ \amb -> (True <$ subtype loc amb req) `orElse` pure False
  when (not success) $
    failNote $ AbilityCheckFailure ambient requested

abilityCheck :: (Var v, Eq loc) => loc -> [Type v loc] -> M v loc ()
abilityCheck loc requested = do
  enabled <- abilityCheckEnabled
  when enabled $ do
    ambient <- getAbilities
    abilityCheck' loc ambient requested

instance (Var v, Show loc) => Show (Element v loc) where
  show (Var v) = case v of
    TypeVar.Universal x -> "@" <> show x
    TypeVar.Existential x -> "'" ++ show x
  show (Solved v t) = "'"++Text.unpack (Var.shortName v)++" = "++show t
  show (Ann v t) = Text.unpack (Var.shortName v) ++ " : " ++ show t
  show (Marker v) = "|"++Text.unpack (Var.shortName v)++"|"

instance (Var v, Show loc) => Show (Context v loc) where
  show (Context es) = "Γ\n  " ++ (intercalate "\n  " . map (show . fst)) (reverse es)

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

instance Var v => Monoid (Context v loc) where
  mempty = context0
  mappend ctxL (Context es) =
    -- since `es` is a snoc list, we add it to `ctxL` in reverse order
    foldl' f ctxL (reverse es) where
      f ctx (e,_) = extend e ctx

instance Var v => Semigroup (Context v loc) where
  (<>) = mappend
