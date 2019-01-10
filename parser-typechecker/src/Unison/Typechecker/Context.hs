{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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
  , isSubtype
  , Suggestion(..)
  , isExact
  )
where

import           Control.Applicative            ( Alternative(..) )
import           Control.Monad
import           Control.Monad.Reader.Class
import           Control.Monad.State            ( get
                                                , put
                                                , StateT
                                                , runStateT
                                                )
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Writer           ( MonadWriter(..) )
import           Data.Bifunctor                 ( first
                                                , second
                                                )
import           Data.Foldable                  ( for_ )
import qualified Data.Foldable                 as Foldable
import           Data.Functor
import           Data.List
import           Data.List.NonEmpty             ( NonEmpty )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Sequence                  ( Seq )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Word                      ( Word64 )
import           Debug.Trace
import qualified Unison.ABT                    as ABT
import qualified Unison.Blank                  as B
import           Unison.DataDeclaration         ( DataDeclaration'
                                                , EffectDeclaration'
                                                )
import qualified Unison.DataDeclaration        as DD
import           Unison.PatternP                ( Pattern )
import qualified Unison.PatternP               as Pattern
import           Unison.Reference               ( Reference )
import           Unison.Referent                ( Referent )
import           Unison.Term                    ( AnnotatedTerm' )
import qualified Unison.Term                   as Term
import           Unison.Type                    ( AnnotatedType )
import qualified Unison.Type                   as Type
import           Unison.Typechecker.Components  ( minimize' )
import qualified Unison.Typechecker.TypeLookup as TL
import qualified Unison.TypeVar                as TypeVar
import           Unison.Var                     ( Var )
import qualified Unison.Var                    as Var

type TypeVar v loc = TypeVar.TypeVar (B.Blank loc) v
type Type v loc = AnnotatedType (TypeVar v loc) loc
type Term v loc = AnnotatedTerm' (TypeVar v loc) v loc
type Monotype v loc = Type.Monotype (TypeVar v loc) loc
type RedundantTypeAnnotation = Bool

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

data Env v loc = Env { freshId :: Word64, ctx :: Context v loc }

type DataDeclarations v loc = Map Reference (DataDeclaration' v loc)
type EffectDeclarations v loc = Map Reference (EffectDeclaration' v loc)

data Result v loc a = Result { errors :: Seq (ErrorNote v loc)
                             , infos :: Seq (InfoNote v loc)
                             , resultValue :: Maybe a
                             } deriving (Functor)

instance Applicative (Result v loc) where
  pure = Result mempty mempty . Just
  Result es is a <*> Result es' is' a' =
    Result (es <> es') (is <> is') (a <*> a')

instance Alternative (Result v loc) where
  empty = Result mempty mempty Nothing
  a <|> b = if isJust $ resultValue a then a else b

instance Monad (Result v loc) where
  Result es is a >>= f =
    let Result es' is' b = traverse f a
     in joinMaybe $ Result (es <> es') (is <> is') b

instance MonadWriter
           (Seq (ErrorNote v loc), Seq (InfoNote v loc))
           (Result v loc)
  where
    tell (es, is) = Result es is (Just ())
    listen (Result es is ma) = Result es is ((, (es, is)) <$> ma)
    pass (Result es is maf) =
      joinMaybe $ traverse (\(a, f) -> tell (f (es, is)) *> pure a) maf

joinMaybe :: Result v loc (Maybe a) -> Result v loc a
joinMaybe (Result v loc ma) = Result v loc (join ma)

-- Emit an errorNote without failing
-- errorNote' :: Cause v loc -> Result v loc ()
-- errorNote' cause = tell (pure (ErrorNote cause mempty), mempty)

btw' :: InfoNote v loc -> Result v loc ()
btw' note = tell (mempty, pure note)

-- | Typechecking monad
newtype M v loc a = M {
  runM :: MEnv v loc -> Result v loc (a, Env v loc)
}

liftResult :: Result v loc a -> M v loc a
liftResult r = M (\m -> (, env m) <$> r)

-- errorNote :: Cause v loc -> M v loc ()
-- errorNote = liftResult . errorNote

btw :: InfoNote v loc -> M v loc ()
btw = liftResult . btw'

modEnv :: (Env v loc -> Env v loc) -> M v loc ()
modEnv f = modEnv' $ ((), ) . f

modEnv' :: (Env v loc -> (a, Env v loc)) -> M v loc a
modEnv' f = M (\menv -> pure . f $ env menv)

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
  | UnknownTermReference Reference
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

data Suggestion v loc =
  Suggestion { suggestionName :: Text
             , suggestionType :: Type v loc
             , suggestionReplacement :: Either v Referent
             } |
  WrongType { suggestionName :: Text
            , suggestionType :: Type v loc
            } |
  WrongName { suggestionName :: Text
            , suggestionType :: Type v loc
            }
  deriving (Eq, Show)

isExact :: Suggestion v loc -> Bool
isExact (Suggestion _ _ _) = True
isExact _ = False

data ErrorNote v loc = ErrorNote {
  cause :: Cause v loc,
  path :: Seq (PathElement v loc)
} deriving Show

-- `Decision v loc fqn` is a decision to replace the name v at location loc
-- with the fully qualified name fqn.
data InfoNote v loc
  = SolvedBlank (B.Recorded loc) v (Type v loc)
  | Decision v loc (Term.AnnotatedTerm v loc)
  | TopLevelComponent [(v, Type.AnnotatedType v loc, RedundantTypeAnnotation)]
  deriving (Show)

data Cause v loc
  = TypeMismatch (Context v loc)
  | IllFormedType (Context v loc)
  | UnknownSymbol loc v
  | UnknownTerm loc v [Suggestion v loc] (Type v loc)
  | CompilerBug (CompilerBug v loc)
  | AbilityCheckFailure [Type v loc] [Type v loc] (Context v loc) -- ambient, requested
  | EffectConstructorWrongArgCount ExpectedArgCount ActualArgCount Reference ConstructorId
  | MalformedEffectBind (Type v loc) (Type v loc) [Type v loc] -- type of ctor, type of ctor result
  -- Type of ctor, number of arguments we got
  | PatternArityMismatch loc (Type v loc) Int
  -- A variable is defined twice in the same block
  | DuplicateDefinitions (NonEmpty (v, [loc]))
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
scope p (M m) = M go
 where
  go menv =
    let (Result errors infos r) = m menv
    in  Result (scope' p <$> errors) infos r

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

unsolved' :: Context v loc -> [(B.Blank loc, v)]
unsolved' (Context ctx) = [(b,v) | (Existential b v, _) <- ctx]

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
  -- Like `retract`, but returns the empty context if retracting would remove
  -- all elements.
  retract'
    :: (Var v, Ord loc) => Element v loc -> Context v loc -> Context v loc
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

fromMEnv :: (MEnv v loc -> a) -> M v loc a
fromMEnv f = f <$> ask

getBuiltinLocation :: M v loc loc
getBuiltinLocation = fromMEnv builtinLocation

getAbilityCheckMask :: M v loc (Type v loc -> M v loc Bool)
getAbilityCheckMask = fromMEnv abilityCheckMask

getContext :: M v loc (Context v loc)
getContext = fromMEnv $ ctx . env

setContext :: Context v loc -> M v loc ()
setContext ctx = modEnv (\e -> e { ctx = ctx })

modifyContext :: (Context v loc -> M v loc (Context v loc)) -> M v loc ()
modifyContext f = do
  c <- getContext
  c <- f c
  setContext c

modifyContext' :: (Context v loc -> Context v loc) -> M v loc ()
modifyContext' f = modifyContext (pure . f)

appendContext :: (Var v, Ord loc) => Context v loc -> M v loc ()
appendContext tl = modifyContext' (<> tl)

universals :: Context v loc -> Set v
universals = universalVars . info

existentials :: Context v loc -> Set v
existentials = existentialVars . info

freshenVar :: Var v => v -> M v loc v
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
  go menv = runM m1 menv <|> runM m2 menv

-- If the action fails, preserve all the notes it emitted and then return the
-- provided `a`; if the action succeeds, we're good, just use its result
recover :: a -> M v loc a -> M v loc a
recover ifFail (M action) = M go where
  go menv = case action menv of
    Result es is Nothing -> Result es is $ Just (ifFail, env menv)
    r -> r

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

-- run `m` without doing ability checks on requests which match `ambient0`
-- are a subtype of `ambient0`.
withoutAbilityCheckFor :: (Ord loc, Var v) => Type v loc -> M v loc a -> M v loc a
withoutAbilityCheckFor ft _ | debugEnabled && traceShow ("withoutAbilityCheckFor"::String, ft) False = undefined
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
failWith cause =
  M (const $ Result (pure $ ErrorNote cause mempty) mempty Nothing)

compilerCrashResult :: CompilerBug v loc -> Result v loc a
compilerCrashResult bug =
  Result (pure $ ErrorNote (CompilerBug bug) mempty) mempty Nothing

failSecretlyAndDangerously :: M v loc a
failSecretlyAndDangerously = empty

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
withEffects :: [Type v loc] -> M v loc a -> M v loc a
withEffects abilities' m =
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
synthesizeApp ft arg | debugEnabled && traceShow ("synthesizeApp"::String, ft, arg) False = undefined
synthesizeApp (Type.Effect'' es ft) argp@(arg, argNum) =
  scope (InSynthesizeApp ft arg argNum) $ abilityCheck es >> go ft
  where
  go (Type.Forall' body) = do -- Forall1App
    v <- ABT.freshen body freshenTypeVar
    appendContext (context [existential v])
    let ft2 = ABT.bindInheritAnnotation body (Type.existential B.Blank v)
    synthesizeApp ft2 argp
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

noteTopLevelType
  :: (Ord loc, Var v)
  => ABT.Subst f v a
  -> Term v loc
  -> Type v loc
  -> M v loc ()
noteTopLevelType e binding typ = case binding of
  Term.Ann' strippedBinding annotatedType -> do
    inferred <- (Just <$> synthesize strippedBinding) `orElse` pure Nothing
    -- TODO: We're kind of assuming that `annotatedType` is the same as `typ`.
    -- Is that actually true?
    let redundant = case inferred of
          Nothing -> False
          Just t  -> t == annotatedType
    btw $ TopLevelComponent
      [(ABT.variable e, Type.generalizeAndUnTypeVar annotatedType, redundant)]
  _ -> btw $ TopLevelComponent
    [(ABT.variable e, Type.generalizeAndUnTypeVar typ, False)]

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
    v <- freshNamed "_"
    appendContext $ context [Existential blank v]
    pure $ Type.existential' l blank v -- forall (TypeVar.Universal v) (Type.universal v)
  go (Term.Ann' (Term.Ref' _) t) = case ABT.freeVars t of
    s | Set.null s ->
      -- innermost Ref annotation assumed to be correctly provided by `synthesizeClosed`
      pure t
    s -> compilerCrash $ FreeVarsInTypeAnnotation s
  go (Term.Ref' h) = compilerCrash $ UnannotatedReference h
  go (Term.Constructor' r cid) = do
    t <- getDataConstructorType r cid
    pure $ Type.generalizeEffects (Type.arity t) t
  go (Term.Request' r cid) = do
    t <- ungeneralize =<< getEffectConstructorType r cid
    pure $ Type.generalizeEffects (Type.arity t) t
  go (Term.Ann' e' t) = t <$ check e' t
  go (Term.Float' _) = pure $ Type.float l -- 1I=>
  go (Term.Int' _) = pure $ Type.int l -- 1I=>
  go (Term.Nat' _) = pure $ Type.nat l -- 1I=>
  go (Term.Boolean' _) = pure $ Type.boolean l
  go (Term.Text' _) = pure $ Type.text l
  go (Term.Apps' f args) = do -- ->EEEEE
    ft <- synthesize f
    ctx <- getContext
    (vs, ft) <- ungeneralize' ft
    scope (InFunctionCall vs f ft args) $ synthesizeApps (apply ctx ft) args
  go (Term.Vector' v) = do
    ft <- vectorConstructorOfArity (Foldable.length v)
    case Foldable.toList v of
      [] -> pure ft
      v1 : _ ->
        scope (InVectorApp (ABT.annotation v1)) $ synthesizeApps ft v
  go (Term.Let1Top' top binding e) | Set.null (ABT.freeVars binding) = do
    -- special case when it is definitely safe to generalize - binding contains
    -- no free variables, i.e. `let id x = x in ...`
    abilities <- getAbilities
    t  <- synthesizeClosed' abilities binding
    when top $ noteTopLevelType e binding t
    v' <- ABT.freshen e freshenVar
    -- note: `Ann' (Ref'  _) t` synthesizes to `t`
    e  <- pure $ ABT.bindInheritAnnotation e (Term.ann () (Term.builtin() (Var.name v')) t)
    synthesize e
  go (Term.Let1Top' top binding e) = do
    -- note: no need to freshen binding, it can't refer to v
    tbinding <- synthesize binding
    v' <- ABT.freshen e freshenVar
    appendContext (context [Ann v' tbinding])
    t <- synthesize (ABT.bindInheritAnnotation e (Term.var() v'))
    when top $ noteTopLevelType e binding t
    doRetract $ Ann v' tbinding
    pure t
  go (Term.Lam' body) = do -- ->I=> (Full Damas Milner rule)
    -- arya: are there more meaningful locations we could put into and pull out of the abschain?)
    [arg, i, e, o] <- sequence [ ABT.freshen body freshenVar
                               , freshenVar (ABT.variable body)
                               , freshNamed "inferred-effect"
                               , freshNamed "inferred-output" ]
    let it = Type.existential' l B.Blank i
        ot = Type.existential' l B.Blank o
        et = Type.existential' l B.Blank e
    appendContext $
      context [existential i, existential e, existential o, Ann arg it]
    body <- pure $ ABT.bindInheritAnnotation body (Term.var() arg)
    withEffects0 [et] $ check body ot
    ctx <- getContext
    pure $ Type.arrow l it (Type.effect l (apply ctx <$> [et]) ot)
  go (Term.LetRecNamed' [] body) = synthesize body
  go (Term.LetRecTop' isTop letrec) = do
    (marker, e) <- annotateLetRecBindings isTop letrec
    t <- synthesize e
    (_, _, ctx2) <- breakAt marker <$> getContext
    (generalizeExistentials ctx2 t) <$ doRetract marker
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
  for_ guard' $ \g -> scope InMatchGuard $ check g (Type.boolean (loc g))
  outputType <- applyM outputType
  scope InMatchBody $ check rhs' outputType
  doRetract $ Marker m

checkPattern
  :: (Var v, Ord loc)
  => Type v loc
  -> Pattern loc
  -> StateT [v] (M v loc) [(v, v)]
checkPattern tx ty | debugEnabled && traceShow ("checkPattern"::String, tx, ty) False = undefined
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
    Pattern.Int loc _ ->
      lift $ subtype (Type.int loc) scrutineeType $> mempty
    Pattern.Nat loc _ ->
      lift $ subtype (Type.nat loc) scrutineeType $> mempty
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

resetContextAfter :: a -> M v loc a -> M v loc a
resetContextAfter x a = do
  ctx <- getContext
  a <- a `orElse` pure x
  setContext ctx
  pure a

-- | Synthesize and generalize the type of each binding in a let rec
-- and return the new context in which all bindings are annotated with
-- their type. Also returns the freshened version of `body` and a marker
-- which should be used to retract the context after checking/synthesis
-- of `body` is complete. See usage in `synthesize` and `check` for `LetRec'` case.
annotateLetRecBindings
  :: (Var v, Ord loc)
  => Term.IsTop
  -> ((v -> M v loc v) -> M v loc ([(v, Term v loc)], Term v loc))
  -> M v loc (Element v loc, Term v loc)
annotateLetRecBindings isTop letrec =
  -- If this is a top-level letrec, then emit a TopLevelComponent note,
  -- which asks if the user-provided type annotations were needed.
  if isTop
  then do
    -- First, typecheck (using annotateLetRecBindings') the bindings with any
    -- user-provided annotations.
    (marker, body, vts) <- annotateLetRecBindings' True
    -- Then, try typechecking again, but ignoring any user-provided annotations.
    -- This will infer whatever type.  If it altogether fails to typecheck here
    -- then, ...(1)
    withoutAnnotations  <-
      resetContextAfter Nothing $ (Just <$> annotateLetRecBindings' False)
    -- convert from typechecker TypeVar back to regular `v` vars
    let unTypeVar (v, t) = (v, Type.generalizeAndUnTypeVar t)
    case withoutAnnotations of
      -- If the types (snd vts) are the same, then we know that the annotations
      -- were redundant, and we discard them.
      Just (_, _, vts') | fmap snd vts == fmap snd vts' ->
        btw $ TopLevelComponent ((\(a, b) -> (a, b, True)) . unTypeVar <$> vts)
      -- ...(1) we'll assume all the user-provided annotations were needed
      _otherwise -> btw
        $ TopLevelComponent ((\(a, b) -> (a, b, False)) . unTypeVar <$> vts)
    pure (marker, body)
  -- If this isn't a top-level letrec, then we don't have to do anything special
  else (\(marker, body, _) -> (marker, body)) <$> annotateLetRecBindings' True
 where
  annotateLetRecBindings' useUserAnnotations = do
    (bindings, body) <- letrec freshenVar
    let vs = map fst bindings
    -- generate a fresh existential variable `e1, e2 ...` for each binding
    es  <- traverse freshenVar vs
    e1  <- freshNamed "bindings-start"
    ctx <- getContext
    -- Introduce these existentials into the context and
    -- annotate each term variable w/ corresponding existential
    -- [marker e1, 'e1, 'e2, ... v1 : 'e1, v2 : 'e2 ...]
    let f e (_, binding) = case binding of
          -- TODO: Think about whether `apply` here is always correct
          -- Used to have a guard that would only do this if t had no free vars
          Term.Ann' _ t | useUserAnnotations -> apply ctx t
          _ -> Type.existential' (loc binding) B.Blank e
    let bindingTypes   = zipWith f es bindings
        bindingArities = Term.arity . snd <$> bindings
    appendContext $ context
      (Marker e1 : map existential es ++ zipWith Ann vs bindingTypes)
    -- check each `bi` against `ei`; sequencing resulting contexts
    Foldable.for_ (zip bindings bindingTypes) $ \((_, b), t) -> check b t
    -- compute generalized types `gt1, gt2 ...` for each binding `b1, b2...`;
    -- add annotations `v1 : gt1, v2 : gt2 ...` to the context
    (_, _, ctx2) <- breakAt (Marker e1) <$> getContext
    let gen bindingType arity =
          Type.generalizeEffects arity $ generalizeExistentials ctx2 bindingType
        bindingTypesGeneralized = zipWith gen bindingTypes bindingArities
        annotations             = zipWith Ann vs bindingTypesGeneralized
    marker <- Marker <$> freshenVar (ABT.v' "let-rec-marker")
    doRetract (Marker e1)
    appendContext . context $ marker : annotations
    pure (marker, body, vs `zip` bindingTypesGeneralized)


ungeneralize :: (Var v, Ord loc) => Type v loc -> M v loc (Type v loc)
ungeneralize t = snd <$> ungeneralize' t

ungeneralize' :: (Var v, Ord loc) => Type v loc -> M v loc ([v], Type v loc)
ungeneralize' (Type.Forall' t) = do
  v <- ABT.freshen t freshenTypeVar
  appendContext $ context [existential v]
  t <- pure $ ABT.bindInheritAnnotation t (Type.existential B.Blank v)
  first (v:) <$> ungeneralize' t
ungeneralize' t = pure ([], t)

-- | Apply the context to the input type, then convert any unsolved existentials
-- to universals.
generalizeExistentials :: (Var v, Ord loc) => Context v loc -> Type v loc -> Type v loc
generalizeExistentials ctx t =
  foldr gen (Type.removePureEffects $ apply ctx t) (unsolved ctx)
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
          t@(Type.Existential' _ _) -> abilityCheck es >> go e (apply ctx t)
          t                         -> go e t
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
    withEffects0 es $ check (ABT.bindInheritAnnotation body (Term.var () x)) ot
    doRetract $ Ann x i
  go (Term.Let1' binding e) t = do
    v        <- ABT.freshen e freshenVar
    tbinding <- synthesize binding
    modifyContext' (extend (Ann v tbinding))
    check (ABT.bindInheritAnnotation e (Term.var () v)) t
    doRetract $ Ann v tbinding
  go (Term.LetRecNamed' [] e) t = check e t
  go (Term.LetRecTop' isTop letrec) t = do
    (marker, e) <- annotateLetRecBindings isTop letrec
    check e t
    doRetract marker
  go block@(Term.Handle' h body) t = do
    -- `h` should check against `Effect e i -> t` (for new existentials `e` and `i`)
    -- `body` should check against `i`
    [e, i] <- sequence [freshNamed "e", freshNamed "i"]
    appendContext $ context [existential e, existential i]
    let l = loc block
    -- t <- applyM t
    ambient <- getAbilities
    let t' = Type.effect l ambient t
    check h $ Type.arrow
      l
      (Type.effectV l (l, Type.existentialp l e) (l, Type.existentialp l i))
      t'
    ctx <- getContext
    let et = apply ctx (Type.existentialp l e)
    withoutAbilityCheckFor et
      . withEffects [et]
      . check body
      . apply ctx
      $ Type.existentialp l i
  go e t = do -- Sub
    a   <- synthesize e
    ctx <- getContext
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
  go _ (Type.Effect1' es a) a2 = do
     subtype es (Type.effects (loc es) [])
     subtype a a2
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
     if all (`elem` es2') es1' then pure () else abilityCheck' es2' es1'
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
        [i',o'] <- traverse freshenVar [nameFrom "i" i, nameFrom "o" o]
        let s = Solved blank v (Type.Monotype (Type.arrow (loc t)
                                                 (Type.existentialp (loc i) i')
                                                 (Type.existentialp (loc o) o')))
        modifyContext' $ replace (existential v)
                                 (context [existential o', existential i', s])
        instantiateR i B.Blank i' -- todo: not sure about this, could also be `blank`
        applyM o >>= instantiateL B.Blank o'
      Type.App' x y -> do -- analogue of InstLArr
        [x', y'] <- traverse freshenVar [nameFrom "x" x, nameFrom "y" y]
        let s = Solved blank v (Type.Monotype (Type.app (loc t)
                                                  (Type.existentialp (loc x) x')
                                                  (Type.existentialp (loc y) y')))
        modifyContext' $ replace (existential v)
                                 (context [existential y', existential x', s])
        applyM x >>= instantiateL B.Blank x'
        applyM y >>= instantiateL B.Blank y'
      Type.Effect1' es vt -> do
        es' <- freshNamed "effect1-e"
        vt' <- freshNamed "vt"
        let t' = Type.effect1 (loc t) (Type.existentialp (loc es) es')
                                      (Type.existentialp (loc vt) vt')
            s = Solved blank v (Type.Monotype t')
        modifyContext' $ replace (existential v)
                         (context [existential es', existential vt', s])
        applyM es >>= instantiateL B.Blank es'
        applyM vt >>= instantiateL B.Blank vt'
      Type.Effects' es -> do
        es' <- traverse (\e -> freshenVar (nameFrom "e" e)) es
        let locs = loc <$> es
            t' = Type.effects (loc t) (uncurry Type.existentialp <$> locs `zip` es')
            -- t = Type.effects (loc t) (uncurry Type.existentialp <$> locs `zip` es')
            s = Solved blank v $ Type.Monotype t'
        modifyContext' $ replace (existential v)
                                 (context $ (existential <$> es') ++ [s])
        Foldable.for_ (es' `zip` es) $ \(e',e) ->
          applyM e >>= instantiateL B.Blank e'
      Type.Forall' body -> do -- InstLIIL
        v <- extendUniversal =<< ABT.freshen body freshenTypeVar
        instantiateL B.Blank v (ABT.bindInheritAnnotation body (Type.universal v))
        doRetract (Universal v)
      _ -> failWith $ TypeMismatch ctx

nameFrom :: Var v => Text -> Type v loc -> v
nameFrom _ (Type.Var' v) = Var.named (Var.name v)
nameFrom ifNotVar _ = ABT.v' (":" `mappend` ifNotVar `mappend` ":")

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
        [i', o'] <- traverse freshenVar [nameFrom "instR-i" i, nameFrom "instR-o" o]
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
        [x', y'] <- traverse freshenVar [nameFrom "instR-x" x, nameFrom "instR-y" y]
        let s = Solved blank v (Type.Monotype (Type.app (loc t) (Type.existentialp (loc x) x') (Type.existentialp (loc y) y')))
        modifyContext' $ replace (existential v) (context [existential y', existential x', s])
        applyM x >>= \x -> instantiateR x B.Blank x'
        applyM y >>= \y -> instantiateR y B.Blank y'
      Type.Effect1' es vt -> do
        es' <- freshenVar (nameFrom "e" es)
        vt' <- freshenVar (nameFrom "vt" vt)
        let t' = Type.effect1 (loc t) (Type.existentialp (loc es) es')
                                      (Type.existentialp (loc vt) vt')
            s = Solved blank v (Type.Monotype t')
        modifyContext' $ replace (existential v)
                         (context [existential es', existential vt', s])
        applyM es >>= \es -> instantiateR es B.Blank es'
        applyM vt >>= \vt -> instantiateR vt B.Blank vt'
      Type.Effects' es -> do
        es' <- traverse (\e -> freshenVar (nameFrom "e" e)) es
        let locs = loc <$> es
            t' = Type.effects (loc t) (uncurry Type.existentialp <$> locs `zip` es')
            s = Solved blank v $ Type.Monotype t'
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

abilityCheck' :: forall v loc . (Var v, Ord loc) => [Type v loc] -> [Type v loc] -> M v loc ()
abilityCheck' [] [] = pure ()
abilityCheck' ambient0 requested0 = go ambient0 requested0 where
  go _ambient [] = pure ()
  go ambient0 (r:rs) = do
    ambient <- traverse applyM ambient0
    r <- applyM r
    -- 1. Look in ambient to see if `r` has an exact match in the head of `r`.
    case find (headMatch r) ambient of
      -- 2a. If yes for `a` in ambient, do `subtype amb r` and done.
      Just amb -> do
        subtype amb r `orElse` die r
        go ambient rs
      -- 2b. If no:
      Nothing -> do
        ctx <- getContext
        -- find first unsolved existential, 'e, that appears in ambient
        let unsolveds = unsolved' ctx
            ambFlat = Set.fromList (ambient >>= Type.flattenEffects >>= vars)
            vars (Type.Var' (TypeVar.Existential _ v)) = [v]
            vars _ = []
            intersection = [ (b,v) | (b,v) <- unsolveds, Set.member v ambFlat ]
        case listToMaybe intersection of
          Just (b, e') -> do
            -- introduce fresh existential 'e2 to context
            e2' <- extendExistential e'
            let et2 = Type.effects (loc r) [r, Type.existentialp (loc r) e2']
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
    Type.Existential' b v ->
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
  :: (Var v, Ord loc)
  => loc
  -> [Type v loc]
  -> TL.TypeLookup v loc
  -> Term v loc
  -> Result v loc (Type v loc)
synthesizeClosed builtinLoc abilities lookupType term0 = let
  datas = TL.dataDecls lookupType
  effects = TL.effectDecls lookupType
  term = annotateRefs (TL.typeOfTerm' lookupType) term0
  in case term of
    Left missingRef ->
      compilerCrashResult (UnknownTermReference missingRef)
    Right term -> run builtinLoc [] datas effects $ do
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

annotateRefs :: (Applicative f, Var v)
             => (Reference -> f (Type.AnnotatedType v loc))
             -> Term v loc
             -> f (Term v loc)
annotateRefs synth = ABT.visit f where
  -- already annotated; skip this subtree
  f r@(Term.Ann' (Term.Ref' _) _) = Just (pure r)
  f r@(Term.Ref' h) = Just (Term.ann ra (Term.ref ra h) <$> (ge <$> synth h))
    where ra = ABT.annotation r
          ge t = ABT.vmap TypeVar.Universal $ Type.generalizeEffects (Type.arity t) t
  f _ = Nothing

run
  :: (Var v, Ord loc)
  => loc
  -> [Type v loc]
  -> DataDeclarations v loc
  -> EffectDeclarations v loc
  -> M v loc a
  -> Result v loc a
run builtinLoc ambient datas effects m =
  fmap fst
    . runM m
    . MEnv (Env 0 mempty) ambient builtinLoc datas effects
    . const
    $ pure True

generalizeEffectSignatures :: Var v => Term v loc -> Term v loc
generalizeEffectSignatures t = ABT.rebuildUp go t
  where go (Term.Ann e t) = Term.Ann e (Type.generalizeEffects (Term.arity e) t)
        go e = e

synthesizeClosed' :: (Var v, Ord loc)
                  => [Type v loc]
                  -> Term v loc
                  -> M v loc (Type v loc)
synthesizeClosed' abilities term = do
  -- save current context, for restoration when done
  ctx0 <- getContext
  setContext $ context []
  v <- extendMarker $ Var.named "start"
  t <- withEffects0 abilities (synthesize $ generalizeEffectSignatures term)
  ctx <- getContext
  -- retract will cause notes to be written out for
  -- any `Blank`-tagged existentials passing out of scope
  doRetract (Marker v)
  setContext ctx0 -- restore the initial context
  pure $ generalizeExistentials ctx t

-- Check if the given typechecking action succeeds
succeeds :: M v loc a -> M v loc Bool
succeeds m = do
  e <- ask
  let Result _ _ r = runM m e
  pure $ isJust r

-- Check if `t1` is a subtype of `t2`. Doesn't update the typechecking context.
isSubtype' :: (Var v, Ord loc) => Type v loc -> Type v loc -> M v loc Bool
isSubtype' type1 type2 = do
  let vars = Set.union (ABT.freeVars type1) (ABT.freeVars type2)
  appendContext $ context (Var <$> Set.toList vars)
  succeeds $ subtype type1 type2

-- Public interface to `isSubtype`
isSubtype
  :: (Var v, Ord loc) => loc -> Type v loc -> Type v loc -> Result v loc Bool
isSubtype builtinLoc t1 t2 =
  run builtinLoc [] Map.empty Map.empty (isSubtype' t1 t2)

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

-- MEnv v loc -> (Seq (ErrorNote v loc), (a, Env v loc))
instance Monad (M v loc) where
  return a = M (\menv -> pure (a, env menv))
  m >>= f = M go where
    go menv = do
      (a, env1) <- runM m menv
      runM (f a) (menv { env = env1 })

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
  ask = M (\e -> pure (e, env e))
  local f m = M $ runM m . f

instance Alternative (M v loc) where
  empty = liftResult empty
  a <|> b = a `orElse` b

