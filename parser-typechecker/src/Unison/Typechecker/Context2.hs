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
-- import           Control.Monad.Loops (anyM, allM)
-- import           Control.Monad.State
-- import qualified Data.Foldable as Foldable
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
import qualified Unison.DataDeclaration as DataDeclaration
-- import           Unison.Note (Note,Noted(..))
-- import qualified Unison.Note as Note
-- import           Unison.Pattern (Pattern)
-- import qualified Unison.Pattern as Pattern
import           Unison.Reference (Reference)
-- import qualified Unison.Term as Term
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
data Element' v loc
  = Var (TypeVar v)        -- A variable declaration
  | Solved v (Monotype v loc)  -- `v` is solved to some monotype
  | Ann v (Type v loc)         -- `v` has type `a`, which may be quantified
  | Marker v deriving (Eq) -- used for scoping

type Element v loc = (Element' v loc, loc)

data Env v loc = Env { freshId :: Word, ctx :: Context v loc }

type DataDeclarations v loc = Map Reference (DataDeclaration' v loc)
type EffectDeclarations v loc = Map Reference (EffectDeclaration' v loc)

-- | Typechecking monad
newtype M v loc a = M {
  runM :: MEnv v loc -> (Seq (Note loc), Maybe (a, Env v loc))
}

data Note loc = InternalError

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
-- the given `Element`. Returns `Left` if the element is not found.
retract :: (Eq loc, Var v) => Element' v loc -> Context v loc -> Maybe (Context v loc)
retract m (Context ctx) =
  let maybeTail [] = Nothing
      maybeTail (_:t) = Just t
  -- note: no need to recompute used variables; any suffix of the
  -- context snoc list is also a valid context
  in Context <$> maybeTail (dropWhile (\((e,_),_) -> e /= m) ctx)

-- | Like `retract`, but returns the empty context if retracting would remove all elements.
retract' :: (Eq loc, Var v) => Element' v loc -> Context v loc -> Context v loc
retract' e ctx = case retract e ctx of
  Nothing -> context []
  Just ctx -> ctx

solved :: Context v loc -> [(v, Monotype v loc)]
solved (Context ctx) = [(v, sa) | ((Solved v sa,_),_) <- ctx]

unsolved :: Context v loc -> [v]
unsolved (Context ctx) = [v | ((Existential v,_),_) <- ctx]

replace :: Var v => Element' v loc -> Context v loc -> Context v loc -> Context v loc
replace e focus ctx =
  let (l,r) = breakAt e ctx
  in l `mappend` focus `mappend` r

breakAt :: Var v => Element' v loc -> Context v loc -> (Context v loc, Context v loc)
breakAt m (Context xs) =
  let
    (r, l) = break (\(e,_) -> e === m) xs
  -- l is a suffix of xs and is already a valid context;
  -- r needs to be rebuilt
    (Existential v, _) === Existential v2 | v == v2 = True
    (Universal v, _)   === Universal v2 | v == v2 = True
    (Marker v, _)      === Marker v2 | v == v2 = True
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
         -> (Seq (Note loc), Maybe (a, Env v loc))
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
  _ -> compilerCrash $ "Context.wellformedType - ill formed type - " ++ show t
  where
  -- | Extend this `Context` with a single variable, guaranteed fresh
  extendUniversal ctx = case Var.freshIn (usedVars ctx) (Var.named "var") of
    v -> (v, extend (Universal v, ABT.annotation t) ctx)

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
  addInfo e (Info es us vs ok) = case fst e of
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

getAbilities :: M v loc [Type v loc]
getAbilities = M $ fromMEnv abilities

abilityCheckEnabled :: M v loc Bool
abilityCheckEnabled = M $ fromMEnv abilityChecks

withoutAbilityCheck :: M v loc a -> M v loc a
withoutAbilityCheck m = M (\menv -> runM m $ menv { abilityChecks = False })

-- todo: better error
compilerCrash :: String -> a
compilerCrash msg = error msg

-- todo: better error
getFromTypeEnv :: (Ord r, Show r)
               => String -> M v loc (Map r (f v loc)) -> r ->  M v loc (f v loc)
getFromTypeEnv what get r = get >>= \decls ->
  case Map.lookup r decls of
    Nothing -> compilerCrash $ "unknown " ++ what ++ " reference: " ++ show r ++ " " ++
                      show (Map.keys decls)
    Just decl -> pure decl

getDataDeclaration :: Reference -> M v loc (DataDeclaration' v loc)
getDataDeclaration = getFromTypeEnv "data type" getDataDeclarations

getEffectDeclaration :: Reference -> M v loc (DataDeclaration' v loc)
getEffectDeclaration = getFromTypeEnv "data type" getDataDeclarations

getDataConstructorType :: Var v => Reference -> Int -> M v loc (Type v loc)
getDataConstructorType = getConstructorType' getDataDeclaration

getEffectConstructorType :: Var v => Reference -> Int -> M v loc (Type v loc)
getEffectConstructorType = getConstructorType' getDataDeclaration

-- todo: crash the compiler with as much info as needed to submit a bug report
-- it's a compiler bug, not your fault, etc.
-- Encountered an unknown constructor in the typechecker; unknown constructors
-- should have been detected earlier though.
getConstructorType' :: (Var v, Show r)
                    => (r -> M v loc (DataDeclaration' v loc))
                    -> r
                    -> Int
                    -> M v loc (Type v loc)
getConstructorType' get r cid = do
  decl <- get r
  case drop cid (DataDeclaration.constructors decl) of
    [] -> compilerCrash $ "invalid constructor id: " ++ show cid ++ " in " ++ show r
    (_v, typ) : _ -> pure $ ABT.vmap TypeVar.Universal typ

-- abilityCheck' :: Var v => [Type v] -> [Type v] -> M v ()
-- abilityCheck' ambient requested = do
--   success <- flip allM requested $ \req ->
--     flip anyM ambient $ \amb -> (True <$ subtype amb req) `orElse` pure False
--   when (not success) $
--     fail $ "Ability check failed. Requested abilities " <> show requested <>
--            " but ambient abilities only included " <> show ambient <> "."
--
-- abilityCheck :: Var v => [Type v] -> M v ()
-- abilityCheck requested = do
--   enabled <- abilityCheckEnabled
--   when enabled $ do
--     ambient <- getAbilities
--     abilityCheck' ambient requested

instance (Var v, Show loc) => Show (Element' v loc) where
  show (Var v) = case v of
    TypeVar.Universal x -> "@" <> show x
    TypeVar.Existential x -> "'" ++ show x
  show (Solved v t) = "'"++Text.unpack (Var.shortName v)++" = "++show t
  show (Ann v t) = Text.unpack (Var.shortName v) ++ " : " ++ show t
  show (Marker v) = "|"++Text.unpack (Var.shortName v)++"|"

instance (Var v, Show loc) => Show (Context v loc) where
  show (Context es) = "Γ\n  " ++ (intercalate "\n  " . map (show . fst)) (reverse es)

-- MEnv v loc -> (Seq (Note loc), (a, Env v loc))
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
