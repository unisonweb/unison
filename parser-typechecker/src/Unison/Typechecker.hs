{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

-- | This module is the primary interface to the Unison typechecker
-- module Unison.Typechecker (admissibleTypeAt, check, check', checkAdmissible', equals, locals, subtype, isSubtype, synthesize, synthesize', typeAt, wellTyped) where

module Unison.Typechecker where

import Control.Monad
import Unison.Paths (Path)
import Unison.Term (AnnotatedTerm)
import Unison.Type (AnnotatedType)
import Unison.Var (Var)
import Unison.DataDeclaration (DataDeclaration', EffectDeclaration')
import Unison.Reference (Reference)
-- import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import qualified Unison.ABT as ABT
-- import qualified Unison.Paths as Paths
import qualified Unison.Term as Term
-- import qualified Unison.Type as Type
import qualified Unison.TypeVar as TypeVar
import qualified Unison.Typechecker.Context as Context

-- import Debug.Trace
-- watch msg a = trace (msg ++ show a) a

type Term v loc = AnnotatedTerm v loc
type Type v loc = AnnotatedType v loc

data Result v loc a = Result { notes :: Seq (Note v loc), result :: Maybe a }

data Note v loc
  = InvalidPath Path (Term v loc)
  | UnknownSymbol v loc
  | Typechecking (Context.Note v loc)
  -- WithinLocals (Note v loc)

failNote :: Note v loc -> Result v loc a
failNote note = Result (pure note) Nothing

data Env f v loc = Env {
  builtinLoc :: loc,
  ambientAbilities :: [Type v loc],
  typeOf :: Reference -> f (Type v loc),
  dataDeclaration :: Reference -> f (DataDeclaration' v loc),
  effectDeclaration :: Reference -> f (EffectDeclaration' v loc)
}

-- -- | Compute the allowed type of a replacement for a given subterm.
-- -- Example, in @\g -> map g [1,2,3]@, @g@ has an admissible type of
-- -- @Int -> r@, where @r@ is an unbound universal type variable, which
-- -- means that an @Int -> Bool@, an @Int -> String@, etc could all be
-- -- substituted for @g@.
-- --
-- -- Algorithm works by replacing the subterm, @e@ with
-- -- @(f e)@, where @f@ is a fresh function parameter. We then
-- -- read off the type of @e@ from the inferred result type of @f@.
-- admissibleTypeAt :: (Monad f, Var v)
--                  => (Env f v loc)
--                  -> Path
--                  -> Term v loc
--                  -> f (Result v loc (Type v loc))
-- admissibleTypeAt env path t =
--   let
--     f = ABT.v' "f"
--     shake (Type.Arrow' (Type.Arrow' _ tsub) _) = Type.generalize tsub
--     shake (Type.ForallNamed' _ t) = shake t
--     shake _ = error "impossible, f had better be a function"
--   in case Term.lam() f <$> Paths.modifyTerm (\t -> Term.app() (Term.var() (ABT.Free f)) (Term.wrapV t)) path t of
--     Nothing -> pure . failNote $ InvalidPath path t
--     Just t -> fmap shake <$> synthesize env t

-- -- | Compute the type of the given subterm.
-- typeAt :: (Monad f, Var v) => Env f v loc -> Path -> Term v loc -> f (Type v loc)
-- typeAt env [] t = synthesize env t
-- typeAt env path t =
--   let
--     f = ABT.v' "f"
--     remember e = Term.var() (ABT.Free f) `Term.app_` Term.wrapV e
--     shake (Type.Arrow' (Type.Arrow' tsub _) _) = Type.generalize tsub
--     shake (Type.ForallNamed' _ t) = shake t
--     shake _ = error "impossible, f had better be a function"
--   in case Term.lam() f <$> Paths.modifyTerm remember path t of
--     Nothing -> failNote $ InvalidPath path t
--     Just t -> pure . shake <$> synthesize env t
--
-- -- | Return the type of all local variables in scope at the given location
-- locals :: (Monad f, Var v) => Env f v loc -> Path -> Term v loc
--        -> f [(v, Type v loc)]
-- locals env path ctx | ABT.isClosed ctx =
--   zip (map ABT.unvar vars) <$> types
--   where
--     -- replace focus, x, with `let saved = f v1 v2 v3 ... vn in x`,
--     -- where `f` is fresh variable, then infer type of `f`, read off the
--     -- types of `v1`, `v2`, ...
--     vars = map ABT.Bound (Paths.inScopeAtTerm path ctx)
--     f = ABT.v' "f"
--     saved = ABT.v' "saved"
--     remember e = Term.let1_ [(saved, Term.var() (ABT.Free f) `Term.apps` map (((),) . Term.var()) vars)] (Term.wrapV e)
--     usingAllLocals = Term.lam() f (Paths.modifyTerm' remember path ctx)
--     types = if null vars then pure []
--             else extract <$> typeAt env [] usingAllLocals
--     extract (Type.Arrow' i _) = extract1 i
--     extract (Type.ForallNamed' _ t) = extract t
--     extract t = error $ "expected function type, got: " ++ show t
--     extract1 (Type.Arrow' i o) = i : extract1 o
--     extract1 _ = []
-- locals _ _ _ _ ctx =
--   -- need to call failNote multiple times
--   failNote <$> (uncurry UnknownSymbol <$> ABT.freeVarAnnotations ctx)


-- | Infer the type of a 'Unison.Term', using
-- a function to resolve the type of @Ref@ constructors
-- contained in that term.
synthesize :: (Monad f, Var v) => Env f v loc -> Term v loc
           -> f (Result v loc (Type v loc))
synthesize env t =
  let go (notes, ot) = Result (Typechecking <$> notes) (ABT.vmap TypeVar.underlying <$> ot)
  in go <$> Context.synthesizeClosed
      (builtinLoc env)
      (ABT.vmap TypeVar.Universal <$> ambientAbilities env)
      (typeOf env)
      (dataDeclaration env)
      (effectDeclaration env)
      (Term.vtmap TypeVar.Universal t)

-- | Check whether a term matches a type, using a
-- function to resolve the type of @Ref@ constructors
-- contained in the term. Returns @typ@ if successful,
-- and a note about typechecking failure otherwise.
check :: (Monad f, Var v) => Env f v loc -> Term v loc -> Type v loc
      -> f (Result v loc (Type v loc))
check env term typ = synthesize env (Term.ann (ABT.annotation term) term typ)

-- | `checkAdmissible' e t` tests that `(f : t -> r) e` is well-typed.
-- If `t` has quantifiers, these are moved outside, so if `t : forall a . a`,
-- this will check that `(f : forall a . a -> a) e` is well typed.
-- checkAdmissible' :: Var v => Term v -> Type v -> Either Note (Type v)
-- checkAdmissible' term typ =
--   synthesize' (Term.blank() `Term.ann_` tweak typ `Term.app_` term)
--   where
--     tweak (Type.ForallNamed' v body) = Type.forall() v (tweak body)
--     tweak t = Type.arrow() t t

-- | Returns `True` if the expression is well-typed, `False` otherwise
wellTyped :: (Monad f, Var v) => Env f v loc -> Term v loc -> f Bool
wellTyped env term = isJust . result <$> synthesize env term

-- | @subtype a b@ is @Right b@ iff @f x@ is well-typed given
-- @x : a@ and @f : b -> t@. That is, if a value of type `a`
-- can be passed to a function expecting a `b`, then `subtype a b`
-- returns `Right b`. This function returns @Left note@ with information
-- about the reason for subtyping failure otherwise.
--
-- Example: @subtype (forall a. a -> a) (Int -> Int)@ returns @Right (Int -> Int)@.
-- subtype :: Var v => Type v -> Type v -> Either Note (Type v)
-- subtype t1 t2 = error "todo"
  -- let (t1', t2') = (ABT.vmap TypeVar.Universal t1, ABT.vmap TypeVar.Universal t2)
  -- in case Context.runM (Context.subtype t1' t2')
  --                      (Context.MEnv Context.env0 [] Map.empty True) of
  --   Left e -> Left e
  --   Right _ -> Right t2

-- | Returns true if @subtype t1 t2@ returns @Right@, false otherwise
-- isSubtype :: Var v => Type v -> Type v -> Bool
-- isSubtype t1 t2 = case subtype t1 t2 of
--   Left _ -> False
--   Right _ -> True

-- | Returns true if the two type are equal, up to alpha equivalence and
-- order of quantifier introduction. Note that alpha equivalence considers:
-- `forall b a . a -> b -> a` and
-- `forall a b . a -> b -> a` to be different types
-- equals :: Var v => Type v -> Type v -> Bool
-- equals t1 t2 = isSubtype t1 t2 && isSubtype t2 t1


instance Functor (Result v loc) where
  fmap = liftM

instance Applicative (Result v loc) where
  pure = return
  (<*>) = ap

instance Monad (Result v loc) where
  return a = Result mempty (Just a)
  Result notes Nothing >>= _f = Result notes Nothing
  Result notes (Just a) >>= f = case f a of
    Result notes2 b -> Result (notes `mappend` notes2) b
