{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | This module is the primary interface to the Unison typechecker
module Unison.Typechecker (admissibleTypeAt, check, check', checkAdmissible', equals, locals, subtype, isSubtype, synthesize, synthesize', typeAt, wellTyped) where

import Control.Monad
import Unison.Note (Note,Noted)
import Unison.Paths (Path)
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Var (Var)
import Unison.DataDeclaration (DataDeclaration)
import Unison.Reference (Reference)
import qualified Data.Map as Map
import qualified Unison.ABT as ABT
import qualified Unison.Note as Note
import qualified Unison.Paths as Paths
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.TypeVar as TypeVar
import qualified Unison.Typechecker.Context as Context

-- import Debug.Trace
-- watch msg a = trace (msg ++ show a) a

invalid :: (Show a1, Show a) => a -> a1 -> String
invalid loc ctx = "invalid path " ++ show loc ++ " in:\n" ++ show ctx

-- | Compute the allowed type of a replacement for a given subterm.
-- Example, in @\g -> map g [1,2,3]@, @g@ has an admissible type of
-- @Int -> r@, where @r@ is an unbound universal type variable, which
-- means that an @Int -> Bool@, an @Int -> String@, etc could all be
-- substituted for @g@.
--
-- Algorithm works by replacing the subterm, @e@ with
-- @(f e)@, where @f@ is a fresh function parameter. We then
-- read off the type of @e@ from the inferred result type of @f@.
admissibleTypeAt :: (Monad f, Var v)
                 => [Type v]
                 -> (Reference -> Noted f (Type v))
                 -> (Reference -> Noted f (DataDeclaration v))
                 -> Path
                 -> Term v
                 -> Noted f (Type v)
admissibleTypeAt abilities typeOf decl loc t = Note.scoped ("admissibleTypeAt@" ++ show loc ++ " " ++ show t) $
  let
    f = ABT.v' "f"
    shake (Type.Arrow' (Type.Arrow' _ tsub) _) = Type.generalize tsub
    shake (Type.ForallNamed' _ t) = shake t
    shake _ = error "impossible, f had better be a function"
  in case Term.lam f <$> Paths.modifyTerm (\t -> Term.app (Term.var (ABT.Free f)) (Term.wrapV t)) loc t of
    Nothing -> Note.failure $ invalid loc t
    Just t -> shake <$> synthesize abilities typeOf decl t

-- | Compute the type of the given subterm.
typeAt :: (Monad f, Var v)
       => [Type v]
       -> (Reference -> Noted f (Type v))
       -> (Reference -> Noted f (DataDeclaration v))
       -> Path
       -> Term v -> Noted f (Type v)
typeAt abilities typeOf decl [] t = Note.scoped ("typeAt: " ++ show t) $ synthesize abilities typeOf decl t
typeAt abilities typeOf decl loc t = Note.scoped ("typeAt@"++show loc ++ " " ++ show t) $
  let
    f = ABT.v' "f"
    remember e = Term.var (ABT.Free f) `Term.app` Term.wrapV e
    shake (Type.Arrow' (Type.Arrow' tsub _) _) = Type.generalize tsub
    shake (Type.ForallNamed' _ t) = shake t
    shake _ = error "impossible, f had better be a function"
  in case Term.lam f <$> Paths.modifyTerm remember loc t of
    Nothing -> Note.failure $ invalid loc t
    Just t -> shake <$> synthesize abilities typeOf decl t

-- | Return the type of all local variables in scope at the given location
locals :: (Show v, Monad f, Var v)
       => [Type v]
       -> (Reference -> Noted f (Type v))
       -> (Reference -> Noted f (DataDeclaration v))
       -> Path
       -> Term v -> Noted f [(v, Type v)]
locals abilities typeOf decl path ctx | ABT.isClosed ctx =
  Note.scoped ("locals@"++show path ++ " " ++ show ctx)
              ((zip (map ABT.unvar vars)) <$> types)
  where
    -- replace focus, x, with `let saved = f v1 v2 v3 ... vn in x`,
    -- where `f` is fresh variable, then infer type of `f`, read off the
    -- types of `v1`, `v2`, ...
    vars = map ABT.Bound (Paths.inScopeAtTerm path ctx)
    f = ABT.v' "f"
    saved = ABT.v' "saved"
    remember e = Term.let1 [(saved, Term.var (ABT.Free f) `Term.apps` map Term.var vars)] (Term.wrapV e)
    usingAllLocals = Term.lam f (Paths.modifyTerm' remember path ctx)
    types = if null vars then pure []
            else extract <$> typeAt abilities typeOf decl [] usingAllLocals
    extract (Type.Arrow' i _) = extract1 i
    extract (Type.ForallNamed' _ t) = extract t
    extract t = error $ "expected function type, got: " ++ show t
    extract1 (Type.Arrow' i o) = i : extract1 o
    extract1 _ = []
locals _ _ _ _ ctx =
  Note.failure $ "Term.locals: term contains free variables - " ++ show ctx

-- | Infer the type of a 'Unison.Term', using
-- a function to resolve the type of @Ref@ constructors
-- contained in that term.
synthesize
  :: (Monad f, Var v)
  => [Type v]
  -> (Reference -> Noted f (Type v))
  -> (Reference -> Noted f (DataDeclaration v))
  -> Term v
  -> Noted f (Type v)
synthesize abilities typeOf decl t =
  ABT.vmap TypeVar.underlying <$>
    Context.synthesizeClosed (ABT.vmap TypeVar.Universal <$> abilities)
                             typeOf
                             decl
                             (Term.vtmap TypeVar.Universal t)

-- | Infer the type of a 'Unison.Term', assumed
-- not to contain any @Ref@ constructors
synthesize' :: Var v => Term v -> Either Note (Type v)
synthesize' term = join . Note.unnote $ synthesize [] missing missingD term
  where missing h = Note.failure $ "unexpected term ref: " ++ show h
        missingD h = Note.failure $ "unexpected data declaration reference: " ++ show h

-- | Check whether a term matches a type, using a
-- function to resolve the type of @Ref@ constructors
-- contained in the term. Returns @typ@ if successful,
-- and a note about typechecking failure otherwise.
check
  :: (Monad f, Var v)
  => [Type v]
  -> (Reference -> Noted f (Type v))
  -> (Reference -> Noted f (DataDeclaration v))
  -> Term v
  -> Type v -> Noted f (Type v)
check abilities typeOf decl term typ = synthesize abilities typeOf decl (Term.ann term typ)

-- | Check whether a term, assumed to contain no @Ref@ constructors,
-- matches a given type. Return @Left@ if any references exist, or
-- if typechecking fails.
check' :: Var v => Term v -> Type v -> Either Note (Type v)
check' term typ = join . Note.unnote $ check [] missing missingD term typ
  where missing h = Note.failure $ "unexpected term reference: " ++ show h
        missingD h = Note.failure $ "unexpected data declaration reference: " ++ show h

-- | `checkAdmissible' e t` tests that `(f : t -> r) e` is well-typed.
-- If `t` has quantifiers, these are moved outside, so if `t : forall a . a`,
-- this will check that `(f : forall a . a -> a) e` is well typed.
checkAdmissible' :: Var v => Term v -> Type v -> Either Note (Type v)
checkAdmissible' term typ =
  synthesize' (Term.blank `Term.ann` tweak typ `Term.app` term)
  where
    tweak (Type.ForallNamed' v body) = Type.forall() v (tweak body)
    tweak t = Type.arrow() t t

-- | Returns `True` if the expression is well-typed, `False` otherwise
wellTyped
  :: (Monad f, Var v)
  => [Type v]
  -> (Reference -> Noted f (Type v))
  -> (Reference -> Noted f (DataDeclaration v))
  -> Term v
  -> Noted f Bool
wellTyped abilities typeOf decl term =
  (const True <$> synthesize abilities typeOf decl term) `Note.orElse` pure False

-- | @subtype a b@ is @Right b@ iff @f x@ is well-typed given
-- @x : a@ and @f : b -> t@. That is, if a value of type `a`
-- can be passed to a function expecting a `b`, then `subtype a b`
-- returns `Right b`. This function returns @Left note@ with information
-- about the reason for subtyping failure otherwise.
--
-- Example: @subtype (forall a. a -> a) (Int -> Int)@ returns @Right (Int -> Int)@.
subtype :: Var v => Type v -> Type v -> Either Note (Type v)
subtype t1 t2 =
  let (t1', t2') = (ABT.vmap TypeVar.Universal t1, ABT.vmap TypeVar.Universal t2)
  in case Context.runM (Context.subtype t1' t2')
                       (Context.MEnv Context.env0 [] Map.empty True) of
    Left e -> Left e
    Right _ -> Right t2

-- | Returns true if @subtype t1 t2@ returns @Right@, false otherwise
isSubtype :: Var v => Type v -> Type v -> Bool
isSubtype t1 t2 = case subtype t1 t2 of
  Left _ -> False
  Right _ -> True

-- | Returns true if the two type are equal, up to alpha equivalence and
-- order of quantifier introduction. Note that alpha equivalence considers:
-- `forall b a . a -> b -> a` and
-- `forall a b . a -> b -> a` to be different types
equals :: Var v => Type v -> Type v -> Bool
equals t1 t2 = isSubtype t1 t2 && isSubtype t2 t1
