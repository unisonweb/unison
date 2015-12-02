{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | This module is the primary interface to the Unison typechecker
module Unison.Typechecker (admissibleTypeAt, check, check', checkAdmissible', equals, isSubtype, locals, subtype, synthesize, synthesize', typeAt, wellTyped) where

import Control.Monad
import Unison.Type (Type)
import Unison.Term (Term)
import Unison.Note (Note,Noted)
import Unison.Var (Var)
import Unison.Paths (Path)
import qualified Unison.Paths as Paths
import qualified Unison.ABT as ABT
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.Note as Note
import qualified Unison.Typechecker.Context as Context

--import Debug.Trace
--watch msg a = trace (msg ++ show a) a

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
admissibleTypeAt :: (Applicative f, Var v)
                 => Type.Env f v
                 -> Path
                 -> Term v
                 -> Noted f (Type v)
admissibleTypeAt synth loc t = Note.scoped ("admissibleTypeAt@" ++ show loc ++ " " ++ show t) $
  let
    f = Term.fresh t (ABT.v' "s")
    shake (Type.Arrow' (Type.Arrow' _ tsub) _) = Type.generalize tsub
    shake (Type.Forall' _ t) = shake t
    shake _ = error "impossible, f had better be a function"
  in case Term.lam f <$> Paths.modifyTerm (Term.app (Term.var f)) loc t of
    Nothing -> Note.failure $ invalid loc t
    Just t -> shake <$> synthesize synth t

-- | Compute the type of the given subterm.
typeAt :: (Applicative f, Var v) => Type.Env f v -> Path -> Term v -> Noted f (Type v)
typeAt synth [] t = Note.scoped ("typeAt: " ++ show t) $ synthesize synth t
typeAt synth loc t = Note.scoped ("typeAt@"++show loc ++ " " ++ show t) $
  let
    f = Term.fresh t (ABT.v' "t")
    shake (Type.Arrow' (Type.Arrow' tsub _) _) = Type.generalize tsub
    shake (Type.Forall' _ t) = shake t
    shake _ = error "impossible, f had better be a function"
  in case Term.lam f <$> Paths.modifyTerm (Term.app (Term.var f)) loc t of
    Nothing -> Note.failure $ invalid loc t
    Just t -> shake <$> synthesize synth t

-- | Return the type of all local variables in scope at the given location
locals :: (Applicative f, Var v) => Type.Env f v -> Path -> Term v -> Noted f [(v, Type v)]
locals synth path ctx | ABT.isClosed ctx =
  Note.scoped ("locals@"++show path ++ " " ++ show ctx)
              (zip (map fst lambdas) <$> lambdaTypes)
  where
    -- lambdas :: [(v, Term.Path)]
    lambdas = Paths.pathPrefixes path >>= \path -> case Paths.atTerm path ctx of
      Just (Term.Lam' v _) -> [(v, path)]
      _ -> []

    lambdaTypes = traverse t (map snd lambdas)
      where t path = extract <$> typeAt synth path ctx

    extract :: Var v => Type v -> Type v
    extract (Type.Arrow' i _) = i
    extract (Type.Forall' _ t) = extract t
    extract t = error $ "expecting function type, got " ++ show t
locals _ _ ctx =
  Note.failure $ "Term.locals: term contains free variables - " ++ show ctx

-- | Infer the type of a 'Unison.Syntax.Term', using
-- a function to resolve the type of @Ref@ constructors
-- contained in that term.
synthesize :: (Applicative f, Var v) => Type.Env f v -> Term v -> Noted f (Type v)
synthesize = Context.synthesizeClosed

-- | Infer the type of a 'Unison.Syntax.Term', assumed
-- not to contain any @Ref@ constructors
synthesize' :: Var v => Term v -> Either Note (Type v)
synthesize' term = join . Note.unnote $ synthesize missing term
  where missing h = Note.failure $ "unexpected ref: " ++ show h

-- | Check whether a term matches a type, using a
-- function to resolve the type of @Ref@ constructors
-- contained in the term. Returns @typ@ if successful,
-- and a note about typechecking failure otherwise.
check :: (Applicative f, Var v) => Type.Env f v -> Term v -> Type v -> Noted f (Type v)
check env term typ = synthesize env (Term.ann term typ)

-- | Check whether a term, assumed to contain no @Ref@ constructors,
-- matches a given type. Return @Left@ if any references exist, or
-- if typechecking fails.
check' :: Var v => Term v -> Type v -> Either Note (Type v)
check' term typ = join . Note.unnote $ check missing term typ
  where missing h = Note.failure $ "unexpected ref: " ++ show h

-- | `checkAdmissible' e t` tests that `(f : t -> r) e` is well-typed.
checkAdmissible' :: Var v => Term v -> Type v -> Either Note (Type v)
checkAdmissible' term typ = synthesize' (Term.blank `Term.ann` (typ `Type.arrow` typ) `Term.app` term)

-- | Returns `True` if the expression is well-typed, `False` otherwise
wellTyped :: (Monad f, Var v) => Type.Env f v -> Term v -> Noted f Bool
wellTyped synth term = (const True <$> synthesize synth term) `Note.orElse` pure False

-- | @subtype a b@ is @Right b@ iff @f x@ is well-typed given
-- @x : a@ and @f : b -> t@. That is, if a value of type `a`
-- can be passed to a function expecting a `b`, then `subtype a b`
-- returns `Right b`. This function returns @Left note@ with information
-- about the reason for subtyping failure otherwise.
--
-- Example: @subtype (forall a. a -> a) (Int -> Int)@ returns @Right (Int -> Int)@.
subtype :: Var v => Type v -> Type v -> Either Note (Type v)
subtype t1 t2 = case Context.subtype (Context.context []) t1 t2 of
  Left e -> Left e
  Right _ -> Right t2

-- | Returns true if @subtype t1 t2@ returns @Right@, false otherwise
isSubtype :: Var v => Type v -> Type v -> Bool
isSubtype t1 t2 = case Context.subtype (Context.context []) t1 t2 of
  Left _ -> False
  Right _ -> True

-- | Returns true if the two type are equal, up to alpha equivalence and
-- order of quantifier introduction. Note that alpha equivalence considers:
-- `forall b a . a -> b -> a` and
-- `forall a b . a -> b -> a` to be different types
equals :: Var v => Type v -> Type v -> Bool
equals t1 t2 = isSubtype t1 t2 && isSubtype t2 t1

