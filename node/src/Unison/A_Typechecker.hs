{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | This module is the primary interface to the Unison typechecker
module Unison.A_Typechecker (admissibleTypeAt, check, check', isSubtype, locals, subtype, synthesize, synthesize', typeAt, wellTyped) where

import Control.Applicative
import Control.Monad
import Data.Traversable
import Unison.A_Type (Type)
import Unison.A_Term (Term)
import Unison.Note (Note,Noted)
import qualified Unison.ABT as ABT
import qualified Unison.A_Term as Term
import qualified Unison.A_Type as Type
import qualified Unison.Note as Note
import qualified Unison.Typechecker.A_Context as Context

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
--
-- Note: the returned type may contain free type variables, since
-- we strip off any outer foralls.
admissibleTypeAt :: Applicative f
                 => Type.Env f
                 -> Term.Path
                 -> Term
                 -> Noted f Type
admissibleTypeAt synth loc t =
  let
    f = Term.freshIn t (ABT.v' "s")
    shake (Type.Arrow' (Type.Arrow' _ tsub) _) = tsub
    shake (Type.Forall' _ t) = shake t
    shake _ = error "impossible, f had better be a function"
  in case Term.lam f <$> Term.modify (Term.app (Term.var f)) loc t of
    Nothing -> Note.failure $ invalid loc t
    Just t -> shake <$> synthesize synth t

-- | Compute the type of the given subterm.
typeAt :: Applicative f => Type.Env f -> Term.Path -> Term -> Noted f Type
typeAt synth [] t = Note.scoped ("typeOf: " ++ show t) $ synthesize synth t
typeAt synth loc t = Note.scoped ("typeOf@"++show loc ++ " " ++ show t) $
  let
    f = Term.freshIn t (ABT.v' "t")
    shake (Type.Arrow' (Type.Arrow' tsub _) _) = tsub
    shake (Type.Forall' _ t) = shake t
    shake _ = error "impossible, f had better be a function"
  in case Term.lam f <$> Term.modify (Term.app (Term.var f)) loc t of
    Nothing -> Note.failure $ invalid loc t
    Just t -> shake <$> synthesize synth t

-- | Return the type of all local variables in scope at the given location
locals :: Applicative f => Type.Env f -> Term.Path -> Term -> Noted f [(ABT.V, Type)]
locals synth path ctx | ABT.isClosed ctx =
  Note.scoped ("locals@"++show path ++ " " ++ show ctx)
              (zip (map fst lambdas) <$> lambdaTypes)
  where
    lambdas :: [(ABT.V, Term.Path)]
    lambdas = Term.pathPrefixes path >>= \path -> case Term.at path ctx of
      Just (Term.Lam' v _) -> [(v, path)]
      _ -> []

    lambdaTypes = traverse t (map snd lambdas)
      where t path = extract <$> typeAt synth path ctx

    extract :: Type -> Type
    extract (Type.Arrow' i o) = i
    extract (Type.Forall' _ t) = extract t
    extract t = error $ "expecting function type, got " ++ show t
locals _ _ ctx =
  Note.failure $ "Term.locals: term contains free variables - " ++ show ctx

-- | Infer the type of a 'Unison.Syntax.Term', using
-- a function to resolve the type of @Ref@ constructors
-- contained in that term.
synthesize :: Applicative f => Type.Env f -> Term -> Noted f Type
synthesize = Context.synthesizeClosed

-- | Infer the type of a 'Unison.Syntax.Term', assumed
-- not to contain any @Ref@ constructors
synthesize' :: Term -> Either Note Type
synthesize' term = join . Note.unnote $ synthesize missing term
  where missing h = Note.failure $ "unexpected ref: " ++ show h

-- | Check whether a term matches a type, using a
-- function to resolve the type of @Ref@ constructors
-- contained in the term. Returns @typ@ if successful,
-- and a note about typechecking failure otherwise.
check :: Applicative f => Type.Env f -> Term -> Type -> Noted f Type
check synth term typ = synthesize synth (Term.ann term typ)

-- | Check whether a term, assumed to contain no @Ref@ constructors,
-- matches a given type. Return @Left@ if any references exist, or
-- if typechecking fails.
check' :: Term -> Type -> Either Note Type
check' term typ = join . Note.unnote $ check missing term typ
  where missing h = Note.failure $ "unexpected ref: " ++ show h

-- | Returns `True` if the expression is well-typed, `False` otherwise
wellTyped :: (Monad f, Applicative f) => Type.Env f -> Term -> Noted f Bool
wellTyped synth term = (const True <$> synthesize synth term) `Note.orElse` pure False

-- | @subtype a b@ is @Right b@ iff @f x@ is well-typed given
-- @x : a@ and @f : b -> t@. That is, if a value of type `a`
-- can be passed to a function expecting a `b`, then `subtype a b`
-- returns `Right b`. This function returns @Left note@ with information
-- about the reason for subtyping failure otherwise.
--
-- Example: @subtype (forall a. a -> a) (Int -> Int)@ returns @Right (Int -> Int)@.
subtype :: Type -> Type -> Either Note Type
subtype t1 t2 = case Context.subtype (Context.context []) t1 t2 of
  Left e -> Left e
  Right _ -> Right t2

-- | Returns true if @subtype t1 t2@ returns @Right@, false otherwise
isSubtype :: Type -> Type -> Bool
isSubtype t1 t2 = case Context.subtype (Context.context []) t1 t2 of
  Left _ -> False
  Right _ -> True
