-- | This module is the primary interface to the Unison typechecker
module Unison.A_Typechecker (synthesize, synthesize', check, check', wellTyped, subtype, isSubtype) where

import Control.Applicative
import Control.Monad
import Unison.A_Type (Type)
import Unison.A_Term (Term)
import Unison.Note (Note,Noted)
import qualified Unison.Note as Note
import qualified Unison.A_Term as Term
import qualified Unison.A_Type as Type
import qualified Unison.Typechecker.A_Context as Context

--import Debug.Trace
--watch msg a = trace (msg ++ show a) a

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
