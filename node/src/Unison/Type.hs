-- | This module is the primary interface to the Unison typechecker
module Unison.Type where

import Debug.Trace
import Control.Monad
import Control.Applicative
import qualified Unison.Type.Context as C
import qualified Unison.Syntax.Type as T
import qualified Unison.Syntax.Term as E
import qualified Unison.Syntax.Var as V
import Unison.Note as N

-- | Infer the type of a 'Unison.Syntax.Term', using
-- a function to resolve the type of @Ref@ constructors
-- contained in that term.
synthesize :: Applicative f => T.Env f -> E.Term -> Noted f T.Type
synthesize = C.synthesizeClosed

-- | Infer the type of a 'Unison.Syntax.Term', assumed
-- not to contain any @Ref@ constructors
synthesize' :: E.Term -> Either Note T.Type
synthesize' term = join . N.unnote $ synthesize missing term
  where missing h = N.failure $ "unexpected ref: " ++ show h

-- | Check whether a term matches a type, using a
-- function to resolve the type of @Ref@ constructors
-- contained in the term. Returns @typ@ if successful,
-- and a note about typechecking failure otherwise.
check :: Applicative f => T.Env f -> E.Term -> T.Type -> Noted f T.Type
check synth term typ = synthesize synth (E.Ann term typ)

-- | Check whether a term, assumed to contain no @Ref@ constructors,
-- matches a given type. Return @Left@ if any references exist, or
-- if typechecking fails.
check' :: E.Term -> T.Type -> Either Note T.Type
check' term typ = join . N.unnote $ check missing term typ
  where missing h = N.failure $ "unexpected ref: " ++ show h

watch msg a = trace (msg ++ show a) a

-- | Returns `True` if the expression is well-typed, `False` otherwise
wellTyped :: (Monad f, Applicative f) => T.Env f -> E.Term -> Noted f Bool
wellTyped synth term = (const True <$> synthesize synth term) `N.orElse` pure False

-- | @subtype a b@ is @Right b@ iff @f x@ is well-typed given
-- @x : a@ and @f : b -> t@. That is, if a value of type `a`
-- can be passed to a function expecting a `b`, then `subtype a b`
-- returns `Right b`. This function returns @Left note@ with information
-- about the reason for subtyping failure otherwise.
--
-- Example: @subtype (forall a. a -> a) (Int -> Int)@ returns @Right (Int -> Int)@.
subtype :: T.Type -> T.Type -> Either Note T.Type
subtype t1 t2 = case C.subtype (C.context []) t1 t2 of
  Left e -> Left e
  Right _ -> Right t2

-- | Returns true if @subtype t1 t2@ returns @Right@, false otherwise
isSubtype :: T.Type -> T.Type -> Bool
isSubtype t1 t2 = case C.subtype (C.context []) t1 t2 of
  Left _ -> False
  Right _ -> True
