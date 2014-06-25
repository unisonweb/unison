-- | This module is the primary interface to the Unison typechecker
module Unison.Type where

import Control.Monad
import Control.Applicative
import qualified Unison.Type.Context as C
import qualified Unison.Syntax.Hash as H
import qualified Unison.Syntax.Type as T
import qualified Unison.Syntax.Term as E
import Unison.Note as N

-- | Infer the type of a 'Unison.Syntax.Term', using
-- a function to resolve the type of @Ref@ constructors
-- contained in that term.
synthesize :: Applicative f => (H.Hash -> Noted f T.Type) -> E.Term -> Noted f T.Type
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
check :: Applicative f => (H.Hash -> Noted f T.Type) -> E.Term -> T.Type -> Noted f T.Type
check synth term typ = synthesize synth (E.Ann term typ)

-- | Check whether a term, assumed to contain no @Ref@ constructors,
-- matches a given type. Return @Left@ if any references exist, or
-- if typechecking fails.
check' :: E.Term -> T.Type -> Either Note T.Type
check' term typ = join . N.unnote $ check missing term typ
  where missing h = N.failure $ "unexpected ref: " ++ show h

-- | @subtype a b@ is @Right b@ iff any @b -> t@ is well-typed when
-- given a value of type @a@, and is @Left note@ with information
-- about the reason for subtyping failure otherwise.
-- Example: the identity function, of type @forall a. a -> a@,
-- is a subtype of @Int -> Int@.
subtype :: T.Type -> T.Type -> Either Note T.Type
subtype t1 t2 = case C.subtype (C.context []) t1 t2 of
  Left e -> Left e
  Right _ -> Right t2

isSubtype :: T.Type -> T.Type -> Bool
isSubtype t1 t2 = case C.subtype (C.context []) t1 t2 of
  Left e -> False
  Right _ -> True
