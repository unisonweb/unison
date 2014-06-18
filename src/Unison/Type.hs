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
synthesize :: Applicative f => (H.Hash -> f (Either Note T.Type)) -> E.Term -> f (Either Note T.Type)
synthesize = C.synthesizeClosed

-- | Infer the type of a 'Unison.Syntax.Term', assumed
-- not to contain any @Ref@ constructors
synthesize' :: E.Term -> Either Note T.Type
synthesize' term = join $ synthesize missing term
  where missing h = pure . Left . N.note $ "unexpected ref: " ++ show h

-- | Check whether a term matches a type, using a
-- function to resolve the type of @Ref@ constructors
-- contained in the term
check :: Applicative f => (H.Hash -> f (Either Note T.Type)) -> E.Term -> T.Type -> f Bool
check synth term typ = go <$> synthesize synth (E.Ann term typ)
  where go (Left _) = False
        go (Right _) = True

-- | Check whether a term, assumed to contain no @Ref@ constructors,
-- matches a given type. Return @Left@ if any references exist.
check' :: E.Term -> T.Type -> Either Note Bool
check' term typ = check missing term typ
  where missing h = Left . N.note $ "unexpected ref: " ++ show h

-- | @subtype a b@ is @Right b@ iff any @b -> t@ is well-typed when
-- given a value of type @a@, and is @Left note@ with information
-- about the reason for subtyping failure otherwise.
-- Example: the identity function, of type @forall a. a -> a@,
-- is a subtype of @Int -> Int@.
subtype :: T.Type -> T.Type -> Either Note T.Type
subtype t1 t2 = case C.subtype (C.context []) t1 t2 of
  Left e -> Left e
  Right _ -> Right t2
