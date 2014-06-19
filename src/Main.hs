module Main where

import Control.Monad
import Unison.Syntax.Term as E
import Unison.Syntax.Type as T
import Unison.Type as Type
import Unison.Note as N
import Unison.Syntax.Var as V

identity :: E.Term
identity = E.lam1 $ \x -> x

-- type Any = forall r . (forall a . a -> r) -> r

anyT :: Type
anyT = forall1 $ \r -> (forall1 $ \a -> a `T.Arrow` r) `T.Arrow` r

anyE :: Term
anyE = lam2 $ \x f -> f `E.App` x

--
-- Forall x1
--   (Forall x1
--     (Arrow (Universal x1)
--            (Arrow (Arrow (Universal x1) (Universal x2))
--                   (Universal x2)
--            )
--     )
--   )
expr :: E.Term
expr = anyE

identityAnn = E.Ann identity (forall1 $ \x -> T.Arrow x x)

showType :: Either N.Note T.Type -> String
showType (Left err) = show err
showType (Right a) = show a

idType :: Type
idType = forall1 $ \x -> x

substIdType :: Type -> Type
substIdType (Forall v t) = subst t v (T.Universal (V.decr V.bound1))

main :: IO ()
-- main = putStrLn . show $ (idType, substIdType idType)
-- main = putStrLn . showCtx . snd $ extendUniversal C.empty
main = putStrLn . showType $ Type.synthesize' expr
