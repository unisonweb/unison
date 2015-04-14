module Main where

import Control.Monad
import Unison.Term as E
import Unison.Type as T
import Unison.Typechecker as Typechecker
import Unison.Note as N
import Unison.Var as V

identity :: E.Term
identity = E.unscope (E.lam E.var)

constant :: E.Term
constant = E.unscope (E.lam (E.lam (E.weaken E.var)))

apply :: E.Term
apply = E.unscope (E.lam (E.lam (E.weaken E.var `E.app` E.var)))

-- type Any = forall r . (forall a . a -> r) -> r
anyT :: Type
anyT = forall1 $ \r -> (forall1 $ \a -> a `T.Arrow` r) `T.Arrow` r

-- (x4 x3 → (x2 x1 → x2) x1 (x4 x1 : Number))
expr :: E.Term
expr = identityAnn

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
main = putStrLn . showType $ Typechecker.synthesize' expr
