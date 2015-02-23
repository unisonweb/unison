module Main where

import Control.Monad
import Unison.Syntax.Term as E
import Unison.Syntax.Type as T
import Unison.Type as Type
import Unison.Note as N
import Unison.Syntax.Var as V

identity :: E.Term
identity = E.lam1 $ \x -> x

(@) = E.App

constant :: E.Term
constant = E.lam2 $ \x y -> x

apply :: E.Term
apply = E.lam2 $ \f x -> f `E.App` x

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
main = putStrLn . showType $ Type.synthesize' expr
