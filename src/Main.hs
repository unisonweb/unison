module Main where

import Unison.Syntax.Term as E
import Unison.Syntax.Term.Examples

import Unison.Syntax.Type as T
import Unison.Type.Context as C
import Unison.Type.Note as N
import Unison.Syntax.Var as V

expr :: E.Term () a
expr = identity

identityAnn = E.Ann identity (forall1 $ \x -> x)
-- (subst t' v (T.Universal v'))

unit :: E.Term () a
unit = E.Lit ()

synthLit :: () -> ()
synthLit = id

showType :: Either N.Note (T.Type () ()) -> String
showType (Left err) = show err
showType (Right a) = show a

showCtx :: Context () () -> String
showCtx = show

idType :: Type () ()
idType = forall1 $ \x -> x

substIdType :: Type () () -> Type () ()
substIdType (Forall v t) = subst t v (T.Universal (V.decr V.bound1))

main :: IO ()
-- main = putStrLn . show $ (idType, substIdType idType)
-- main = putStrLn . showCtx . snd $ extendUniversal C.empty
main = putStrLn . showType $ C.synthesizeClosed synthLit identityAnn
