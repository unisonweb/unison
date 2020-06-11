{-# language PatternGuards #-}
{-# language TupleSections #-}
{-# language PatternSynonyms #-}

module Unison.Runtime.Decompile
  ( decompile ) where

import Data.String (fromString)
import Data.Word (Word64)

import Unison.ABT (absChain, substs, pattern AbsN')
import Unison.Term
  ( Term
  , nat, int, float, boolean, constructor, apps', text
  )
import Unison.Type
  ( natRef, intRef, floatRef, booleanRef
  )
import Unison.Var (Var)
import Unison.Reference (Reference)

import Unison.Runtime.ANF (RTag, CTag, Tag(..))
import Unison.Runtime.Foreign (Foreign, unwrapText)
import Unison.Runtime.Stack
  (Closure(..), pattern DataC, pattern PApV, IComb(..))

import Unison.Codebase.Runtime (Error)
import Unison.Util.Pretty (lit)

import Unsafe.Coerce -- for Int -> Double

con :: Var v => Reference -> CTag -> Term v ()
con rf ct = constructor () rf . fromIntegral $ rawTag ct

err :: String -> Either Error a
err = Left . lit . fromString

decompile
  :: Var v
  => (RTag -> Maybe Reference)
  -> (Word64 -> Maybe (Term v ()))
  -> Closure
  -> Either Error (Term v ())
decompile tyRef _ (DataC rt ct [] [])
  | Just rf <- tyRef rt
  , rf == booleanRef
  = boolean () <$> tag2bool ct
decompile tyRef _ (DataC rt ct [i] [])
  | Just rf <- tyRef rt
  = decompileUnboxed rf ct i
decompile tyRef topTerms (DataC rt ct [] bs)
  | Just rf <- tyRef rt
  = apps' (con rf ct) <$> traverse (decompile tyRef topTerms) bs
decompile tyRef topTerms (PApV (IC rt _) [] bs)
  | Just t <- topTerms rt
  = substitute t <$> traverse (decompile tyRef topTerms) bs
  | otherwise
  = err "reference to unknown combinator"
decompile _ _ (PAp _ _ _)
  = err "cannot decompile a partial application to unboxed values"
decompile _ _ (DataC{})
  = err "cannot decompile data type with multiple unboxed fields"
decompile _ _ BlackHole = err "exception"
decompile _ _ (Captured{}) = err "decompiling a captured continuation"
decompile tyRef _ (Foreign f) = decompileForeign tyRef f

tag2bool :: CTag -> Either Error Bool
tag2bool c = case rawTag c of
  0 -> Right False
  1 -> Right True
  _ -> err "bad boolean tag"

substitute :: Var v => Term v () -> [Term v ()] -> Term v ()
substitute (AbsN' vs bd) ts = align [] vs ts
  where
  align vts (v:vs) (t:ts) = align ((v,t):vts) vs ts
  align vts vs [] = substs vts (absChain vs bd)
  -- this should not happen
  align vts [] ts = apps' (substs vts bd) ts
-- TODO: these aliases are not actually very conveniently written
substitute _ _ = error "impossible"

decompileUnboxed
  :: Var v => Reference -> CTag -> Int -> Either Error (Term v ())
decompileUnboxed r _ i
  | r == natRef = pure . nat () $ fromIntegral i
  | r == intRef = pure . int () $ fromIntegral i
  | r == floatRef = pure . float () $ unsafeCoerce i
decompileUnboxed r _ _
  = err $ "cannot decompile unboxed data type with reference: " ++ show r

decompileForeign
  :: Var v
  => (RTag -> Maybe Reference)
  -> Foreign
  -> Either Error (Term v ())
decompileForeign _ f
  | Just t <- unwrapText f = Right $ text () t
decompileForeign _ _ = err "cannot decompile Foreign"
