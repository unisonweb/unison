{-# language PatternGuards #-}
{-# language TupleSections #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Unison.Runtime.Decompile
  ( decompile ) where

import Prelude hiding (seq)
import Unison.Prelude
import qualified Data.ByteArray

import Unison.ABT (absChain, substs, pattern AbsN')
import Unison.Term
  ( Term
  , nat, int, char, float, boolean, constructor, app, apps', text, ref
  , seq, seq', builtin
  )
import Unison.Type
  ( natRef, intRef, charRef, floatRef, booleanRef, vectorRef
  )
import Unison.Var (Var)
import Unison.Reference (Reference)

import Unison.Runtime.ANF (RTag, CTag, Tag(..))
import Unison.Runtime.Foreign
  (Foreign, Hasher(..), HashAlgorithm(..), Hmacinator(..), maybeUnwrapBuiltin, maybeUnwrapForeign)
import Unison.Runtime.Stack
  (Closure(..), pattern DataC, pattern PApV, IComb(..))

import Unison.Codebase.Runtime (Error)
import Unison.Util.Pretty (lit)

import qualified Unison.Util.Bytes as By
import qualified Crypto.MAC.HMAC as HMAC

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
decompile _ _ cl@(PAp _ _ _)
  = err $ "cannot decompile a partial application to unboxed values: "
       ++ show cl
decompile _ _ (DataC{})
  = err "cannot decompile data type with multiple unboxed fields"
decompile _ _ BlackHole = err "exception"
decompile _ _ (Captured{}) = err "decompiling a captured continuation"
decompile tyRef topTerms (Foreign f) = decompileForeign tyRef topTerms f

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
  | r == charRef = pure . char () $ toEnum i
decompileUnboxed r _ _
  = err $ "cannot decompile unboxed data type with reference: " ++ show r

decompileForeign
  :: Var v
  => (RTag -> Maybe Reference)
  -> (Word64 -> Maybe (Term v ()))
  -> Foreign
  -> Either Error (Term v ())
decompileForeign tyRef topTerms f
  | Just t <- maybeUnwrapBuiltin f = Right $ text () t
  | Just b <- maybeUnwrapBuiltin f = Right $ decompileBytes b
  | Just h <- maybeUnwrapBuiltin f = Right $ decompileHasher h
  | Just h <- maybeUnwrapBuiltin f = Right $ decompileHashAlgorithm h
  | Just h <- maybeUnwrapBuiltin f = Right $ decompileHmacinator h
  | Just s <- unwrapSeq f
  = seq' () <$> traverse (decompile tyRef topTerms) s
decompileForeign _ _ _ = err "cannot decompile Foreign"

decompileBytes :: Var v => By.Bytes -> Term v ()
decompileBytes
  = app () (builtin () $ fromString "Bytes.fromList")
  . seq () . fmap (nat () . fromIntegral) . By.toWord8s

decompileHashAlgorithm :: Var v => HashAlgorithm -> Term v ()
decompileHashAlgorithm (HashAlgorithm r _) = ref () r

decompileHmacinator :: Var v => Hmacinator -> Term v ()
decompileHmacinator (Hmacinator r (HMAC.Context a b)) =
  apps' (builtin () "crypto.Hmac._internal.init") [
    ref () r,
    decompileBytes $ bs a,
    decompileBytes $ bs b
    ]
  where
  -- NB: a hashing context is just `newtype Context a = Context Data.ByteArray.Bytes`
  -- but cryptonite doesn't expose the constructor sadly
  bs ctx = By.fromArray (unsafeCoerce ctx :: Data.ByteArray.Bytes)

decompileHasher :: Var v => Hasher -> Term v ()
decompileHasher (Hasher r ctx) =
  apps' (builtin () "crypto.Hash._internal.init") [ref () r, decompileBytes bs]
  where
  -- NB: a hashing context is just `newtype Context a = Context Data.ByteArray.Bytes`
  -- but cryptonite doesn't expose the constructor sadly
  bs = By.fromArray (unsafeCoerce ctx :: Data.ByteArray.Bytes)

unwrapSeq :: Foreign -> Maybe (Seq Closure)
unwrapSeq = maybeUnwrapForeign vectorRef
