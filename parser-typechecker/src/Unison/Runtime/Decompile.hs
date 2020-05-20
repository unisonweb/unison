{-# language PatternGuards #-}
{-# language TupleSections #-}
{-# language PatternSynonyms #-}

module Unison.Runtime.Decompile
  ( decompile ) where

import Data.Word (Word64)

import Unison.Term
  ( Term
  , nat, int, float, boolean, constructor, apps
  )
import Unison.Type
  ( natRef, intRef, floatRef, booleanRef
  )
import Unison.Var (Var)
import Unison.Reference (Reference)

import Unison.Runtime.ANF (CTag, Tag(..))
import Unison.Runtime.Stack (Closure(..), pattern DataC)

import Unsafe.Coerce -- for Int -> Double

con :: Var v => Reference -> CTag -> Term v ()
con rf ct = constructor () rf . fromIntegral $ rawTag ct

decompile
  :: Var v => (Word64 -> Maybe Reference) -> Closure
  -> Either String (Term v ())
decompile backref (DataC rt ct [] [])
  | Just rf <- backref $ rawTag rt
  , rf == booleanRef
  = Right $ boolean () $ tag2bool ct
decompile backref (DataC rt ct [i] [])
  | Just rf <- backref $ rawTag rt
  = decompileUnboxed rf ct i
decompile backref (DataC rt ct [] bs)
  | Just rf <- backref $ rawTag rt
  = apps (con rf ct) . fmap ((),) <$> traverse (decompile backref) bs
decompile _ (DataC{})
  = Left "cannot decompile data type with multiple unboxed fields"
decompile _ BlackHole = Left "exception"
decompile _ (Captured{}) = Left "decompiling a captured continuation"
decompile _ (Foreign _) = Left "todo: decompile Foreign"
decompile _ (PAp _ _ _) = Left "todo: decompile PAp"

tag2bool :: CTag -> Bool
tag2bool c = case rawTag c of
  0 -> False
  1 -> True
  _ -> error "bad boolean tag"

decompileUnboxed
  :: Var v => Reference -> CTag -> Int -> Either String (Term v ())
decompileUnboxed r _ i
  | r == natRef = pure . nat () $ fromIntegral i
  | r == intRef = pure . int () $ fromIntegral i
  | r == floatRef = pure . float () $ unsafeCoerce i
decompileUnboxed _ _ _ = Left "cannot decompile unboxed data type"
