{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Unison.Runtime.Foreign
  ( Foreign (..),
    HashAlgorithm (..),
    unwrapForeign,
    maybeUnwrapForeign,
    wrapBuiltin,
    maybeUnwrapBuiltin,
    unwrapBuiltin,
    BuiltinForeign (..),
    Tls (..),
    Failure (..),
  )
where

import Control.Concurrent (MVar, ThreadId)
import qualified Crypto.Hash as Hash
import Data.IORef (IORef)
import Data.Tagged (Tagged (..))
import qualified Data.X509 as X509
import Network.Socket (Socket)
import qualified Network.TLS as TLS (ClientParams, Context, ServerParams)
import System.Clock (TimeSpec)
import System.IO (Handle)
import Unison.Reference (Reference)
import Unison.Referent (Referent)
import Unison.Runtime.ANF (SuperGroup, Value)
import Unison.Symbol (Symbol)
import qualified Unison.Type as Ty
import Unison.Util.Bytes (Bytes)
import Unison.Util.Text (Text)
import Unsafe.Coerce

data Foreign where
  Wrap :: Reference -> !e -> Foreign

promote :: (a -> a -> r) -> b -> c -> r
promote (~~) x y = unsafeCoerce x ~~ unsafeCoerce y

tylEq :: Reference -> Reference -> Bool
tylEq r l = r == l
{-# NOINLINE tylEq #-}

tmlEq :: Referent -> Referent -> Bool
tmlEq r l = r == l
{-# NOINLINE tmlEq #-}

tylCmp :: Reference -> Reference -> Ordering
tylCmp r l = compare r l
{-# NOINLINE tylCmp #-}

tmlCmp :: Referent -> Referent -> Ordering
tmlCmp r l = compare r l
{-# NOINLINE tmlCmp #-}

ref2eq :: Reference -> Maybe (a -> b -> Bool)
ref2eq r
  | r == Ty.textRef = Just $ promote ((==) @Text)
  | r == Ty.termLinkRef = Just $ promote tmlEq
  | r == Ty.typeLinkRef = Just $ promote tylEq
  | r == Ty.bytesRef = Just $ promote ((==) @Bytes)
  -- Note: MVar equality is just reference equality, so it shouldn't
  -- matter what type the MVar holds.
  | r == Ty.mvarRef = Just $ promote ((==) @(MVar ()))
  -- Ditto
  | r == Ty.refRef = Just $ promote ((==) @(IORef ()))
  | r == Ty.threadIdRef = Just $ promote ((==) @ThreadId)
  | otherwise = Nothing

ref2cmp :: Reference -> Maybe (a -> b -> Ordering)
ref2cmp r
  | r == Ty.textRef = Just $ promote (compare @Text)
  | r == Ty.termLinkRef = Just $ promote tmlCmp
  | r == Ty.typeLinkRef = Just $ promote tylCmp
  | r == Ty.bytesRef = Just $ promote (compare @Bytes)
  | r == Ty.threadIdRef = Just $ promote (compare @ThreadId)
  | otherwise = Nothing

instance Eq Foreign where
  Wrap rl t == Wrap rr u
    | rl == rr, Just (~~) <- ref2eq rl = t ~~ u
  _ == _ = error "Eq Foreign"

instance Ord Foreign where
  Wrap rl t `compare` Wrap rr u
    | rl == rr, Just cmp <- ref2cmp rl = cmp t u
  compare (Wrap rl1 _) (Wrap rl2 _) =
    error $
      "Attempting to compare two values of different types: "
        <> show (rl1, rl2)

instance Show Foreign where
  showsPrec p !(Wrap r v) =
    showParen (p > 9) $
      showString "Wrap " . showsPrec 10 r . showString " " . contents
    where
      contents
        | r == Ty.textRef = shows @Text (unsafeCoerce v)
        | otherwise = showString "_"

unwrapForeign :: Foreign -> a
unwrapForeign (Wrap _ e) = unsafeCoerce e

maybeUnwrapForeign :: Reference -> Foreign -> Maybe a
maybeUnwrapForeign rt (Wrap r e)
  | rt == r = Just (unsafeCoerce e)
  | otherwise = Nothing

class BuiltinForeign f where
  foreignRef :: Tagged f Reference

instance BuiltinForeign Text where foreignRef = Tagged Ty.textRef

instance BuiltinForeign Bytes where foreignRef = Tagged Ty.bytesRef

instance BuiltinForeign Handle where foreignRef = Tagged Ty.fileHandleRef

instance BuiltinForeign Socket where foreignRef = Tagged Ty.socketRef

instance BuiltinForeign ThreadId where foreignRef = Tagged Ty.threadIdRef

instance BuiltinForeign TLS.ClientParams where foreignRef = Tagged Ty.tlsClientConfigRef

instance BuiltinForeign TLS.ServerParams where foreignRef = Tagged Ty.tlsServerConfigRef

instance BuiltinForeign X509.SignedCertificate where foreignRef = Tagged Ty.tlsSignedCertRef

instance BuiltinForeign X509.PrivKey where foreignRef = Tagged Ty.tlsPrivateKeyRef

instance BuiltinForeign FilePath where foreignRef = Tagged Ty.filePathRef

instance BuiltinForeign TLS.Context where foreignRef = Tagged Ty.tlsRef

instance BuiltinForeign (SuperGroup Symbol) where
  foreignRef = Tagged Ty.codeRef

instance BuiltinForeign Value where foreignRef = Tagged Ty.valueRef

instance BuiltinForeign TimeSpec where foreignRef = Tagged Ty.timeSpecRef


data HashAlgorithm where
  -- Reference is a reference to the hash algorithm
  HashAlgorithm :: Hash.HashAlgorithm a => Reference -> a -> HashAlgorithm

newtype Tls = Tls TLS.Context

data Failure a = Failure Reference Text a

instance BuiltinForeign HashAlgorithm where foreignRef = Tagged Ty.hashAlgorithmRef

wrapBuiltin :: forall f. BuiltinForeign f => f -> Foreign
wrapBuiltin x = Wrap r x
  where
    Tagged r = foreignRef :: Tagged f Reference

unwrapBuiltin :: BuiltinForeign f => Foreign -> f
unwrapBuiltin (Wrap _ x) = unsafeCoerce x

maybeUnwrapBuiltin :: forall f. BuiltinForeign f => Foreign -> Maybe f
maybeUnwrapBuiltin (Wrap r x)
  | r == r0 = Just (unsafeCoerce x)
  | otherwise = Nothing
  where
    Tagged r0 = foreignRef :: Tagged f Reference
