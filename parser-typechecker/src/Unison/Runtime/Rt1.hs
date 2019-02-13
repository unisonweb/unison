{-# Language OverloadedStrings #-}
{-# Language StrictData #-}
{-# Language Strict #-}
{-# Language BangPatterns #-}
{-# Language LambdaCase #-}

module Unison.Runtime.Rt1 where

import Control.Monad (foldM)
import Data.Foldable (for_)
import Data.IORef
import Data.Int (Int64)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Word (Word64)
import Unison.Runtime.IR
import Unison.Symbol (Symbol)
import qualified Data.Vector.Mutable as MV

type Stack = MV.IOVector V

push :: Size -> V -> Stack -> IO Stack
push size v s0 = do
  s1 <-
    if (size >= MV.length s0)
    then do
      -- increase the size to fit
      s1 <- MV.grow s0 size
      pure s1
    else pure s0
  MV.write s1 size v
  pure s1

type Size = Int

force :: V -> IO V
force (Ref _ _ r) = readIORef r >>= force
force v = pure v

at :: Size -> Z -> Stack -> IO V
at size i m = case i of
  Val v -> force v
  Slot i ->
    -- the top of the stack is slot 0, at index size - 1
    force =<< MV.read m (size - i - 1)
  LazySlot i ->
    MV.read m (size - i - 1)

ati :: Size -> Z -> Stack -> IO Int64
ati size i m = at size i m >>= \case
  I i -> pure i
  _ -> fail "type error"

atn :: Size -> Z -> Stack -> IO Word64
atn size i m = at size i m >>= \case
  N i -> pure i
  _ -> fail "type error"

atf :: Size -> Z -> Stack -> IO Double
atf size i m = at size i m >>= \case
  F i -> pure i
  _ -> fail "type error"

atb :: Size -> Z -> Stack -> IO Bool
atb size i m = at size i m >>= \case
  B b -> pure b
  _ -> fail "type error"

att :: Size -> Z -> Stack -> IO Text
att size i m = at size i m >>= \case
  T t -> pure t
  _ -> fail "type error"

data Result = RRequest Req | RMatchFail | RDone V deriving (Eq,Show)

done :: V -> IO Result
done v = pure (RDone v)

arity :: V -> Int
arity (Lam n _ _) = n
arity _ = 0

run :: CompilationEnv -> IR -> IO Result
run _env ir = do
  supply <- newIORef 0
  m0 <- MV.new 256
  MV.set m0 (T "uninitialized")
  let
    fresh :: IO Int
    fresh = atomicModifyIORef' supply (\n -> (n + 1, n))

    go :: Size -> Stack -> IR -> IO Result
    go size m ir = case ir of
      If c t f -> atb size c m >>= \case
        True -> go size m t
        False -> go size m f
      And i j -> atb size i m >>= \case
        True -> go size m j
        False -> done (B False)
      Or i j -> atb size i m >>= \case
        True -> done (B True)
        False -> go size m j
      Not i -> atb size i m >>= (done . B . not)
      Leaf (Val v) -> done v
      Leaf s -> done =<< at size s m
      Let b body -> go size m b >>= \case
        RRequest req -> pure $ RRequest (req `appendCont` body)
        RDone v -> push size v m >>= \m -> go (size + 1) m body
        e -> error $ show e
      LetRec bs body -> letrec size m bs body
      _ -> error $ "TODO - fill in the rest of Rt1.go " <> show ir

    letrec :: Size -> Stack -> [(Symbol, IR)] -> IR -> IO Result
    letrec size m bs body = do
      let size' = size + length bs
      refs <- for bs $ \(v,b) -> do
        r <- newIORef (N 99)
        i <- fresh
        pure (Ref i v r, b)
      m <- foldM (\m ((r,_), i) -> push (size + i) r m) m (refs `zip` [0..])
      for_ refs $ \(Ref _ _ r, ir) -> do
        let toVal (RDone a) = a
            toVal e = error ("bindings in a let rec must not have effects " ++ show e)
        result <- toVal <$> go size' m ir
        writeIORef r result
      go size' m body

  go 0 m0 ir
