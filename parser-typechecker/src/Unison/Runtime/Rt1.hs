{-# Language OverloadedStrings #-}
{-# Language StrictData #-}
{-# Language BangPatterns #-}
{-# Language LambdaCase #-}

module Unison.Runtime.Rt1 where

import Data.Foldable (for_)
import Data.IORef
import Data.Int (Int64)
import Data.Text (Text)
import Data.Traversable (for)
import Data.Word (Word64)
import Unison.Runtime.IR
import qualified Data.Vector.Mutable as MV

data Machine
  = Machine { stack :: IORef (MV.IOVector V)
            , supply :: IORef Int }

-- An empty machine
machine0 :: IO Machine
machine0 = do
  stack <- MV.new 256
  MV.set stack (T "uninitialized")
  stackRef <- newIORef stack
  supply <- newIORef 0
  pure $ Machine stackRef supply

push :: Size -> V -> Machine -> IO ()
push size v m = do
  s0 <- readIORef (stack m)
  s1 <-
    if (size >= MV.length s0)
    then do
      -- increase the size to fit
      s1 <- MV.grow s0 size
      writeIORef (stack m) s1
      pure s1
    else pure s0
  MV.write s1 size v

fresh :: Machine -> IO Int
fresh m = atomicModifyIORef' (supply m) (\n -> (n + 1, n))

type Size = Int

at :: Size -> Z -> Machine -> IO V
at size i m = case i of
  Val v -> pure v
  Slot i -> do
    s <- readIORef (stack m)
    -- the top of the stack is slot 0, at index size - 1
    MV.read s (size - i - 1)
  LazySlot i -> at size (Slot i) m
    -- let nonce = 42 -- todo: we need to conjure up a unique id here, using some monad
    -- in Lazy nonce s (m !! fromIntegral i)

ati :: Size -> Z -> Machine -> IO Int64
ati size i m = at size i m >>= \case
  I i -> pure i
  _ -> fail "type error"

atn :: Size -> Z -> Machine -> IO Word64
atn size i m = at size i m >>= \case
  N i -> pure i
  _ -> fail "type error"

atf :: Size -> Z -> Machine -> IO Double
atf size i m = at size i m >>= \case
  F i -> pure i
  _ -> fail "type error"

atb :: Size -> Z -> Machine -> IO Bool
atb size i m = at size i m >>= \case
  B b -> pure b
  _ -> fail "type error"

att :: Size -> Z -> Machine -> IO Text
att size i m = at size i m >>= \case
  T t -> pure t
  _ -> fail "type error"

data Result = RRequest Req | RMatchFail | RDone V deriving (Eq,Show)

done :: V -> IO Result
done v = pure (RDone v)

arity :: V -> Int
arity (Lam n _ _) = n
arity _ = 0

run :: CompilationEnv -> IR -> Machine -> IO Result
run env ir m = go 0 ir where
  go size ir = case ir of
    If c t f -> atb size c m >>= \case True -> go size t; False -> go size f
    And i j -> atb size i m >>= \case True -> go size j; False -> done (B False)
    Or i j -> atb size i m >>= \case True -> done (B True); False -> go size j
    Not i -> atb size i m >>= (done . B . not)
    Leaf (Val v) -> done v
    Leaf s -> done =<< at size s m
    Let b body -> go size b >>= \case
      RRequest req -> pure $ RRequest (req `appendCont` body)
      RDone v -> push size v m >> go (size + 1) body
      e -> error $ show e
    LetRec bs body -> letrec size bs body

  letrec size bs body = do
    let size' = size + length bs
    refs <- for bs $ \(v,b) -> do
      r <- newIORef (N 99)
      i <- fresh m
      pure (Ref i v r, b)
    for_ (refs `zip` [0..]) $ \((r,_), i) -> push (size + i) r m
    for_ refs $ \(Ref _ _ r,b) -> do
      let toVal (RDone a) = a
          toVal e = error ("bindings in a let rec must not have effects " ++ show e)
      result <- toVal <$> go size' ir
      writeIORef r result
    go size' body
