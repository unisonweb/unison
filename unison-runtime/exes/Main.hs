module Main where

import Data.Vector.Mutable qualified as V
import Data.Vector.Primitive.Mutable qualified as UV
-- import Data.Vector qualified as IV
import Data.Word (Word16, Word64)
import System.CPUTime (getCPUTime)
import Text.Printf

-- | An absolute stack index
type Slot = Int

-- | A local variable reference, relative to the current stack frame
type Var = Word16

{-
Machine code representation.
Uses a register-based VM with an infinite number of registers.
-}
data MCode ref
  = Nat !Word64 -- 42
  | TailCall ref !Var !Var -- foo x y
  | NatIncrement !Var -- increment x
  | NatDecrement !Var -- decrement x
  | If0 !Var !(MCode ref) !(MCode ref) -- if cond then t else f
  | Let !Var !(MCode ref) !(MCode ref) -- let x = <expr> in body
  | Print !Var -- printLine x
  deriving (-- | DynamicCall !Slot !Slot !Slot
            Eq, Ord, Show)

data Function = Function {code :: !(MCode Function), arity :: !Int}

-- currently unused
data Value = Null | Closure [Word64] [Value] deriving (Eq, Ord, Show)

main :: IO ()
main = do
  {-
  prog acc rem =
    if rem == 0 then printLine acc
    else
      acc' = increment acc
      rem' = decrement rem
      prog acc' rem'
  -}
  let fn = Function (If0 2 (Print 1) (Let 3 (NatIncrement 1) $ Let 4 (NatDecrement 2) $ TailCall fn 3 4)) 2
  let prog = Let 1 (Nat 0) (Let 2 (Nat countUpTo) (TailCall fn 1 2))
  run prog

countUpTo :: Word64
countUpTo = 1000 * 1000 * 1000

time :: IO t -> IO t
time a = do
  start <- getCPUTime
  v <- a
  end <- getCPUTime
  let diff = fromIntegral (end - start) / 1e12
  printf "Computation time  : %0.3f sec\n" (diff :: Double)
  printf "Ops / s (millions): %0.3f \n" (fromIntegral countUpTo * (1 / diff :: Double) / 1e6)
  return v

run :: MCode Function -> IO ()
run prog = do
  let n = 1024
  boxed <- V.replicate n Null
  unboxed <- UV.replicate n 0
  time $ go unboxed boxed (n - 1) 0 (n - 1) prog
  pure ()
  where
    {-
    Results are always written to `out :: Slot`.

    `unboxed` and `boxed` are manipulated in lockstep. Operations
    like `NatIncrement` write `Null` to the boxed stack.

    The stack grows downward toward index 0. This is thought to have
    better locality, since when the CPU loads position i in the stack,
    it will cache the items immediately below i in the stack.

    First argument or local variable of a function is at `framePtr - 1`.
    Second is at `framePtr - 2`, etc.
    Function args are just local variables!

    No debruijn indexing is used.

    Local variable declarations with `Let` should be of strictly
    increasing number within a branch of the function.

    So `Let 10 (Nat 42) (Let 3 (Nat 16) body)` is no good, since
    it declares variable `10` before variable `3`.

    maxVar is the maximum local variable declared in the current
    call frame. You can grab all the local variables by slicing
    from `framePtr - maxVar` to `framePtr`.
    -}
    go :: UV.IOVector Word64 -> V.IOVector Value -> Slot -> Var -> Slot -> MCode Function -> IO ()
    go !unboxed !boxed !framePtr !maxVar !out !prog = do
      -- stack <- UV.foldr (:) [] unboxed
      -- putStrLn ("stack:    " <> show stack)
      -- putStrLn ("frame:    " <> show framePtr)
      -- putStrLn ("maxVar:   " <> show maxVar)
      -- putStrLn ("out:      " <> show out)
      -- putStrLn ("rem:      " <> show prog)
      -- putStrLn ""
      case prog of
        Nat n -> do
          UV.write unboxed out n
          V.write boxed out Null
        TailCall ref a b -> do
          au <- UV.read unboxed (framePtr - fromIntegral a)
          ab <- V.read boxed (framePtr - fromIntegral a)
          bu <- UV.read unboxed (framePtr - fromIntegral b)
          bb <- V.read boxed (framePtr - fromIntegral b)
          let aslot = framePtr - 1
          let bslot = framePtr - 2
          UV.write unboxed aslot au
          UV.write unboxed bslot bu
          V.write boxed aslot ab
          V.write boxed bslot bb
          let mc = code ref
          go unboxed boxed framePtr 2 out mc
        NatIncrement a -> do
          au <- UV.read unboxed (framePtr - fromIntegral a)
          UV.write unboxed out (au + 1)
          V.write boxed out Null
        NatDecrement a -> do
          au <- UV.read unboxed (framePtr - fromIntegral a)
          UV.write unboxed out (au - 1)
          V.write boxed out Null
        If0 a t f -> do
          au <- UV.read unboxed (framePtr - fromIntegral a)
          if au == 0
            then go unboxed boxed framePtr maxVar out t
            else go unboxed boxed framePtr maxVar out f
        Let var a b -> do
          go unboxed boxed framePtr var (framePtr - fromIntegral var) a
          go unboxed boxed framePtr var out b
        -- we assume that `Let` slots are assigned in increasing order
        -- otherwise we'd do:
        -- go unboxed boxed framePtr (max out maxVar) b
        Print a -> do
          au <- UV.read unboxed (framePtr - fromIntegral a)
          ab <- V.read boxed (framePtr - fromIntegral a)
          putStrLn (show (au, ab))
