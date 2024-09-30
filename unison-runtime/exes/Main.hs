module Main where

import Data.Word (Word16, Word64)
import GHC.IOArray (IOArray)
import GHC.IOArray qualified as A
import System.CPUTime (getCPUTime)
import Text.Printf

-- | An absolute stack index
type Slot = Int

-- | A local variable reference, relative to the current stack frame
type Var = Word16

type Arr = IOArray Slot

{-
Machine code representation.
Uses a register-based VM with an infinite number of registers.
-}
data MCode ref
  = NatLit !Word64 -- 42
  | TailCall ref !Var !Var -- foo x y
  | NatIncrement !Var -- increment x
  | NatDecrement !Var -- decrement x
  | If0 !Var !(MCode ref) !(MCode ref) -- if cond then t else f
  | Let !Var !(MCode ref) !(MCode ref) -- let x = <expr> in body
  | Print !Var -- printLine x
  deriving
    ( -- | DynamicCall !Slot !Slot !Slot
      Eq,
      Ord,
      Show
    )

data Function = Function {code :: !(MCode Function), arity :: !Int}

data Value = Null | Nat {-# UNPACK #-} !Word64 | Closure ![Word64] ![Value] deriving (Eq, Ord, Show)

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
  let prog = Let 1 (NatLit 0) (Let 2 (NatLit countUpTo) (TailCall fn 1 2))
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
  boxed <- A.newIOArray (0, n) Null
  time $ go boxed (n - 1) 0 (n - 1) prog
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

    So `Let 10 (NatLit 42) (Let 3 (NatLit 16) body)` is no good, since
    it declares variable `10` before variable `3`.

    maxVar is the maximum local variable declared in the current
    call frame. You can grab all the local variables by slicing
    from `framePtr - maxVar` to `framePtr`.
    -}
    go :: Arr Value -> Slot -> Var -> Slot -> MCode Function -> IO ()
    go !boxed !framePtr !maxVar !out !prog = do
      -- stack <- UV.foldr (:) [] unboxed
      -- putStrLn ("stack:    " <> show stack)
      -- putStrLn ("frame:    " <> show framePtr)
      -- putStrLn ("maxVar:   " <> show maxVar)
      -- putStrLn ("out:      " <> show out)
      -- putStrLn ("rem:      " <> show prog)
      -- putStrLn ""
      case prog of
        NatLit n -> do
          A.writeIOArray boxed out (Nat n)
        TailCall ref aInd bInd -> do
          ab <- A.readIOArray boxed (framePtr - fromIntegral aInd)
          bb <- A.readIOArray boxed (framePtr - fromIntegral bInd)
          let aslot = framePtr - 1
          let bslot = framePtr - 2
          A.writeIOArray boxed aslot ab
          A.writeIOArray boxed bslot bb
          let mc = code ref
          go boxed framePtr 2 out mc
        NatIncrement i -> do
          Nat n <- A.readIOArray boxed (framePtr - fromIntegral i)
          A.writeIOArray boxed out (Nat $ n + 1)
        NatDecrement i -> do
          Nat n <- A.readIOArray boxed (framePtr - fromIntegral i)
          A.writeIOArray boxed out (Nat $ n - 1)
        If0 a t f -> do
          Nat au <- A.readIOArray boxed (framePtr - fromIntegral a)
          if au == 0
            then go boxed framePtr maxVar out t
            else go boxed framePtr maxVar out f
        Let var a b -> do
          go boxed framePtr var (framePtr - fromIntegral var) a
          go boxed framePtr var out b
        -- we assume that `Let` slots are assigned in increasing order
        -- otherwise we'd do:
        -- go unboxed boxed framePtr (max out maxVar) b
        Print i -> do
          a <- A.readIOArray boxed (framePtr - fromIntegral i)
          putStrLn (show a)
