module Main where

import Data.Vector.Mutable qualified as V
import Data.Vector.Primitive.Mutable qualified as UV
import Data.Vector qualified as IV
-- import Unison.Util.Text as T
import Data.Word (Word64, Word16)
import System.CPUTime (getCPUTime)
import Text.Printf
-- import Data.Int (Int64)
-- import Data.Map qualified as M
-- import Data.IntMap qualified as IM

-- Machine state is:
--   boxed : V.Vector Closure
--   unboxed : UV.Vector Int
--   pointer : Int
--   handlers : V.Vector Closure
-- Stack is a (V.Vector Closure, UV.Vector Int)
-- Each op

type Ref = Int
type Slot = Int
type Var = Word16

data MCode
  = Nat !Word64
  | TailCall !Ref !Var !Var    -- foo x y
  | NatIncrement !Var          -- increment x
  | NatDecrement !Var          -- decrement x
  | If0 !Var !MCode !MCode     -- if cond then t else f
  | Let !Var !MCode !MCode     -- let x = <expr> in body 
  | Print !Var
  -- | DynamicCall !Slot !Slot !Slot  
  deriving (Eq, Ord, Show)

data Value = Null | Closure [Word64] [Value] deriving (Eq,Ord,Show)

main :: IO ()
main = do
  {-
  prog acc rem = 
    if rem == 0 then printLine acc 
    else prog (increment acc) (decrement rem)
  -}
  let n = 1000 * 1000
  let fn = If0 2 (Print 1) (Let 3 (NatIncrement 1) $ Let 4 (NatDecrement 2) $ TailCall 0 3 4)
  let prog = Let 1 (Nat 0) (Let 2 (Nat n) (TailCall 0 1 2))
  run (IV.fromList [fn]) prog

time :: IO t -> IO t
time a = do
  start <- getCPUTime
  v <- a
  end <- getCPUTime
  let diff = fromIntegral (end - start) / 1e12
  printf "Computation time: %0.3f sec\n" (diff :: Double)
  return v

run :: IV.Vector MCode -> MCode -> IO ()
run codes prog = do 
  let n = 8
  -- create an empty vector of size 1024
  boxed <- V.replicate n Null 
  unboxed <- UV.replicate n 0 

  -- record timing
  time $ go unboxed boxed (n - 1) 0 (n - 1) prog
  pure ()
  where
    {-
    Results are always written to `out`. 
    `unboxed` and `boxed` are manipulated in lockstep.
    Stack grows downward toward index 0.
    First local var of a function is at `framePtr - 1`.
    Second local var of a function is at `framePtr - 2`.
    
    Function args are just local variables, in order.
    maxVar is the max local variable declared in the current frame.
    -}
    go :: UV.IOVector Word64 -> V.IOVector Value -> Slot -> Var -> Slot -> MCode -> IO () 
    go unboxed boxed framePtr maxVar out prog = do
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
          let mc = codes IV.! ref 
          go unboxed boxed framePtr 3 out mc 
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
          if au == 0 then go unboxed boxed framePtr maxVar out t 
          else go unboxed boxed framePtr maxVar out f
        Let var a b -> do
          go unboxed boxed framePtr var (framePtr - fromIntegral var) a
          go unboxed boxed framePtr var out b
          -- we assume that `Let` slots are assigned in increasing order
          -- go unboxed boxed framePtr (max out maxVar) b
        Print a -> do
          au <- UV.read unboxed (framePtr - fromIntegral a)
          ab <- V.read boxed (framePtr - fromIntegral a)
          putStrLn (show (au, ab))


-- type Term = ABTN.Term ANF.ANormalF

-- compile :: [(v, ABT.Term ANormalF v, Int)] -> ABTN.Term ANormalF v -> MachineCode
-- machineCode.run :: MachineCode -> MachineState -> IO ()
-- compile :: Map Reference MCode2 -> [(Reference, v, Term v, Int)] -> [(Reference, MCode2)] 
-- compile mcode defs = 
   

{-
data ANormalF v e
  = ALet (Direction Word16) [Mem] e e -- [Mem] is calling convention for the result
  -- not sure what the Word16 is
  | AName (Either Reference v) [v] e
  -- used for handle blah with ..., the blah becomes an `AName`
  -- passed as first arg to handler
  | ALit Lit -- old stuff, unused
  | ABLit Lit -- direct boxed literal
  | AMatch v (Branched e)
  | AShift Reference e   -- get the continuation up to nearest enclosing AHnd with matching Ref
  -- how do you do this?
  | AHnd [Reference] v e -- multihandler for that list of references 
  -- handle v with e, v will be an unevaluated AName
  | AApp (Func v) [v]
  | AFrc v -- forces a Name
  | AVar v
  deriving (Show, Eq)

data Branched e
  = MatchIntegral (EnumMap Word64 e) (Maybe e)
  | MatchText (Map.Map Util.Text.Text e) (Maybe e)
  | MatchRequest (Map Reference (EnumMap CTag ([Mem], e))) e
  | MatchEmpty
  | MatchData Reference (EnumMap CTag ([Mem], e)) (Maybe e)
  | MatchSum (EnumMap Word64 ([Mem], e))
  | MatchNumeric Reference (EnumMap Word64 e) (Maybe e)
  deriving (Show, Eq, Functor, Foldable, Traversable)
-}
