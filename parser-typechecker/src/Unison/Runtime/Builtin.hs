{-# language OverloadedStrings #-}

module Unison.Runtime.Builtin
  ( builtinLookup
  , builtinNumbering
  , numberedLookup
  ) where

import Unison.ABT.Normalized
import Unison.Reference
import Unison.Runtime.ANF
import Unison.Var
import Unison.Symbol

import qualified Unison.Type as Ty

import Data.Set (insert)

import Data.IntMap.Strict (singleton, fromList)
import qualified Data.Map as Map

freshes :: Var v => Int -> [v]
freshes = go mempty []
  where
  go _     vs 0 = vs
  go avoid vs n
    = let v = freshIn avoid $ typed ANFBlank
       in go (insert v avoid) (v:vs) (n-1)

fls, tru :: Var v => ANormal v
fls = TCon Ty.booleanRef 0 []
tru = TCon Ty.booleanRef 1 []

unbox :: Var v => v -> Reference -> v -> ANormal v -> ANormal v
unbox v0 r v b
  = TMatch v0 $ MatchData r (singleton 0 $ ([UN], TAbs v b)) Nothing

unop0 :: Var v => Int -> ([v] -> ANormal v) -> SuperNormal v
unop0 n f
  = Lambda [BX]
  . TAbss [x0]
  $ f xs
  where
  xs@(x0:_) = freshes (1+n)

binop0 :: Var v => Int -> ([v] -> ANormal v) -> SuperNormal v
binop0 n f
  = Lambda [BX,BX]
  . TAbss [x0,y0]
  $ f xs
  where
  xs@(x0:y0:_) = freshes (2+n)

unop :: Var v => POp -> Reference -> SuperNormal v
unop pop rf
  = unop0 2 $ \[x0,x,r]
 -> unbox x0 rf x
  . TLet r UN (APrm pop [x])
  $ TCon rf 0 [r]

binop :: Var v => POp -> Reference -> SuperNormal v
binop pop rf = binop' pop rf rf rf

binop'
  :: Var v
  => POp
  -> Reference -> Reference -> Reference
  -> SuperNormal v
binop' pop rfx rfy rfr
  = binop0 3 $ \[x0,y0,x,y,r]
 -> unbox x0 rfx x
  . unbox y0 rfy y
  . TLet r UN (APrm pop [x,y])
  $ TCon rfr 0 [r]

cmpop :: Var v => POp -> Reference -> SuperNormal v
cmpop pop rf
  = binop0 3 $ \[x0,y0,x,y,b]
 -> unbox x0 rf x
  . unbox y0 rf y
  . TLet b UN (APrm pop [x,y])
  . TMatch b
  $ MatchIntegral (fromList [(0,fls), (1,tru)]) Nothing

cmpopb :: Var v => POp -> Reference -> SuperNormal v
cmpopb pop rf
  = binop0 3 $ \[x0,y0,x,y,b]
 -> unbox x0 rf x
  . unbox y0 rf y
  . TLet b UN (APrm pop [y,x])
  . TMatch b
  $ MatchIntegral (fromList [(0,fls), (1,tru)]) Nothing

cmpopn :: Var v => POp -> Reference -> SuperNormal v
cmpopn pop rf
  = binop0 3 $ \[x0,y0,x,y,b]
 -> unbox x0 rf x
  . unbox y0 rf y
  . TLet b UN (APrm pop [x,y])
  . TMatch b
  $ MatchIntegral (fromList [(0,tru), (1,fls)]) Nothing

-- data POp
--   -- Int
--   | PADI | PSUI | PMUI | PDII
--   | PGTI | PLTI | PGEI | PLEI | PEQI
--   | PSGI | PNEI | PTRI | PMDI
--   -- Nat
--   | PADN | PSUN | PMUN | PDIN
--   | PGTN | PLTN | PGEN | PLEN | PEQN
--   | PSGN | PNEN | PTRN | PMDN
--   -- Float
--   | PADF | PSUF | PMUF | PDIF
--   | PGTF | PLTF | PGEF | PLEF | PEQF
--   deriving (Show)

addi,subi,muli,divi,modi,shli,shri,powi :: Var v => SuperNormal v
addi = binop ADDI Ty.intRef
subi = binop SUBI Ty.intRef
muli = binop MULI Ty.intRef
divi = binop DIVI Ty.intRef
modi = binop MODI Ty.intRef
shli = binop' SHLI Ty.intRef Ty.natRef Ty.intRef
shri = binop' SHRI Ty.intRef Ty.natRef Ty.intRef
powi = binop' POWI Ty.intRef Ty.natRef Ty.intRef

addn,subn,muln,divn,modn,shln,shrn,pown :: Var v => SuperNormal v
addn = binop ADDN Ty.natRef
subn = binop SUBN Ty.natRef
muln = binop MULN Ty.natRef
divn = binop DIVN Ty.natRef
modn = binop MODN Ty.natRef
shln = binop SHLN Ty.natRef
shrn = binop SHRN Ty.natRef
pown = binop POWN Ty.natRef

eqi, eqn, lti, ltn, lei, len :: Var v => SuperNormal v
eqi = cmpop EQLI Ty.intRef
lti = cmpop LESI Ty.intRef
lei = cmpop LEQI Ty.intRef
eqn = cmpop EQLN Ty.natRef
ltn = cmpop LESN Ty.natRef
len = cmpop LEQN Ty.natRef

gti, gtn, gei, gen :: Var v => SuperNormal v
gti = cmpopb LESI Ty.intRef
gei = cmpopb LEQI Ty.intRef
gtn = cmpopb LESN Ty.intRef
gen = cmpopb LEQN Ty.intRef

neqi, neqn :: Var v => SuperNormal v
neqi = cmpopn EQLI Ty.intRef
neqn = cmpopn EQLN Ty.intRef

inci, incn :: Var v => SuperNormal v
inci = unop INCI Ty.intRef
incn = unop INCN Ty.natRef

sgni, negi :: Var v => SuperNormal v
sgni = unop SGNI Ty.intRef
negi = unop NEGI Ty.intRef

trni :: Var v => SuperNormal v
trni = unop0 3 $ \[x0,x,z,b]
    -> unbox x0 Ty.intRef x
     . TLet z UN (ALit $ I 0)
     . TLet b UN (APrm LEQI [x, z])
     . TMatch b
     $ MatchIntegral
         (singleton 1 $ TCon Ty.natRef 0 [z])
         (Just $ TVar x0)

modular :: Var v => POp -> (Bool -> ANormal v) -> SuperNormal v
modular pop ret
  = unop0 3 $ \[x0,x,m,t]
 -> unbox x0 Ty.intRef x
  . TLet t UN (ALit $ I 2)
  . TLet m UN (APrm pop [x,t])
  . TMatch m
  $ MatchIntegral
      (singleton 1 $ ret True)
      (Just $ ret False)

evni, evnn, oddi, oddn :: Var v => SuperNormal v
evni = modular MODI (\b -> if b then fls else tru)
oddi = modular MODI (\b -> if b then tru else fls)
evnn = modular MODN (\b -> if b then fls else tru)
oddn = modular MODN (\b -> if b then tru else fls)

dropn :: Var v => SuperNormal v
dropn = binop0 4 $ \[x0,y0,x,y,b,r]
     -> unbox x0 Ty.natRef x
      . unbox y0 Ty.natRef y
      . TLet b UN (APrm LEQN [y,x])
      . TLet r UN
          (AMatch b $ MatchIntegral
             (singleton 1 $ TLit $ N 0)
             (Just $ TPrm SUBN [y,x]))
      $ TCon Ty.natRef 0 [r]

builtinLookup :: Var v => Map.Map Reference (SuperNormal v)
builtinLookup
  = Map.fromList
  $ map (\(t, f) -> (Builtin t, f))
  [ ("Int.+", addi)
  , ("Int.-", subi)
  , ("Int.*", muli)
  , ("Int./", divi)
  , ("Int.mod", modi)
  , ("Int.==", eqi)
  , ("Int.!=", neqi)
  , ("Int.<", lti)
  , ("Int.<=", lei)
  , ("Int.>", gti)
  , ("Int.>=", gei)
  , ("Int.increment", inci)
  , ("Int.signum", sgni)
  , ("Int.negate", negi)
  , ("Int.truncate0", trni)
  , ("Int.isEven", evni)
  , ("Int.isOdd", oddi)
  , ("Int.shiftLeft", shli)
  , ("Int.shiftRight", shri)
  , ("Int.pow", powi)
--   , B "Int.toText" $ int --> text
--   , B "Int.fromText" $ text --> optional int
--   , B "Int.toFloat" $ int --> float

  , ("Nat.+", addn)
  , ("Nat.-", subn)
  , ("Nat.sub", subn)
  , ("Nat.*", muln)
  , ("Nat./", divn)
  , ("Nat.mod", modn)
  , ("Nat.==", eqn)
  , ("Int.!=", neqn)
  , ("Nat.<", ltn)
  , ("Nat.<=", len)
  , ("Nat.>", gtn)
  , ("Nat.>=", gen)
  , ("Nat.increment", incn)
  , ("Nat.isEven", evnn)
  , ("Nat.isOdd", oddn)
  , ("Nat.shiftLeft", shln)
  , ("Nat.shiftRight", shrn)
  , ("Nat.pow", pown)
  , ("Nat.drop", dropn)
--   , B "Nat.toInt" $ nat --> int
--   , B "Nat.toText" $ nat --> text
--   , B "Nat.fromText" $ text --> optional nat
--   , B "Nat.toFloat" $ nat --> float

--   , B "Float.+" $ float --> float --> float
--   , B "Float.-" $ float --> float --> float
--   , B "Float.*" $ float --> float --> float
--   , B "Float./" $ float --> float --> float
--   , B "Float.<" $ float --> float --> boolean
--   , B "Float.>" $ float --> float --> boolean
--   , B "Float.<=" $ float --> float --> boolean
--   , B "Float.>=" $ float --> float --> boolean
--   , B "Float.==" $ float --> float --> boolean
--
--   -- Trigonmetric Functions
--   , B "Float.acos" $ float --> float
--   , B "Float.asin" $ float --> float
--   , B "Float.atan" $ float --> float
--   , B "Float.atan2" $ float --> float --> float
--   , B "Float.cos" $ float --> float
--   , B "Float.sin" $ float --> float
--   , B "Float.tan" $ float --> float
--
--   -- Hyperbolic Functions
--   , B "Float.acosh" $ float --> float
--   , B "Float.asinh" $ float --> float
--   , B "Float.atanh" $ float --> float
--   , B "Float.cosh" $ float --> float
--   , B "Float.sinh" $ float --> float
--   , B "Float.tanh" $ float --> float
--
--   -- Exponential Functions
--   , B "Float.exp" $ float --> float
--   , B "Float.log" $ float --> float
--   , B "Float.logBase" $ float --> float --> float
--
--   -- Power Functions
--   , B "Float.pow" $ float --> float --> float
--   , B "Float.sqrt" $ float --> float
--
--   -- Rounding and Remainder Functions
--   , B "Float.ceiling" $ float --> int
--   , B "Float.floor" $ float --> int
--   , B "Float.round" $ float --> int
--   , B "Float.truncate" $ float --> int
--
--   -- Float Utils
--   , B "Float.abs" $ float --> float
--   , B "Float.max" $ float --> float --> float
--   , B "Float.min" $ float --> float --> float
--   , B "Float.toText" $ float --> text
--   , B "Float.fromText" $ text --> optional float
--
--   , B "Universal.==" $ forall1 "a" (\a -> a --> a --> boolean)
--   -- Don't we want a Universal.!= ?
--
--   -- Universal.compare intended as a low level function that just returns
--   -- `Int` rather than some Ordering data type. If we want, later,
--   -- could provide a pure Unison wrapper for Universal.compare that
--   -- returns a proper data type.
--   --
--   -- 0 is equal, < 0 is less than, > 0 is greater than
--   , B "Universal.compare" $ forall1 "a" (\a -> a --> a --> int)
--   , B "Universal.>" $ forall1 "a" (\a -> a --> a --> boolean)
--   , B "Universal.<" $ forall1 "a" (\a -> a --> a --> boolean)
--   , B "Universal.>=" $ forall1 "a" (\a -> a --> a --> boolean)
--   , B "Universal.<=" $ forall1 "a" (\a -> a --> a --> boolean)
--
--   , B "bug" $ forall1 "a" (\a -> forall1 "b" (\b -> a --> b))
--   , B "todo" $ forall1 "a" (\a -> forall1 "b" (\b -> a --> b))
--
--   , B "Boolean.not" $ boolean --> boolean
--
--   , B "Text.empty" text
--   , B "Text.++" $ text --> text --> text
--   , B "Text.take" $ nat --> text --> text
--   , B "Text.drop" $ nat --> text --> text
--   , B "Text.size" $ text --> nat
--   , B "Text.==" $ text --> text --> boolean
--   , D "Text.!=" $ text --> text --> boolean
--   , B "Text.<=" $ text --> text --> boolean
--   , B "Text.>=" $ text --> text --> boolean
--   , B "Text.<" $ text --> text --> boolean
--   , B "Text.>" $ text --> text --> boolean
--   , B "Text.uncons" $ text --> optional (tuple [char, text])
--   , B "Text.unsnoc" $ text --> optional (tuple [text, char])
--
--   , B "Text.toCharList" $ text --> list char
--   , B "Text.fromCharList" $ list char --> text
--
--   , B "Char.toNat" $ char --> nat
--   , B "Char.fromNat" $ nat --> char
--
--   , B "Bytes.empty" bytes
--   , B "Bytes.fromList" $ list nat --> bytes
--   , B "Bytes.++" $ bytes --> bytes --> bytes
--   , B "Bytes.take" $ nat --> bytes --> bytes
--   , B "Bytes.drop" $ nat --> bytes --> bytes
--   , B "Bytes.at" $ nat --> bytes --> optional nat
--   , B "Bytes.toList" $ bytes --> list nat
--   , B "Bytes.size" $ bytes --> nat
--   , B "Bytes.flatten" $ bytes --> bytes
--
--   , B "List.empty" $ forall1 "a" list
--   , B "List.cons" $ forall1 "a" (\a -> a --> list a --> list a)
--   , Alias "List.cons" "List.+:"
--   , B "List.snoc" $ forall1 "a" (\a -> list a --> a --> list a)
--   , Alias "List.snoc" "List.:+"
--   , B "List.take" $ forall1 "a" (\a -> nat --> list a --> list a)
--   , B "List.drop" $ forall1 "a" (\a -> nat --> list a --> list a)
--   , B "List.++" $ forall1 "a" (\a -> list a --> list a --> list a)
--   , B "List.size" $ forall1 "a" (\a -> list a --> nat)
--   , B "List.at" $ forall1 "a" (\a -> nat --> list a --> optional a)
--
--   , B "Debug.watch" $ forall1 "a" (\a -> text --> a --> a)
  ]

builtinNumbering :: Map.Map Reference Int
builtinNumbering
  = Map.fromList $ zip (Map.keys $ builtinLookup @Symbol) [1..]

numberedLookup :: Var v => Map.Map Int (SuperNormal v)
numberedLookup
  = Map.fromList . zip [1..] . Map.elems $ builtinLookup
