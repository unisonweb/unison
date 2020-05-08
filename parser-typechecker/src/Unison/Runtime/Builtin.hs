{-# language RankNTypes #-}
{-# language ViewPatterns #-}
{-# language TypeApplications #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language FunctionalDependencies #-}

module Unison.Runtime.Builtin
  ( builtinLookup
  , builtinNumbering
  , numberedLookup
  , handle'io
  ) where

import Unison.ABT.Normalized
import Unison.Reference
import Unison.Runtime.ANF
import Unison.Var
import Unison.Symbol
import Unison.Runtime.IOSource

import qualified Unison.Type as Ty

import Data.Set (Set, insert)
import qualified Data.Set as Set

import Data.IntMap.Strict (singleton, fromList)
import qualified Data.Map as Map

freshes :: Var v => Int -> [v]
freshes = freshes' mempty

freshes' :: Var v => Set v -> Int -> [v]
freshes' avoid0 = go avoid0 []
  where
  go _     vs 0 = vs
  go avoid vs n
    = let v = freshIn avoid $ typed ANFBlank
       in go (insert v avoid) (v:vs) (n-1)

fls, tru :: Var v => ANormal v
fls = TCon Ty.booleanRef 0 []
tru = TCon Ty.booleanRef 1 []

boolift :: Var v => v -> ANormal v
boolift v
  = TMatch v $ MatchIntegral (fromList [(0,fls), (1,tru)]) Nothing

notlift :: Var v => v -> ANormal v
notlift v
  = TMatch v $ MatchIntegral (fromList [(1,fls), (0,tru)]) Nothing

unbox :: Var v => v -> Reference -> v -> ANormal v -> ANormal v
unbox v0 r v b
  = TMatch v0 $ MatchData r (singleton 0 $ ([UN], TAbs v b)) Nothing

unwrap :: Var v => v -> Reference -> v -> ANormal v -> ANormal v
unwrap v0 r v b
  = TMatch v0 $ MatchData r (singleton 0 $ ([BX], TAbs v b)) Nothing

unenum :: Var v => v -> Reference -> v -> ANormal v -> ANormal v
unenum = error "unenum: todo"

mkenum :: Var v => v -> Reference -> ANormal v
mkenum = error "mkenum: todo"

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
  $ boolift b

cmpopb :: Var v => POp -> Reference -> SuperNormal v
cmpopb pop rf
  = binop0 3 $ \[x0,y0,x,y,b]
 -> unbox x0 rf x
  . unbox y0 rf y
  . TLet b UN (APrm pop [y,x])
  $ boolift b

cmpopn :: Var v => POp -> Reference -> SuperNormal v
cmpopn pop rf
  = binop0 3 $ \[x0,y0,x,y,b]
 -> unbox x0 rf x
  . unbox y0 rf y
  . TLet b UN (APrm pop [x,y])
  $ notlift b

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

jumpk :: Var v => SuperNormal v
jumpk = binop0 0 $ \[k,a] -> TKon k [a]

handle'io :: Var v => SuperNormal v
handle'io
  = unop0 0 $ \[rq]
 -> TMatch rq
  . MatchRequest
  . singleton 0 $ cases rq
  where
  cases (Set.singleton -> avoid0)
    = fmap ($ avoid0)
    . fromList
    . zip [0..]
    $ [ open'file
      , close'file
      , is'file'eof
      , is'file'open
      , is'seekable
      , seek'handle
      , handle'position
      , get'buffering
      -- , set'buffering
      , get'line
      , get'text
      , put'text
      , system'time
      , get'temp'directory
      , get'current'directory
      , set'current'directory
      , file'exists
      , is'directory
      , create'directory
      , remove'directory
      , rename'directory
      , remove'file
      , rename'file
      , get'file'timestamp
      , get'file'size
      -- , server'socket
      , listen
      -- , client'socket
      , close'socket
      , socket'accept
      , socket'send
      -- , socket'receive :: IOOP
      , forked'value
      ]

type IOOP = forall v. Var v => Set v -> ([Mem], ANormal v)

open'file :: IOOP
open'file avoid
  = ([BX,BX],)
  . TAbss [fp0,m0]
  . unwrap fp0 filePathReference fp
  $ TIOp OPENFI [fp,m0]
  where
  [fp0,m0,fp] = freshes' avoid 3

close'file :: IOOP
close'file avoid
  = ([BX],)
  . TAbss [h0]
  . unwrap h0 handleReference h
  $ TIOp CLOSFI [h]
  where
  [h0,h] = freshes' avoid 2

is'file'eof :: IOOP
is'file'eof avoid
  = ([BX],)
  . TAbss [h0]
  . unwrap h0 handleReference h
  . TLet b UN (AIOp ISFEOF [h])
  $ boolift b
  where
  [h0,h,b] = freshes' avoid 3

is'file'open :: IOOP
is'file'open avoid
  = ([BX],)
  . TAbss [h0]
  . unwrap h0 handleReference h
  . TLet b UN (AIOp ISFOPN [h])
  $ boolift b
  where
  [h0,h,b] = freshes' avoid 3

is'seekable :: IOOP
is'seekable avoid
  = ([BX],)
  . TAbss [h0]
  . unwrap h0 handleReference h
  . TLet b UN (AIOp ISSEEK [h])
  $ boolift b
  where
  [h0,h,b] = freshes' avoid 3

seek'handle :: IOOP
seek'handle avoid
  = ([BX,BX,BX],)
  . TAbss [h0,sm0,po0]
  . unwrap h0 handleReference h
  . unenum sm0 seekModeReference sm
  . unbox po0 Ty.natRef po
  $ TIOp SEEKFI [h,sm,po]
  where
  [h0,sm0,po0,h,sm,po] = freshes' avoid 6

handle'position :: IOOP
handle'position avoid
  = ([BX],)
  . TAbss [h0]
  . unwrap h0 handleReference h
  . TLet i UN (AIOp POSITN [h])
  $ TCon Ty.intRef 0 [i]
  where
  [h0,h,i] = freshes' avoid 3

get'buffering :: IOOP
get'buffering avoid
  = ([BX],)
  . TAbss [h0]
  . unwrap h0 handleReference h
  . TLet bu UN (AIOp GBUFFR [h])
  $ mkenum bu bufferModeReference
  where
  [h0,h,bu] = freshes' avoid 3

-- set'buffering :: IOOP
-- set'buffering avoid
--   = ([BX,BX],)
--   . TAbss [h0,bm0]
--   . unwrap h0 handleReference h
--   . TLet bm UN (AMatch bm0 mbcases)
--   $ TIOp SBUFFR [bm]
--   where
--   [h0,bm0,h,bm] = freshes' avoid 4
--   mbcases
--     = fromList
--     [ (0,)
--     , (1,)
--     ]
  -- | SBUFFR
  -- setBuffering_ : io.Handle -> Optional io.BufferMode -> (Either io.Error ())

get'line :: IOOP
get'line avoid
  = ([BX],)
  . TAbss [h0]
  . unwrap h0 handleReference h
  $ TIOp GTLINE [h]
  where
  [h0,h] = freshes' avoid 2

get'text :: IOOP
get'text avoid
  = ([BX],)
  . TAbss [h0]
  . unwrap h0 handleReference h
  $ TIOp GTTEXT [h]
  where
  [h0,h] = freshes' avoid 2

put'text :: IOOP
put'text avoid
  = ([BX,BX],)
  . TAbss [h0,tx]
  . unwrap h0 handleReference h
  $ TIOp PUTEXT [h,tx]
  where
  [h0,h,tx] = freshes' avoid 3

system'time :: IOOP
system'time avoid
  = ([],)
  . TLet n UN (AIOp SYTIME [])
  $ TCon Ty.natRef 0 [n]
  where
  [n] = freshes' avoid 1

get'temp'directory :: IOOP
get'temp'directory avoid
  = ([],)
  . TLet t BX (AIOp GTMPDR [])
  $ TCon Ty.textRef 0 [t]
  where
  [t] = freshes' avoid 1

get'current'directory :: IOOP
get'current'directory avoid
  = ([],)
  . TLet t BX (AIOp GCURDR [])
  $ TCon Ty.textRef 0 [t]
  where
  [t] = freshes' avoid 1

set'current'directory :: IOOP
set'current'directory avoid
  = ([BX],)
  . TAbs fp0
  . unwrap fp0 filePathReference fp
  $ TIOp SCURDR [fp]
  where
  [fp0,fp] = freshes' avoid 2

-- directory'contents
-- DCNTNS
--   directoryContents_ : io.FilePath -> Either io.Error [io.FilePath]


file'exists :: IOOP
file'exists avoid
  = ([BX],)
  . TAbs fp0
  . unwrap fp0 filePathReference fp
  . TLet b UN (AIOp FEXIST [fp])
  $ boolift b
  where
  [fp0,fp,b] = freshes' avoid 3

is'directory :: IOOP
is'directory avoid
  = ([BX],)
  . TAbs fp0
  . unwrap fp0 filePathReference fp
  . TLet b UN (AIOp ISFDIR [fp])
  $ boolift b
  where
  [fp0,fp,b] = freshes' avoid 3

create'directory :: IOOP
create'directory avoid
  = ([BX],)
  . TAbs fp0
  . unwrap fp0 filePathReference fp
  $ TIOp CRTDIR [fp]
  where
  [fp0,fp] = freshes' avoid 2

remove'directory :: IOOP
remove'directory avoid
  = ([BX],)
  . TAbs fp0
  . unwrap fp0 filePathReference fp
  $ TIOp REMDIR [fp]
  where
  [fp0,fp] = freshes' avoid 2

rename'directory :: IOOP
rename'directory avoid
  = ([BX,BX],)
  . TAbss [from0,to0]
  . unwrap from0 filePathReference from
  . unwrap to0 filePathReference to
  $ TIOp RENDIR [from,to]
  where
  [from0,to0,from,to] = freshes' avoid 4

remove'file :: IOOP
remove'file avoid
  = ([BX],)
  . TAbs fp0
  . unwrap fp0 filePathReference fp
  $ TIOp REMOFI [fp]
  where
  [fp0,fp] = freshes' avoid 2

rename'file :: IOOP
rename'file avoid
  = ([BX,BX],)
  . TAbss [from0,to0]
  . unwrap from0 filePathReference from
  . unwrap to0 filePathReference to
  $ TIOp RENAFI [from,to]
  where
  [from0,to0,from,to] = freshes' avoid 4

get'file'timestamp :: IOOP
get'file'timestamp avoid
  = ([BX],)
  . TAbs fp0
  . unwrap fp0 filePathReference fp
  . TLet n UN (AIOp GFTIME [fp])
  $ TCon Ty.natRef 0 [n]
  where
  [fp0,fp,n] = freshes' avoid 3

get'file'size :: IOOP
get'file'size avoid
  = ([BX],)
  . TAbs fp0
  . unwrap fp0 filePathReference fp
  . TLet n UN (AIOp GFSIZE [fp])
  $ TCon Ty.natRef 0 [n]
  where
  [fp0,fp,n] = freshes' avoid 3

-- server'socket :: IOOP
-- server'socket avoid
--   = ([BX,BX],)
--   . TABss [mhn,sn0]
--   . unwrap sn0 serviceNameReference sn
--   

listen :: IOOP
listen avoid
  = ([BX],)
  . TAbs sk0
  . unwrap sk0 socketReference sk
  $ TIOp LISTEN [sk]
  where
  [sk0,sk] = freshes' avoid 2

-- client'socket :: IOOP
-- client'socket avoid
--   = ([BX,BX],)
--   . TAbss [hn0,sn0]
--   . unwrap hn0 hostNameReference hn
--   . unwrap sn0 serviceNameReference sn
--   . TLet r BX (AIOp CLISCK [hn,sn])
--   $ TCon socketReference 0 [r]
--   where
--   [hn0,sn0,hn,sn,r] = freshes' avoid 5

close'socket :: IOOP
close'socket avoid
  = ([BX,BX],)
  . TAbs sk0
  . unwrap sk0 socketReference sk
  $ TIOp CLOSCK [sk]
  where
  [sk0, sk] = freshes' avoid 2

socket'accept :: IOOP
socket'accept avoid
  = ([BX],)
  . TAbs sk0
  . unwrap sk0 socketReference sk
  . TLet r BX (AIOp SKACPT [sk])
  $ TCon socketReference 0 [r]
  where
  [sk0,sk,r] = freshes' avoid 3

socket'send :: IOOP
socket'send avoid
  = ([BX,BX],)
  . TAbss [sk0,by]
  . unwrap sk0 socketReference sk
  $ TIOp SKSEND [sk,by]
  where
  [sk0,sk,by] = freshes' avoid 3

-- socket'receive :: IOOP
-- socket'receive avoid
--   = ([BX,BX],)
--   . TAbss [sk0,n0]
--   . unwrap sk0 socketReference sk
--   . unbox n0 Ty.natRef n
--   . TLet r BX (AIOp SKRECV [sk,n])
--   $ TCon 

forked'value :: IOOP
forked'value avoid
  = ([],)
  . TLet t UN (APrm FORK [])
  . TMatch t
  . MatchSum
  $ fromList
      [ (0, ([], TCon optionReference 0 []))
      , (1, ([BX], TAbs ti $ TCon optionReference 1 [ti]))
      ]
  where
  [t,ti] = freshes' avoid 1

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
  , ("jumpCont", jumpk)
  ]

builtinNumbering :: Map.Map Reference Int
builtinNumbering
  = Map.fromList $ zip (Map.keys $ builtinLookup @Symbol) [1..]

numberedLookup :: Var v => Map.Map Int (SuperNormal v)
numberedLookup
  = Map.fromList . zip [1..] . Map.elems $ builtinLookup
