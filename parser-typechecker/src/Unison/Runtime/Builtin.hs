{-# language RankNTypes #-}
{-# language ViewPatterns #-}
{-# language PatternGuards #-}
{-# language TypeApplications #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language FunctionalDependencies #-}

module Unison.Runtime.Builtin
  ( builtinLookup
  , builtinTermNumbering
  , builtinTypeNumbering
  , builtinTermBackref
  , builtinTypeBackref
  , builtinForeigns
  , numberedTermLookup
  ) where

import Control.Exception (IOException, try)
import Control.Monad.State.Strict (State, modify, execState)

import Unison.ABT.Normalized hiding (TTm)
import Unison.Reference
import Unison.Runtime.ANF as ANF
import Unison.Var
import Unison.Symbol
import Unison.Runtime.Stack (Closure)
import Unison.Runtime.Foreign.Function
import Unison.Runtime.IOSource
import Unison.Runtime.Foreign (HashAlgorithm(..), Failure(..))

import qualified Unison.Type as Ty
import qualified Unison.Builtin as Ty (builtinTypes)
import qualified Unison.Builtin.Decls as Ty
import qualified Crypto.Hash as Hash
import qualified Crypto.MAC.HMAC as HMAC

import Unison.Util.EnumContainers as EC

import Data.Default (def)
import Data.Either.Combinators (mapLeft)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Data.ByteString (hGet, hPut)
import Data.Word (Word64)
import Data.Text as Text (Text, pack, unpack)
import qualified System.X509 as X
import qualified Data.ByteArray as BA

import Data.Set (Set, insert)

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Unison.Util.Bytes as Bytes
import Network.Socket as SYS
  ( accept
  , Socket
  )
import Network.Simple.TCP as SYS
  ( HostPreference(..)
  , bindSock
  , connectSock
  , listenSock
  , closeSock
  , send
  , recv
  )
import Network.TLS as TLS
import Network.TLS.Extra.Cipher as Cipher

import System.IO as SYS
  ( openFile
  , hClose
  , hGetBuffering
  , hSetBuffering
  , hIsEOF
  , hIsOpen
  , hIsSeekable
  , hSeek
  , hTell
  , stdin, stdout, stderr
  )
import Data.Text.IO as SYS (hGetLine)
import Control.Concurrent as SYS
  ( threadDelay
  , killThread
  )
import Control.Concurrent.MVar as SYS
import Data.Time.Clock.POSIX as SYS
  ( getPOSIXTime
  , utcTimeToPOSIXSeconds
  )
import System.Directory as SYS
  ( getCurrentDirectory
  , setCurrentDirectory
  , getTemporaryDirectory
  -- , getDirectoryContents
  , doesPathExist
  , doesDirectoryExist
  , renameDirectory
  , removeFile
  , renameFile
  , createDirectoryIfMissing
  , removeDirectoryRecursive
  , getModificationTime
  , getFileSize
  )
import System.IO.Temp (createTempDirectory)

freshes :: Var v => Int -> [v]
freshes = freshes' mempty

freshes' :: Var v => Set v -> Int -> [v]
freshes' avoid0 = go avoid0 []
  where
  go _     vs 0 = vs
  go avoid vs n
    = let v = freshIn avoid $ typed ANFBlank
       in go (insert v avoid) (v:vs) (n-1)

fresh1 :: Var v => v
fresh1 = head $ freshes 1

fresh2 :: Var v => (v, v)
fresh2 = (v1, v2) where
  [v1, v2] = freshes 2

fresh3 :: Var v => (v, v, v)
fresh3 = (v1, v2, v3) where
  [v1, v2, v3] = freshes 3

fresh5 :: Var v => (v, v, v, v, v)
fresh5 = (v1, v2, v3, v4, v5) where
  [v1, v2, v3, v4, v5] = freshes 5

fresh6 :: Var v => (v, v, v, v, v, v)
fresh6 = (v1, v2, v3, v4, v5, v6) where
  [v1, v2, v3, v4, v5, v6] = freshes 6

fresh7 :: Var v => (v, v, v, v, v, v, v)
fresh7 = (v1, v2, v3, v4, v5, v6, v7) where
  [v1, v2, v3, v4, v5, v6, v7] = freshes 7

fresh10 :: Var v => (v, v, v, v, v, v, v, v, v, v)
fresh10 = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) where
  [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10] = freshes 10

fls, tru :: Var v => ANormal v
fls = TCon Ty.booleanRef 0 []
tru = TCon Ty.booleanRef 1 []

boolift :: Var v => v -> ANormalT v
boolift v
  = AMatch v $ MatchIntegral (mapFromList [(0,fls), (1,tru)]) Nothing

notlift :: Var v => v -> ANormalT v
notlift v
  = AMatch v $ MatchIntegral (mapFromList [(1,fls), (0,tru)]) Nothing

unbox :: Var v => v -> Reference -> v -> ANormal v -> ANormal v
unbox v0 r v b
  = TMatch v0
  $ MatchData r (mapSingleton 0 $ ([UN], TAbs v b)) Nothing

unenum :: Var v => Int -> v -> Reference -> v -> ANormal v -> ANormal v
unenum n v0 r v nx
  = TMatch v0 $ MatchData r cases Nothing
  where
  mkCase i = (toEnum i, ([], TLet v UN (ALit . I $ fromIntegral i) nx))
  cases = mapFromList . fmap mkCase $ [0..n-1]

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
unop pop rf = unop' pop rf rf

unop' :: Var v => POp -> Reference -> Reference -> SuperNormal v
unop' pop rfi rfo
  = unop0 2 $ \[x0,x,r]
 -> unbox x0 rfi x
  . TLet r UN (APrm pop [x])
  $ TCon rfo 0 [r]

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
  $ TTm $ boolift b

cmpopb :: Var v => POp -> Reference -> SuperNormal v
cmpopb pop rf
  = binop0 3 $ \[x0,y0,x,y,b]
 -> unbox x0 rf x
  . unbox y0 rf y
  . TLet b UN (APrm pop [y,x])
  $ TTm $ boolift b

cmpopn :: Var v => POp -> Reference -> SuperNormal v
cmpopn pop rf
  = binop0 3 $ \[x0,y0,x,y,b]
 -> unbox x0 rf x
  . unbox y0 rf y
  . TLet b UN (APrm pop [x,y])
  $ TTm $ notlift b

cmpopbn :: Var v => POp -> Reference -> SuperNormal v
cmpopbn pop rf
  = binop0 3 $ \[x0,y0,x,y,b]
 -> unbox x0 rf x
  . unbox y0 rf y
  . TLet b UN (APrm pop [y,x])
  $ TTm $ notlift b

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
subn = binop' SUBN Ty.natRef Ty.natRef Ty.intRef
muln = binop MULN Ty.natRef
divn = binop DIVN Ty.natRef
modn = binop MODN Ty.natRef
shln = binop SHLN Ty.natRef
shrn = binop SHRN Ty.natRef
pown = binop POWN Ty.natRef

eqi, eqn, lti, ltn, lei, len :: Var v => SuperNormal v
eqi = cmpop EQLI Ty.intRef
lti = cmpopbn LEQI Ty.intRef
lei = cmpop LEQI Ty.intRef
eqn = cmpop EQLN Ty.natRef
ltn = cmpopbn LEQN Ty.natRef
len = cmpop LEQN Ty.natRef

gti, gtn, gei, gen :: Var v => SuperNormal v
gti = cmpopn LEQI Ty.intRef
gei = cmpopb LEQI Ty.intRef
gtn = cmpopn LEQN Ty.intRef
gen = cmpopb LEQN Ty.intRef

inci, incn :: Var v => SuperNormal v
inci = unop INCI Ty.intRef
incn = unop INCN Ty.natRef

sgni, negi :: Var v => SuperNormal v
sgni = unop SGNI Ty.intRef
negi = unop NEGI Ty.intRef

lzeron, tzeron, lzeroi, tzeroi, popn, popi :: Var v => SuperNormal v
lzeron = unop LZRO Ty.natRef
tzeron = unop TZRO Ty.natRef
popn = unop POPC Ty.natRef
popi = unop' POPC Ty.intRef Ty.natRef
lzeroi = unop' LZRO Ty.intRef Ty.natRef
tzeroi = unop' TZRO Ty.intRef Ty.natRef

andn, orn, xorn, compln, andi, ori, xori, compli :: Var v => SuperNormal v
andn = binop ANDN Ty.natRef
orn = binop IORN Ty.natRef
xorn = binop XORN Ty.natRef
compln = unop COMN Ty.natRef
andi = binop ANDN Ty.intRef
ori = binop IORN Ty.intRef
xori = binop XORN Ty.intRef
compli = unop COMN Ty.intRef

addf, subf, mulf, divf, powf, sqrtf, logf, logbf
  :: Var v => SuperNormal v
addf = binop ADDF Ty.floatRef
subf = binop SUBF Ty.floatRef
mulf = binop MULF Ty.floatRef
divf = binop DIVF Ty.floatRef
powf = binop POWF Ty.floatRef
sqrtf = unop SQRT Ty.floatRef
logf = unop LOGF Ty.floatRef
logbf = binop LOGB Ty.floatRef

expf, absf :: Var v => SuperNormal v
expf = unop EXPF Ty.floatRef
absf = unop ABSF Ty.floatRef

cosf, sinf, tanf, acosf, asinf, atanf :: Var v => SuperNormal v
cosf = unop COSF Ty.floatRef
sinf = unop SINF Ty.floatRef
tanf = unop TANF Ty.floatRef
acosf = unop ACOS Ty.floatRef
asinf = unop ASIN Ty.floatRef
atanf = unop ATAN Ty.floatRef

coshf, sinhf, tanhf, acoshf, asinhf, atanhf, atan2f
  :: Var v => SuperNormal v
coshf = unop COSH Ty.floatRef
sinhf = unop SINH Ty.floatRef
tanhf = unop TANH Ty.floatRef
acoshf = unop ACSH Ty.floatRef
asinhf = unop ASNH Ty.floatRef
atanhf = unop ATNH Ty.floatRef
atan2f = binop ATN2 Ty.floatRef

ltf, gtf, lef, gef, eqf, neqf :: Var v => SuperNormal v
ltf = cmpopbn LEQF Ty.floatRef
gtf = cmpopn LEQF Ty.floatRef
lef = cmpop LEQF Ty.floatRef
gef = cmpopb LEQF Ty.floatRef
eqf = cmpop EQLF Ty.floatRef
neqf = cmpopn EQLF Ty.floatRef

minf, maxf :: Var v => SuperNormal v
minf = binop MINF Ty.floatRef
maxf = binop MAXF Ty.floatRef

ceilf, floorf, truncf, roundf, i2f, n2f :: Var v => SuperNormal v
ceilf = unop' CEIL Ty.floatRef Ty.intRef
floorf = unop' FLOR Ty.floatRef Ty.intRef
truncf = unop' TRNF Ty.floatRef Ty.intRef
roundf = unop' RNDF Ty.floatRef Ty.intRef
i2f = unop' ITOF Ty.intRef Ty.floatRef
n2f = unop' NTOF Ty.natRef Ty.floatRef

trni :: Var v => SuperNormal v
trni = unop0 3 $ \[x0,x,z,b]
    -> unbox x0 Ty.intRef x
     . TLet z UN (ALit $ I 0)
     . TLet b UN (APrm LEQI [x, z])
     . TMatch b
     $ MatchIntegral
         (mapSingleton 1 $ TCon Ty.natRef 0 [z])
         (Just $ TCon Ty.natRef 0 [x])

modular :: Var v => POp -> (Bool -> ANormal v) -> SuperNormal v
modular pop ret
  = unop0 3 $ \[x0,x,m,t]
 -> unbox x0 Ty.intRef x
  . TLet t UN (ALit $ I 2)
  . TLet m UN (APrm pop [x,t])
  . TMatch m
  $ MatchIntegral
      (mapSingleton 1 $ ret True)
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
      . TLet b UN (APrm LEQN [x,y])
      . TLet r UN
          (AMatch b $ MatchIntegral
             (mapSingleton 1 $ TLit $ N 0)
             (Just $ TPrm SUBN [x,y]))
      $ TCon Ty.natRef 0 [r]

appendt, taket, dropt, sizet, unconst, unsnoct :: Var v => SuperNormal v
appendt = binop0 0 $ \[x,y] -> TPrm CATT [x,y]
taket = binop0 1 $ \[x0,y,x]
     -> unbox x0 Ty.natRef x
      $ TPrm TAKT [x,y]
dropt = binop0 1 $ \[x0,y,x]
     -> unbox x0 Ty.natRef x
      $ TPrm DRPT [x,y]
sizet = unop0 1 $ \[x,r]
     -> TLet r UN (APrm SIZT [x])
      $ TCon Ty.natRef 0 [r]
unconst = unop0 5 $ \[x,t,c0,c,y,p]
     -> TLet t UN (APrm UCNS [x])
      . TMatch t . MatchSum $ mapFromList
      [ (0, ([], TCon Ty.optionalRef 0 []))
      , (1, ([UN,BX], TAbss [c0,y]
                    . TLet c BX (ACon Ty.charRef 0 [c0])
                    . TLet p BX (ACon Ty.pairRef 0 [c,y])
                    $ TCon Ty.optionalRef 1 [p]))
      ]
unsnoct = unop0 5 $ \[x,t,c0,c,y,p]
     -> TLet t UN (APrm USNC [x])
      . TMatch t . MatchSum $ mapFromList
      [ (0, ([], TCon Ty.optionalRef 0 []))
      , (1, ([BX,UN], TAbss [y,c0]
                    . TLet c BX (ACon Ty.charRef 0 [c0])
                    . TLet p BX (ACon Ty.pairRef 0 [y,c])
                    $ TCon Ty.optionalRef 1 [p]))
      ]

appends, conss, snocs :: Var v => SuperNormal v
appends = binop0 0 $ \[x,y] -> TPrm CATS [x,y]
conss = binop0 0 $ \[x,y] -> TPrm CONS [x,y]
snocs = binop0 0 $ \[x,y] -> TPrm SNOC [x,y]

takes, drops, sizes, ats, emptys :: Var v => SuperNormal v
takes = binop0 1 $ \[x0,y,x]
     -> unbox x0 Ty.natRef x
      $ TPrm TAKS [x,y]
drops = binop0 1 $ \[x0,y,x]
     -> unbox x0 Ty.natRef x
      $ TPrm DRPS [x,y]
sizes = unop0 1 $ \[x,r]
     -> TLet r UN (APrm SIZS [x])
      $ TCon Ty.natRef 0 [r]
ats = binop0 3 $ \[x0,y,x,t,r]
   -> unbox x0 Ty.natRef x
    . TLet t UN (APrm IDXS [x,y])
    . TMatch t . MatchSum $ mapFromList
    [ (0, ([], TCon Ty.optionalRef 0 []))
    , (1, ([BX], TAbs r $ TCon Ty.optionalRef 1 [r]))
    ]
emptys = Lambda [] $ TPrm BLDS []

viewls, viewrs :: Var v => SuperNormal v
viewls = unop0 3 $ \[s,u,h,t]
      -> TLet u UN (APrm VWLS [s])
       . TMatch u . MatchSum $ mapFromList
       [ (0, ([], TCon Ty.seqViewRef 0 []))
       , (1, ([BX,BX], TAbss [h,t] $ TCon Ty.seqViewRef 1 [h,t]))
       ]
viewrs = unop0 3 $ \[s,u,i,l]
      -> TLet u UN (APrm VWRS [s])
       . TMatch u . MatchSum $ mapFromList
       [ (0, ([], TCon Ty.seqViewRef 0 []))
       , (1, ([BX,BX], TAbss [i,l] $ TCon Ty.seqViewRef 1 [i,l]))
       ]

eqt, neqt, leqt, geqt, lesst, great :: Var v => SuperNormal v
eqt = binop0 1 $ \[x,y,b]
   -> TLet b UN (APrm EQLT [x,y])
    . TTm $ boolift b
neqt = binop0 1 $ \[x,y,b]
    -> TLet b UN (APrm EQLT [x,y])
     . TTm $ notlift b
leqt = binop0 1 $ \[x,y,b]
    -> TLet b UN (APrm LEQT [x,y])
     . TTm $ boolift b
geqt = binop0 1 $ \[x,y,b]
    -> TLet b UN (APrm LEQT [y,x])
     . TTm $ boolift b
lesst = binop0 1 $ \[x,y,b]
     -> TLet b UN (APrm LEQT [y,x])
      . TTm $ notlift b
great = binop0 1 $ \[x,y,b]
     -> TLet b UN (APrm LEQT [x,y])
      . TTm $ notlift b

packt, unpackt :: Var v => SuperNormal v
packt = unop0 0 $ \[s] -> TPrm PAKT [s]
unpackt = unop0 0 $ \[t] -> TPrm UPKT [t]

packb, unpackb, emptyb, appendb :: Var v => SuperNormal v
packb = unop0 0 $ \[s] -> TPrm PAKB [s]
unpackb = unop0 0 $ \[b] -> TPrm UPKB [b]
emptyb
  = Lambda []
  . TLet es BX (APrm BLDS [])
  $ TPrm PAKB [es]
  where
  es = fresh1
appendb = binop0 0 $ \[x,y] -> TPrm CATB [x,y]

takeb, dropb, atb, sizeb, flattenb :: Var v => SuperNormal v
takeb = binop0 1 $ \[n0,b,n]
     -> unbox n0 Ty.natRef n
      $ TPrm TAKB [n,b]

dropb = binop0 1 $ \[n0,b,n]
     -> unbox n0 Ty.natRef n
      $ TPrm DRPB [n,b]

atb = binop0 4 $ \[n0,b,n,t,r0,r]
   -> unbox n0 Ty.natRef n
    . TLet t UN (APrm IDXB [n,b])
    . TMatch t . MatchSum $ mapFromList
    [ (0, ([], TCon Ty.optionalRef 0 []))
    , (1, ([UN], TAbs r0
               . TLet r BX (ACon Ty.natRef 0 [r0])
               $ TCon Ty.optionalRef 1 [r]))
    ]

sizeb = unop0 1 $ \[b,n]
     -> TLet n UN (APrm SIZB [b])
      $ TCon Ty.natRef 0 [n]

flattenb = unop0 0 $ \[b] -> TPrm FLTB [b]

i2t, n2t, f2t :: Var v => SuperNormal v
i2t = unop0 1 $ \[n0,n]
   -> unbox n0 Ty.intRef n
    $ TPrm ITOT [n]
n2t = unop0 1 $ \[n0,n]
   -> unbox n0 Ty.natRef n
    $ TPrm NTOT [n]
f2t = unop0 1 $ \[f0,f]
   -> unbox f0 Ty.floatRef f
    $ TPrm FTOT [f]

t2i, t2n, t2f :: Var v => SuperNormal v
t2i = unop0 3 $ \[x,t,n0,n]
   -> TLet t UN (APrm TTOI [x])
    . TMatch t . MatchSum $ mapFromList
    [ (0, ([], TCon Ty.optionalRef 0 []))
    , (1, ([UN], TAbs n0
               . TLet n BX (ACon Ty.intRef 0 [n0])
               $ TCon Ty.optionalRef 1 [n]))
    ]
t2n = unop0 3 $ \[x,t,n0,n]
   -> TLet t UN (APrm TTON [x])
    . TMatch t . MatchSum $ mapFromList
    [ (0, ([], TCon Ty.optionalRef 0 []))
    , (1, ([UN], TAbs n0
               . TLet n BX (ACon Ty.natRef 0 [n0])
               $ TCon Ty.optionalRef 1 [n]))
    ]
t2f = unop0 3 $ \[x,t,f0,f]
   -> TLet t UN (APrm TTOF [x])
    . TMatch t . MatchSum $ mapFromList
    [ (0, ([], TCon Ty.optionalRef 0 []))
    , (1, ([UN], TAbs f0
               . TLet f BX (ACon Ty.floatRef 0 [f0])
               $ TCon Ty.optionalRef 1 [f]))
    ]

equ :: Var v => SuperNormal v
equ = binop0 1 $ \[x,y,b]
   -> TLet b UN (APrm EQLU [x,y])
    . TTm $ boolift b

cmpu :: Var v => SuperNormal v
cmpu = binop0 2 $ \[x,y,c,i]
    -> TLet c UN (APrm CMPU [x,y])
     . TLet i UN (APrm DECI [c])
     $ TCon Ty.intRef 0 [i]

ltu :: Var v => SuperNormal v
ltu = binop0 1 $ \[x,y,c]
   -> TLet c UN (APrm CMPU [x,y])
    . TMatch c
    $ MatchIntegral
        (mapFromList [ (0, TCon Ty.booleanRef 1 []) ])
        (Just $ TCon Ty.booleanRef 0 [])

gtu :: Var v => SuperNormal v
gtu = binop0 1 $ \[x,y,c]
   -> TLet c UN (APrm CMPU [x,y])
    . TMatch c
    $ MatchIntegral
        (mapFromList [ (2, TCon Ty.booleanRef 1 []) ])
        (Just $ TCon Ty.booleanRef 0 [])

geu :: Var v => SuperNormal v
geu = binop0 1 $ \[x,y,c]
   -> TLet c UN (APrm CMPU [x,y])
    . TMatch c
    $ MatchIntegral
        (mapFromList [ (0, TCon Ty.booleanRef 0 []) ])
        (Just $ TCon Ty.booleanRef 1 [])

leu :: Var v => SuperNormal v
leu = binop0 1 $ \[x,y,c]
   -> TLet c UN (APrm CMPU [x,y])
    . TMatch c
    $ MatchIntegral
        (mapFromList [ (2, TCon Ty.booleanRef 0 []) ])
        (Just $ TCon Ty.booleanRef 1 [])

notb :: Var v => SuperNormal v
notb = unop0 0 $ \[b]
    -> TMatch b . flip (MatchData Ty.booleanRef) Nothing
     $ mapFromList [ (0, ([], tru)), (1, ([], fls)) ]

orb :: Var v => SuperNormal v
orb = binop0 0 $ \[p,q]
   -> TMatch p . flip (MatchData Ty.booleanRef) Nothing
    $ mapFromList [ (1, ([], tru)), (0, ([], TVar q)) ]

andb :: Var v => SuperNormal v
andb = binop0 0 $ \[p,q]
    -> TMatch p . flip (MatchData Ty.booleanRef) Nothing
     $ mapFromList [ (0, ([], fls)), (1, ([], TVar q)) ]

-- unsafeCoerce, used for numeric types where conversion is a
-- no-op on the representation. Ideally this will be inlined and
-- eliminated so that no instruction is necessary.
cast :: Var v => Reference -> Reference -> SuperNormal v
cast ri ro
  = unop0 1 $ \[x0,x]
 -> unbox x0 ri x
  $ TCon ro 0 [x]

jumpk :: Var v => SuperNormal v
jumpk = binop0 0 $ \[k,a] -> TKon k [a]

fork'comp :: Var v => SuperNormal v
fork'comp
  = Lambda [BX]
  . TAbs act
  . TLet unit BX (ACon Ty.unitRef 0 [])
  . TName lz (Right act) [unit]
  $ TPrm FORK [lz]
  where
  (act,unit,lz) = fresh3

bug :: Var v => SuperNormal v
bug = unop0 0 $ \[x] -> TPrm EROR [x]

watch :: Var v => SuperNormal v
watch
  = binop0 0 $ \[t,v]
 -> TLets [] [] (APrm PRNT [t])
  $ TVar v

type ForeignOp = forall v. Var v => FOp -> ([Mem], ANormal v)

standard'handle :: ForeignOp
standard'handle instr
  = ([BX],)
  . TAbss [h0]
  . unenum 3 h0 Ty.stdHandleRef h
  $ TFOp instr [h]
  where
  (h0,h) = fresh2

seek'handle :: ForeignOp
seek'handle instr
  = ([BX,BX,BX],)
  . TAbss [arg1, arg2, arg3]
  . unenum 3 arg2 Ty.seekModeRef seek
  . unbox arg3 Ty.natRef nat
  . TLet result BX (AFOp instr [arg1, seek, nat])
  $ outIoFailUnit stack1 stack2 unit fail result
  where
    (arg1, arg2, arg3, seek, nat, stack1, stack2, unit, fail, result) = fresh10

get'buffering'output :: forall v. Var v => v -> v -> v -> v -> ANormal v
get'buffering'output bu m n b =
  TMatch bu . MatchSum  $ mapFromList
  [ (0, ([], TCon Ty.optionalRef 0 []))
  , (1, ([], line))
  , (2, ([], block'nothing))
  , (3, ([UN], TAbs n $ block'n))
  ]
  where
  final = TCon Ty.optionalRef 1 [b]
  block = TLet b BX (ACon bufferModeReference 1 [m]) $ final
  line
    = TLet b BX (ACon bufferModeReference 0 []) $ final
  block'nothing
    = TLet m BX (ACon Ty.optionalRef 0 [])
    $ block
  block'n
    = TLet m BX (ACon Ty.optionalRef 1 [n])
    $ block

get'buffering :: ForeignOp
get'buffering = inBx arg1 result
              $ get'buffering'output result m n b
  where
    (arg1, result, m, n, b) = fresh5


-- Input Shapes
-- These will represent different argument lists a foreign might expect
--
-- These functions will usually have a few common parameters:
--   result : a variable containing the result of the foreign call
--   cont : a term which will be evaluated when a result from the foreign call is on the stack
--
-- They will be named according to their shape:
-- inBx     : one boxed arg
-- inBxBx   : two boxed args

-- Accept one boxed org as input and evaluate `cont` with `result`
-- bound to the result of the foreign `instr`
inUnit :: forall v. Var v => v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
inUnit unit result cont instr
  = ([BX], TAbs unit $ TLet result UN (AFOp instr []) cont)

inBx :: forall v. Var v => v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
inBx arg result cont instr =
  ([BX],)
  . TAbs arg
  $ TLet result UN (AFOp instr [arg]) cont

inNat :: forall v. Var v => v -> v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
inNat arg nat result cont instr =
  ([BX],)
  . TAbs arg
  . unbox arg Ty.natRef nat
  $ TLet result UN (AFOp instr [nat]) cont

-- Accept two boxed args as input and evaluate `cont` with `result`
-- bound to the result of the foreign `instr`
inBxBx :: forall v. Var v => v -> v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
inBxBx arg1 arg2 result cont instr =
  ([BX, BX],)
  . TAbss [arg1, arg2]
  $ TLet result UN (AFOp instr [arg1, arg2]) cont

-- Accept one boxed arg and one nat as input and evaluate `cont` with
-- `result` bound to the result of the foreign `instr`
inBxNat ::  forall v. Var v => v -> v -> v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
inBxNat arg1 arg2 nat result cont instr =
  ([BX,BX],)
  . TAbss [arg1, arg2]
  . unbox arg2 Ty.natRef nat
  $ TLet result UN (AFOp instr [arg1, nat]) cont

inBxIomr :: forall v. Var v => v -> v -> v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
inBxIomr arg1 arg2 enum result cont instr
  = ([BX,BX],)
  . TAbss [arg1, arg2]
  . unenum 4 arg2 ioModeReference enum
  $ TLet result UN (AFOp instr [arg1]) cont

-- Accept three boxed args as input and evaluate `cont` with `result`
-- bound to the result of the foreign `instr`
-- inBxBxBx :: forall v. Var v => v -> v -> v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
-- inBxBxBx arg1 arg2 arg3 result cont instr =
--   ([BX, BX, BX],)
--   . TAbss [arg1, arg2, arg3]
--   $ TLet result UN (AFOp instr [arg1, arg2, arg3]) cont

-- Handling results of foreign calls
--
-- These will represent Terms which are evaluated when the result of a
-- foreign call are is the stack. These functions will turn the result
-- into Unison values
--
-- They will be named according to the Unison Type they return
--
-- These should be passed as an argument to an input handler (such as
-- inBxBx, InBxUn, etc)

-- outInt :: Int -> Int
outInt :: forall v. Var v => v -> ANormal v
outInt i = TCon Ty.intRef 0 [i]

-- outBool :: Bool -> Bool
outBool :: forall v. Var v => v -> ANormal v
outBool result = TTm $ boolift result

-- outMaybe :: Either IOException a -> Either Failure a
outMaybe :: forall v. Var v => v -> v -> ANormal v
outMaybe maybe result =
  TMatch result . MatchSum $ mapFromList
  [ (0, ([], TCon Ty.optionalRef 0 []))
  , (1, ([BX], TAbs maybe $ TCon Ty.optionalRef 1 [maybe]))
  ]

-- outIoFail :: Either IOException a -> Either Failure a
outIoFail :: forall v. Var v => v -> v -> v -> v -> ANormal v
outIoFail stack1 stack2 fail result =
  TMatch result . MatchSum
  $ mapFromList
  [ (0, ([BX, BX],)
        . TAbss [stack1, stack2]
        . TLet fail BX (ACon failureReference 0 [stack1, stack2])
        $ TCon eitherReference 0 [fail])
  , (1, ([BX], TAbs stack1 $ TCon eitherReference 1 [stack1]))
  ]

outIoFailNat :: forall v. Var v => v -> v -> v -> v -> v -> ANormal v
outIoFailNat stack1 stack2 fail nat result =
  TMatch result . MatchSum  $ mapFromList
  [ (0, ([BX, BX],)
        . TAbss [stack1, stack2]
        . TLet fail BX (ACon failureReference 0 [stack1, stack2])
        $ TCon eitherReference 0 [fail])
  , (1, ([UN],)
        . TAbs stack1
        . TLet nat UN (ACon Ty.natRef 0 [stack1])
        $ TCon eitherReference 1 [nat])
  ]

outIoFailBox :: forall v. Var v => v -> v -> v -> v -> ANormal v
outIoFailBox stack1 stack2 fail result =
  TMatch result . MatchSum  $ mapFromList
  [ (0, ([BX, BX],)
        . TAbss [stack1, stack2]
        . TLet fail BX (ACon failureReference 0 [stack1, stack2])
        $ TCon eitherReference 0 [fail])
  , (1, ([BX],)
        . TAbs stack1
        $ TCon eitherReference 1 [stack1])
  ]

-- outIoFailUnit :: Either IOException a -> Either Failure Unit
outIoFailUnit :: forall v. Var v => v -> v -> v -> v -> v -> ANormal v
outIoFailUnit stack1 stack2 unit fail result =
  TMatch result . MatchSum
  $ mapFromList
  [ (0, ([BX, BX],)
        . TAbss [stack1, stack2]
        . TLet fail BX (ACon failureReference 0 [stack1, stack2])
        $ TCon eitherReference 0 [fail])
  , (1, ([BX],)
        . TLet unit UN (ACon Ty.unitRef 0 [])
        $ TCon eitherReference 1 [unit])
  ]

-- outIoFailUnit :: Either IOException a -> Either Failure Bool
outIoFailBool :: forall v. Var v => v -> v -> v -> v -> v -> ANormal v
outIoFailBool stack1 stack2 bool fail result =
  TMatch result . MatchSum
  $ mapFromList
  [ (0, ([BX, BX],)
        . TAbss [stack1, stack2]
        . TLet fail BX (ACon failureReference 0 [stack1, stack2])
        $ TCon eitherReference 0 [fail])
  , (1, ([UN],)
        . TAbs stack2
        . TLet bool BX (boolift stack2)
        $ TCon eitherReference 1 [bool])
  ]



-- Input / Output glue
--
-- These are pairings of input and output functions to handle a
-- foreign call.  The input function represents the numbers and types
-- of the inputs to a forein call.  The output function takes the
-- result of the foreign call and turns it into a Unison type.
--

-- Pure ForeignOp taking no values and returning the result directly
-- pfop0 :: () -> Box
unitDirect :: ForeignOp
unitDirect instr = ([],) $ TFOp instr []

-- Pure ForeignOp taking one boxed value and directly returning the result
-- pfob_unit :: Box -> Box
boxDirect :: ForeignOp
boxDirect instr =
  ([BX],)
  . TAbs arg
  $ TFOp instr [arg] where
  arg = fresh1

unitToEFNat :: ForeignOp
unitToEFNat = inUnit unit result
            $ outIoFailNat stack1 stack2 fail nat result
  where (unit, stack1, stack2, fail, nat, result) = fresh6

unitToEFBox :: ForeignOp
unitToEFBox = inUnit unit result
            $ outIoFailBox stack1 stack2 fail result
  where (unit, stack1, stack2, fail, result) = fresh5

-- Pure ForeignOp taking one boxed value and returning an Int
-- pfobb_int :: Box -> Int
boxToInt :: ForeignOp
boxToInt = inBx arg result
          $ outInt result
  where
    (arg, result) = fresh2

boxIomrToEFBox :: ForeignOp
boxIomrToEFBox = inBxIomr arg1 arg2 enum result
              $ outIoFailBox stack1 stack2 fail result
  where
    (arg1, arg2, enum, stack1, stack2, fail, result) = fresh7

-- Pure ForeignOp taking one boxed value and returning unit
-- pfobb_unit :: Box -> Unit
boxTo0 :: ForeignOp
boxTo0 = inBx arg result (TCon Ty.unitRef 0 [])
  where
    (arg, result) = fresh2

-- Pure ForeignOp taking Nat value and returning unit
-- pfobb_unit :: Box -> Unit
natToUnit :: ForeignOp
natToUnit = inNat arg nat result (TCon Ty.unitRef 0 [])
  where
    (arg, nat, result) = fresh3

boxToBool :: ForeignOp
boxToBool = inBx arg result
          $ outBool result
  where
    (arg, result) = fresh2

boxBoxToBool :: ForeignOp
boxBoxToBool = inBxBx arg1 arg2 result
            $ outBool result
  where
    (arg1, arg2, result) = fresh3


-- Pure ForeignOp taking two boxed values and returning the result directly
boxBoxDirect :: ForeignOp
boxBoxDirect instr
  = ([BX,BX],)
  . TAbss [b1,b2]
  $ TFOp instr [b1,b2]
  where
  (b1,b2) = fresh2

-- Pure ForeignOp taking three boxed values and returning the result directly
boxBoxBoxDirect :: ForeignOp
boxBoxBoxDirect instr
  = ([BX,BX,BX],)
  . TAbss [b1,b2,b3]
  $ TFOp instr [b1,b2,b3]
  where
  (b1,b2,b3) = fresh3

-- ForeignOp taking one boxed value and returning (Either Failure a)
-- boxDirect_efb :: Box -> Either Failure Box
boxToEFBox :: ForeignOp
boxToEFBox =
  inBx arg result $
    outIoFail stack1 stack2 fail result
  where
    (arg, result, stack1, stack2, fail) = fresh5

boxToMaybeBox :: ForeignOp
boxToMaybeBox =
  inBx arg result $
    outMaybe maybe result
  where
    (arg, maybe, result) = fresh3

-- ForeignOp taking one boxed value and returning (Either Failure a)
-- boxToEFBoxool :: Box -> Either Failure Box
boxToEFBool :: ForeignOp
boxToEFBool = inBx arg result
             $ outIoFailBool stack1 stack2 bool fail result
  where
    (arg, stack1, stack2, bool, fail, result) = fresh6

-- ForeignOp taking one boxed value and returning (Either Failure ())
-- boxDirect_efu :: Box -> Either Failure Unit
boxToEF0 :: ForeignOp
boxToEF0 = inBx arg result
          $ outIoFailUnit stack1 stack2 unit fail result
  where
    (arg, result, stack1, stack2, unit, fail) = fresh6

-- ForeignOp taking one boxed value and returning (Either Failure Nat)
-- boxDirect_nat :: Box -> Either Failure Box
boxToEFNat :: ForeignOp
boxToEFNat = inBx arg result
          $ outIoFailNat stack1 stack2 nat fail result
  where
    (arg, result, stack1, stack2, nat, fail) = fresh6

-- pfobb_efb :: Box -> Box -> Either Failure Box
boxBoxToEFBox :: ForeignOp
boxBoxToEFBox = inBxBx arg1 arg2 result
            $ outIoFail stack1 stack2 fail result
  where
    (arg1, arg2, result, stack1, stack2, fail) = fresh6

-- pfobb_efu :: Box -> Box -> Either Failure Unit
boxBoxToEF0 :: ForeignOp
boxBoxToEF0 = inBxBx arg1 arg2 result
            $ outIoFailUnit stack1 stack2 fail unit result
  where
    (arg1, arg2, result, stack1, stack2, fail, unit) = fresh7


-- pfobn_efb :: Box -> Nat -> Either Failure Box
boxNatToEFBox :: ForeignOp
boxNatToEFBox = inBxNat arg1 arg2 nat result
           $ outIoFail stack1 stack2 fail result
  where (arg1, arg2, nat, stack1, stack2, fail, result) = fresh7

-- Pure ForeignOp taking 1 boxed value and returning 1 Either, both sides boxed
boxToEBoxBox :: ForeignOp
boxToEBoxBox instr
  = ([BX],)
  . TAbss [b]
  . TLet e UN (AFOp instr [b])
  . TMatch e . MatchSum
  $ mapFromList
  [ (0, ([BX], TAbs ev $ TCon eitherReference 0 [ev]))
  , (1, ([BX], TAbs ev $ TCon eitherReference 1 [ev]))
  ]
  where
  (e,b,ev) = fresh3

builtinLookup :: Var v => Map.Map Reference (SuperNormal v)
builtinLookup
  = Map.fromList
  . map (\(t, f) -> (Builtin t, f)) $
  [ ("Int.+", addi)
  , ("Int.-", subi)
  , ("Int.*", muli)
  , ("Int./", divi)
  , ("Int.mod", modi)
  , ("Int.==", eqi)
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
  , ("Int.trailingZeros", tzeroi)
  , ("Int.leadingZeros", lzeroi)
  , ("Int.and", andi)
  , ("Int.or", ori)
  , ("Int.xor", xori)
  , ("Int.complement", compli)
  , ("Int.pow", powi)
  , ("Int.toText", i2t)
  , ("Int.fromText", t2i)
  , ("Int.toFloat", i2f)
  , ("Int.popCount", popi)

  , ("Nat.+", addn)
  , ("Nat.-", subn)
  , ("Nat.sub", subn)
  , ("Nat.*", muln)
  , ("Nat./", divn)
  , ("Nat.mod", modn)
  , ("Nat.==", eqn)
  , ("Nat.<", ltn)
  , ("Nat.<=", len)
  , ("Nat.>", gtn)
  , ("Nat.>=", gen)
  , ("Nat.increment", incn)
  , ("Nat.isEven", evnn)
  , ("Nat.isOdd", oddn)
  , ("Nat.shiftLeft", shln)
  , ("Nat.shiftRight", shrn)
  , ("Nat.trailingZeros", tzeron)
  , ("Nat.leadingZeros", lzeron)
  , ("Nat.and", andn)
  , ("Nat.or", orn)
  , ("Nat.xor", xorn)
  , ("Nat.complement", compln)
  , ("Nat.pow", pown)
  , ("Nat.drop", dropn)
  , ("Nat.toInt", cast Ty.natRef Ty.intRef)
  , ("Nat.toFloat", n2f)
  , ("Nat.toText", n2t)
  , ("Nat.fromText", t2n)
  , ("Nat.popCount", popn)

  , ("Float.+", addf)
  , ("Float.-", subf)
  , ("Float.*", mulf)
  , ("Float./", divf)
  , ("Float.pow", powf)
  , ("Float.log", logf)
  , ("Float.logBase", logbf)
  , ("Float.sqrt", sqrtf)

  , ("Float.min", minf)
  , ("Float.max", maxf)

  , ("Float.<", ltf)
  , ("Float.>", gtf)
  , ("Float.<=", lef)
  , ("Float.>=", gef)
  , ("Float.==", eqf)
  , ("Float.!=", neqf)

  , ("Float.acos", acosf)
  , ("Float.asin", asinf)
  , ("Float.atan", atanf)
  , ("Float.cos", cosf)
  , ("Float.sin", sinf)
  , ("Float.tan", tanf)

  , ("Float.acosh", acoshf)
  , ("Float.asinh", asinhf)
  , ("Float.atanh", atanhf)
  , ("Float.cosh", coshf)
  , ("Float.sinh", sinhf)
  , ("Float.tanh", tanhf)

  , ("Float.exp", expf)
  , ("Float.abs", absf)

  , ("Float.ceiling", ceilf)
  , ("Float.floor", floorf)
  , ("Float.round", roundf)
  , ("Float.truncate", truncf)
  , ("Float.atan2", atan2f)

  , ("Float.toText", f2t)
  , ("Float.fromText", t2f)

  -- text
  , ("Text.empty", Lambda [] $ TLit (T ""))
  , ("Text.++", appendt)
  , ("Text.take", taket)
  , ("Text.drop", dropt)
  , ("Text.size", sizet)
  , ("Text.==", eqt)
  , ("Text.!=", neqt)
  , ("Text.<=", leqt)
  , ("Text.>=", geqt)
  , ("Text.<", lesst)
  , ("Text.>", great)
  , ("Text.uncons", unconst)
  , ("Text.unsnoc", unsnoct)
  , ("Text.toCharList", unpackt)
  , ("Text.fromCharList", packt)

  , ("Boolean.not", notb)
  , ("Boolean.or", orb)
  , ("Boolean.and", andb)

  , ("bug", bug)
  , ("todo", bug)
  , ("Debug.watch", watch)

  , ("Char.toNat", cast Ty.charRef Ty.natRef)
  , ("Char.fromNat", cast Ty.natRef Ty.charRef)

  , ("Bytes.empty", emptyb)
  , ("Bytes.fromList", packb)
  , ("Bytes.toList", unpackb)
  , ("Bytes.++", appendb)
  , ("Bytes.take", takeb)
  , ("Bytes.drop", dropb)
  , ("Bytes.at", atb)
  , ("Bytes.size", sizeb)
  , ("Bytes.flatten", flattenb)

  , ("List.take", takes)
  , ("List.drop", drops)
  , ("List.size", sizes)
  , ("List.++", appends)
  , ("List.at", ats)
  , ("List.cons", conss)
  , ("List.snoc", snocs)
  , ("List.empty", emptys)
  , ("List.viewl", viewls)
  , ("List.viewr", viewrs)
--
--   , B "Debug.watch" $ forall1 "a" (\a -> text --> a --> a)
  , ("Universal.==", equ)
  , ("Universal.compare", cmpu)
  , ("Universal.>", gtu)
  , ("Universal.<", ltu)
  , ("Universal.>=", geu)
  , ("Universal.<=", leu)

  , ("jumpCont", jumpk)

  , ("IO.forkComp", fork'comp)
  ] ++ foreignWrappers

type FDecl v
  = State (Word64, [(Text, SuperNormal v)], EnumMap Word64 ForeignFunc)

declareForeign
  :: Var v => Text -> ForeignOp -> ForeignFunc -> FDecl v ()
declareForeign name op func
  = modify $ \(w, cs, fs)
      -> (w+1, (name, uncurry Lambda (op w)) : cs, mapInsert w func fs)

mkForeignIOF
  :: (ForeignConvention a, ForeignConvention r)
  => (a -> IO r) -> ForeignFunc
mkForeignIOF f = mkForeign $ \a -> tryIOE (f a)
  where
  tryIOE :: IO a -> IO (Either Failure a)
  tryIOE = fmap handleIOE . try
  handleIOE :: Either IOException a -> Either Failure a
  handleIOE (Left e) = Left $ Failure ioFailureReference (pack (show e))
  handleIOE (Right a) = Right a

mkForeignTls
  :: forall a r.(ForeignConvention a, ForeignConvention r)
  => (a -> IO r) -> ForeignFunc
mkForeignTls f = mkForeign $ \a -> fmap flatten (tryIO2 (tryIO1 (f a)))
  where
  tryIO1 :: IO r -> IO (Either TLS.TLSException r)
  tryIO1 = try
  tryIO2 :: IO (Either TLS.TLSException r) -> IO (Either IOException (Either TLS.TLSException r))
  tryIO2 = try
  flatten :: Either IOException (Either TLS.TLSException r) -> Either Failure r
  flatten (Left e) = Left (Failure ioFailureReference (pack (show e)))
  flatten (Right (Left e)) = Left (Failure tlsFailureReference (pack (show e)))
  flatten (Right (Right a)) = Right a

declareForeigns :: Var v => FDecl v ()
declareForeigns = do

  declareForeign "IO.openFile.v2" boxIomrToEFBox $ mkForeignIOF (uncurry openFile)
  declareForeign "IO.closeFile.v2" boxToEF0 $ mkForeignIOF hClose
  declareForeign "IO.isFileEOF.v2" boxToBool $ mkForeignIOF hIsEOF
  declareForeign "IO.isFileOpen.v2" boxToBool $ mkForeignIOF hIsOpen
  declareForeign "IO.isSeekable.v2" boxToBool $ mkForeignIOF hIsSeekable

  declareForeign "IO.seekHandle.v2" seek'handle
    . mkForeignIOF $ \(h,sm,n) -> hSeek h sm (fromIntegral (n :: Int))

  declareForeign "IO.handlePosition.v2" boxToInt
    -- TODO: truncating integer
    . mkForeignIOF $ \h -> fromInteger @Word64 <$> hTell h

  declareForeign "IO.getBuffering.v2" get'buffering
    $ mkForeignIOF hGetBuffering

  declareForeign "IO.setBuffering.v2" boxBoxToEF0
    . mkForeignIOF $ uncurry hSetBuffering

  declareForeign "IO.getLine.v2" boxToEFBox $ mkForeignIOF hGetLine

  declareForeign "IO.getBytes.v2" boxNatToEFBox .  mkForeignIOF $ \(h,n) -> Bytes.fromArray <$> hGet h n

  declareForeign "IO.putBytes.v2" boxBoxToEFBox .  mkForeignIOF $ \(h,bs) -> hPut h (Bytes.toArray bs)
  declareForeign "IO.systemTime.v2" unitToEFNat
    $ mkForeignIOF $ \() -> getPOSIXTime

  declareForeign "IO.getTempDirectory.v2" unitToEFBox
    $ mkForeignIOF $ \() -> getTemporaryDirectory

  declareForeign "IO.createTempDirectory" boxToEFBox
    $ mkForeignIOF $ \prefix -> do
       temp <- getTemporaryDirectory
       createTempDirectory temp prefix

  declareForeign "IO.getCurrentDirectory.v2" unitDirect
    . mkForeignIOF $ \() -> getCurrentDirectory

  declareForeign "IO.setCurrentDirectory.v2" boxToEF0
    $ mkForeignIOF setCurrentDirectory

  declareForeign "IO.fileExists.v2" boxToEFBool
    $ mkForeignIOF doesPathExist

  declareForeign "IO.isDirectory.v2" boxToEFBool
    $ mkForeignIOF doesDirectoryExist

  declareForeign "IO.createDirectory.v2" boxToEF0
    $ mkForeignIOF $ createDirectoryIfMissing True

  declareForeign "IO.removeDirectory.v2" boxToEF0
    $ mkForeignIOF removeDirectoryRecursive

  declareForeign "IO.renameDirectory.v2" boxBoxToEF0
    $ mkForeignIOF $ uncurry renameDirectory

  declareForeign "IO.removeFile.v2" boxToEF0
    $ mkForeignIOF removeFile

  declareForeign "IO.renameFile.v2" boxBoxToEF0
    $ mkForeignIOF $ uncurry renameFile

  declareForeign "IO.getFileTimestamp.v2" boxToEFNat
    . mkForeignIOF $ fmap utcTimeToPOSIXSeconds . getModificationTime

  declareForeign "IO.getFileSize.v2" boxToEFNat
    -- TODO: truncating integer
    . mkForeignIOF $ \fp -> fromInteger @Word64 <$> getFileSize fp

  declareForeign "IO.serverSocket.v2" boxBoxToEFBox
    . mkForeignIOF $ \(mhst,port) ->
        fst <$> SYS.bindSock (hostPreference mhst) port

  declareForeign "IO.listen.v2" boxToEF0
    . mkForeignIOF $ \sk -> SYS.listenSock sk 2048

  declareForeign "IO.clientSocket.v2" boxBoxDirect
    . mkForeignIOF $ fmap fst . uncurry SYS.connectSock

  declareForeign "IO.closeSocket.v2" boxToEF0
    $ mkForeignIOF SYS.closeSock

  declareForeign "IO.socketAccept.v2" boxDirect
    . mkForeignIOF $ fmap fst . SYS.accept

  declareForeign "IO.socketSend.v2" boxBoxToEF0
    . mkForeignIOF $ \(sk,bs) -> SYS.send sk (Bytes.toArray bs)

  declareForeign "IO.socketReceive.v2" boxNatToEFBox
    . mkForeignIOF $ \(hs,n) ->
    maybe Bytes.empty Bytes.fromArray <$> SYS.recv hs n

  declareForeign "IO.kill.v2" boxTo0 $ mkForeignIOF killThread

  declareForeign "IO.delay.v2" natToUnit $ mkForeignIOF threadDelay

  declareForeign "IO.stdHandle" standard'handle
    . mkForeign $ \(n :: Int) -> case n of
        0 -> pure (Just SYS.stdin)
        1 -> pure (Just SYS.stdout)
        2 -> pure (Just SYS.stderr)
        _ -> pure Nothing

  declareForeign "MVar.new" boxDirect
    . mkForeign $ \(c :: Closure) -> newMVar c

  declareForeign "MVar.empty" unitDirect
    . mkForeign $ \() -> newEmptyMVar @Closure

  declareForeign "MVar.take.v2" boxToEFBox
    . mkForeignIOF $ \(mv :: MVar Closure) -> takeMVar mv

  declareForeign "MVar.tryTake" boxToMaybeBox
    . mkForeign $ \(mv :: MVar Closure) -> tryTakeMVar mv

  declareForeign "MVar.put.v2 " boxBoxToEF0
    . mkForeignIOF $ \(mv :: MVar Closure, x) -> putMVar mv x

  declareForeign "MVar.tryPut" boxBoxToBool
    . mkForeign $ \(mv :: MVar Closure, x) -> tryPutMVar mv x

  declareForeign "MVar.swap.v2" boxBoxToEFBox
    . mkForeignIOF $ \(mv :: MVar Closure, x) -> swapMVar mv x

  declareForeign "MVar.isEmpty" boxToBool
    . mkForeign $ \(mv :: MVar Closure) -> isEmptyMVar mv

  declareForeign "MVar.read.v2" boxBoxToEFBox
    . mkForeignIOF $ \(mv :: MVar Closure) -> readMVar mv

  declareForeign "MVar.tryRead" boxToMaybeBox
    . mkForeign $ \(mv :: MVar Closure) -> tryReadMVar mv

  declareForeign "Text.toUtf8" boxDirect . mkForeign
    $ pure . Bytes.fromArray . encodeUtf8

  declareForeign "Text.fromUtf8" boxToEBoxBox . mkForeign
    $ pure . mapLeft (pack . show) . decodeUtf8' . Bytes.toArray

  let
    defaultSupported :: TLS.Supported
    defaultSupported = def { TLS.supportedCiphers = Cipher.ciphersuite_strong }

  declareForeign "Tls.Config.defaultClient" boxBoxDirect
    .  mkForeign $ \(hostName::Text, serverId:: Bytes.Bytes) -> do
       store <- X.getSystemCertificateStore
       let shared :: TLS.Shared
           shared = def { TLS.sharedCAStore = store }
           defaultParams = (defaultParamsClient (unpack hostName) (Bytes.toArray serverId)) { TLS.clientSupported = defaultSupported, TLS.clientShared = shared }
       pure defaultParams

  declareForeign "Tls.Config.defaultServer" unitDirect . mkForeign $ \() -> do
    pure $ (def :: ServerParams) { TLS.serverSupported = defaultSupported }

  declareForeign "Tls.newClient" boxBoxToEFBox . mkForeignTls $
    \(config :: TLS.ClientParams,
      socket :: SYS.Socket) -> TLS.contextNew socket config

  declareForeign "Tls.handshake" boxToEFBox . mkForeignTls $
    \(tls :: TLS.Context) -> TLS.handshake tls

  declareForeign "Tls.send" boxBoxToEFBox . mkForeignTls $
    \(tls :: TLS.Context,
      bytes :: Bytes.Bytes) -> TLS.sendData tls (Bytes.toLazyByteString bytes)

  declareForeign "Tls.receive" boxToEFBox . mkForeignTls $
    \(tls :: TLS.Context) -> do
      bs <- TLS.recvData tls
      pure $ Bytes.fromArray bs

  declareForeign "Tls.terminate" boxToEFBox . mkForeignTls $
    \(tls :: TLS.Context) -> TLS.bye tls

  -- Hashing functions
  let declareHashAlgorithm :: forall v alg . Var v => Hash.HashAlgorithm alg => Text -> alg -> FDecl v ()
      declareHashAlgorithm txt alg = do
        let algoRef = Builtin ("crypto.HashAlgorithm." <> txt)
        declareForeign ("crypto.HashAlgorithm." <> txt) unitDirect . mkForeign $ \() ->
          pure (HashAlgorithm algoRef alg)

  declareHashAlgorithm "Sha3_512" Hash.SHA3_512
  declareHashAlgorithm "Sha3_256" Hash.SHA3_256
  declareHashAlgorithm "Sha2_512" Hash.SHA512
  declareHashAlgorithm "Sha2_256" Hash.SHA256
  declareHashAlgorithm "Blake2b_512" Hash.Blake2b_512
  declareHashAlgorithm "Blake2b_256" Hash.Blake2b_256
  declareHashAlgorithm "Blake2s_256" Hash.Blake2s_256

  -- declareForeign ("crypto.hash") boxBoxDirect . mkForeign $ \(HashAlgorithm _ref _alg, _a :: Closure) ->
  --   pure $ Bytes.empty -- todo : implement me

  declareForeign "crypto.hashBytes" boxBoxDirect . mkForeign $
    \(HashAlgorithm _ alg, b :: Bytes.Bytes) ->
        let ctx = Hash.hashInitWith alg
        in pure . Bytes.fromArray . Hash.hashFinalize $ Hash.hashUpdates ctx (Bytes.chunks b)

  declareForeign "crypto.hmacBytes" boxBoxBoxDirect
    . mkForeign $ \(HashAlgorithm _ alg, key :: Bytes.Bytes, msg :: Bytes.Bytes) ->
        let out = u alg $ HMAC.hmac (Bytes.toArray @BA.Bytes key) (Bytes.toArray @BA.Bytes msg)
            u :: a -> HMAC.HMAC a -> HMAC.HMAC a
            u _ h = h -- to help typechecker along
        in pure $ Bytes.fromArray out

  declareForeign "Bytes.toBase16" boxDirect . mkForeign $ pure . Bytes.toBase16
  declareForeign "Bytes.toBase32" boxDirect . mkForeign $ pure . Bytes.toBase32
  declareForeign "Bytes.toBase64" boxDirect . mkForeign $ pure . Bytes.toBase64
  declareForeign "Bytes.toBase64UrlUnpadded" boxDirect . mkForeign $ pure . Bytes.toBase64UrlUnpadded

  declareForeign "Bytes.fromBase16" boxToEBoxBox . mkForeign $ pure . Bytes.fromBase16
  declareForeign "Bytes.fromBase32" boxToEBoxBox . mkForeign $ pure . Bytes.fromBase32
  declareForeign "Bytes.fromBase64" boxToEBoxBox . mkForeign $ pure . Bytes.fromBase64
  declareForeign "Bytes.fromBase64UrlUnpadded" boxDirect . mkForeign $ pure . Bytes.fromBase64UrlUnpadded

hostPreference :: Maybe Text -> SYS.HostPreference
hostPreference Nothing = SYS.HostAny
hostPreference (Just host) = SYS.Host $ Text.unpack host

typeReferences :: [(Reference, Word64)]
typeReferences = zip rs [1..]
  where
  rs = [ r | (_,r) <- Ty.builtinTypes ]
    ++ [ DerivedId i | (_,i,_) <- Ty.builtinDataDecls @Symbol ]
    ++ [ DerivedId i | (_,i,_) <- Ty.builtinEffectDecls @Symbol ]

foreignDeclResults
  :: Var v
  => (Word64, [(Text, SuperNormal v)], EnumMap Word64 ForeignFunc)
foreignDeclResults = execState declareForeigns (0, [], mempty)

foreignWrappers :: Var v => [(Text, SuperNormal v)]
foreignWrappers | (_, l, _) <- foreignDeclResults = reverse l

numberedTermLookup :: Var v => EnumMap Word64 (SuperNormal v)
numberedTermLookup
  = mapFromList . zip [1..] . Map.elems $ builtinLookup

builtinTermNumbering :: Map Reference Word64
builtinTermNumbering
  = Map.fromList (zip (Map.keys $ builtinLookup @Symbol) [1..])

builtinTermBackref :: EnumMap Word64 Reference
builtinTermBackref
  = mapFromList . zip [1..] . Map.keys $ builtinLookup @Symbol

builtinTypeNumbering :: Map Reference Word64
builtinTypeNumbering = Map.fromList typeReferences

builtinTypeBackref :: EnumMap Word64 Reference
builtinTypeBackref = mapFromList $ swap <$> typeReferences
  where swap (x, y) = (y, x)

builtinForeigns :: EnumMap Word64 ForeignFunc
builtinForeigns | (_, _, m) <- foreignDeclResults @Symbol = m
