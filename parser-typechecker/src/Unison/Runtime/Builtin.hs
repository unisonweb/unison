{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
{-# language RankNTypes #-}
{-# language ViewPatterns #-}
{-# language PatternGuards #-}
{-# language PatternSynonyms #-}
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
  , Sandbox(..)
  , baseSandboxInfo
  ) where

import Control.Monad.State.Strict (State, modify, execState)
import qualified Control.Exception.Safe as Exception
import Control.Monad.Catch (MonadCatch)
import Control.DeepSeq (NFData)
import Control.Concurrent (ThreadId)
import System.IO (Handle)

import Unison.ABT.Normalized hiding (TTm)
import Unison.Reference
import Unison.Runtime.ANF as ANF
import Unison.Runtime.ANF.Serialize as ANF
import Unison.Referent (pattern Ref)
import Unison.Var
import Unison.Symbol
import qualified Unison.Runtime.Stack as Closure
import Unison.Runtime.Stack (Closure)
import Unison.Runtime.Foreign
    ( Foreign(Wrap), HashAlgorithm(..), pattern Failure)
import qualified Unison.Runtime.Foreign as F
import Unison.Runtime.Foreign.Function

import qualified Unison.Type as Ty
import qualified Unison.Builtin as Ty (builtinTypes)
import qualified Unison.Builtin.Decls as Ty
import qualified Crypto.Hash as Hash
import qualified Crypto.MAC.HMAC as HMAC

import Unison.Util.EnumContainers as EC

import Data.Default (def)
import Data.ByteString (hGet, hPut)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as L
import qualified System.X509 as X
import qualified Data.X509 as X
import qualified Data.X509.Memory as X
import qualified Data.X509.CertificateStore as X
import Data.PEM (pemContent, pemParseLBS, PEM)
import Data.Set (insert)
import qualified Data.Set as Set

import qualified Data.Map as Map
import Unison.Prelude hiding (some,Text)
import qualified Unison.Util.Bytes as Bytes
import qualified Data.Text
import qualified Data.Text.IO as Text.IO
import qualified Unison.Util.Text as Util.Text
import Network.Socket as SYS
  ( accept
  , socketPort
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

import Data.IORef as SYS
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  )
import System.IO as SYS
  ( IOMode(..)
  , openFile
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
  , doesPathExist
  , doesDirectoryExist
  , renameDirectory
  , removeFile
  , renameFile
  , createDirectoryIfMissing
  , removeDirectoryRecursive
  , getDirectoryContents
  , getModificationTime
  , getFileSize
  )
import System.Environment as SYS
  ( getEnv
  , getArgs
  )
import System.IO.Temp (createTempDirectory)

import qualified Control.Concurrent.STM as STM
import qualified GHC.Conc as STM

import GHC.IO (IO(IO))

type Failure = F.Failure Closure

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

fresh4 :: Var v => (v, v, v, v)
fresh4 = (v1, v2, v3, v4) where
  [v1, v2, v3, v4] = freshes 4

fresh5 :: Var v => (v, v, v, v, v)
fresh5 = (v1, v2, v3, v4, v5) where
  [v1, v2, v3, v4, v5] = freshes 5

fresh6 :: Var v => (v, v, v, v, v, v)
fresh6 = (v1, v2, v3, v4, v5, v6) where
  [v1, v2, v3, v4, v5, v6] = freshes 6

fresh7 :: Var v => (v, v, v, v, v, v, v)
fresh7 = (v1, v2, v3, v4, v5, v6, v7) where
  [v1, v2, v3, v4, v5, v6, v7] = freshes 7

fresh8 :: Var v => (v, v, v, v, v, v, v, v)
fresh8 = (v1, v2, v3, v4, v5, v6, v7, v8) where
  [v1, v2, v3, v4, v5, v6, v7, v8] = freshes 8

fresh11 :: Var v => (v, v, v, v, v, v, v, v, v, v, v)
fresh11 = (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) where
  [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11] = freshes 11

fls, tru :: Var v => ANormal v
fls = TCon Ty.booleanRef 0 []
tru = TCon Ty.booleanRef 1 []

none :: Var v => ANormal v
none = TCon Ty.optionalRef (toEnum Ty.noneId) []
some, left, right :: Var v => v -> ANormal v
some a = TCon Ty.optionalRef (toEnum Ty.someId) [a]
left x = TCon Ty.eitherRef (toEnum Ty.eitherLeftId) [x]
right x = TCon Ty.eitherRef (toEnum Ty.eitherRightId) [x]
seqViewEmpty :: Var v => ANormal v
seqViewEmpty = TCon Ty.seqViewRef (toEnum Ty.seqViewEmpty) []
seqViewElem :: Var v => v -> v -> ANormal v
seqViewElem l r = TCon Ty.seqViewRef (toEnum Ty.seqViewElem) [l,r]

boolift :: Var v => v -> ANormal v
boolift v
  = TMatch v $ MatchIntegral (mapFromList [(0,fls), (1,tru)]) Nothing

notlift :: Var v => v -> ANormal v
notlift v
  = TMatch v $ MatchIntegral (mapFromList [(1,fls), (0,tru)]) Nothing

unbox :: Var v => v -> Reference -> v -> ANormal v -> ANormal v
unbox v0 r v b
  = TMatch v0
  $ MatchData r (mapSingleton 0 $ ([UN], TAbs v b)) Nothing

unenum :: Var v => Int -> v -> Reference -> v -> ANormal v -> ANormal v
unenum n v0 r v nx
  = TMatch v0 $ MatchData r cases Nothing
  where
  mkCase i = (toEnum i, ([], TLetD v UN (TLit . I $ fromIntegral i) nx))
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
  . TLetD r UN (TPrm pop [x])
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
  . TLetD r UN (TPrm pop [x,y])
  $ TCon rfr 0 [r]

cmpop :: Var v => POp -> Reference -> SuperNormal v
cmpop pop rf
  = binop0 3 $ \[x0,y0,x,y,b]
 -> unbox x0 rf x
  . unbox y0 rf y
  . TLetD b UN (TPrm pop [x,y])
  $ boolift b

cmpopb :: Var v => POp -> Reference -> SuperNormal v
cmpopb pop rf
  = binop0 3 $ \[x0,y0,x,y,b]
 -> unbox x0 rf x
  . unbox y0 rf y
  . TLetD b UN (TPrm pop [y,x])
  $ boolift b

cmpopn :: Var v => POp -> Reference -> SuperNormal v
cmpopn pop rf
  = binop0 3 $ \[x0,y0,x,y,b]
 -> unbox x0 rf x
  . unbox y0 rf y
  . TLetD b UN (TPrm pop [x,y])
  $ notlift b

cmpopbn :: Var v => POp -> Reference -> SuperNormal v
cmpopbn pop rf
  = binop0 3 $ \[x0,y0,x,y,b]
 -> unbox x0 rf x
  . unbox y0 rf y
  . TLetD b UN (TPrm pop [y,x])
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
     . TLetD z UN (TLit $ I 0)
     . TLetD b UN (TPrm LEQI [x, z])
     . TMatch b
     $ MatchIntegral
         (mapSingleton 1 $ TCon Ty.natRef 0 [z])
         (Just $ TCon Ty.natRef 0 [x])

modular :: Var v => POp -> (Bool -> ANormal v) -> SuperNormal v
modular pop ret
  = unop0 3 $ \[x0,x,m,t]
 -> unbox x0 Ty.intRef x
  . TLetD t UN (TLit $ I 2)
  . TLetD m UN (TPrm pop [x,t])
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
      . TLetD b UN (TPrm LEQN [x,y])
      . TLet (Indirect 1) r UN
          (TMatch b $ MatchIntegral
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
     -> TLetD r UN (TPrm SIZT [x])
      $ TCon Ty.natRef 0 [r]
unconst = unop0 7 $ \[x,t,c0,c,y,p,u,yp]
     -> TLetD t UN (TPrm UCNS [x])
      . TMatch t . MatchSum $ mapFromList
      [ (0, ([], none))
      , (1, ([UN,BX], TAbss [c0,y]
                    . TLetD u BX (TCon Ty.unitRef 0 [])
                    . TLetD yp BX (TCon Ty.pairRef 0 [y,u])
                    . TLetD c BX (TCon Ty.charRef 0 [c0])
                    . TLetD p BX (TCon Ty.pairRef 0 [c,yp])
                    $ some p))
      ]
unsnoct = unop0 7 $ \[x,t,c0,c,y,p,u,cp]
     -> TLetD t UN (TPrm USNC [x])
      . TMatch t . MatchSum $ mapFromList
      [ (0, ([], none))
      , (1, ([BX,UN], TAbss [y,c0]
                    . TLetD u BX (TCon Ty.unitRef 0 [])
                    . TLetD c BX (TCon Ty.charRef 0 [c0])
                    . TLetD cp BX (TCon Ty.pairRef 0 [c,u])
                    . TLetD p BX (TCon Ty.pairRef 0 [y,cp])
                    $ some p))
      ]

appends, conss, snocs :: Var v => SuperNormal v
appends = binop0 0 $ \[x,y] -> TPrm CATS [x,y]
conss = binop0 0 $ \[x,y] -> TPrm CONS [x,y]
snocs = binop0 0 $ \[x,y] -> TPrm SNOC [x,y]

coerceType :: Var v => Reference -> Reference -> SuperNormal v
coerceType fromType toType = unop0 1 $ \[x,r]
     -> unbox x fromType r
      $ TCon toType 0 [r]

takes, drops, sizes, ats, emptys :: Var v => SuperNormal v
takes = binop0 1 $ \[x0,y,x]
     -> unbox x0 Ty.natRef x
      $ TPrm TAKS [x,y]
drops = binop0 1 $ \[x0,y,x]
     -> unbox x0 Ty.natRef x
      $ TPrm DRPS [x,y]
sizes = unop0 1 $ \[x,r]
     -> TLetD r UN (TPrm SIZS [x])
      $ TCon Ty.natRef 0 [r]
ats = binop0 3 $ \[x0,y,x,t,r]
   -> unbox x0 Ty.natRef x
    . TLetD t UN (TPrm IDXS [x,y])
    . TMatch t . MatchSum $ mapFromList
    [ (0, ([], none))
    , (1, ([BX], TAbs r $ some r))
    ]
emptys = Lambda [] $ TPrm BLDS []

viewls, viewrs :: Var v => SuperNormal v
viewls = unop0 3 $ \[s,u,h,t]
      -> TLetD u UN (TPrm VWLS [s])
       . TMatch u . MatchSum $ mapFromList
       [ (0, ([], seqViewEmpty))
       , (1, ([BX,BX], TAbss [h,t] $ seqViewElem h t))
       ]
viewrs = unop0 3 $ \[s,u,i,l]
      -> TLetD u UN (TPrm VWRS [s])
       . TMatch u . MatchSum $ mapFromList
       [ (0, ([], seqViewEmpty))
       , (1, ([BX,BX], TAbss [i,l] $ seqViewElem i l))
       ]

eqt, neqt, leqt, geqt, lesst, great :: SuperNormal Symbol
eqt = binop0 1 $ \[x,y,b]
   -> TLetD b UN (TPrm EQLT [x,y])
    $ boolift b
neqt = binop0 1 $ \[x,y,b]
    -> TLetD b UN (TPrm EQLT [x,y])
     $ notlift b
leqt = binop0 1 $ \[x,y,b]
    -> TLetD b UN (TPrm LEQT [x,y])
     $ boolift b
geqt = binop0 1 $ \[x,y,b]
    -> TLetD b UN (TPrm LEQT [y,x])
     $ boolift b
lesst = binop0 1 $ \[x,y,b]
     -> TLetD b UN (TPrm LEQT [y,x])
      $ notlift b
great = binop0 1 $ \[x,y,b]
     -> TLetD b UN (TPrm LEQT [x,y])
      $ notlift b

packt, unpackt :: SuperNormal Symbol
packt = unop0 0 $ \[s] -> TPrm PAKT [s]
unpackt = unop0 0 $ \[t] -> TPrm UPKT [t]

packb, unpackb, emptyb, appendb :: SuperNormal Symbol
packb = unop0 0 $ \[s] -> TPrm PAKB [s]
unpackb = unop0 0 $ \[b] -> TPrm UPKB [b]
emptyb
  = Lambda []
  . TLetD es BX (TPrm BLDS [])
  $ TPrm PAKB [es]
  where
  es = fresh1
appendb = binop0 0 $ \[x,y] -> TPrm CATB [x,y]

takeb, dropb, atb, sizeb, flattenb :: SuperNormal Symbol
takeb = binop0 1 $ \[n0,b,n]
     -> unbox n0 Ty.natRef n
      $ TPrm TAKB [n,b]

dropb = binop0 1 $ \[n0,b,n]
     -> unbox n0 Ty.natRef n
      $ TPrm DRPB [n,b]

atb = binop0 4 $ \[n0,b,n,t,r0,r]
   -> unbox n0 Ty.natRef n
    . TLetD t UN (TPrm IDXB [n,b])
    . TMatch t . MatchSum $ mapFromList
    [ (0, ([], none))
    , (1, ([UN], TAbs r0
               . TLetD r BX (TCon Ty.natRef 0 [r0])
               $ some r))
    ]

sizeb = unop0 1 $ \[b,n]
     -> TLetD n UN (TPrm SIZB [b])
      $ TCon Ty.natRef 0 [n]

flattenb = unop0 0 $ \[b] -> TPrm FLTB [b]

i2t, n2t, f2t :: SuperNormal Symbol
i2t = unop0 1 $ \[n0,n]
   -> unbox n0 Ty.intRef n
    $ TPrm ITOT [n]
n2t = unop0 1 $ \[n0,n]
   -> unbox n0 Ty.natRef n
    $ TPrm NTOT [n]
f2t = unop0 1 $ \[f0,f]
   -> unbox f0 Ty.floatRef f
    $ TPrm FTOT [f]

t2i, t2n, t2f :: SuperNormal Symbol
t2i = unop0 3 $ \[x,t,n0,n]
   -> TLetD t UN (TPrm TTOI [x])
    . TMatch t . MatchSum $ mapFromList
    [ (0, ([], none))
    , (1, ([UN], TAbs n0
               . TLetD n BX (TCon Ty.intRef 0 [n0])
               $ some n))
    ]
t2n = unop0 3 $ \[x,t,n0,n]
   -> TLetD t UN (TPrm TTON [x])
    . TMatch t . MatchSum $ mapFromList
    [ (0, ([], none))
    , (1, ([UN], TAbs n0
               . TLetD n BX (TCon Ty.natRef 0 [n0])
               $ some n))
    ]
t2f = unop0 3 $ \[x,t,f0,f]
   -> TLetD t UN (TPrm TTOF [x])
    . TMatch t . MatchSum $ mapFromList
    [ (0, ([], none))
    , (1, ([UN], TAbs f0
               . TLetD f BX (TCon Ty.floatRef 0 [f0])
               $ some f))
    ]

equ :: SuperNormal Symbol
equ = binop0 1 $ \[x,y,b]
   -> TLetD b UN (TPrm EQLU [x,y])
    $ boolift b

cmpu :: SuperNormal Symbol
cmpu = binop0 2 $ \[x,y,c,i]
    -> TLetD c UN (TPrm CMPU [x,y])
     . TLetD i UN (TPrm DECI [c])
     $ TCon Ty.intRef 0 [i]

ltu :: SuperNormal Symbol
ltu = binop0 1 $ \[x,y,c]
   -> TLetD c UN (TPrm CMPU [x,y])
    . TMatch c
    $ MatchIntegral
        (mapFromList [ (0, TCon Ty.booleanRef 1 []) ])
        (Just $ TCon Ty.booleanRef 0 [])

gtu :: SuperNormal Symbol
gtu = binop0 1 $ \[x,y,c]
   -> TLetD c UN (TPrm CMPU [x,y])
    . TMatch c
    $ MatchIntegral
        (mapFromList [ (2, TCon Ty.booleanRef 1 []) ])
        (Just $ TCon Ty.booleanRef 0 [])

geu :: SuperNormal Symbol
geu = binop0 1 $ \[x,y,c]
   -> TLetD c UN (TPrm CMPU [x,y])
    . TMatch c
    $ MatchIntegral
        (mapFromList [ (0, TCon Ty.booleanRef 0 []) ])
        (Just $ TCon Ty.booleanRef 1 [])

leu :: SuperNormal Symbol
leu = binop0 1 $ \[x,y,c]
   -> TLetD c UN (TPrm CMPU [x,y])
    . TMatch c
    $ MatchIntegral
        (mapFromList [ (2, TCon Ty.booleanRef 0 []) ])
        (Just $ TCon Ty.booleanRef 1 [])

notb :: SuperNormal Symbol
notb = unop0 0 $ \[b]
    -> TMatch b . flip (MatchData Ty.booleanRef) Nothing
     $ mapFromList [ (0, ([], tru)), (1, ([], fls)) ]

orb :: SuperNormal Symbol
orb = binop0 0 $ \[p,q]
   -> TMatch p . flip (MatchData Ty.booleanRef) Nothing
    $ mapFromList [ (1, ([], tru)), (0, ([], TVar q)) ]

andb :: SuperNormal Symbol
andb = binop0 0 $ \[p,q]
    -> TMatch p . flip (MatchData Ty.booleanRef) Nothing
     $ mapFromList [ (0, ([], fls)), (1, ([], TVar q)) ]

-- unsafeCoerce, used for numeric types where conversion is a
-- no-op on the representation. Ideally this will be inlined and
-- eliminated so that no instruction is necessary.
cast :: Reference -> Reference -> SuperNormal Symbol
cast ri ro
  = unop0 1 $ \[x0,x]
 -> unbox x0 ri x
  $ TCon ro 0 [x]

-- This version of unsafeCoerce is the identity function. It works
-- only if the two types being coerced between are actually the same,
-- because it keeps the same representation. It is not capable of
-- e.g. correctly translating between two types with compatible bit
-- representations, because tagging information will be retained.
poly'coerce :: SuperNormal Symbol
poly'coerce = unop0 0 $ \[x] -> TVar x

jumpk :: SuperNormal Symbol
jumpk = binop0 0 $ \[k,a] -> TKon k [a]

scope'run :: SuperNormal Symbol
scope'run
  = unop0 1 $ \[e, un]
 -> TLetD un BX (TCon Ty.unitRef 0 [])
  $ TApp (FVar e) [un]

fork'comp :: SuperNormal Symbol
fork'comp
  = Lambda [BX]
  . TAbs act
  . TLetD unit BX (TCon Ty.unitRef 0 [])
  . TName lz (Right act) [unit]
  $ TPrm FORK [lz]
  where
  (act,unit,lz) = fresh3

bug :: Util.Text.Text -> SuperNormal Symbol
bug name
  = unop0 1 $ \[x, n]
 -> TLetD n BX (TLit $ T name)
  $ TPrm EROR [n, x]

watch :: SuperNormal Symbol
watch
  = binop0 0 $ \[t,v]
 -> TLets Direct [] [] (TPrm PRNT [t])
  $ TVar v

raise :: SuperNormal Symbol
raise
  = unop0 4 $ \[r,f,n,j,k]
 -> TMatch r . flip (MatchData Ty.exceptionRef) Nothing $ mapFromList
  [ (0, ([BX], TAbs f $ TVar f))
  , (i, ([UN,BX]
      , TAbss [j,f]
      . TShift Ty.exceptionRef k
      . TLetD n BX (TLit $ T "builtin.raise")
      $ TPrm EROR [n, f]))
  ]
  where
  i = fromIntegral $ builtinTypeNumbering Map.! Ty.exceptionRef

gen'trace :: SuperNormal Symbol
gen'trace
  = binop0 0 $ \[t,v]
 -> TLets Direct [] [] (TPrm TRCE [t,v])
  $ TCon Ty.unitRef 0 []

code'missing :: SuperNormal Symbol
code'missing
  = unop0 1 $ \[link,b]
 -> TLetD b UN (TPrm MISS [link])
  $ boolift b

code'cache :: SuperNormal Symbol
code'cache = unop0 0 $ \[new] -> TPrm CACH [new]

code'lookup :: SuperNormal Symbol
code'lookup
  = unop0 2 $ \[link,t,r]
 -> TLetD t UN (TPrm LKUP [link])
  . TMatch t . MatchSum $ mapFromList
  [ (0, ([], none))
  , (1, ([BX], TAbs r $ some r))
  ]

code'validate :: SuperNormal Symbol
code'validate
  = unop0 5 $ \[item, t, ref, msg, extra, fail]
 -> TLetD t UN (TPrm CVLD [item])
  . TMatch t . MatchSum
  $ mapFromList
  [ (1, ([BX, BX, BX],)
      . TAbss [ref, msg, extra]
      . TLetD fail BX (TCon Ty.failureRef 0 [ref, msg, extra])
      $ some fail)
  , (0, ([],)
      $ none)
  ]

term'link'to'text :: SuperNormal Symbol
term'link'to'text
  = unop0 0 $ \[link] -> TPrm TLTT [link]

value'load :: SuperNormal Symbol
value'load
  = unop0 2 $ \[vlu,t,r]
 -> TLetD t UN (TPrm LOAD [vlu])
  . TMatch t . MatchSum $ mapFromList
  [ (0, ([BX], TAbs r $ left r))
  , (1, ([BX], TAbs r $ right r))
  ]

value'create :: SuperNormal Symbol
value'create = unop0 0 $ \[x] -> TPrm VALU [x]

check'sandbox :: SuperNormal Symbol
check'sandbox
  = Lambda [BX,BX]
  . TAbss [refs, val]
  . TLetD b UN (TPrm SDBX [refs, val])
  $ boolift b
  where
  (refs,val,b) = fresh3

stm'atomic :: SuperNormal Symbol
stm'atomic
  = Lambda [BX]
  . TAbs act
  . TLetD unit BX (TCon Ty.unitRef 0 [])
  . TName lz (Right act) [unit]
  $ TPrm ATOM [lz]
  where
  (act,unit,lz) = fresh3

type ForeignOp = FOp -> ([Mem], ANormal Symbol)

standard'handle :: ForeignOp
standard'handle instr
  = ([BX],)
  . TAbss [h0]
  . unenum 3 h0 Ty.stdHandleRef h
  $ TFOp instr [h]
  where
  (h0,h) = fresh2

any'construct :: SuperNormal Symbol
any'construct
  = unop0 0 $ \[v]
 -> TCon Ty.anyRef 0 [v]

any'extract :: SuperNormal Symbol
any'extract
  = unop0 1
  $ \[v,v1] -> TMatch v
  $ MatchData Ty.anyRef (mapSingleton 0 $ ([BX], TAbs v1 (TVar v1))) Nothing

seek'handle :: ForeignOp
seek'handle instr
  = ([BX,BX,BX],)
  . TAbss [arg1, arg2, arg3]
  . unenum 3 arg2 Ty.seekModeRef seek
  . unbox arg3 Ty.intRef nat
  . TLetD result UN (TFOp instr [arg1, seek, nat])
  $ outIoFailUnit stack1 stack2 stack3 unit fail result
  where
    (arg1, arg2, arg3, seek, nat, stack1, stack2, stack3, unit, fail, result) = fresh11

no'buf, line'buf, block'buf, sblock'buf :: Enum e => e
no'buf = toEnum Ty.bufferModeNoBufferingId
line'buf = toEnum Ty.bufferModeLineBufferingId
block'buf = toEnum Ty.bufferModeBlockBufferingId
sblock'buf = toEnum Ty.bufferModeSizedBlockBufferingId

infixr 0 -->
(-->) :: a -> b -> (a, b)
x --> y = (x, y)

set'buffering :: ForeignOp
set'buffering instr
  = ([BX,BX],)
  . TAbss [handle, bmode]
  . TMatch bmode . MatchDataCover Ty.bufferModeRef $ mapFromList
  [ no'buf --> [] --> k1 no'buf
  , line'buf --> [] --> k1 line'buf
  , block'buf --> [] --> k1 block'buf
  , sblock'buf --> [BX] -->
      TAbs n . TMatch n . MatchDataCover Ty.bufferModeRef $ mapFromList
      [ 0 --> [UN] -->
            TAbs w
          . TLetD tag UN (TLit (N sblock'buf))
          $ k2 [tag,w]
      ]
  ]
  where
  k1 num = TLetD tag UN (TLit (N num))
         $ k2 [tag]
  k2 args = TLetD r UN (TFOp instr (handle:args))
          $ outIoFailUnit s1 s2 s3 u f r
  (handle,bmode,tag,n,w,s1,s2,s3,u,f,r) = fresh11

get'buffering'output :: forall v. Var v => v -> v -> v -> v -> v -> v -> ANormal v
get'buffering'output eitherResult stack1 stack2 resultTag failVar successVar =
  TMatch eitherResult . MatchSum $ mapFromList
    [ (0, ([BX, BX],)
        . TAbss [stack1, stack2]
        . TLetD failVar BX (TCon Ty.failureRef 0 [stack1, stack2])
        $ left failVar)
    , (1, ([UN],)
      . TAbs resultTag
      . TMatch resultTag . MatchSum $ mapFromList
      [ no'buf --> [] -->
        TLetD successVar BX (TCon Ty.bufferModeRef no'buf [])
        $ right successVar
      , line'buf --> [] -->
        TLetD successVar BX (TCon Ty.bufferModeRef line'buf [])
        $ right successVar
      , block'buf --> [] -->
        TLetD successVar BX (TCon Ty.bufferModeRef block'buf [])
        $ right successVar
      , sblock'buf --> [UN] -->
            TAbs stack1
          . TLetD stack2 BX (TCon Ty.natRef 0 [stack1])
          . TLetD successVar BX (TCon Ty.bufferModeRef sblock'buf [stack2])
          $ right successVar
      ])
    ]

get'buffering :: ForeignOp
get'buffering
  = inBx arg1 eitherResult
  $ get'buffering'output eitherResult n n2 resultTag failVar successVar
  where
  (arg1, eitherResult, n, n2, resultTag, failVar, successVar) = fresh7

crypto'hash :: ForeignOp
crypto'hash instr
  = ([BX,BX],)
  . TAbss [alg,x]
  . TLetD vl BX (TPrm VALU [x])
  $ TFOp instr [alg,vl]
  where
  (alg,x,vl) = fresh3

crypto'hmac :: ForeignOp
crypto'hmac instr
  = ([BX,BX,BX],)
  . TAbss [alg,by,x]
  . TLetD vl BX (TPrm VALU [x])
  $ TFOp instr [alg,by,vl]
  where
  (alg,by,x,vl) = fresh4

-- Input Shape -- these will represent different argument lists a
-- foreign might expect
--
-- They will be named according to their shape:
--   inBx     : one boxed input arg
--   inNat     : one Nat input arg
--   inBxBx   : two boxed input args
--
-- All of these functions will have take (at least) the same three arguments
--
--   instr : the foreign instruction to call
--   result : a variable containing the result of the foreign call
--   cont : a term which will be evaluated when a result from the foreign call is on the stack
--


-- () -> ...
inUnit :: forall v. Var v => v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
inUnit unit result cont instr
  = ([BX], TAbs unit $ TLetD result UN (TFOp instr []) cont)

-- a -> ...
inBx :: forall v. Var v => v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
inBx arg result cont instr =
  ([BX],)
  . TAbs arg
  $ TLetD result UN (TFOp instr [arg]) cont

-- Nat -> ...
inNat :: forall v. Var v => v -> v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
inNat arg nat result cont instr =
  ([BX],)
  . TAbs arg
  . unbox arg Ty.natRef nat
  $ TLetD result UN (TFOp instr [nat]) cont


-- Maybe a -> b -> ...
inMaybeBx :: forall v. Var v => v -> v -> v -> v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
inMaybeBx arg1 arg2 arg3 mb result cont instr =
  ([BX, BX],)
  . TAbss [arg1, arg2]
  . TMatch arg1 . flip (MatchData Ty.optionalRef) Nothing
  $ mapFromList
    [ (toEnum Ty.noneId, ([], TLetD mb UN (TLit $ I 0)
               $ TLetD result UN (TFOp instr [mb, arg2]) cont))
    , (toEnum Ty.someId, ([BX], TAbs arg3 . TLetD mb UN (TLit $ I 1) $ TLetD result UN (TFOp instr [mb, arg3, arg2]) cont))
    ]

-- a -> b -> ...
inBxBx :: forall v. Var v => v -> v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
inBxBx arg1 arg2 result cont instr =
  ([BX, BX],)
  . TAbss [arg1, arg2]
  $ TLetD result UN (TFOp instr [arg1, arg2]) cont

-- a -> Nat -> ...
inBxNat ::  forall v. Var v => v -> v -> v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
inBxNat arg1 arg2 nat result cont instr =
  ([BX,BX],)
  . TAbss [arg1, arg2]
  . unbox arg2 Ty.natRef nat
  $ TLetD result UN (TFOp instr [arg1, nat]) cont

-- a -> IOMode -> ...
inBxIomr :: forall v. Var v => v -> v -> v -> v -> ANormal v -> FOp -> ([Mem], ANormal v)
inBxIomr arg1 arg2 fm result cont instr
  = ([BX,BX],)
  . TAbss [arg1, arg2]
  . unenum 4 arg2 Ty.fileModeRef fm
  $ TLetD result UN (TFOp instr [arg1, fm]) cont

-- Output Shape -- these will represent different ways of translating
-- the result of a foreign call to a Unison Term
--
-- They will be named according to the output type
--   outInt    : a foreign function returning an Int
--   outBool   : a foreign function returning a boolean
--   outIOFail : a function returning (Either Failure a)
--
-- All of these functions will take a Var named result containing the
-- result of the foreign call
--
outMaybe :: forall v. Var v => v -> v -> ANormal v
outMaybe maybe result =
  TMatch result . MatchSum $ mapFromList
  [ (0, ([], none))
  , (1, ([BX], TAbs maybe $ some maybe))
  ]

outMaybeTup :: forall v. Var v => v -> v -> v -> v -> v -> v -> v -> ANormal v
outMaybeTup a b n u bp p result =
  TMatch result . MatchSum $ mapFromList
  [ (0, ([], none))
  , (1, ([UN,BX], TAbss [a,b]
                . TLetD u BX (TCon Ty.unitRef 0 [])
                . TLetD bp BX (TCon Ty.pairRef 0 [b,u])
                . TLetD n BX (TCon Ty.natRef 0 [a])
                . TLetD p BX (TCon Ty.pairRef 0 [n,bp])
                $ some p))
  ]

outIoFail :: forall v. Var v => v -> v -> v -> v -> ANormal v
outIoFail stack1 stack2 fail result =
  TMatch result . MatchSum $ mapFromList
  [ (0, ([BX, BX],)
        . TAbss [stack1, stack2]
        . TLetD fail BX (TCon Ty.failureRef 0 [stack1, stack2])
        $ left fail)
  , (1, ([BX], TAbs stack1 $ right stack1))
  ]

outIoFailNat :: forall v. Var v => v -> v -> v -> v -> v -> v -> ANormal v
outIoFailNat stack1 stack2 stack3 fail nat result =
  TMatch result . MatchSum $ mapFromList
  [ (0, ([BX, BX],)
        . TAbss [stack1, stack2]
        . TLetD fail BX (TCon Ty.failureRef 0 [stack1, stack2])
        $ left fail)
  , (1, ([UN],)
        . TAbs stack3
        . TLetD nat BX (TCon Ty.natRef 0 [stack3])
        $ right nat)
  ]

outIoFailBox :: forall v. Var v => v -> v -> v -> v -> ANormal v
outIoFailBox stack1 stack2 fail result =
  TMatch result . MatchSum  $ mapFromList
  [ (0, ([BX, BX],)
        . TAbss [stack1, stack2]
        . TLetD fail BX (TCon Ty.failureRef 0 [stack1, stack2])
        $ left fail)
  , (1, ([BX],)
        . TAbs stack1
        $ right stack1)
  ]

outIoFailUnit :: forall v. Var v => v -> v -> v -> v -> v -> v -> ANormal v
outIoFailUnit stack1 stack2 stack3 unit fail result =
  TMatch result . MatchSum
  $ mapFromList
  [ (0, ([BX, BX],)
        . TAbss [stack1, stack2]
        . TLetD fail BX (TCon Ty.failureRef 0 [stack1, stack2])
        $ left fail)
  , (1, ([BX],)
        . TAbss [stack3]
        . TLetD unit BX (TCon Ty.unitRef 0 [])
        $ right unit)
  ]

outIoFailBool :: forall v. Var v => v -> v -> v -> v -> v -> v -> ANormal v
outIoFailBool stack1 stack2 stack3 bool fail result =
  TMatch result . MatchSum
  $ mapFromList
  [ (0, ([BX, BX],)
        . TAbss [stack1, stack2]
        . TLetD fail BX (TCon Ty.failureRef 0 [stack1, stack2])
        $ left fail)
  , (1, ([UN],)
        . TAbs stack3
        . TLet (Indirect 1) bool BX (boolift stack3)
        $ right bool)
  ]

outIoFailG
  :: Var v => v -> v -> v -> v -> v
  -> ((ANormal v -> ANormal v) -> ([Mem], ANormal v))
  -> ANormal v
outIoFailG stack1 stack2 fail result output k
  = TMatch result . MatchSum $ mapFromList
  [ (0, ([BX, BX],)
      . TAbss [stack1, stack2]
      . TLetD fail BX (TCon Ty.failureRef 0 [stack1, stack2])
      $ left fail)
  , (1, k $ \t -> TLetD output BX t
                $ right output)
  ]

-- Input / Output glue
--
-- These are pairings of input and output functions to handle a
-- foreign call.  The input function represents the numbers and types
-- of the inputs to a forein call.  The output function takes the
-- result of the foreign call and turns it into a Unison type.
--

-- a
direct :: ForeignOp
direct instr = ([], TFOp instr [])

--  () -> a
unitDirect :: ForeignOp
unitDirect instr = ([BX],) . TAbs arg $ TFOp instr [] where arg = fresh1

-- a -> b
boxDirect :: ForeignOp
boxDirect instr =
  ([BX],)
  . TAbs arg
  $ TFOp instr [arg] where
  arg = fresh1

-- () -> Either Failure Nat
unitToEFNat :: ForeignOp
unitToEFNat = inUnit unit result
            $ outIoFailNat stack1 stack2 stack3 fail nat result
  where (unit, stack1, stack2, stack3, fail, nat, result) = fresh7

-- () -> Int
unitToInt :: ForeignOp
unitToInt = inUnit unit result
            $ TCon Ty.intRef 0 [result]
  where (unit, result) = fresh2

-- () -> Either Failure a
unitToEFBox :: ForeignOp
unitToEFBox = inUnit unit result
            $ outIoFailBox stack1 stack2 fail result
  where (unit, stack1, stack2, fail, result) = fresh5

boxIomrToEFBox :: ForeignOp
boxIomrToEFBox = inBxIomr arg1 arg2 enum result
              $ outIoFailBox stack1 stack2 fail result
  where
    (arg1, arg2, enum, stack1, stack2, fail, result) = fresh7

-- a -> ()
boxTo0 :: ForeignOp
boxTo0 = inBx arg result (TCon Ty.unitRef 0 [])
  where
    (arg, result) = fresh2

-- a -> b ->{E} ()
boxBoxTo0 :: ForeignOp
boxBoxTo0 instr
  = ([BX,BX],)
  . TAbss [arg1,arg2]
  . TLets Direct [] [] (TFOp instr [arg1,arg2])
  $ TCon Ty.unitRef 0 []
  where
  (arg1, arg2) = fresh2

-- Nat -> ()
-- natToUnit :: ForeignOp
-- natToUnit = inNat arg nat result (TCon Ty.unitRef 0 [])
--   where
--     (arg, nat, result) = fresh3

-- a -> Bool
boxToBool :: ForeignOp
boxToBool = inBx arg result
          $ boolift result
  where
    (arg, result) = fresh2

-- Nat -> c
-- Works for an type that's packed into a word, just
-- pass `wordDirect Ty.natRef`, `wordDirect Ty.floatRef`
-- etc
wordDirect :: Reference -> ForeignOp
wordDirect wordType instr
  = ([BX],)
  . TAbss [b1]
  . unbox b1 wordType ub1
  $ TFOp instr [ub1]
  where
  (b1,ub1) = fresh2


-- Nat -> a -> c
-- Works for an type that's packed into a word, just
-- pass `wordBoxDirect Ty.natRef`, `wordBoxDirect Ty.floatRef`
-- etc
wordBoxDirect :: Reference -> ForeignOp
wordBoxDirect wordType instr
  = ([BX,BX],)
  . TAbss [b1,b2]
  . unbox b1 wordType ub1
  $ TFOp instr [ub1,b2]
  where
  (b1,b2,ub1) = fresh3

-- a -> b -> c
boxBoxDirect :: ForeignOp
boxBoxDirect instr
  = ([BX,BX],)
  . TAbss [b1,b2]
  $ TFOp instr [b1,b2]
  where
  (b1,b2) = fresh2

-- a -> b -> c -> d
boxBoxBoxDirect :: ForeignOp
boxBoxBoxDirect instr
  = ([BX,BX,BX],)
  . TAbss [b1,b2,b3]
  $ TFOp instr [b1,b2,b3]
  where
  (b1,b2,b3) = fresh3

-- a -> Either Failure b
boxToEFBox :: ForeignOp
boxToEFBox =
  inBx arg result $
    outIoFail stack1 stack2 fail result
  where
    (arg, result, stack1, stack2, fail) = fresh5

-- a -> Either Failure (Maybe b)
boxToEFMBox :: ForeignOp
boxToEFMBox
  = inBx arg result
  . outIoFailG stack1 stack2 fail result output $ \k ->
  ([UN], TAbs stack3 . TMatch stack3 . MatchSum $ mapFromList
         [ (0, ([], k $ none))
         , (1, ([BX], TAbs stack4 . k $ some stack4))
         ])
  where
  (arg, result, stack1, stack2, stack3, stack4, fail, output) = fresh8

-- a -> Maybe b
boxToMaybeBox :: ForeignOp
boxToMaybeBox =
  inBx arg result $ outMaybe maybe result
  where
    (arg, maybe, result) = fresh3

-- a -> Maybe b
boxToMaybeTup :: ForeignOp
boxToMaybeTup =
  inBx arg result $ outMaybeTup a b c u bp p result
  where
    (arg, a, b, c, u, bp, p, result) = fresh8



-- a -> Either Failure Bool
boxToEFBool :: ForeignOp
boxToEFBool = inBx arg result
             $ outIoFailBool stack1 stack2 stack3 bool fail result
  where
    (arg, stack1, stack2, stack3, bool, fail, result) = fresh7

-- a -> b -> Either Failure Bool
boxBoxToEFBool :: ForeignOp
boxBoxToEFBool = inBxBx arg1 arg2 result
             $ outIoFailBool stack1 stack2 stack3 bool fail result
  where
    (arg1, arg2, stack1, stack2, stack3, bool, fail, result) = fresh8

-- a -> Either Failure ()
boxToEF0 :: ForeignOp
boxToEF0 = inBx arg result
          $ outIoFailUnit stack1 stack2 stack3 unit fail result
  where
    (arg, result, stack1, stack2, stack3, unit, fail) = fresh7

-- a -> b -> Either Failure ()
boxBoxToEF0 :: ForeignOp
boxBoxToEF0 = inBxBx arg1 arg2 result
            $ outIoFailUnit stack1 stack2 stack3 fail unit result
  where
    (arg1, arg2, result, stack1, stack2, stack3, fail, unit) = fresh8

-- a -> Either Failure Nat
boxToEFNat :: ForeignOp
boxToEFNat = inBx arg result
          $ outIoFailNat stack1 stack2 stack3 nat fail result
  where
    (arg, result, stack1, stack2, stack3, nat, fail) = fresh7

-- Maybe a -> b -> Either Failure c
maybeBoxToEFBox :: ForeignOp
maybeBoxToEFBox = inMaybeBx arg1 arg2 arg3 mb result
                $ outIoFail stack1 stack2 fail result
  where
    (arg1, arg2, arg3, mb, result, stack1, stack2, fail) = fresh8


-- a -> b -> Either Failure c
boxBoxToEFBox :: ForeignOp
boxBoxToEFBox = inBxBx arg1 arg2 result
            $ outIoFail stack1 stack2 fail result
  where
    (arg1, arg2, result, stack1, stack2, fail) = fresh6

-- a -> Nat -> Either Failure b
boxNatToEFBox :: ForeignOp
boxNatToEFBox = inBxNat arg1 arg2 nat result
           $ outIoFail stack1 stack2 fail result
  where (arg1, arg2, nat, stack1, stack2, fail, result) = fresh7

-- Nat -> Either Failure ()
natToEFUnit :: ForeignOp
natToEFUnit
  = inNat arg nat result
  . TMatch result . MatchSum $ mapFromList
  [ (0, ([BX, BX],)
      . TAbss [stack1, stack2]
      . TLetD fail BX (TCon Ty.failureRef 0 [stack1, stack2])
      $ left fail)
  , (1, ([],)
      . TLetD unit BX (TCon Ty.unitRef 0 [])
      $ right unit)

  ]
  where
    (arg, nat, result, fail, stack1, stack2, unit) = fresh7

-- a -> Either b c
boxToEBoxBox :: ForeignOp
boxToEBoxBox instr
  = ([BX],)
  . TAbss [b]
  . TLetD e UN (TFOp instr [b])
  . TMatch e . MatchSum
  $ mapFromList
  [ (0, ([BX], TAbs ev $ left ev))
  , (1, ([BX], TAbs ev $ right ev))
  ]
  where
  (e,b,ev) = fresh3

builtinLookup :: Map.Map Reference (Sandbox, SuperNormal Symbol)
builtinLookup
  = Map.fromList
  . map (\(t, f) -> (Builtin t, f)) $
  [ ("Int.+", (Untracked, addi))
  , ("Int.-", (Untracked, subi))
  , ("Int.*", (Untracked, muli))
  , ("Int./", (Untracked, divi))
  , ("Int.mod", (Untracked, modi))
  , ("Int.==", (Untracked, eqi))
  , ("Int.<", (Untracked, lti))
  , ("Int.<=", (Untracked, lei))
  , ("Int.>", (Untracked, gti))
  , ("Int.>=", (Untracked, gei))
  , ("Int.fromRepresentation", (Untracked, coerceType Ty.natRef Ty.intRef))
  , ("Int.toRepresentation", (Untracked, coerceType Ty.intRef Ty.natRef))
  , ("Int.increment", (Untracked, inci))
  , ("Int.signum", (Untracked, sgni))
  , ("Int.negate", (Untracked, negi))
  , ("Int.truncate0", (Untracked, trni))
  , ("Int.isEven", (Untracked, evni))
  , ("Int.isOdd", (Untracked, oddi))
  , ("Int.shiftLeft", (Untracked, shli))
  , ("Int.shiftRight", (Untracked, shri))
  , ("Int.trailingZeros", (Untracked, tzeroi))
  , ("Int.leadingZeros", (Untracked, lzeroi))
  , ("Int.and", (Untracked, andi))
  , ("Int.or", (Untracked, ori))
  , ("Int.xor", (Untracked, xori))
  , ("Int.complement", (Untracked, compli))
  , ("Int.pow", (Untracked, powi))
  , ("Int.toText", (Untracked, i2t))
  , ("Int.fromText", (Untracked, t2i))
  , ("Int.toFloat", (Untracked, i2f))
  , ("Int.popCount", (Untracked, popi))

  , ("Nat.+", (Untracked, addn))
  , ("Nat.-", (Untracked, subn))
  , ("Nat.sub", (Untracked, subn))
  , ("Nat.*", (Untracked, muln))
  , ("Nat./", (Untracked, divn))
  , ("Nat.mod", (Untracked, modn))
  , ("Nat.==", (Untracked, eqn))
  , ("Nat.<", (Untracked, ltn))
  , ("Nat.<=", (Untracked, len))
  , ("Nat.>", (Untracked, gtn))
  , ("Nat.>=", (Untracked, gen))
  , ("Nat.increment", (Untracked, incn))
  , ("Nat.isEven", (Untracked, evnn))
  , ("Nat.isOdd", (Untracked, oddn))
  , ("Nat.shiftLeft", (Untracked, shln))
  , ("Nat.shiftRight", (Untracked, shrn))
  , ("Nat.trailingZeros", (Untracked, tzeron))
  , ("Nat.leadingZeros", (Untracked, lzeron))
  , ("Nat.and", (Untracked, andn))
  , ("Nat.or", (Untracked, orn))
  , ("Nat.xor", (Untracked, xorn))
  , ("Nat.complement", (Untracked, compln))
  , ("Nat.pow", (Untracked, pown))
  , ("Nat.drop", (Untracked, dropn))
  , ("Nat.toInt", (Untracked, cast Ty.natRef Ty.intRef))
  , ("Nat.toFloat", (Untracked, n2f))
  , ("Nat.toText", (Untracked, n2t))
  , ("Nat.fromText", (Untracked, t2n))
  , ("Nat.popCount", (Untracked, popn))
  , ("Float.+", (Untracked, addf))
  , ("Float.-", (Untracked, subf))
  , ("Float.*", (Untracked, mulf))
  , ("Float./", (Untracked, divf))
  , ("Float.pow", (Untracked, powf))
  , ("Float.log", (Untracked, logf))
  , ("Float.logBase", (Untracked, logbf))
  , ("Float.sqrt", (Untracked, sqrtf))
  , ("Float.fromRepresentation", (Untracked, coerceType Ty.natRef Ty.floatRef))
  , ("Float.toRepresentation", (Untracked, coerceType Ty.floatRef Ty.natRef))

  , ("Float.min", (Untracked, minf))
  , ("Float.max", (Untracked, maxf))

  , ("Float.<", (Untracked, ltf))
  , ("Float.>", (Untracked, gtf))
  , ("Float.<=", (Untracked, lef))
  , ("Float.>=", (Untracked, gef))
  , ("Float.==", (Untracked, eqf))
  , ("Float.!=", (Untracked, neqf))

  , ("Float.acos", (Untracked, acosf))
  , ("Float.asin", (Untracked, asinf))
  , ("Float.atan", (Untracked, atanf))
  , ("Float.cos", (Untracked, cosf))
  , ("Float.sin", (Untracked, sinf))
  , ("Float.tan", (Untracked, tanf))

  , ("Float.acosh", (Untracked, acoshf))
  , ("Float.asinh", (Untracked, asinhf))
  , ("Float.atanh", (Untracked, atanhf))
  , ("Float.cosh", (Untracked, coshf))
  , ("Float.sinh", (Untracked, sinhf))
  , ("Float.tanh", (Untracked, tanhf))

  , ("Float.exp", (Untracked, expf))
  , ("Float.abs", (Untracked, absf))

  , ("Float.ceiling", (Untracked, ceilf))
  , ("Float.floor", (Untracked, floorf))
  , ("Float.round", (Untracked, roundf))
  , ("Float.truncate", (Untracked, truncf))
  , ("Float.atan2", (Untracked, atan2f))

  , ("Float.toText", (Untracked, f2t))
  , ("Float.fromText", (Untracked, t2f))

  -- text
  , ("Text.empty", (Untracked, Lambda [] $ TLit (T "")))
  , ("Text.++", (Untracked, appendt))
  , ("Text.take", (Untracked, taket))
  , ("Text.drop", (Untracked, dropt))
  , ("Text.size", (Untracked, sizet))
  , ("Text.==", (Untracked, eqt))
  , ("Text.!=", (Untracked, neqt))
  , ("Text.<=", (Untracked, leqt))
  , ("Text.>=", (Untracked, geqt))
  , ("Text.<", (Untracked, lesst))
  , ("Text.>", (Untracked, great))
  , ("Text.uncons", (Untracked, unconst))
  , ("Text.unsnoc", (Untracked, unsnoct))
  , ("Text.toCharList", (Untracked, unpackt))
  , ("Text.fromCharList", (Untracked, packt))

  , ("Boolean.not", (Untracked, notb))
  , ("Boolean.or", (Untracked, orb))
  , ("Boolean.and", (Untracked, andb))

  , ("bug", (Tracked, bug "builtin.bug"))
  , ("todo", (Tracked, bug "builtin.todo"))
  , ("Debug.watch", (Tracked, watch))
  , ("Debug.trace", (Tracked, gen'trace))
  , ("unsafe.coerceAbilities", (Tracked, poly'coerce))

  , ("Char.toNat", (Untracked, cast Ty.charRef Ty.natRef))
  , ("Char.fromNat", (Untracked, cast Ty.natRef Ty.charRef))

  , ("Bytes.empty", (Untracked, emptyb))
  , ("Bytes.fromList", (Untracked, packb))
  , ("Bytes.toList", (Untracked, unpackb))
  , ("Bytes.++", (Untracked, appendb))
  , ("Bytes.take", (Untracked, takeb))
  , ("Bytes.drop", (Untracked, dropb))
  , ("Bytes.at", (Untracked, atb))
  , ("Bytes.size", (Untracked, sizeb))
  , ("Bytes.flatten", (Untracked, flattenb))

  , ("List.take", (Untracked, takes))
  , ("List.drop", (Untracked, drops))
  , ("List.size", (Untracked, sizes))
  , ("List.++", (Untracked, appends))
  , ("List.at", (Untracked, ats))
  , ("List.cons", (Untracked, conss))
  , ("List.snoc", (Untracked, snocs))
  , ("List.empty", (Untracked, emptys))
  , ("List.viewl", (Untracked, viewls))
  , ("List.viewr", (Untracked, viewrs))
--
--   , B "Debug.watch" $ forall1 "a" (\a -> text --> a --> a)
  , ("Universal.==", (Untracked, equ))
  , ("Universal.compare", (Untracked, cmpu))
  , ("Universal.>", (Untracked, gtu))
  , ("Universal.<", (Untracked, ltu))
  , ("Universal.>=", (Untracked, geu))
  , ("Universal.<=", (Untracked, leu))

  -- internal stuff
  , ("jumpCont", (Untracked, jumpk))
  , ("raise", (Untracked, raise))

  , ("IO.forkComp.v2", (Tracked, fork'comp))

  , ("Scope.run", (Untracked, scope'run))

  , ("Code.isMissing", (Tracked, code'missing))
  , ("Code.cache_", (Tracked, code'cache))
  , ("Code.lookup", (Tracked, code'lookup))
  , ("Code.validate", (Tracked, code'validate))
  , ("Value.load", (Tracked, value'load))
  , ("Value.value", (Tracked, value'create))
  , ("Any.Any", (Untracked, any'construct))
  , ("Any.unsafeExtract", (Tracked, any'extract))
  , ("Link.Term.toText", (Untracked, term'link'to'text))
  , ("STM.atomically", (Untracked, stm'atomic))

  , ("validateSandboxed", (Untracked, check'sandbox))
  ] ++ foreignWrappers

type FDecl v
  = State (Word64, [(Data.Text.Text, (Sandbox, SuperNormal v))], EnumMap Word64 ForeignFunc)

-- Data type to determine whether a builtin should be tracked for
-- sandboxing. Untracked means that it can be freely used, and Tracked
-- means that the sandboxing check will by default consider them
-- disallowed.
data Sandbox = Tracked | Untracked
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

declareForeign
  :: Sandbox
  -> Data.Text.Text
  -> ForeignOp
  -> ForeignFunc
  -> FDecl Symbol ()
declareForeign sand name op func
  = modify $ \(w, cs, fs)
      -> (w+1, (name, (sand, uncurry Lambda (op w))) : cs, mapInsert w func fs)

mkForeignIOF
  :: (ForeignConvention a, ForeignConvention r)
  => (a -> IO r) -> ForeignFunc
mkForeignIOF f = mkForeign $ \a -> tryIOE (f a)
  where
  tryIOE :: IO a -> IO (Either Failure a)
  tryIOE = fmap handleIOE . try
  handleIOE :: Either IOException a -> Either Failure a
  handleIOE (Left e) = Left $ Failure Ty.ioFailureRef (Util.Text.pack (show e)) unitValue
  handleIOE (Right a) = Right a

unitValue :: Closure
unitValue = Closure.Enum Ty.unitRef 0

mkForeignTls
  :: forall a r.(ForeignConvention a, ForeignConvention r)
  => (a -> IO r) -> ForeignFunc
mkForeignTls f = mkForeign $ \a -> fmap flatten (tryIO2 (tryIO1 (f a)))
  where
  tryIO1 :: IO r -> IO (Either TLS.TLSException r)
  tryIO1 = try
  tryIO2 :: IO (Either TLS.TLSException r) -> IO (Either IOException (Either TLS.TLSException r))
  tryIO2 = try
  flatten :: Either IOException (Either TLS.TLSException r) -> Either (Failure ) r
  flatten (Left e) = Left (Failure Ty.ioFailureRef (Util.Text.pack (show e)) unitValue)
  flatten (Right (Left e)) = Left (Failure Ty.tlsFailureRef (Util.Text.pack (show e)) (unitValue))
  flatten (Right (Right a)) = Right a

declareForeigns :: FDecl Symbol ()
declareForeigns = do
  declareForeign Tracked "IO.openFile.impl.v3" boxIomrToEFBox $
    mkForeignIOF $ \(fnameText :: Util.Text.Text, n :: Int) ->
      let fname = Util.Text.toString fnameText
          mode = case n of
            0 -> ReadMode
            1 -> WriteMode
            2 -> AppendMode
            _ -> ReadWriteMode
      in openFile fname mode

  declareForeign Tracked "IO.closeFile.impl.v3" boxToEF0 $ mkForeignIOF hClose
  declareForeign Tracked "IO.isFileEOF.impl.v3" boxToEFBool $ mkForeignIOF hIsEOF
  declareForeign Tracked "IO.isFileOpen.impl.v3" boxToEFBool $ mkForeignIOF hIsOpen
  declareForeign Tracked "IO.isSeekable.impl.v3" boxToEFBool $ mkForeignIOF hIsSeekable

  declareForeign Tracked "IO.seekHandle.impl.v3" seek'handle
    . mkForeignIOF $ \(h,sm,n) -> hSeek h sm (fromIntegral (n :: Int))

  declareForeign Tracked "IO.handlePosition.impl.v3" boxToEFNat
    -- TODO: truncating integer
    . mkForeignIOF $ \h -> fromInteger @Word64 <$> hTell h

  declareForeign Tracked "IO.getBuffering.impl.v3" get'buffering
    $ mkForeignIOF hGetBuffering

  declareForeign Tracked "IO.setBuffering.impl.v3" set'buffering
    . mkForeignIOF $ uncurry hSetBuffering

  declareForeign Tracked "IO.getLine.impl.v1" boxToEFBox $ mkForeignIOF $
    fmap Util.Text.fromText . Text.IO.hGetLine

  declareForeign Tracked "IO.getBytes.impl.v3" boxNatToEFBox .  mkForeignIOF
    $ \(h,n) -> Bytes.fromArray <$> hGet h n

  declareForeign Tracked "IO.putBytes.impl.v3" boxBoxToEF0 .  mkForeignIOF $ \(h,bs) -> hPut h (Bytes.toArray bs)

  declareForeign Tracked "IO.systemTime.impl.v3" unitToEFNat
    $ mkForeignIOF $ \() -> getPOSIXTime

  declareForeign Tracked "IO.systemTimeMicroseconds.v1" unitToInt
    $ mkForeign $ \() -> fmap (1e6 *) getPOSIXTime

  declareForeign Tracked "IO.getTempDirectory.impl.v3" unitToEFBox
    $ mkForeignIOF $ \() -> getTemporaryDirectory

  declareForeign Tracked "IO.createTempDirectory.impl.v3" boxToEFBox
    $ mkForeignIOF $ \prefix -> do
       temp <- getTemporaryDirectory
       createTempDirectory temp prefix

  declareForeign Tracked "IO.getCurrentDirectory.impl.v3" unitToEFBox
    . mkForeignIOF $ \() -> getCurrentDirectory

  declareForeign Tracked "IO.setCurrentDirectory.impl.v3" boxToEF0
    $ mkForeignIOF setCurrentDirectory

  declareForeign Tracked "IO.fileExists.impl.v3" boxToEFBool
    $ mkForeignIOF doesPathExist

  declareForeign Tracked "IO.getEnv.impl.v1" boxToEFBox
    $ mkForeignIOF getEnv

  declareForeign Tracked "IO.getArgs.impl.v1" unitToEFBox
    $ mkForeignIOF $ \() -> fmap Util.Text.pack <$> SYS.getArgs

  declareForeign Tracked "IO.isDirectory.impl.v3" boxToEFBool
    $ mkForeignIOF doesDirectoryExist

  declareForeign Tracked "IO.createDirectory.impl.v3" boxToEF0
    $ mkForeignIOF $ createDirectoryIfMissing True

  declareForeign Tracked "IO.removeDirectory.impl.v3" boxToEF0
    $ mkForeignIOF removeDirectoryRecursive

  declareForeign Tracked "IO.renameDirectory.impl.v3" boxBoxToEF0
    $ mkForeignIOF $ uncurry renameDirectory

  declareForeign Tracked "IO.directoryContents.impl.v3" boxToEFBox
    $ mkForeignIOF $ (fmap Util.Text.pack <$>) . getDirectoryContents

  declareForeign Tracked "IO.removeFile.impl.v3" boxToEF0
    $ mkForeignIOF removeFile

  declareForeign Tracked "IO.renameFile.impl.v3" boxBoxToEF0
    $ mkForeignIOF $ uncurry renameFile

  declareForeign Tracked "IO.getFileTimestamp.impl.v3" boxToEFNat
    . mkForeignIOF $ fmap utcTimeToPOSIXSeconds . getModificationTime

  declareForeign Tracked "IO.getFileSize.impl.v3" boxToEFNat
    -- TODO: truncating integer
    . mkForeignIOF $ \fp -> fromInteger @Word64 <$> getFileSize fp

  declareForeign Tracked "IO.serverSocket.impl.v3" maybeBoxToEFBox
    . mkForeignIOF $ \(mhst :: Maybe Util.Text.Text
                      , port) ->
        fst <$> SYS.bindSock (hostPreference mhst) port

  declareForeign Untracked "Socket.toText" boxDirect
    . mkForeign $ \(sock :: Socket) -> pure $ show sock

  declareForeign Untracked "Handle.toText" boxDirect
    . mkForeign $ \(hand :: Handle) -> pure $ show hand

  declareForeign Untracked "ThreadId.toText" boxDirect
    . mkForeign $ \(threadId :: ThreadId) -> pure $ show threadId

  declareForeign Tracked "IO.socketPort.impl.v3" boxToEFNat
    . mkForeignIOF $ \(handle :: Socket) -> do
        n <- SYS.socketPort handle
        return (fromIntegral n :: Word64)

  declareForeign Tracked "IO.listen.impl.v3" boxToEF0
    . mkForeignIOF $ \sk -> SYS.listenSock sk 2

  declareForeign Tracked "IO.clientSocket.impl.v3" boxBoxToEFBox
    . mkForeignIOF $ fmap fst . uncurry SYS.connectSock

  declareForeign Tracked "IO.closeSocket.impl.v3" boxToEF0
    $ mkForeignIOF SYS.closeSock

  declareForeign Tracked "IO.socketAccept.impl.v3" boxToEFBox
    . mkForeignIOF $ fmap fst . SYS.accept

  declareForeign Tracked "IO.socketSend.impl.v3" boxBoxToEF0
    . mkForeignIOF $ \(sk,bs) -> SYS.send sk (Bytes.toArray bs)

  declareForeign Tracked "IO.socketReceive.impl.v3" boxNatToEFBox
    . mkForeignIOF $ \(hs,n) ->
    maybe mempty Bytes.fromArray <$> SYS.recv hs n

  declareForeign Tracked "IO.kill.impl.v3" boxTo0 $ mkForeignIOF killThread

  declareForeign Tracked "IO.delay.impl.v3" natToEFUnit
    $ mkForeignIOF threadDelay

  declareForeign Tracked "IO.stdHandle" standard'handle
    . mkForeign $ \(n :: Int) -> case n of
        0 -> pure (Just SYS.stdin)
        1 -> pure (Just SYS.stdout)
        2 -> pure (Just SYS.stderr)
        _ -> pure Nothing

  declareForeign Tracked "MVar.new" boxDirect
    . mkForeign $ \(c :: Closure) -> newMVar c

  declareForeign Tracked "MVar.newEmpty.v2" unitDirect
    . mkForeign $ \() -> newEmptyMVar @Closure

  declareForeign Tracked "MVar.take.impl.v3" boxToEFBox
    . mkForeignIOF $ \(mv :: MVar Closure) -> takeMVar mv

  declareForeign Tracked "MVar.tryTake" boxToMaybeBox
    . mkForeign $ \(mv :: MVar Closure) -> tryTakeMVar mv

  declareForeign Tracked "MVar.put.impl.v3" boxBoxToEF0
    . mkForeignIOF $ \(mv :: MVar Closure, x) -> putMVar mv x

  declareForeign Tracked "MVar.tryPut.impl.v3" boxBoxToEFBool
    . mkForeignIOF $ \(mv :: MVar Closure, x) -> tryPutMVar mv x

  declareForeign Tracked "MVar.swap.impl.v3" boxBoxToEFBox
    . mkForeignIOF $ \(mv :: MVar Closure, x) -> swapMVar mv x

  declareForeign Tracked "MVar.isEmpty" boxToBool
    . mkForeign $ \(mv :: MVar Closure) -> isEmptyMVar mv

  declareForeign Tracked "MVar.read.impl.v3" boxToEFBox
    . mkForeignIOF $ \(mv :: MVar Closure) -> readMVar mv

  declareForeign Tracked "MVar.tryRead.impl.v3" boxToEFMBox
    . mkForeignIOF $ \(mv :: MVar Closure) -> tryReadMVar mv


  declareForeign Untracked "Char.toText" (wordDirect Ty.charRef) . mkForeign $
    \(ch :: Char) -> pure (Util.Text.singleton ch)

  declareForeign Untracked "Text.repeat" (wordBoxDirect Ty.natRef) . mkForeign $
    \(n :: Word64, txt :: Util.Text.Text) -> pure (Util.Text.replicate (fromIntegral n) txt)

  declareForeign Untracked "Text.toUtf8" boxDirect . mkForeign
    $ pure . Util.Text.toUtf8

  declareForeign Untracked "Text.fromUtf8.impl.v3" boxToEFBox . mkForeign
    $ pure . mapLeft (\t -> Failure Ty.ioFailureRef (Util.Text.pack t) unitValue) . Util.Text.fromUtf8

  declareForeign Tracked "Tls.ClientConfig.default" boxBoxDirect .  mkForeign
    $ \(hostName :: Util.Text.Text, serverId:: Bytes.Bytes) ->
        fmap (\store ->
              (defaultParamsClient (Util.Text.unpack hostName) (Bytes.toArray serverId)) {
                 TLS.clientSupported = def { TLS.supportedCiphers = Cipher.ciphersuite_strong },
                 TLS.clientShared = def { TLS.sharedCAStore = store }
                 }) X.getSystemCertificateStore

  declareForeign Tracked "Tls.ServerConfig.default" boxBoxDirect $ mkForeign
    $ \(certs :: [X.SignedCertificate], key :: X.PrivKey) ->
        pure $ (def :: TLS.ServerParams) { TLS.serverSupported = def { TLS.supportedCiphers = Cipher.ciphersuite_strong }
                                         , TLS.serverShared = def { TLS.sharedCredentials = Credentials [(X.CertificateChain certs, key)] }
                                         }

  let updateClient :: X.CertificateStore -> TLS.ClientParams -> TLS.ClientParams
      updateClient certs client = client { TLS.clientShared = ((clientShared client) { TLS.sharedCAStore = certs }) } in

        declareForeign Tracked "Tls.ClientConfig.certificates.set" boxBoxDirect . mkForeign $
          \(certs :: [X.SignedCertificate], params :: ClientParams) -> pure $ updateClient (X.makeCertificateStore certs) params

  let updateServer :: X.CertificateStore -> TLS.ServerParams -> TLS.ServerParams
      updateServer certs client = client { TLS.serverShared = ((serverShared client) { TLS.sharedCAStore = certs }) } in
        declareForeign Tracked "Tls.ServerConfig.certificates.set" boxBoxDirect . mkForeign $
          \(certs :: [X.SignedCertificate], params :: ServerParams) -> pure $ updateServer (X.makeCertificateStore certs) params

  declareForeign Tracked "TVar.new" boxDirect . mkForeign
    $ \(c :: Closure) -> unsafeSTMToIO $ STM.newTVar c

  declareForeign Tracked "TVar.read" boxDirect . mkForeign
    $ \(v :: STM.TVar Closure) -> unsafeSTMToIO $ STM.readTVar v

  declareForeign Tracked "TVar.write" boxBoxTo0 . mkForeign
    $ \(v :: STM.TVar Closure, c :: Closure)
        -> unsafeSTMToIO $ STM.writeTVar v c

  declareForeign Tracked "TVar.newIO" boxDirect . mkForeign
    $ \(c :: Closure) -> STM.newTVarIO c

  declareForeign Tracked "TVar.readIO" boxDirect . mkForeign
    $ \(v :: STM.TVar Closure) -> STM.readTVarIO v

  declareForeign Tracked "TVar.swap" boxBoxDirect . mkForeign
    $ \(v, c :: Closure) -> unsafeSTMToIO $ STM.swapTVar v c

  declareForeign Tracked "STM.retry" unitDirect . mkForeign
    $ \() -> unsafeSTMToIO STM.retry :: IO Closure

  -- Scope and Ref stuff
  declareForeign Tracked "Scope.ref" boxDirect
    . mkForeign $ \(c :: Closure) -> newIORef c

  declareForeign Tracked "IO.ref" boxDirect
    . mkForeign $ \(c :: Closure) -> newIORef c

  declareForeign Tracked "Ref.read" boxDirect . mkForeign $
    \(r :: IORef Closure) -> readIORef r

  declareForeign Tracked "Ref.write" boxBoxTo0 . mkForeign $
    \(r :: IORef Closure, c :: Closure) -> writeIORef r c

  declareForeign Tracked "Tls.newClient.impl.v3" boxBoxToEFBox . mkForeignTls $
    \(config :: TLS.ClientParams,
      socket :: SYS.Socket) -> TLS.contextNew socket config

  declareForeign Tracked "Tls.newServer.impl.v3" boxBoxToEFBox . mkForeignTls $
    \(config :: TLS.ServerParams,
      socket :: SYS.Socket) -> TLS.contextNew socket config

  declareForeign Tracked "Tls.handshake.impl.v3" boxToEF0 . mkForeignTls $
    \(tls :: TLS.Context) -> do
        i <- contextGetInformation tls
        traceShow i $ TLS.handshake tls

  declareForeign Tracked "Tls.send.impl.v3" boxBoxToEFBox . mkForeignTls $
    \(tls :: TLS.Context,
      bytes :: Bytes.Bytes) -> TLS.sendData tls (Bytes.toLazyByteString bytes)

  let wrapFailure t = Failure Ty.tlsFailureRef (Util.Text.pack t) unitValue
      decoded :: Bytes.Bytes -> Either String PEM
      decoded bytes = case pemParseLBS $ Bytes.toLazyByteString bytes of
        Right (pem : _) -> Right pem
        Right _ -> Left "no PEM found"
        Left l -> Left l
      asCert :: PEM -> Either String X.SignedCertificate
      asCert pem = X.decodeSignedCertificate  $ pemContent pem
    in
      declareForeign Tracked "Tls.decodeCert.impl.v3" boxToEFBox . mkForeignTls $
        \(bytes :: Bytes.Bytes) -> pure $ mapLeft wrapFailure $ (decoded >=> asCert) bytes

  declareForeign Tracked "Tls.encodeCert" boxDirect . mkForeign $
    \(cert :: X.SignedCertificate) -> pure $ Bytes.fromArray $ X.encodeSignedObject cert

  declareForeign Tracked "Tls.decodePrivateKey" boxDirect . mkForeign $
    \(bytes :: Bytes.Bytes) -> pure $ X.readKeyFileFromMemory $ L.toStrict $ Bytes.toLazyByteString bytes

  declareForeign Tracked "Tls.encodePrivateKey" boxDirect . mkForeign $
    \(privateKey :: X.PrivKey) -> pure $ Util.Text.pack $ show privateKey

  declareForeign Tracked "Tls.receive.impl.v3" boxToEFBox . mkForeignTls $
    \(tls :: TLS.Context) -> do
      bs <- TLS.recvData tls
      pure $ Bytes.fromArray bs

  declareForeign Tracked "Tls.terminate.impl.v3" boxToEFBox . mkForeignTls $
    \(tls :: TLS.Context) -> TLS.bye tls

  declareForeign Tracked "Code.dependencies" boxDirect
    . mkForeign $ \(sg :: SuperGroup Symbol)
        -> pure $ Wrap Ty.termLinkRef . Ref <$> groupTermLinks sg
  declareForeign Untracked "Code.serialize" boxDirect
    . mkForeign $ \(sg :: SuperGroup Symbol)
        -> pure . Bytes.fromArray $ serializeGroup sg
  declareForeign Untracked "Code.deserialize" boxToEBoxBox
    . mkForeign $ pure . deserializeGroup @Symbol . Bytes.toArray
  declareForeign Untracked "Code.display" boxBoxDirect . mkForeign
    $ \(nm,sg) -> pure $ prettyGroup @Symbol (Util.Text.unpack nm) sg ""
  declareForeign Tracked "Value.dependencies" boxDirect
    . mkForeign $
        pure . fmap (Wrap Ty.termLinkRef . Ref) . valueTermLinks
  declareForeign Untracked "Value.serialize" boxDirect
    . mkForeign $ pure . Bytes.fromArray . serializeValue
  declareForeign Untracked "Value.deserialize" boxToEBoxBox
    . mkForeign $ pure . deserializeValue . Bytes.toArray
  -- Hashing functions
  let declareHashAlgorithm :: forall alg . Hash.HashAlgorithm alg => Data.Text.Text -> alg -> FDecl Symbol ()
      declareHashAlgorithm txt alg = do
        let algoRef = Builtin ("crypto.HashAlgorithm." <> txt)
        declareForeign Untracked ("crypto.HashAlgorithm." <> txt) direct . mkForeign $ \() ->
          pure (HashAlgorithm algoRef alg)

  declareHashAlgorithm "Sha3_512" Hash.SHA3_512
  declareHashAlgorithm "Sha3_256" Hash.SHA3_256
  declareHashAlgorithm "Sha2_512" Hash.SHA512
  declareHashAlgorithm "Sha2_256" Hash.SHA256
  declareHashAlgorithm "Blake2b_512" Hash.Blake2b_512
  declareHashAlgorithm "Blake2b_256" Hash.Blake2b_256
  declareHashAlgorithm "Blake2s_256" Hash.Blake2s_256

  declareForeign Untracked "crypto.hashBytes" boxBoxDirect . mkForeign $
    \(HashAlgorithm _ alg, b :: Bytes.Bytes) ->
        let ctx = Hash.hashInitWith alg
        in pure . Bytes.fromArray . Hash.hashFinalize $ Hash.hashUpdates ctx (Bytes.byteStringChunks b)

  declareForeign Untracked "crypto.hmacBytes" boxBoxBoxDirect
    . mkForeign $ \(HashAlgorithm _ alg, key :: Bytes.Bytes, msg :: Bytes.Bytes) ->
        let out = u alg $ HMAC.hmac (Bytes.toArray @BA.Bytes key) (Bytes.toArray @BA.Bytes msg)
            u :: a -> HMAC.HMAC a -> HMAC.HMAC a
            u _ h = h -- to help typechecker along
        in pure $ Bytes.fromArray out

  declareForeign Untracked "crypto.hash" crypto'hash . mkForeign
    $ \(HashAlgorithm _ alg, x)
   -> let hashlazy
            :: Hash.HashAlgorithm a
            => a -> L.ByteString -> Hash.Digest a
          hashlazy _ l = Hash.hashlazy l
       in pure . Bytes.fromArray . hashlazy alg $ serializeValueLazy x

  declareForeign Untracked "crypto.hmac" crypto'hmac . mkForeign
    $ \(HashAlgorithm _ alg, key, x)
   -> let hmac
            :: Hash.HashAlgorithm a => a -> L.ByteString -> HMAC.HMAC a
          hmac _ s
            = HMAC.finalize
            . HMAC.updates
                (HMAC.initialize $ Bytes.toArray @BA.Bytes key)
            $ L.toChunks s
      in pure . Bytes.fromArray . hmac alg $ serializeValueLazy x


  let
    catchAll :: (MonadCatch m, MonadIO m, NFData a) => m a -> m (Either Util.Text.Text a)
    catchAll e = do
      e <- Exception.tryAnyDeep e
      pure $ case e of
        Left se -> Left (Util.Text.pack (show se))
        Right a -> Right a

  declareForeign Untracked "Bytes.zlib.compress" boxDirect . mkForeign $ pure . Bytes.zlibCompress
  declareForeign Untracked "Bytes.gzip.compress" boxDirect . mkForeign $ pure . Bytes.gzipCompress
  declareForeign Untracked "Bytes.zlib.decompress" boxToEBoxBox . mkForeign $ \bs ->
    catchAll (pure (Bytes.zlibDecompress bs))
  declareForeign Untracked "Bytes.gzip.decompress" boxToEBoxBox . mkForeign $ \bs ->
    catchAll (pure (Bytes.gzipDecompress bs))

  declareForeign Untracked "Bytes.toBase16" boxDirect . mkForeign $ pure . Bytes.toBase16
  declareForeign Untracked "Bytes.toBase32" boxDirect . mkForeign $ pure . Bytes.toBase32
  declareForeign Untracked "Bytes.toBase64" boxDirect . mkForeign $ pure . Bytes.toBase64
  declareForeign Untracked "Bytes.toBase64UrlUnpadded" boxDirect . mkForeign $ pure . Bytes.toBase64UrlUnpadded

  declareForeign Untracked "Bytes.fromBase16" boxToEBoxBox . mkForeign $
    pure . mapLeft Util.Text.fromText . Bytes.fromBase16
  declareForeign Untracked "Bytes.fromBase32" boxToEBoxBox . mkForeign $
    pure . mapLeft Util.Text.fromText . Bytes.fromBase32
  declareForeign Untracked "Bytes.fromBase64" boxToEBoxBox . mkForeign $
    pure . mapLeft Util.Text.fromText . Bytes.fromBase64
  declareForeign Untracked "Bytes.fromBase64UrlUnpadded" boxDirect . mkForeign $
    pure . mapLeft Util.Text.fromText . Bytes.fromBase64UrlUnpadded

  declareForeign Untracked "Bytes.decodeNat64be" boxToMaybeTup . mkForeign $ pure . Bytes.decodeNat64be
  declareForeign Untracked "Bytes.decodeNat64le" boxToMaybeTup . mkForeign $ pure . Bytes.decodeNat64le
  declareForeign Untracked "Bytes.decodeNat32be" boxToMaybeTup . mkForeign $ pure . Bytes.decodeNat32be
  declareForeign Untracked "Bytes.decodeNat32le" boxToMaybeTup . mkForeign $ pure . Bytes.decodeNat32le
  declareForeign Untracked "Bytes.decodeNat16be" boxToMaybeTup . mkForeign $ pure . Bytes.decodeNat16be
  declareForeign Untracked "Bytes.decodeNat16le" boxToMaybeTup . mkForeign $ pure . Bytes.decodeNat16le

  declareForeign Untracked "Bytes.encodeNat64be" (wordDirect Ty.natRef) . mkForeign $ pure . Bytes.encodeNat64be
  declareForeign Untracked "Bytes.encodeNat64le" (wordDirect Ty.natRef) . mkForeign $ pure . Bytes.encodeNat64le
  declareForeign Untracked "Bytes.encodeNat32be" (wordDirect Ty.natRef) . mkForeign $ pure . Bytes.encodeNat32be
  declareForeign Untracked "Bytes.encodeNat32le" (wordDirect Ty.natRef) . mkForeign $ pure . Bytes.encodeNat32le
  declareForeign Untracked "Bytes.encodeNat16be" (wordDirect Ty.natRef) . mkForeign $ pure . Bytes.encodeNat16be
  declareForeign Untracked "Bytes.encodeNat16le" (wordDirect Ty.natRef) . mkForeign $ pure . Bytes.encodeNat16le

hostPreference :: Maybe Util.Text.Text -> SYS.HostPreference
hostPreference Nothing = SYS.HostAny
hostPreference (Just host) = SYS.Host $ Util.Text.unpack host

typeReferences :: [(Reference, Word64)]
typeReferences = zip rs [1..]
  where
  rs = [ r | (_,r) <- Ty.builtinTypes ]
    ++ [ DerivedId i | (_,i,_) <- Ty.builtinDataDecls ]
    ++ [ DerivedId i | (_,i,_) <- Ty.builtinEffectDecls ]

foreignDeclResults
  :: (Word64, [(Data.Text.Text, (Sandbox, SuperNormal Symbol))], EnumMap Word64 ForeignFunc)
foreignDeclResults = execState declareForeigns (0, [], mempty)

foreignWrappers :: [(Data.Text.Text, (Sandbox, SuperNormal Symbol))]
foreignWrappers | (_, l, _) <- foreignDeclResults = reverse l

numberedTermLookup :: EnumMap Word64 (SuperNormal Symbol)
numberedTermLookup
  = mapFromList . zip [1..] . Map.elems . fmap snd $ builtinLookup

builtinTermNumbering :: Map Reference Word64
builtinTermNumbering
  = Map.fromList (zip (Map.keys $ builtinLookup) [1..])

builtinTermBackref :: EnumMap Word64 Reference
builtinTermBackref
  = mapFromList . zip [1..] . Map.keys $ builtinLookup

builtinTypeNumbering :: Map Reference Word64
builtinTypeNumbering = Map.fromList typeReferences

builtinTypeBackref :: EnumMap Word64 Reference
builtinTypeBackref = mapFromList $ swap <$> typeReferences
  where swap (x, y) = (y, x)

builtinForeigns :: EnumMap Word64 ForeignFunc
builtinForeigns | (_, _, m) <- foreignDeclResults = m

-- Bootstrapping for sandbox check. The eventual map will be one with
-- associations `r -> s` where `s` is all the 'sensitive' base
-- functions that `r` calls.
baseSandboxInfo :: Map Reference (Set Reference)
baseSandboxInfo
  = Map.fromList $
  [ (r, Set.singleton r)
  | (r, (sb, _)) <- Map.toList builtinLookup
  , sb == Tracked
  ]

unsafeSTMToIO :: STM.STM a -> IO a
unsafeSTMToIO (STM.STM m) = IO m
