module Unison.Runtime.ANF.POp where

import Data.Map.Strict hiding (map)
import Data.Word

-- Note: Enum/Bounded instances should only be used for things like
-- getting a list of all ops. Using auto-generated numberings for
-- serialization, for instance, could cause observable changes to
-- formats that we want to control and version.
data POp
  = -- Int
    ADDI -- +
  | SUBI -- -
  | MULI
  | DIVI -- /
  | SGNI -- sgn
  | NEGI -- neg
  | MODI -- mod
  | POWI -- pow
  | SHLI -- shiftl
  | SHRI -- shiftr
  | ANDI -- and
  | IORI -- or
  | XORI -- xor
  | COMI -- complement
  | INCI -- inc
  | DECI -- dec
  | LEQI -- <=
  | LESI -- <
  | EQLI -- ==
  | NEQI -- !=
  | TRNC -- truncate0
  -- Nat
  | ADDN -- +
  | SUBN -- -
  | DRPN -- drop
  | MULN
  | DIVN -- /
  | MODN -- mod
  | TZRO -- trailingZeros
  | LZRO -- leadingZeros
  | POPC -- popCount
  | POWN -- pow
  | SHLN -- shiftl
  | SHRN -- shiftr
  | ANDN -- and
  | IORN -- or
  | XORN -- xor
  | COMN -- complement
  | INCN -- inc
  | DECN -- dec
  | LEQN -- <=
  | LESN -- <
  | EQLN -- ==
  | NEQN -- !=
  -- Float
  | ADDF -- +
  | SUBF -- -
  | MULF
  | DIVF -- /
  | MINF -- min
  | MAXF -- max
  | LEQF -- <=
  | LESF -- <
  | EQLF -- ==
  | NEQF -- !=
  | POWF -- pow
  | EXPF -- exp
  | SQRT -- sqrt
  | LOGF -- log
  | LOGB -- logBase
  | ABSF -- abs
  | CEIL -- ceil
  | FLOR -- floor
  | TRNF -- truncate
  | RNDF -- round
  -- Trig
  | COSF -- cos
  | ACOS -- acos
  | COSH -- cosh
  | ACSH -- acosh
  | SINF -- sin
  | ASIN -- asin
  | SINH -- sinh
  | ASNH -- asinh
  | TANF -- tan
  | ATAN -- atan
  | TANH -- tanh
  | ATNH -- atanh
  | ATN2 -- atan2
  -- Text
  | CATT -- ++
  | TAKT -- take
  | DRPT -- drop
  | SIZT -- size
  | IXOT -- indexOf
  | UCNS -- uncons
  | USNC -- unsnoc
  | EQLT -- ==
  | LEQT -- <=
  | PAKT -- pack
  | UPKT -- unpack
  -- Sequence
  | CATS -- ++
  | TAKS -- take
  | DRPS -- drop
  | SIZS -- size
  | CONS -- cons
  | SNOC -- snoc
  | IDXS -- at
  | BLDS -- build
  | VWLS -- viewl
  | VWRS -- viewr
  | SPLL -- splitl
  | SPLR -- splitr
  -- Bytes
  | PAKB -- pack
  | UPKB -- unpack
  | TAKB -- take
  | DRPB -- drop
  | IXOB -- indexOf
  | IDXB -- index
  | SIZB -- size
  | FLTB -- flatten
  | CATB -- append
  -- Conversion
  | ITOF -- intToFloat
  | NTOF -- natToFloat
  | ITOT -- intToText
  | NTOT -- natToText
  | TTOI -- textToInt
  | TTON -- textToNat
  | TTOF -- textToFloat
  | FTOT -- floatToText
  | CAST -- runtime type cast for unboxed values.
  | -- Concurrency
    FORK -- fork
  | -- Universal operations
    EQLU -- ==
  | CMPU -- compare
  | LEQU -- <=
  | LESU -- <
  | EROR -- error
  | -- Code
    MISS -- isMissing
  | CACH -- cache_
  | LKUP -- lookup
  | LOAD -- load
  | CVLD -- validate
  | SDBX -- sandbox
  | VALU -- value
  | TLTT -- Term.Link.toText
  -- Debug
  | PRNT -- print
  | INFO -- info
  | TRCE -- trace
  | DBTX -- debugText
  | -- STM
    ATOM -- atomically
  | TFRC -- try force
  | SDBL -- sandbox link list
  | SDBV -- sandbox check for Values
  -- Refs
  | REFN -- Ref.new
  | REFR -- Ref.read
  | REFW -- Ref.write
  | RCAS -- Ref.cas
  | RRFC -- Ref.readForCas
  | TIKR -- Ref.Ticket.read
  -- Bools
  | NOTB -- not
  | ANDB -- and
  | IORB -- or
  -- low level
  | KEEP -- keepAlive
  | FGNN -- foreign pointer new
  | FGNF -- foreign pointer add finalizer
  -- meta
  | MDCM -- Meta.decompile
  | MTYC -- Meta.typecheck
  | MEVL -- Meta.eval
  | MLOD -- Meta.load
  | MSTR -- Meta.store
  | MDDS -- Meta.dataDeclShape
  | MLNR -- Meta.linkRef
  | MATM -- Meta.alias.term
  deriving (Show, Eq, Ord, Enum, Bounded)

pOpCode :: POp -> Word16
pOpCode op = case op of
  ADDI -> 0
  SUBI -> 1
  MULI -> 2
  DIVI -> 3
  SGNI -> 4
  NEGI -> 5
  MODI -> 6
  POWI -> 7
  SHLI -> 8
  SHRI -> 9
  INCI -> 10
  DECI -> 11
  LEQI -> 12
  EQLI -> 13
  ADDN -> 14
  SUBN -> 15
  MULN -> 16
  DIVN -> 17
  MODN -> 18
  TZRO -> 19
  LZRO -> 20
  POWN -> 21
  SHLN -> 22
  SHRN -> 23
  ANDN -> 24
  IORN -> 25
  XORN -> 26
  COMN -> 27
  INCN -> 28
  DECN -> 29
  LEQN -> 30
  EQLN -> 31
  ADDF -> 32
  SUBF -> 33
  MULF -> 34
  DIVF -> 35
  MINF -> 36
  MAXF -> 37
  LEQF -> 38
  EQLF -> 39
  POWF -> 40
  EXPF -> 41
  SQRT -> 42
  LOGF -> 43
  LOGB -> 44
  ABSF -> 45
  CEIL -> 46
  FLOR -> 47
  TRNF -> 48
  RNDF -> 49
  COSF -> 50
  ACOS -> 51
  COSH -> 52
  ACSH -> 53
  SINF -> 54
  ASIN -> 55
  SINH -> 56
  ASNH -> 57
  TANF -> 58
  ATAN -> 59
  TANH -> 60
  ATNH -> 61
  ATN2 -> 62
  CATT -> 63
  TAKT -> 64
  DRPT -> 65
  SIZT -> 66
  UCNS -> 67
  USNC -> 68
  EQLT -> 69
  LEQT -> 70
  PAKT -> 71
  UPKT -> 72
  CATS -> 73
  TAKS -> 74
  DRPS -> 75
  SIZS -> 76
  CONS -> 77
  SNOC -> 78
  IDXS -> 79
  BLDS -> 80
  VWLS -> 81
  VWRS -> 82
  SPLL -> 83
  SPLR -> 84
  PAKB -> 85
  UPKB -> 86
  TAKB -> 87
  DRPB -> 88
  IDXB -> 89
  SIZB -> 90
  FLTB -> 91
  CATB -> 92
  ITOF -> 93
  NTOF -> 94
  ITOT -> 95
  NTOT -> 96
  TTOI -> 97
  TTON -> 98
  TTOF -> 99
  FTOT -> 100
  FORK -> 101
  EQLU -> 102
  CMPU -> 103
  EROR -> 104
  PRNT -> 105
  INFO -> 106
  POPC -> 107
  MISS -> 108
  CACH -> 109
  LKUP -> 110
  LOAD -> 111
  CVLD -> 112
  SDBX -> 113
  VALU -> 114
  TLTT -> 115
  TRCE -> 116
  ATOM -> 117
  TFRC -> 118
  DBTX -> 119
  IXOT -> 120
  IXOB -> 121
  SDBL -> 122
  SDBV -> 123
  CAST -> 124
  ANDI -> 125
  IORI -> 126
  XORI -> 127
  COMI -> 128
  DRPN -> 129
  TRNC -> 130
  REFN -> 131
  REFR -> 132
  REFW -> 133
  RCAS -> 134
  RRFC -> 135
  TIKR -> 136
  LESI -> 137
  NEQI -> 138
  LESN -> 139
  NEQN -> 140
  LESF -> 141
  NEQF -> 142
  LEQU -> 143
  LESU -> 144
  NOTB -> 145
  ANDB -> 146
  IORB -> 147
  KEEP -> 148
  FGNN -> 149
  FGNF -> 150
  MDCM -> 151
  MTYC -> 152
  MEVL -> 153
  MLOD -> 154
  MSTR -> 155
  MDDS -> 156
  MLNR -> 157
  MATM -> 158

pOpAssoc :: [(POp, Word16)]
pOpAssoc = map (\op -> (op, pOpCode op)) [minBound .. maxBound]

pop2word :: Map POp Word16
pop2word = fromList pOpAssoc

word2pop :: Map Word16 POp
word2pop = fromList $ swap <$> pOpAssoc
  where
    swap (x, y) = (y, x)
