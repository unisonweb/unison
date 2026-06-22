module Unison.Runtime.TypeTags
  ( Tag (..),
    RTag (..),
    CTag (..),
    PackedTag (..),
    packTags,
    unpackTags,
    maskTags,
    typeTag,
    anyTag,
    floatTag,
    natTag,
    intTag,
    charTag,
    unitTag,
    leftTag,
    rightTag,
    noneTag,
    someTag,
    falseTag,
    trueTag,
    pairTag,
    failureTag,
    noBufTag,
    lineBufTag,
    blockBufTag,
    sizedBlockBufTag,
    readModeTag,
    writeModeTag,
    appendModeTag,
    readWriteModeTag,
    seekAbsoluteTag,
    seekRelativeTag,
    seekEndTag,
    exceptionTag,
    exceptionRaiseTag,
    stdInTag,
    stdOutTag,
    stdErrTag,
    pureEffectTag,
    seqViewElemTag,
    seqViewEmptyTag,
    mapTipTag,
    mapBinTag,
    setWrapTag,
    jsonNullTag,
    jsonBoolTag,
    jsonObjTag,
    jsonNumTag,
    jsonTextTag,
    jsonArrTag,
    jsonParseErrorTag,
    avroNullTag,
    avroRecordTag,
    avroBytesTag,
    avroFixedTag,
    avroArrayTag,
    avroMapTag,
    avroUnionTag,
    avroEnumTag,
    avroStringTag,
    avroIntTag,
    avroLongTag,
    avroFloatTag,
    avroDoubleTag,
    avroBooleanTag,
    avroReadSchemaNullTag,
    avroReadSchemaBooleanTag,
    avroReadSchemaIntTag,
    avroReadSchemaLongTag,
    avroReadSchemaFloatTag,
    avroReadSchemaDoubleTag,
    avroReadSchemaBytesTag,
    avroReadSchemaStringTag,
    avroReadSchemaArrayTag,
    avroReadSchemaMapTag,
    avroReadSchemaRecordTag,
    avroReadSchemaEnumTag,
    avroReadSchemaUnionTag,
    avroReadSchemaFixedTag,
    avroReadSchemaFreeUnionTag,
    avroReadSchemaNamedTypeTag,
    avroTypeNameTag,
    avroLogicalIntDateTag,
    avroLogicalIntTimeTag,
    avroLogicalIntDecimalTag,
    avroReadFloatFromInt32Tag,
    avroReadFloatFromInt64Tag,
    avroReadFloatTag,
    avroLogicalLongTimeMicrosTag,
    avroLogicalLongTimestampMillisTag,
    avroLogicalLongTimestampMicrosTag,
    avroLogicalLongLocalTimestampMillisTag,
    avroLogicalLongLocalTimestampMicrosTag,
    avroLogicalLongDecimalTag,
    avroLogicalFixedDurationTag,
    avroLogicalFixedDecimalTag,
    avroLogicalStringUUIDTag,
    avroReadDoubleFromInt32Tag,
    avroReadDoubleFromInt64Tag,
    avroReadDoubleFromFloatTag,
    avroReadDoubleTag,
    avroDecimalTag,
    avroReadLongInt32Tag,
    avroReadLongTag,
    avroLogicalBytesDecimalTag,
    avroLogicalStringUuidTag,
    avroReadFieldTag,
    avroFieldTag,
    avroFieldStatusAsIsTag,
    avroFieldStatusDefaultedTag,
    avroFieldStatusIgnoredTag,
    avroOrderAscendingTag,
    avroOrderDescendingTag,
    avroOrderIgnoreTag,
    avroDefaultValueIntTag,
    avroDefaultValueLongTag,
    avroDefaultValueFloatTag,
    avroDefaultValueDoubleTag,
    avroDefaultValueBytesTag,
    avroDefaultValueStringTag,
    avroDefaultValueArrayTag,
    avroDefaultValueMapTag,
    avroDefaultValueRecordTag,
    avroDefaultValueUnionTag,
    avroDefaultValueFixedTag,
    avroDefaultValueEnumTag,
    avroDefaultValueNullTag,
    avroDefaultValueBooleanTag,
    avroSchemaNullTag,
    avroSchemaBooleanTag,
    avroSchemaIntTag,
    avroSchemaLongTag,
    avroSchemaFloatTag,
    avroSchemaDoubleTag,
    avroSchemaBytesTag,
    avroSchemaStringTag,
    avroSchemaArrayTag,
    avroSchemaMapTag,
    avroSchemaNamedTypeTag,
    avroSchemaRecordTag,
    avroSchemaEnumTag,
    avroSchemaUnionTag,
    avroSchemaFixedTag,
    avroReadRecordTag,
    avroFixedTypeTag,
    avroRecordTypeTag,
    avroEnumTypeTag,
    -- meta tree
    metaNameNameTag,
    metaHashHashTag,
    metaReferenceBuiltinTag,
    metaReferenceDerivedTag,
    metaConstructorReferenceTag,
    metaReferentRefRefTag,
    metaReferentRefConTag,
    metaLitNatTag,
    metaLitIntTag,
    metaLitTextTag,
    metaLitCharTag,
    metaLitFloatTag,
    metaLitBooleanTag,
    metaLitBytesTag,
    metaSeqOpPConsTag,
    metaSeqOpPSnocTag,
    metaSeqOpPConcatTag,
    metaPatternPUnboundTag,
    metaPatternPVarTag,
    metaPatternPBooleanTag,
    metaPatternPIntTag,
    metaPatternPNatTag,
    metaPatternPFloatTag,
    metaPatternPTextTag,
    metaPatternPCharTag,
    metaPatternPBytesTag,
    metaPatternPConstructorTag,
    metaPatternPAsTag,
    metaPatternPEffectPureTag,
    metaPatternPEffectBindTag,
    metaPatternPSequenceLiteralTag,
    metaPatternPSequenceOpTag,
    metaKindKStarTag,
    metaKindKArrowTag,
    metaMatchCaseTag,
    metaAbtVarTag,
    metaAbtAbsTag,
    metaAbtCycleTag,
    metaAbtTmTag,
    metaTermTermTag,
    metaTermFAppTag,
    metaTermFLamTag,
    metaTermFLetTag,
    metaTermFLetRecTag,
    metaTermFIfTag,
    metaTermFMatchTag,
    metaTermFHandleTag,
    metaTermFAnnTag,
    metaTermFRefTag,
    metaTermFConstructorTag,
    metaTermFRequestTag,
    metaTermFLitTag,
    metaTermFListTag,
    metaTermFTermLinkTag,
    metaTermFTypeLinkTag,
    metaTypeFArrowTag,
    metaTypeFImplicitArrowTag,
    metaTypeFAppTag,
    metaTypeFEffectTag,
    metaTypeFEffectsTag,
    metaTypeFForallTag,
    metaTypeFIntroOuterTag,
    metaTypeFRefTag,
    metaTypeFAnnTag,
  )
where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.List hiding (and, or)
import Data.Map qualified as Map
import U.Codebase.Reference (Reference)
import Unison.Builtin.Decls qualified as Ty
import Unison.Prelude
import Unison.Runtime.Builtin.Types (builtinTypeNumbering)
import Unison.Runtime.InternalError (internalBug)
import Unison.Runtime.MetaSource qualified as Meta
import Unison.Type qualified as Ty
import Unison.Util.EnumContainers as EC
import Prelude hiding (abs, and, or, seq)
import Prelude qualified

-- Types representing components that will go into the runtime tag of
-- a data type value. RTags correspond to references, while CTags
-- correspond to constructors.
newtype RTag = RTag Word64
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (EC.EnumKey)

newtype CTag = CTag Word16
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (EC.EnumKey)

-- | A combined tag, which is a packed representation of an RTag and a CTag
newtype PackedTag = PackedTag Word64
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (EC.EnumKey)

class Tag t where rawTag :: t -> Word64

instance Tag RTag where
  rawTag (RTag w) = w
  {-# INLINE rawTag #-}

instance Tag CTag where
  rawTag (CTag w) = fromIntegral w
  {-# INLINE rawTag #-}

packTags :: RTag -> CTag -> PackedTag
packTags (RTag rt) (CTag ct) = PackedTag (ri .|. ci)
  where
    ri = rt `shiftL` 16
    ci = fromIntegral ct

unpackTags :: PackedTag -> (RTag, CTag)
unpackTags (PackedTag w) =
  (RTag $ w `shiftR` 16, CTag . fromIntegral $ w .&. 0xFFFF)
{-# INLINE unpackTags #-}

-- Masks a packed tag to extract just the constructor tag portion
maskTags :: PackedTag -> Word64
maskTags (PackedTag w) = (w .&. 0xFFFF)

-- Extract the type tag portion of some packed tags
typeTag :: PackedTag -> Word64
typeTag (PackedTag w) = w `shiftR` 16
{-# INLINE typeTag #-}

ensureRTag :: (Ord n, Show n, Num n) => String -> n -> r -> r
ensureRTag s n x
  | n > 0xFFFFFFFFFFFF =
      internalBug [] $ s ++ "@RTag: too large: " ++ show n
  | otherwise = x

ensureCTag :: (Ord n, Show n, Num n) => String -> n -> r -> r
ensureCTag s n x
  | n > 0xFFFF =
      internalBug [] $ s ++ "@CTag: too large: " ++ show n
  | otherwise = x

instance Enum RTag where
  toEnum i = ensureRTag "toEnum" i . RTag $ toEnum i
  fromEnum (RTag w) = fromEnum w

instance Enum CTag where
  toEnum i = ensureCTag "toEnum" i . CTag $ toEnum i
  fromEnum (CTag w) = fromEnum w

instance Num RTag where
  fromInteger i = ensureRTag "fromInteger" i . RTag $ fromInteger i
  (+) = internalBug [] "RTag: +"
  (*) = internalBug [] "RTag: *"
  abs = internalBug [] "RTag: abs"
  signum = internalBug [] "RTag: signum"
  negate = internalBug [] "RTag: negate"

instance Num CTag where
  fromInteger i = ensureCTag "fromInteger" i . CTag $ fromInteger i
  (+) = internalBug [] "CTag: +"
  (*) = internalBug [] "CTag: *"
  abs = internalBug [] "CTag: abs"
  signum = internalBug [] "CTag: signum"
  negate = internalBug [] "CTag: negate"

floatTag :: PackedTag
floatTag = mkSimpleTag "floatTag" Ty.floatRef

natTag :: PackedTag
natTag = mkSimpleTag "natTag" Ty.natRef

intTag :: PackedTag
intTag = mkSimpleTag "intTag" Ty.intRef

charTag :: PackedTag
charTag = mkSimpleTag "charTag" Ty.charRef

unitTag :: PackedTag
unitTag = mkSimpleTag "unitTag" Ty.unitRef

falseTag :: PackedTag
falseTag = mkEnumTag "falseTag" Ty.booleanRef 0

trueTag :: PackedTag
trueTag = mkEnumTag "trueTag" Ty.booleanRef 1

anyTag :: PackedTag
anyTag = mkEnumTag "anyTag" Ty.anyRef 0

failureTag :: PackedTag
failureTag = mkEnumTag "failureTag" Ty.failureRef 0

noneTag, someTag :: PackedTag
(noneTag, someTag)
  | [nt, st] <-
      mkTags
        "optional tags"
        Ty.optionalRef
        [Ty.noneId, Ty.someId] =
      (nt, st)
  | otherwise = internalBug [] "missing optional tags"

leftTag, rightTag :: PackedTag
(leftTag, rightTag)
  | [lt, rt] <-
      mkTags
        "either tags"
        Ty.eitherRef
        [Ty.eitherLeftId, Ty.eitherRightId] =
      (lt, rt)
  | otherwise = internalBug [] "missing either tags"

noBufTag, lineBufTag, blockBufTag, sizedBlockBufTag :: PackedTag
(noBufTag, lineBufTag, blockBufTag, sizedBlockBufTag)
  | [nt, lt, bt, st] <-
      mkTags
        "buffer mode tags"
        Ty.bufferModeRef
        [ Ty.bufferModeNoBufferingId,
          Ty.bufferModeLineBufferingId,
          Ty.bufferModeBlockBufferingId,
          Ty.bufferModeSizedBlockBufferingId
        ] =
      (nt, lt, bt, st)
  | otherwise = internalBug [] "missing buffer mode tags"

readModeTag, writeModeTag, appendModeTag, readWriteModeTag :: PackedTag
(readModeTag, writeModeTag, appendModeTag, readWriteModeTag)
  | [rt, wt, at, rwt] <-
      mkTags
        "file mode tags"
        Ty.fileModeRef
        [ Ty.fileModeReadId,
          Ty.fileModeWriteId,
          Ty.fileModeAppendId,
          Ty.fileModeReadWriteId
        ] =
      (rt, wt, at, rwt)
  | otherwise = internalBug [] "missing file mode tags"

seekAbsoluteTag, seekRelativeTag, seekEndTag :: PackedTag
(seekAbsoluteTag, seekRelativeTag, seekEndTag)
  | [at, rt, et] <-
      mkTags
        "seek mode tags"
        Ty.seekModeRef
        [ Ty.seekModeAbsoluteId,
          Ty.seekModeRelativeId,
          Ty.seekModeEndId
        ] =
      (at, rt, et)
  | otherwise = internalBug [] "missing seek mode tags"

stdInTag, stdOutTag, stdErrTag :: PackedTag
(stdInTag, stdOutTag, stdErrTag)
  | [it, ot, et] <-
      mkTags
        "standard handle tags"
        Ty.stdHandleRef
        [ Ty.stdInId,
          Ty.stdOutId,
          Ty.stdErrId
        ] =
      (it, ot, et)
  | otherwise = internalBug [] "missing standard handle tags"

exceptionTag :: Word64
exceptionRaiseTag :: PackedTag
(exceptionTag, exceptionRaiseTag)
  | Just n <- Map.lookup Ty.exceptionRef builtinTypeNumbering,
    et <- toEnum $ fromIntegral n,
    rt <- toEnum $ fromIntegral Ty.exceptionRaiseId =
      (n, packTags et rt)
  | otherwise = internalBug [] "missing Exception tag"

pairTag :: PackedTag
pairTag
  | Just n <- Map.lookup Ty.pairRef builtinTypeNumbering,
    pt <- toEnum (fromIntegral n) =
      packTags pt 0
  | otherwise = internalBug [] "missing pairTag"

seqViewEmptyTag, seqViewElemTag :: PackedTag
(seqViewEmptyTag, seqViewElemTag)
  | [emt, elt] <-
      mkTags
        "seq view tags"
        Ty.seqViewRef
        [ Ty.seqViewEmpty,
          Ty.seqViewElem
        ] =
      (emt, elt)
  | otherwise = internalBug [] "missing seq view tags"

mapTipTag, mapBinTag :: PackedTag
(mapTipTag, mapBinTag)
  | [mtt, mbt] <-
      mkTags
        "map tags"
        Ty.mapRef
        [ Ty.mapTip,
          Ty.mapBin
        ] =
      (mtt, mbt)
  | otherwise = internalBug [] "missing map tags"

setWrapTag :: PackedTag
setWrapTag
  | [swt] <-
      mkTags
        "set tag"
        Ty.setRef
        [Ty.setWrap] =
      swt
  | otherwise = internalBug [] "missing set tag"

jsonNullTag, jsonBoolTag, jsonObjTag, jsonNumTag :: PackedTag
jsonTextTag, jsonArrTag :: PackedTag
(jsonNullTag, jsonBoolTag, jsonObjTag, jsonNumTag, jsonTextTag, jsonArrTag)
  | [nlt, bot, obt, nut, txt, art] <-
      mkTags
        "json tags"
        Ty.jsonRef
        [ Ty.jsonNull,
          Ty.jsonBool,
          Ty.jsonObj,
          Ty.jsonNum,
          Ty.jsonText,
          Ty.jsonArr
        ] =
      (nlt, bot, obt, nut, txt, art)
  | otherwise = internalBug [] "missing json tags"

jsonParseErrorTag :: PackedTag
jsonParseErrorTag
  | [pet] <-
      mkTags
        "json parse error tag"
        Ty.parseErrorRef
        [Ty.jsonParseError] =
      pet
  | otherwise = internalBug [] "missing json parse error tag"

avroNullTag, avroRecordTag, avroBytesTag, avroFixedTag, avroArrayTag, avroMapTag, avroUnionTag, avroEnumTag, avroStringTag, avroIntTag, avroLongTag, avroFloatTag, avroDoubleTag, avroBooleanTag :: PackedTag
(avroNullTag, avroRecordTag, avroBytesTag, avroFixedTag, avroArrayTag, avroMapTag, avroUnionTag, avroEnumTag, avroStringTag, avroIntTag, avroLongTag, avroFloatTag, avroDoubleTag, avroBooleanTag)
  | [nlt, rct, bct, fct, act, mct, ut, et, st, it, lt, ft, dt, bt] <-
      mkTags
        "avro tags"
        Ty.avroRef
        [Ty.avroNull, Ty.avroRecord, Ty.avroBytes, Ty.avroFixed, Ty.avroArray, Ty.avroMap, Ty.avroUnion, Ty.avroEnum, Ty.avroString, Ty.avroInt, Ty.avroLong, Ty.avroFloat, Ty.avroDouble, Ty.avroBoolean] =
      (nlt, rct, bct, fct, act, mct, ut, et, st, it, lt, ft, dt, bt)
  | otherwise = error "internal error: avro tags"

avroFixedTypeTag :: PackedTag
avroFixedTypeTag = mkSimpleTag "avroFixedTypeTag" Ty.avroFixedRef

avroRecordTypeTag :: PackedTag
avroRecordTypeTag = mkSimpleTag "avroRecordTypeTag" Ty.avroRecordRef

avroEnumTypeTag :: PackedTag
avroEnumTypeTag = mkSimpleTag "avroEnumTypeTag" Ty.avroEnumRef

avroDefaultValueIntTag, avroDefaultValueLongTag, avroDefaultValueFloatTag, avroDefaultValueDoubleTag, avroDefaultValueBytesTag, avroDefaultValueStringTag, avroDefaultValueArrayTag, avroDefaultValueMapTag, avroDefaultValueRecordTag, avroDefaultValueUnionTag, avroDefaultValueFixedTag, avroDefaultValueEnumTag, avroDefaultValueNullTag, avroDefaultValueBooleanTag :: PackedTag
(avroDefaultValueIntTag, avroDefaultValueLongTag, avroDefaultValueFloatTag, avroDefaultValueDoubleTag, avroDefaultValueBytesTag, avroDefaultValueStringTag, avroDefaultValueArrayTag, avroDefaultValueMapTag, avroDefaultValueRecordTag, avroDefaultValueUnionTag, avroDefaultValueFixedTag, avroDefaultValueEnumTag, avroDefaultValueNullTag, avroDefaultValueBooleanTag)
  | [it, lt, ft, dt, bct, st, art, mct, rct, ut, fct, ent, nct, bot] <-
      mkTags
        "avro default value tags"
        Ty.avroDefaultValueRef
        [Ty.avroDefaultValueInt32, Ty.avroDefaultValueInt64, Ty.avroDefaultValueFloat, Ty.avroDefaultValueDouble, Ty.avroDefaultValueBytes, Ty.avroDefaultValueString, Ty.avroDefaultValueArray, Ty.avroDefaultValueMap, Ty.avroDefaultValueRecord, Ty.avroDefaultValueUnion, Ty.avroDefaultValueFixed, Ty.avroDefaultValueEnum, Ty.avroDefaultValueNull, Ty.avroDefaultValueBoolean] =
      (it, lt, ft, dt, bct, st, art, mct, rct, ut, fct, ent, nct, bot)
  | otherwise = error "internal error: avro default value tags"

avroReadSchemaNullTag, avroReadSchemaBooleanTag, avroReadSchemaStringTag, avroReadSchemaFloatTag, avroReadSchemaFixedTag, avroReadSchemaDoubleTag, avroReadSchemaBytesTag, avroReadSchemaNamedTypeTag, avroReadSchemaIntTag, avroReadSchemaLongTag, avroReadSchemaMapTag, avroReadSchemaRecordTag, avroReadSchemaFreeUnionTag, avroReadSchemaEnumTag, avroReadSchemaUnionTag, avroReadSchemaArrayTag :: PackedTag
(avroReadSchemaNullTag, avroReadSchemaBooleanTag, avroReadSchemaStringTag, avroReadSchemaFloatTag, avroReadSchemaFixedTag, avroReadSchemaDoubleTag, avroReadSchemaBytesTag, avroReadSchemaNamedTypeTag, avroReadSchemaIntTag, avroReadSchemaLongTag, avroReadSchemaMapTag, avroReadSchemaRecordTag, avroReadSchemaFreeUnionTag, avroReadSchemaEnumTag, avroReadSchemaUnionTag, avroReadSchemaArrayTag)
  | [nullTag, booleanTag, stringTag, floatTag, fixedTag, doubleTag, bytesTag, namedTypeTag, intTag, longTag, mapTag, recordTag, freeUnionTag, enumTag, unionTag, arrayTag] <-
      mkTags
        "avro read schema tags"
        Ty.avroReadSchemaRef
        [Ty.avroReadSchemaNull, Ty.avroReadSchemaBoolean, Ty.avroReadSchemaString, Ty.avroReadSchemaFloat, Ty.avroReadSchemaFixed, Ty.avroReadSchemaDouble, Ty.avroReadSchemaBytes, Ty.avroReadSchemaNamedType, Ty.avroReadSchemaInt, Ty.avroReadSchemaLong, Ty.avroReadSchemaMap, Ty.avroReadSchemaRecord, Ty.avroReadSchemaFreeUnion, Ty.avroReadSchemaEnum, Ty.avroReadSchemaUnion, Ty.avroReadSchemaArray] =
      (nullTag, booleanTag, stringTag, floatTag, fixedTag, doubleTag, bytesTag, namedTypeTag, intTag, longTag, mapTag, recordTag, freeUnionTag, enumTag, unionTag, arrayTag)
  | otherwise = error "internal error: avro readschema tags"

avroSchemaNullTag, avroSchemaBooleanTag, avroSchemaIntTag, avroSchemaLongTag, avroSchemaFloatTag, avroSchemaDoubleTag, avroSchemaBytesTag, avroSchemaStringTag, avroSchemaArrayTag, avroSchemaMapTag, avroSchemaNamedTypeTag, avroSchemaRecordTag, avroSchemaEnumTag, avroSchemaUnionTag, avroSchemaFixedTag :: PackedTag
(avroSchemaNullTag, avroSchemaBooleanTag, avroSchemaIntTag, avroSchemaLongTag, avroSchemaFloatTag, avroSchemaDoubleTag, avroSchemaBytesTag, avroSchemaStringTag, avroSchemaArrayTag, avroSchemaMapTag, avroSchemaNamedTypeTag, avroSchemaRecordTag, avroSchemaEnumTag, avroSchemaUnionTag, avroSchemaFixedTag)
  | [nlt, bot, it, lt, ft, dt, bst, st, at, mt, ntt, rct, ent, ut, fxt] <-
      mkTags
        "avro schema tags"
        Ty.avroSchemaRef
        [Ty.avroSchemaNull, Ty.avroSchemaBoolean, Ty.avroSchemaInt, Ty.avroSchemaLong, Ty.avroSchemaFloat, Ty.avroSchemaDouble, Ty.avroSchemaBytes, Ty.avroSchemaString, Ty.avroSchemaArray, Ty.avroSchemaMap, Ty.avroSchemaNamedType, Ty.avroSchemaRecord, Ty.avroSchemaEnum, Ty.avroSchemaUnion, Ty.avroSchemaFixed] =
      (nlt, bot, it, lt, ft, dt, bst, st, at, mt, ntt, rct, ent, ut, fxt)
  | otherwise = error "internal error: avro schema tags"

avroLogicalIntDateTag, avroLogicalIntTimeTag, avroLogicalIntDecimalTag :: PackedTag
(avroLogicalIntDateTag, avroLogicalIntTimeTag, avroLogicalIntDecimalTag)
  | [dt, tt, dect] <-
      mkTags
        "avro logical int tags"
        Ty.avroLogicalIntRef
        [Ty.avroLogicalIntDate, Ty.avroLogicalIntTime, Ty.avroLogicalIntDecimal] =
      (dt, tt, dect)
  | otherwise = error "internal error: avro logical int tags"

avroReadFieldTag :: PackedTag
avroReadFieldTag = mkSimpleTag "avroReadFieldTag" Ty.avroReadFieldRef

avroFieldTag :: PackedTag
avroFieldTag = mkSimpleTag "avroFieldTag" Ty.avroFieldRef

avroReadRecordTag :: PackedTag
avroReadRecordTag = mkSimpleTag "avroReadRecordTag" Ty.avroReadRecordRef

avroLogicalStringUuidTag :: PackedTag
avroLogicalStringUuidTag = mkSimpleTag "avroLogicalStringUuidTag" Ty.avroLogicalStringRef

avroLogicalBytesDecimalTag :: PackedTag
avroLogicalBytesDecimalTag = mkSimpleTag "avroLogicalBytesDecimalTag" Ty.avroLogicalBytesRef

avroLogicalLongTimeMicrosTag, avroLogicalLongTimestampMillisTag, avroLogicalLongTimestampMicrosTag, avroLogicalLongLocalTimestampMillisTag, avroLogicalLongLocalTimestampMicrosTag, avroLogicalLongDecimalTag :: PackedTag
(avroLogicalLongTimeMicrosTag, avroLogicalLongTimestampMillisTag, avroLogicalLongTimestampMicrosTag, avroLogicalLongLocalTimestampMillisTag, avroLogicalLongLocalTimestampMicrosTag, avroLogicalLongDecimalTag)
  | [tmct, tsmst, tsmcst, ltsmst, ltsmcst, decct] <-
      mkTags
        "avro logical long tags"
        Ty.avroLogicalLongRef
        [Ty.avroLogicalLongTimeMicros, Ty.avroLogicalLongTimestampMillis, Ty.avroLogicalLongTimestampMicros, Ty.avroLogicalLongLocalTimestampMillis, Ty.avroLogicalLongLocalTimestampMicros, Ty.avroLogicalLongDecimal] =
      (tmct, tsmst, tsmcst, ltsmst, ltsmcst, decct)
  | otherwise = error "internal error: avro logical long tags"

avroLogicalFixedDurationTag, avroLogicalFixedDecimalTag :: PackedTag
(avroLogicalFixedDurationTag, avroLogicalFixedDecimalTag)
  | [durt, decct] <-
      mkTags
        "avro logical fixed tags"
        Ty.avroLogicalFixedRef
        [Ty.avroLogicalFixedDuration, Ty.avroLogicalFixedDecimal] =
      (durt, decct)
  | otherwise = error "internal error: avro logical fixed tags"

avroLogicalStringUUIDTag :: PackedTag
avroLogicalStringUUIDTag = mkSimpleTag "avroLogicalStringUUIDTag" Ty.avroLogicalStringRef

avroReadFloatFromInt32Tag, avroReadFloatFromInt64Tag, avroReadFloatTag :: PackedTag
(avroReadFloatFromInt32Tag, avroReadFloatFromInt64Tag, avroReadFloatTag)
  | [i32t, i64t, ft] <-
      mkTags
        "avro read float tags"
        Ty.avroReadFloatRef
        [Ty.avroReadFloatInt32, Ty.avroReadFloatInt64, Ty.avroReadFloat] =
      (i32t, i64t, ft)
  | otherwise = error "internal error: avro read float tags"

avroReadDoubleFromInt32Tag, avroReadDoubleFromInt64Tag, avroReadDoubleFromFloatTag, avroReadDoubleTag :: PackedTag
(avroReadDoubleFromInt32Tag, avroReadDoubleFromInt64Tag, avroReadDoubleFromFloatTag, avroReadDoubleTag)
  | [i32t, i64t, ft, dt] <-
      mkTags
        "avro read double tags"
        Ty.avroReadDoubleRef
        [Ty.avroReadDoubleInt32, Ty.avroReadDoubleInt64, Ty.avroReadDoubleFromFloat, Ty.avroReadDouble] =
      (i32t, i64t, ft, dt)
  | otherwise = error "internal error: avro read double tags"

avroReadLongInt32Tag, avroReadLongTag :: PackedTag
(avroReadLongInt32Tag, avroReadLongTag)
  | [i32t, lt] <-
      mkTags
        "avro read long tags"
        Ty.avroReadLongRef
        [Ty.avroReadLongInt32, Ty.avroReadLong] =
      (i32t, lt)
  | otherwise = error "internal error: avro read long tags"

avroTypeNameTag :: PackedTag
avroTypeNameTag = mkSimpleTag "avroTypeNameTag" Ty.avroTypeNameRef

avroDecimalTag :: PackedTag
avroDecimalTag = mkSimpleTag "avroDecimalTag" Ty.avroDecimalRef

avroFieldStatusAsIsTag, avroFieldStatusDefaultedTag, avroFieldStatusIgnoredTag :: PackedTag
(avroFieldStatusAsIsTag, avroFieldStatusDefaultedTag, avroFieldStatusIgnoredTag)
  | [ast, dct, igt] <-
      mkTags
        "avro field status tags"
        Ty.avroFieldStatusRef
        [Ty.avroFieldStatusAsIs, Ty.avroFieldStatusDefaulted, Ty.avroFieldStatusIgnored] =
      (ast, dct, igt)
  | otherwise = error "internal error: avro field status tags"

avroOrderAscendingTag, avroOrderDescendingTag, avroOrderIgnoreTag :: PackedTag
(avroOrderAscendingTag, avroOrderDescendingTag, avroOrderIgnoreTag)
  | [ast, dct, igt] <-
      mkTags
        "avro order tags"
        Ty.avroOrderRef
        [Ty.avroOrderAscending, Ty.avroOrderDescending, Ty.avroOrderIgnore] =
      (ast, dct, igt)
  | otherwise = error "internal error: avro order tags"

-- | A tag we use to represent the 'pure' effect case.
pureEffectTag :: PackedTag
pureEffectTag = PackedTag 0

-- | Construct a tag for a single-constructor builtin type
mkSimpleTag :: String -> Reference -> PackedTag
mkSimpleTag msg r = mkEnumTag msg r 0

mkEnumTag :: String -> Reference -> Int -> PackedTag
mkEnumTag msg r i
  | Just n <- Map.lookup r builtinTypeNumbering,
    rt <- toEnum (fromIntegral n) =
      packTags rt (toEnum i)
  | otherwise = internalBug [] $ "missing enum tag: " <> msg

mkTags :: String -> Reference -> [Word64] -> [PackedTag]
mkTags msg r cs
  | Just n <- Map.lookup r builtinTypeNumbering,
    tt <- toEnum $ fromIntegral n =
      packTags tt . toEnum . fromIntegral <$> cs
  | otherwise = internalBug [] $ "missing tag: " ++ msg

-- =============================================================
-- Meta-tree type tags
--
-- Packed-tag handles for every constructor we construct from the
-- runtime when implementing the @decompile@ builtin and friends.
-- Looked up by name via 'MetaSource.constructorNamed' so they're
-- immune to canonical-order reshuffling of constructors.
-- =============================================================

metaNameNameTag :: PackedTag
metaNameNameTag
  | [t] <- mkTags "meta Name tags" Meta.nameRef [Meta.nameNameId] = t
  | otherwise = internalBug [] "missing meta Name.Name tag"

metaHashHashTag :: PackedTag
metaHashHashTag
  | [t] <- mkTags "meta Hash tags" Meta.hashRef [Meta.hashHashId] = t
  | otherwise = internalBug [] "missing meta Hash.Hash tag"

metaReferenceBuiltinTag, metaReferenceDerivedTag :: PackedTag
(metaReferenceBuiltinTag, metaReferenceDerivedTag)
  | [a, b] <-
      mkTags
        "meta Reference tags"
        Meta.referenceRef
        [Meta.referenceBuiltinId, Meta.referenceDerivedId] =
      (a, b)
  | otherwise = internalBug [] "missing meta Reference tags"

metaConstructorReferenceTag :: PackedTag
metaConstructorReferenceTag
  | [t] <-
      mkTags
        "meta ConstructorReference tags"
        Meta.constructorReferenceRef
        [Meta.constructorReferenceCtorId] =
      t
  | otherwise = internalBug [] "missing meta ConstructorReference tag"

metaReferentRefRefTag, metaReferentRefConTag :: PackedTag
(metaReferentRefRefTag, metaReferentRefConTag)
  | [a, b] <-
      mkTags
        "meta Referent tags"
        Meta.referentRef
        [Meta.referentRefRefId, Meta.referentRefConId] =
      (a, b)
  | otherwise = internalBug [] "missing meta Referent tags"

metaLitNatTag,
  metaLitIntTag,
  metaLitTextTag,
  metaLitCharTag,
  metaLitFloatTag,
  metaLitBooleanTag,
  metaLitBytesTag ::
    PackedTag
( metaLitNatTag,
  metaLitIntTag,
  metaLitTextTag,
  metaLitCharTag,
  metaLitFloatTag,
  metaLitBooleanTag,
  metaLitBytesTag
  )
    | [a, b, c, d, e, f, g] <-
        mkTags
          "meta Literal tags"
          Meta.literalRef
          [ Meta.litNatId,
            Meta.litIntId,
            Meta.litTextId,
            Meta.litCharId,
            Meta.litFloatId,
            Meta.litBooleanId,
            Meta.litBytesId
          ] =
        (a, b, c, d, e, f, g)
    | otherwise = internalBug [] "missing meta Literal tags"

metaSeqOpPConsTag, metaSeqOpPSnocTag, metaSeqOpPConcatTag :: PackedTag
(metaSeqOpPConsTag, metaSeqOpPSnocTag, metaSeqOpPConcatTag)
  | [a, b, c] <-
      mkTags
        "meta SeqOp tags"
        Meta.seqOpRef
        [Meta.seqOpPConsId, Meta.seqOpPSnocId, Meta.seqOpPConcatId] =
      (a, b, c)
  | otherwise = internalBug [] "missing meta SeqOp tags"

metaPatternPUnboundTag,
  metaPatternPVarTag,
  metaPatternPBooleanTag,
  metaPatternPIntTag,
  metaPatternPNatTag,
  metaPatternPFloatTag,
  metaPatternPTextTag,
  metaPatternPCharTag,
  metaPatternPBytesTag,
  metaPatternPConstructorTag,
  metaPatternPAsTag,
  metaPatternPEffectPureTag,
  metaPatternPEffectBindTag,
  metaPatternPSequenceLiteralTag,
  metaPatternPSequenceOpTag ::
    PackedTag
( metaPatternPUnboundTag,
  metaPatternPVarTag,
  metaPatternPBooleanTag,
  metaPatternPIntTag,
  metaPatternPNatTag,
  metaPatternPFloatTag,
  metaPatternPTextTag,
  metaPatternPCharTag,
  metaPatternPBytesTag,
  metaPatternPConstructorTag,
  metaPatternPAsTag,
  metaPatternPEffectPureTag,
  metaPatternPEffectBindTag,
  metaPatternPSequenceLiteralTag,
  metaPatternPSequenceOpTag
  )
    | [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o] <-
        mkTags
          "meta Pattern tags"
          Meta.patternRef
          [ Meta.patternPUnboundId,
            Meta.patternPVarId,
            Meta.patternPBooleanId,
            Meta.patternPIntId,
            Meta.patternPNatId,
            Meta.patternPFloatId,
            Meta.patternPTextId,
            Meta.patternPCharId,
            Meta.patternPBytesId,
            Meta.patternPConstructorId,
            Meta.patternPAsId,
            Meta.patternPEffectPureId,
            Meta.patternPEffectBindId,
            Meta.patternPSequenceLiteralId,
            Meta.patternPSequenceOpId
          ] =
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    | otherwise = internalBug [] "missing meta Pattern tags"

metaKindKStarTag, metaKindKArrowTag :: PackedTag
(metaKindKStarTag, metaKindKArrowTag)
  | [a, b] <-
      mkTags
        "meta Kind tags"
        Meta.kindRef
        [Meta.kindKStarId, Meta.kindKArrowId] =
      (a, b)
  | otherwise = internalBug [] "missing meta Kind tags"

metaMatchCaseTag :: PackedTag
metaMatchCaseTag
  | [t] <-
      mkTags
        "meta MatchCase tags"
        Meta.matchCaseRef
        [Meta.matchCaseCtorId] =
      t
  | otherwise = internalBug [] "missing meta MatchCase tag"

metaAbtVarTag, metaAbtAbsTag, metaAbtCycleTag, metaAbtTmTag :: PackedTag
(metaAbtVarTag, metaAbtAbsTag, metaAbtCycleTag, metaAbtTmTag)
  | [a, b, c, d] <-
      mkTags
        "meta ABT tags"
        Meta.abtRef
        [Meta.abtVarId, Meta.abtAbsId, Meta.abtCycleId, Meta.abtTmId] =
      (a, b, c, d)
  | otherwise = internalBug [] "missing meta ABT tags"

metaTermTermTag :: PackedTag
metaTermTermTag
  | [t] <- mkTags "meta Term tags" Meta.termRef [Meta.termTermId] = t
  | otherwise = internalBug [] "missing meta Term.Term tag"

metaTermFAppTag,
  metaTermFLamTag,
  metaTermFLetTag,
  metaTermFLetRecTag,
  metaTermFIfTag,
  metaTermFMatchTag,
  metaTermFHandleTag,
  metaTermFAnnTag,
  metaTermFRefTag,
  metaTermFConstructorTag,
  metaTermFRequestTag,
  metaTermFLitTag,
  metaTermFListTag,
  metaTermFTermLinkTag,
  metaTermFTypeLinkTag ::
    PackedTag
( metaTermFAppTag,
  metaTermFLamTag,
  metaTermFLetTag,
  metaTermFLetRecTag,
  metaTermFIfTag,
  metaTermFMatchTag,
  metaTermFHandleTag,
  metaTermFAnnTag,
  metaTermFRefTag,
  metaTermFConstructorTag,
  metaTermFRequestTag,
  metaTermFLitTag,
  metaTermFListTag,
  metaTermFTermLinkTag,
  metaTermFTypeLinkTag
  )
    | [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o] <-
        mkTags
          "meta TermF tags"
          Meta.termFRef
          [ Meta.termFAppId,
            Meta.termFLamId,
            Meta.termFLetId,
            Meta.termFLetRecId,
            Meta.termFIfId,
            Meta.termFMatchId,
            Meta.termFHandleId,
            Meta.termFAnnId,
            Meta.termFRefId,
            Meta.termFConstructorId,
            Meta.termFRequestId,
            Meta.termFLitId,
            Meta.termFListId,
            Meta.termFTermLinkId,
            Meta.termFTypeLinkId
          ] =
        (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    | otherwise = internalBug [] "missing meta TermF tags"

metaTypeFArrowTag,
  metaTypeFImplicitArrowTag,
  metaTypeFAppTag,
  metaTypeFEffectTag,
  metaTypeFEffectsTag,
  metaTypeFForallTag,
  metaTypeFIntroOuterTag,
  metaTypeFRefTag,
  metaTypeFAnnTag ::
    PackedTag
( metaTypeFArrowTag,
  metaTypeFImplicitArrowTag,
  metaTypeFAppTag,
  metaTypeFEffectTag,
  metaTypeFEffectsTag,
  metaTypeFForallTag,
  metaTypeFIntroOuterTag,
  metaTypeFRefTag,
  metaTypeFAnnTag
  )
    | [a, b, c, d, e, f, g, h, i] <-
        mkTags
          "meta TypeF tags"
          Meta.typeFRef
          [ Meta.typeFArrowId,
            Meta.typeFImplicitArrowId,
            Meta.typeFAppId,
            Meta.typeFEffectId,
            Meta.typeFEffectsId,
            Meta.typeFForallId,
            Meta.typeFIntroOuterId,
            Meta.typeFRefId,
            Meta.typeFAnnId
          ] =
        (a, b, c, d, e, f, g, h, i)
    | otherwise = internalBug [] "missing meta TypeF tags"
