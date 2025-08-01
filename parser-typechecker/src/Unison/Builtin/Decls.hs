module Unison.Builtin.Decls where

import Control.Lens (_3)
import Data.List (elemIndex, find)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (unpack)
import Unison.ABT qualified as ABT
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.ConstructorType qualified as CT
import Unison.DataDeclaration (DataDeclaration (..), Modifier (Structural, Unique))
import Unison.DataDeclaration qualified as DD
import Unison.DataDeclaration.ConstructorId (ConstructorId)
import Unison.Hashing.V2.Convert (hashDataDecls, typeToReference)
import Unison.Pattern qualified as Pattern
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Symbol (Symbol)
import Unison.Term (Term, Term2)
import Unison.Term qualified as Term
import Unison.Type (Type)
import Unison.Type qualified as Type
import Unison.Var (Var)
import Unison.Var qualified as Var

lookupDeclRef :: Text -> Reference
lookupDeclRef str
  | [(_, d)] <- filter (\(v, _) -> v == Var.named str) decls = Reference.DerivedId d
  | otherwise = error $ "lookupDeclRef: missing \"" ++ unpack str ++ "\""
  where
    decls = [(a, b) | (a, b, _) <- builtinDataDecls]

lookupEffectRef :: Text -> Reference
lookupEffectRef str
  | [(_, d)] <- filter (\(v, _) -> v == Var.named str) decls = Reference.DerivedId d
  | otherwise = error $ "lookupEffectRef: missing \"" ++ unpack str ++ "\""
  where
    decls = [(a, b) | (a, b, _) <- builtinEffectDecls]

unitRef, pairRef, optionalRef, eitherRef :: Reference
unitRef = lookupDeclRef "Unit"
pairRef = lookupDeclRef "Tuple"
optionalRef = lookupDeclRef "Optional"
eitherRef = lookupDeclRef "Either"

testResultRef, testResultListRef, linkRef, docRef, ioErrorRef, stdHandleRef :: Reference
failureRef, ioFailureRef, tlsFailureRef, arrayFailureRef :: Reference
cryptoFailureRef :: Reference
exceptionRef, tlsSignedCertRef, tlsPrivateKeyRef :: Reference
isPropagatedRef, isTestRef :: Reference
isPropagatedRef = lookupDeclRef "IsPropagated"

isTestRef = lookupDeclRef "IsTest"

testResultRef = lookupDeclRef "Test.Result"

-- Reference for [Test.Result]
testResultListRef = typeToReference @Symbol (testResultListType ())

linkRef = lookupDeclRef "Link"

docRef = lookupDeclRef "Doc"

ioErrorRef = lookupDeclRef "io2.IOError"

stdHandleRef = lookupDeclRef "io2.StdHandle"

failureRef = lookupDeclRef "io2.Failure"

exceptionRef = lookupEffectRef "Exception"

ioFailureRef = lookupDeclRef "io2.IOFailure"

tlsFailureRef = lookupDeclRef "io2.TlsFailure"

arrayFailureRef = lookupDeclRef "io2.ArrayFailure"

cryptoFailureRef = lookupDeclRef "crypto.CryptoFailure"

tlsSignedCertRef = lookupDeclRef "io2.Tls.SignedCert"

tlsPrivateKeyRef = lookupDeclRef "io2.Tls.PrivateKey"

runtimeFailureRef, arithmeticFailureRef, miscFailureRef, stmFailureRef, threadKilledFailureRef :: Reference
runtimeFailureRef = lookupDeclRef "io2.RuntimeFailure"
arithmeticFailureRef = lookupDeclRef "io2.ArithmeticFailure"
miscFailureRef = lookupDeclRef "io2.MiscFailure"
stmFailureRef = lookupDeclRef "io2.STMFailure"
threadKilledFailureRef = lookupDeclRef "io2.ThreadKilledFailure"

fileModeRef, filePathRef, bufferModeRef, seekModeRef, seqViewRef :: Reference
fileModeRef = lookupDeclRef "io2.FileMode"
filePathRef = lookupDeclRef "io2.FilePath"
bufferModeRef = lookupDeclRef "io2.BufferMode"
seekModeRef = lookupDeclRef "io2.SeekMode"
seqViewRef = lookupDeclRef "SeqView"

pairCtorRef, unitCtorRef :: Referent
pairCtorRef = Referent.Con (ConstructorReference pairRef 0) CT.Data
unitCtorRef = Referent.Con (ConstructorReference unitRef 0) CT.Data

constructorId :: Reference -> Text -> Maybe ConstructorId
constructorId ref name = do
  (_, _, dd) <- find (\(_, r, _) -> Reference.DerivedId r == ref) builtinDataDecls
  fmap fromIntegral . elemIndex name $ DD.constructorNames dd

effectId :: Reference -> Text -> Maybe ConstructorId
effectId ref name = do
  (_, _, ed) <- find (\(_, r, _) -> Reference.DerivedId r == ref) builtinEffectDecls
  fmap fromIntegral . elemIndex name . DD.constructorNames $ DD.toDataDecl ed

noneId, someId, okConstructorId, failConstructorId, docBlobId, docLinkId, docSignatureId, docSourceId, docEvaluateId, docJoinId, linkTermId, linkTypeId, eitherRightId, eitherLeftId :: ConstructorId
isPropagatedConstructorId, isTestConstructorId, bufferModeNoBufferingId, bufferModeLineBufferingId, bufferModeBlockBufferingId, bufferModeSizedBlockBufferingId :: ConstructorId
seqViewEmpty, seqViewElem :: ConstructorId
noneId = Maybe.fromJust $ constructorId optionalRef "Optional.None"
someId = Maybe.fromJust $ constructorId optionalRef "Optional.Some"

mapTip, mapBin :: ConstructorId
mapTip = Maybe.fromJust $ constructorId mapRef "Map.Tip"
mapBin = Maybe.fromJust $ constructorId mapRef "Map.Bin"

setWrap :: ConstructorId
setWrap = Maybe.fromJust $ constructorId setRef "Set.Set"

jsonNull, jsonBool, jsonObj, jsonNum, jsonText, jsonArr :: ConstructorId
jsonNull = Maybe.fromJust $ constructorId jsonRef "Json.Null"
jsonBool = Maybe.fromJust $ constructorId jsonRef "Json.Boolean"
jsonObj = Maybe.fromJust $ constructorId jsonRef "Json.Object"
jsonNum = Maybe.fromJust $ constructorId jsonRef "Json.Number.Unparsed"
jsonText = Maybe.fromJust $ constructorId jsonRef "Json.Text"
jsonArr = Maybe.fromJust $ constructorId jsonRef "Json.Array"

jsonParseError :: ConstructorId
jsonParseError =
  Maybe.fromJust $
    constructorId parseErrorRef "Json.ParseError.ParseError"

avroNull, avroRecord, avroBytes, avroFixed, avroArray, avroMap, avroUnion, avroEnum, avroString, avroInt, avroLong, avroFloat, avroDouble, avroBoolean :: ConstructorId
avroNull = Maybe.fromJust $ constructorId avroRef "avro.AvroValue.NullValue"
avroRecord = Maybe.fromJust $ constructorId avroRef "avro.AvroValue.RecordValue"
avroBytes = Maybe.fromJust $ constructorId avroRef "avro.AvroValue.BytesValue"
avroFixed = Maybe.fromJust $ constructorId avroRef "avro.AvroValue.FixedValue"
avroArray = Maybe.fromJust $ constructorId avroRef "avro.AvroValue.ArrayValue"
avroMap = Maybe.fromJust $ constructorId avroRef "avro.AvroValue.MapValue"
avroUnion = Maybe.fromJust $ constructorId avroRef "avro.AvroValue.UnionValue"
avroEnum = Maybe.fromJust $ constructorId avroRef "avro.AvroValue.EnumValue"
avroString = Maybe.fromJust $ constructorId avroRef "avro.AvroValue.StringValue"
avroInt = Maybe.fromJust $ constructorId avroRef "avro.AvroValue.IntValue"
avroLong = Maybe.fromJust $ constructorId avroRef "avro.AvroValue.LongValue"
avroFloat = Maybe.fromJust $ constructorId avroRef "avro.AvroValue.FloatValue"
avroDouble = Maybe.fromJust $ constructorId avroRef "avro.AvroValue.DoubleValue"
avroBoolean = Maybe.fromJust $ constructorId avroRef "avro.AvroValue.BooleanValue"

avroDefaultNull, avroDefaultBoolean, avroDefaultInt, avroDefaultLong, avroDefaultFloat, avroDefaultDouble, avroDefaultBytes, avroDefaultString, avroDefaultArray, avroDefaultMap, avroDefaultRecord, avroDefaultEnum, avroDefaultUnion, avroDefaultFixed, avroDefaultNamedType :: ConstructorId
avroDefaultNull = Maybe.fromJust $ constructorId avroDefaultRef "avro.schema.DefaultValue.Null"
avroDefaultBoolean = Maybe.fromJust $ constructorId avroDefaultRef "avro.schema.DefaultValue.Boolean"
avroDefaultInt = Maybe.fromJust $ constructorId avroDefaultRef "avro.schema.DefaultValue.Int"
avroDefaultLong = Maybe.fromJust $ constructorId avroDefaultRef "avro.schema.DefaultValue.Long"
avroDefaultFloat = Maybe.fromJust $ constructorId avroDefaultRef "avro.schema.DefaultValue.Float"
avroDefaultDouble = Maybe.fromJust $ constructorId avroDefaultRef "avro.schema.DefaultValue.Double"
avroDefaultBytes = Maybe.fromJust $ constructorId avroDefaultRef "avro.schema.DefaultValue.Bytes"
avroDefaultString = Maybe.fromJust $ constructorId avroDefaultRef "avro.schema.DefaultValue.String"
avroDefaultArray = Maybe.fromJust $ constructorId avroDefaultRef "avro.schema.DefaultValue.Array"
avroDefaultMap = Maybe.fromJust $ constructorId avroDefaultRef "avro.schema.DefaultValue.Map"
avroDefaultRecord = Maybe.fromJust $ constructorId avroDefaultRef "avro.schema.DefaultValue.Record"
avroDefaultEnum = Maybe.fromJust $ constructorId avroDefaultRef "avro.schema.DefaultValue.Enum"
avroDefaultUnion = Maybe.fromJust $ constructorId avroDefaultRef "avro.schema.DefaultValue.Union"
avroDefaultFixed = Maybe.fromJust $ constructorId avroDefaultRef "avro.schema.DefaultValue.Fixed"
avroDefaultNamedType = Maybe.fromJust $ constructorId avroDefaultRef "avro.schema.DefaultValue.NamedType"

avroReadSchemaNull, avroReadSchemaBoolean, avroReadSchemaInt, avroReadSchemaLong, avroReadSchemaFloat, avroReadSchemaDouble, avroReadSchemaBytes, avroReadSchemaString, avroReadSchemaArray, avroReadSchemaMap, avroReadSchemaRecord, avroReadSchemaEnum, avroReadSchemaUnion, avroReadSchemaFixed, avroReadSchemaFreeUnion, avroReadSchemaNamedType :: ConstructorId
avroReadSchemaNull = Maybe.fromJust $ constructorId avroReadSchemaRef "avro.schema.deconflicted.ReadSchema.Null"
avroReadSchemaBoolean = Maybe.fromJust $ constructorId avroReadSchemaRef "avro.schema.deconflicted.ReadSchema.Boolean"
avroReadSchemaInt = Maybe.fromJust $ constructorId avroReadSchemaRef "avro.schema.deconflicted.ReadSchema.Int"
avroReadSchemaLong = Maybe.fromJust $ constructorId avroReadSchemaRef "avro.schema.deconflicted.ReadSchema.Long"
avroReadSchemaFloat = Maybe.fromJust $ constructorId avroReadSchemaRef "avro.schema.deconflicted.ReadSchema.Float"
avroReadSchemaDouble = Maybe.fromJust $ constructorId avroReadSchemaRef "avro.schema.deconflicted.ReadSchema.Double"
avroReadSchemaBytes = Maybe.fromJust $ constructorId avroReadSchemaRef "avro.schema.deconflicted.ReadSchema.Bytes"
avroReadSchemaString = Maybe.fromJust $ constructorId avroReadSchemaRef "avro.schema.deconflicted.ReadSchema.String"
avroReadSchemaArray = Maybe.fromJust $ constructorId avroReadSchemaRef "avro.schema.deconflicted.ReadSchema.Array"
avroReadSchemaMap = Maybe.fromJust $ constructorId avroReadSchemaRef "avro.schema.deconflicted.ReadSchema.Map"
avroReadSchemaRecord = Maybe.fromJust $ constructorId avroReadSchemaRef "avro.schema.deconflicted.ReadSchema.Record"
avroReadSchemaEnum = Maybe.fromJust $ constructorId avroReadSchemaRef "avro.schema.deconflicted.ReadSchema.Enum"
avroReadSchemaUnion = Maybe.fromJust $ constructorId avroReadSchemaRef "avro.schema.deconflicted.ReadSchema.Union"
avroReadSchemaFixed = Maybe.fromJust $ constructorId avroReadSchemaRef "avro.schema.deconflicted.ReadSchema.Fixed"
avroReadSchemaFreeUnion = Maybe.fromJust $ constructorId avroReadSchemaRef "avro.schema.deconflicted.ReadSchema.FreeUnion"
avroReadSchemaNamedType = Maybe.fromJust $ constructorId avroReadSchemaRef "avro.schema.deconflicted.ReadSchema.NamedType"

avroSchemaRef :: Reference
avroSchemaRef = lookupDeclRef "avro.schema.Schema"

avroSchemaNull, avroSchemaBoolean, avroSchemaInt, avroSchemaLong, avroSchemaFloat, avroSchemaDouble, avroSchemaBytes, avroSchemaString, avroSchemaArray, avroSchemaMap, avroSchemaNamedType, avroSchemaRecord, avroSchemaEnum, avroSchemaUnion, avroSchemaFixed :: ConstructorId
avroSchemaNull = Maybe.fromJust $ constructorId avroSchemaRef "avro.schema.Schema.Null"
avroSchemaBoolean = Maybe.fromJust $ constructorId avroSchemaRef "avro.schema.Schema.Boolean"
avroSchemaInt = Maybe.fromJust $ constructorId avroSchemaRef "avro.schema.Schema.Int"
avroSchemaLong = Maybe.fromJust $ constructorId avroSchemaRef "avro.schema.Schema.Long"
avroSchemaFloat = Maybe.fromJust $ constructorId avroSchemaRef "avro.schema.Schema.Float"
avroSchemaDouble = Maybe.fromJust $ constructorId avroSchemaRef "avro.schema.Schema.Double"
avroSchemaBytes = Maybe.fromJust $ constructorId avroSchemaRef "avro.schema.Schema.Bytes"
avroSchemaString = Maybe.fromJust $ constructorId avroSchemaRef "avro.schema.Schema.String"
avroSchemaArray = Maybe.fromJust $ constructorId avroSchemaRef "avro.schema.Schema.Array"
avroSchemaMap = Maybe.fromJust $ constructorId avroSchemaRef "avro.schema.Schema.Map"
avroSchemaNamedType = Maybe.fromJust $ constructorId avroSchemaRef "avro.schema.Schema.NamedType"
avroSchemaRecord = Maybe.fromJust $ constructorId avroSchemaRef "avro.schema.Schema.Record"
avroSchemaEnum = Maybe.fromJust $ constructorId avroSchemaRef "avro.schema.Schema.Enum"
avroSchemaUnion = Maybe.fromJust $ constructorId avroSchemaRef "avro.schema.Schema.Union"
avroSchemaFixed = Maybe.fromJust $ constructorId avroSchemaRef "avro.schema.Schema.Fixed"

avroFieldRef :: Reference
avroFieldRef = lookupDeclRef "avro.schema.AvroField"

avroEnumRef :: Reference
avroEnumRef = lookupDeclRef "avro.schema.AvroEnum"

avroDecimalRef :: Reference
avroDecimalRef = lookupDeclRef "avro.schema.AvroDecimal"

avroLogicalIntRef :: Reference
avroLogicalIntRef = lookupDeclRef "avro.schema.LogicalIntType"

avroLogicalIntDate, avroLogicalIntTime, avroLogicalIntDecimal :: ConstructorId
avroLogicalIntDate = Maybe.fromJust $ constructorId avroLogicalIntRef "avro.schema.LogicalIntType.Date"
avroLogicalIntTime = Maybe.fromJust $ constructorId avroLogicalIntRef "avro.schema.LogicalIntType.Time"
avroLogicalIntDecimal = Maybe.fromJust $ constructorId avroLogicalIntRef "avro.schema.LogicalIntType.Decimal"

avroLogicalLongRef :: Reference
avroLogicalLongRef = lookupDeclRef "avro.schema.LogicalLongType"

avroLogicalLongTimeMicros, avroLogicalLongTimestampMillis, avroLogicalLongTimestampMicros, avroLogicalLongLocalTimestampMillis, avroLogicalLongLocalTimestampMicros, avroLogicalLongDecimal :: ConstructorId
avroLogicalLongTimeMicros = Maybe.fromJust $ constructorId avroLogicalLongRef "avro.schema.LogicalLongType.TimeMicros"
avroLogicalLongTimestampMillis = Maybe.fromJust $ constructorId avroLogicalLongRef "avro.schema.LogicalLongType.TimestampMillis"
avroLogicalLongTimestampMicros = Maybe.fromJust $ constructorId avroLogicalLongRef "avro.schema.LogicalLongType.TimestampMicros"
avroLogicalLongLocalTimestampMillis = Maybe.fromJust $ constructorId avroLogicalLongRef "avro.schema.LogicalLongType.LocalTimestampMillis"
avroLogicalLongLocalTimestampMicros = Maybe.fromJust $ constructorId avroLogicalLongRef "avro.schema.LogicalLongType.LocalTimestampMicros"
avroLogicalLongDecimal = Maybe.fromJust $ constructorId avroLogicalLongRef "avro.schema.LogicalLongType.Decimal"

avroLogicalFixedRef :: Reference
avroLogicalFixedRef = lookupDeclRef "avro.schema.LogicalFixedType"

avroLogicalFixedDuration, avroLogicalFixedDecimal :: ConstructorId
avroLogicalFixedDuration = Maybe.fromJust $ constructorId avroLogicalFixedRef "avro.schema.LogicalFixedType.Duration"
avroLogicalFixedDecimal = Maybe.fromJust $ constructorId avroLogicalFixedRef "avro.schema.LogicalFixedType.Decimal"

avroLogicalStringRef :: Reference
avroLogicalStringRef = lookupDeclRef "avro.schema.LogicalStringType"

avroLogicalStringUUID :: ConstructorId
avroLogicalStringUUID = Maybe.fromJust $ constructorId avroLogicalStringRef "avro.schema.LogicalStringType.UUID"

avroLogicalBytesRef :: Reference
avroLogicalBytesRef = lookupDeclRef "avro.schema.LogicalBytesType"

avroLogicalBytesDecimal :: ConstructorId
avroLogicalBytesDecimal = Maybe.fromJust $ constructorId avroLogicalBytesRef "avro.schema.LogicalBytesType.Decimal"

avroReadFloatInt32, avroReadFloatInt64, avroReadFloat :: ConstructorId
avroReadFloatInt32 = Maybe.fromJust $ constructorId avroReadFloatRef "avro.schema.deconflicted.ReadFloatFromInt32"
avroReadFloatInt64 = Maybe.fromJust $ constructorId avroReadFloatRef "avro.schema.deconflicted.ReadFloatFromInt64"
avroReadFloat = Maybe.fromJust $ constructorId avroReadFloatRef "avro.schema.deconflicted.ReadFloat"

avroReadDoubleInt32, avroReadDoubleInt64, avroReadDoubleFromFloat, avroReadDouble :: ConstructorId
avroReadDoubleInt32 = Maybe.fromJust $ constructorId avroReadDoubleRef "avro.schema.deconflicted.ReadDoubleFromInt32"
avroReadDoubleInt64 = Maybe.fromJust $ constructorId avroReadDoubleRef "avro.schema.deconflicted.ReadDoubleFromInt64"
avroReadDoubleFromFloat = Maybe.fromJust $ constructorId avroReadDoubleRef "avro.schema.deconflicted.ReadDoubleFromFloat"
avroReadDouble = Maybe.fromJust $ constructorId avroReadDoubleRef "avro.schema.deconflicted.ReadDouble"

isPropagatedConstructorId = Maybe.fromJust $ constructorId isPropagatedRef "IsPropagated.IsPropagated"

isTestConstructorId = Maybe.fromJust $ constructorId isTestRef "IsTest.IsTest"

okConstructorId = Maybe.fromJust $ constructorId testResultRef "Test.Result.Ok"

failConstructorId = Maybe.fromJust $ constructorId testResultRef "Test.Result.Fail"

docBlobId = Maybe.fromJust $ constructorId docRef "Doc.Blob"

docLinkId = Maybe.fromJust $ constructorId docRef "Doc.Link"

docSignatureId = Maybe.fromJust $ constructorId docRef "Doc.Signature"

docSourceId = Maybe.fromJust $ constructorId docRef "Doc.Source"

docEvaluateId = Maybe.fromJust $ constructorId docRef "Doc.Evaluate"

docJoinId = Maybe.fromJust $ constructorId docRef "Doc.Join"

linkTermId = Maybe.fromJust $ constructorId linkRef "Link.Term"

linkTypeId = Maybe.fromJust $ constructorId linkRef "Link.Type"

eitherRightId = Maybe.fromJust $ constructorId eitherRef "Either.Right"

eitherLeftId = Maybe.fromJust $ constructorId eitherRef "Either.Left"

seqViewEmpty = Maybe.fromJust $ constructorId seqViewRef "SeqView.VEmpty"

seqViewElem = Maybe.fromJust $ constructorId seqViewRef "SeqView.VElem"

bufferModeNoBufferingId = Maybe.fromJust $ constructorId bufferModeRef "io2.BufferMode.NoBuffering"

bufferModeLineBufferingId = Maybe.fromJust $ constructorId bufferModeRef "io2.BufferMode.LineBuffering"

bufferModeBlockBufferingId = Maybe.fromJust $ constructorId bufferModeRef "io2.BufferMode.BlockBuffering"

bufferModeSizedBlockBufferingId = Maybe.fromJust $ constructorId bufferModeRef "io2.BufferMode.SizedBlockBuffering"

fileModeReadId, fileModeWriteId, fileModeAppendId, fileModeReadWriteId :: ConstructorId
fileModeReadId = Maybe.fromJust $ constructorId fileModeRef "io2.FileMode.Read"
fileModeWriteId = Maybe.fromJust $ constructorId fileModeRef "io2.FileMode.Write"
fileModeAppendId = Maybe.fromJust $ constructorId fileModeRef "io2.FileMode.Append"
fileModeReadWriteId = Maybe.fromJust $ constructorId fileModeRef "io2.FileMode.ReadWrite"

seekModeAbsoluteId, seekModeRelativeId, seekModeEndId :: ConstructorId
seekModeAbsoluteId = Maybe.fromJust $ constructorId seekModeRef "io2.SeekMode.AbsoluteSeek"
seekModeRelativeId = Maybe.fromJust $ constructorId seekModeRef "io2.SeekMode.RelativeSeek"
seekModeEndId = Maybe.fromJust $ constructorId seekModeRef "io2.SeekMode.SeekFromEnd"

stdInId, stdOutId, stdErrId :: ConstructorId
stdInId = Maybe.fromJust $ constructorId stdHandleRef "io2.StdHandle.StdIn"
stdOutId = Maybe.fromJust $ constructorId stdHandleRef "io2.StdHandle.StdOut"
stdErrId = Maybe.fromJust $ constructorId stdHandleRef "io2.StdHandle.StdErr"

exceptionRaiseId :: ConstructorId
exceptionRaiseId = Maybe.fromJust $ effectId exceptionRef "Exception.raise"

okConstructorReferent, failConstructorReferent :: Referent.Referent
okConstructorReferent = Referent.Con (ConstructorReference testResultRef okConstructorId) CT.Data
failConstructorReferent = Referent.Con (ConstructorReference testResultRef failConstructorId) CT.Data

rewriteTermRef :: Reference
rewriteTermRef = lookupDeclRef "RewriteTerm"

pattern RewriteTerm' :: Term2 vt at ap v a -> Term2 vt at ap v a -> Term2 vt at ap v a
pattern RewriteTerm' lhs rhs <- (unRewriteTerm -> Just (lhs, rhs))

unRewriteTerm :: Term2 vt at ap v a -> Maybe (Term2 vt at ap v a, Term2 vt at ap v a)
unRewriteTerm (Term.Apps' (Term.Constructor' (ConstructorReference r _)) [lhs, rhs])
  | r == rewriteTermRef = Just (lhs, rhs)
unRewriteTerm _ = Nothing

rewriteCaseRef :: Reference
rewriteCaseRef = lookupDeclRef "RewriteCase"

pattern RewriteCase' :: Term2 vt at ap v a -> Term2 vt at ap v a -> Term2 vt at ap v a
pattern RewriteCase' lhs rhs <- (unRewriteCase -> Just (lhs, rhs))

rewriteCase :: (Ord v) => a -> Term2 vt at ap v a -> Term2 vt at ap v a -> Term2 vt at ap v a
rewriteCase a tm1 tm2 = Term.app a (Term.app a1 (Term.constructor a1 r) tm1) tm2
  where
    a1 = ABT.annotation tm1
    r = ConstructorReference rewriteCaseRef 0

rewriteTerm :: (Ord v) => a -> Term2 vt at ap v a -> Term2 vt at ap v a -> Term2 vt at ap v a
rewriteTerm a tm1 tm2 = Term.app a (Term.app a1 (Term.constructor a1 r) tm1) tm2
  where
    a1 = ABT.annotation tm1
    r = ConstructorReference rewriteTermRef 0

unRewriteCase :: Term2 vt at ap v a -> Maybe (Term2 vt at ap v a, Term2 vt at ap v a)
unRewriteCase (Term.Apps' (Term.Constructor' (ConstructorReference r _)) [lhs, rhs])
  | r == rewriteCaseRef = Just (lhs, rhs)
unRewriteCase _ = Nothing

rewriteTypeRef :: Reference
rewriteTypeRef = lookupDeclRef "RewriteSignature"

pattern RewriteSignature' :: forall vt at ap v a. [vt] -> Type vt at -> Type vt at -> Term2 vt at ap v a
pattern RewriteSignature' vs lhs rhs <- (unRewriteSignature -> Just (vs, lhs, rhs))

rewriteType :: (Var v, Semigroup a) => a -> [v] -> Type v a -> Type v a -> Term2 v a a v a
rewriteType a vs lhs rhs =
  Term.app
    a
    (Term.constructor la (ConstructorReference rewriteTypeRef 0))
    ( Term.ann
        a
        (Term.delay a (Term.delay a (unitTerm a)))
        (Type.foralls a vs (Type.arrow (la <> ra) lhs (Type.arrow ra rhs (unitType ra))))
    )
  where
    la = ABT.annotation lhs
    ra = ABT.annotation rhs

unRewriteSignature :: Term2 vt at ap v a -> Maybe ([vt], Type vt at, Type vt at)
unRewriteSignature
  ( Term.App'
      (Term.Constructor' (ConstructorReference r _))
      (Term.Ann' _ (Type.ForallsNamedOpt' vs (Type.Arrow' lhs (Type.Arrow' rhs _unit))))
    )
    | r == rewriteTypeRef = Just (vs, lhs, rhs)
unRewriteSignature _ = Nothing

rewritesRef :: Reference
rewritesRef = lookupDeclRef "Rewrites"

mapRef :: Reference
mapRef = lookupDeclRef "Map"

setRef :: Reference
setRef = lookupDeclRef "Set"

jsonRef :: Reference
jsonRef = lookupDeclRef "Json"

parseErrorRef :: Reference
parseErrorRef = lookupDeclRef "Json.ParseError"

avroRef :: Reference
avroRef = lookupDeclRef "avro.AvroValue"

avroDefaultRef :: Reference
avroDefaultRef = lookupDeclRef "avro.schema.DefaultValue"

avroReadSchemaRef :: Reference
avroReadSchemaRef = lookupDeclRef "avro.schema.deconflicted.ReadSchema"

avroTypeNameRef :: Reference
avroTypeNameRef = lookupDeclRef "avro.schema.TypeName"

avroReadRecordRef :: Reference
avroReadRecordRef = lookupDeclRef "avro.schema.deconflicted.ReadRecord"

avroRecordRef :: Reference
avroRecordRef = lookupDeclRef "avro.schema.AvroRecord"

avroReadFieldRef :: Reference
avroReadFieldRef = lookupDeclRef "avro.schema.deconflicted.ReadField"

avroFixedRef :: Reference
avroFixedRef = lookupDeclRef "avro.schema.AvroFixed"

avroReadFloatRef :: Reference
avroReadFloatRef = lookupDeclRef "avro.schema.deconflicted.ReadFloat"

avroReadDoubleRef :: Reference
avroReadDoubleRef = lookupDeclRef "avro.schema.deconflicted.ReadDouble"

avroReadLongRef :: Reference
avroReadLongRef = lookupDeclRef "avro.schema.deconflicted.ReadLong"

avroFieldStatusRef :: Reference
avroFieldStatusRef = lookupDeclRef "avro.schema.deconflicted.FieldStatus"

avroOrderRef :: Reference
avroOrderRef = lookupDeclRef "avro.schema.Order"

avroReadLongInt32, avroReadLong :: ConstructorId
avroReadLongInt32 = Maybe.fromJust $ constructorId avroReadLongRef "avro.schema.deconflicted.ReadLong.LongFromInt32"
avroReadLong = Maybe.fromJust $ constructorId avroReadLongRef "avro.schema.deconflicted.ReadLong"

avroFieldStatusAsIs, avroFieldStatusDefaulted, avroFieldStatusIgnored :: ConstructorId
avroFieldStatusAsIs = Maybe.fromJust $ constructorId avroFieldStatusRef "avro.schema.deconflicted.FieldStatus.AsIs"
avroFieldStatusDefaulted = Maybe.fromJust $ constructorId avroFieldStatusRef "avro.schema.deconflicted.FieldStatus.Defaulted"
avroFieldStatusIgnored = Maybe.fromJust $ constructorId avroFieldStatusRef "avro.schema.deconflicted.FieldStatus.Ignored"

avroOrderAscending, avroOrderDescending, avroOrderIgnore :: ConstructorId
avroOrderAscending = Maybe.fromJust $ constructorId avroOrderRef "avro.schema.Order.Ascending"
avroOrderDescending = Maybe.fromJust $ constructorId avroOrderRef "avro.schema.Order.Descending"
avroOrderIgnore = Maybe.fromJust $ constructorId avroOrderRef "avro.schema.Order.Ignore"

avroDefaultValueRef :: Reference
avroDefaultValueRef = lookupDeclRef "avro.schema.DefaultValue"

avroDefaultValueBytes, avroDefaultValueFixed, avroDefaultValueArray, avroDefaultValueInt32, avroDefaultValueInt64, avroDefaultValueEnum, avroDefaultValueBoolean, avroDefaultValueNull, avroDefaultValueMap, avroDefaultValueString, avroDefaultValueFloat, avroDefaultValueDouble, avroDefaultValueRecord, avroDefaultValueUnion :: ConstructorId
avroDefaultValueBytes = Maybe.fromJust $ constructorId avroDefaultValueRef "avro.schema.DefaultValue.Bytes"
avroDefaultValueFixed = Maybe.fromJust $ constructorId avroDefaultValueRef "avro.schema.DefaultValue.Fixed"
avroDefaultValueArray = Maybe.fromJust $ constructorId avroDefaultValueRef "avro.schema.DefaultValue.Array"
avroDefaultValueInt32 = Maybe.fromJust $ constructorId avroDefaultValueRef "avro.schema.DefaultValue.Int32"
avroDefaultValueInt64 = Maybe.fromJust $ constructorId avroDefaultValueRef "avro.schema.DefaultValue.Int64"
avroDefaultValueEnum = Maybe.fromJust $ constructorId avroDefaultValueRef "avro.schema.DefaultValue.Enum"
avroDefaultValueBoolean = Maybe.fromJust $ constructorId avroDefaultValueRef "avro.schema.DefaultValue.Boolean"
avroDefaultValueNull = Maybe.fromJust $ constructorId avroDefaultValueRef "avro.schema.DefaultValue.Null"
avroDefaultValueMap = Maybe.fromJust $ constructorId avroDefaultValueRef "avro.schema.DefaultValue.Map"
avroDefaultValueString = Maybe.fromJust $ constructorId avroDefaultValueRef "avro.schema.DefaultValue.String"
avroDefaultValueFloat = Maybe.fromJust $ constructorId avroDefaultValueRef "avro.schema.DefaultValue.Float"
avroDefaultValueDouble = Maybe.fromJust $ constructorId avroDefaultValueRef "avro.schema.DefaultValue.Double"
avroDefaultValueRecord = Maybe.fromJust $ constructorId avroDefaultValueRef "avro.schema.DefaultValue.Record"
avroDefaultValueUnion = Maybe.fromJust $ constructorId avroDefaultValueRef "avro.schema.DefaultValue.Union"

pattern Rewrites' :: [Term2 vt at ap v a] -> Term2 vt at ap v a
pattern Rewrites' ts <- (unRewrites -> Just ts)

rewrites :: (Var v, Monoid a) => a -> [Term2 vt at ap v a] -> Term2 vt at ap v a
rewrites a [] = Term.app a (Term.constructor a (ConstructorReference rewritesRef 0)) (tupleTerm [])
rewrites a ts@(hd : _) = Term.app a (Term.constructor a1 (ConstructorReference rewritesRef 0)) (tupleTerm ts)
  where
    a1 = ABT.annotation hd

unRewrites :: Term2 vt at ap v a -> Maybe [Term2 vt at ap v a]
unRewrites (Term.App' (Term.Constructor' (ConstructorReference r _)) tup)
  | r == rewritesRef, TupleTerm' ts <- tup = Just ts
unRewrites _ = Nothing

-- | parse some builtin data types, and resolve their free variables using
-- | builtinTypes' and those types defined herein
builtinDataDecls :: [(Symbol, Reference.Id, DataDeclaration Symbol ())]
builtinDataDecls = rs1 ++ rs
  where
    rs1 = case hashDataDecls $
      Map.fromList
        [ (v "Link", link)
        ] of
      Right a -> a
      Left e -> error $ "builtinDataDecls: " <> show e
    rs = case hashDataDecls $
      Map.fromList
        [ (v "Unit", unit),
          (v "Tuple", tuple),
          (v "Optional", opt),
          (v "Either", eith),
          (v "Test.Result", tr),
          (v "IsPropagated", isPropagated),
          (v "IsTest", isTest),
          (v "Doc", doc),
          (v "io2.FileMode", fmode),
          (v "io2.BufferMode", bmode),
          (v "io2.SeekMode", smode),
          (v "SeqView", seqview),
          (v "io2.IOError", ioerr),
          (v "io2.StdHandle", stdhnd),
          (v "io2.Failure", failure),
          (v "io2.TlsFailure", tlsFailure),
          (v "io2.IOFailure", ioFailure),
          (v "io2.ArrayFailure", arrayFailure),
          (v "io2.RuntimeFailure", runtimeFailure),
          (v "io2.ArithmeticFailure", arithmeticFailure),
          (v "io2.MiscFailure", miscFailure),
          (v "io2.STMFailure", stmFailure),
          (v "io2.ThreadKilledFailure", threadKilledFailure),
          (v "crypto.CryptoFailure", cryptoFailure),
          (v "RewriteTerm", rewriteTerm),
          (v "RewriteSignature", rewriteType),
          (v "RewriteCase", rewriteCase),
          (v "Rewrites", rewrites),
          (v "Map", map),
          (v "Set", set),
          (v "Json", json),
          (v "Json.ParseError", jsonParseError),
          (v "avro.AvroValue", avro),
          (v "avro.schema.Schema", avroSchema),
          (v "avro.schema.deconflicted.ReadSchema", avroReadSchema),
          (v "avro.schema.deconflicted.ReadFloat", avroReadFloat),
          (v "avro.schema.deconflicted.ReadDouble", avroReadDouble),
          (v "avro.schema.deconflicted.ReadLong", avroReadLong),
          (v "avro.schema.deconflicted.ReadRecord", avroReadRecord),
          (v "avro.schema.AvroRecord", avroRecord),
          (v "avro.schema.deconflicted.ReadField", avroReadField),
          (v "avro.schema.Order", avroOrder),
          (v "avro.schema.deconflicted.FieldStatus", avroFieldStatus),
          (v "avro.schema.AvroField", avroField),
          (v "avro.schema.AvroEnum", avroEnum),
          (v "avro.schema.DefaultValue", avroDefaultValue),
          (v "avro.schema.TypeName", avroTypeName),
          (v "avro.schema.AvroFixed", avroFixed),
          (v "avro.schema.LogicalFixedType", avroLogicalFixedType),
          (v "avro.schema.LogicalStringType", avroLogicalStringType),
          (v "avro.schema.LogicalBytesType", avroLogicalBytesType),
          (v "avro.schema.LogicalIntType", avroLogicalIntType),
          (v "avro.schema.LogicalLongType", avroLogicalLongType),
          (v "avro.schema.Decimal", avroDecimal)
        ] of
      Right a -> a
      Left e -> error $ "builtinDataDecls: " <> show e
    linkRef = case rs1 of
      [(_, linkRef, _)] -> linkRef
      _ -> error "builtinDataDecls: Expected a single linkRef"
    v = Var.named
    var name = Type.var () (v name)
    infixr 7 `arr`
    arr = Type.arrow'
    -- see note on `hashDecls` above for why ctor must be called `Unit.Unit`.
    unit = DataDeclaration Structural () [] [((), v "Unit.Unit", var "Unit")]
    tuple =
      DataDeclaration
        Structural
        ()
        [v "a", v "b"]
        [ ( (),
            v "Tuple.Cons",
            Type.foralls
              ()
              [v "a", v "b"]
              ( var "a"
                  `arr` (var "b" `arr` Type.apps' (var "Tuple") [var "a", var "b"])
              )
          )
        ]
    opt =
      DataDeclaration
        Structural
        ()
        [v "a"]
        [ ( (),
            v "Optional.None",
            Type.foralls () [v "a"] (Type.app' (var "Optional") (var "a"))
          ),
          ( (),
            v "Optional.Some",
            Type.foralls
              ()
              [v "a"]
              (var "a" `arr` Type.app' (var "Optional") (var "a"))
          )
        ]
    eith =
      DataDeclaration
        Structural
        ()
        [v "a", v "b"]
        [ ( (),
            v "Either.Left",
            Type.foralls
              ()
              [v "a", v "b"]
              (var "a" `arr` Type.apps' (var "Either") [var "a", var "b"])
          ),
          ( (),
            v "Either.Right",
            Type.foralls
              ()
              [v "a", v "b"]
              (var "b" `arr` Type.apps' (var "Either") [var "a", var "b"])
          )
        ]
    rewriteCase =
      DataDeclaration
        (Unique "a116f0f1a8d16aba115b7790b09c56820be48798d9fef64fda3ec2325388f769")
        ()
        [v "a", v "b"]
        [ ( (),
            v "RewriteCase.RewriteCase",
            Type.foralls
              ()
              [v "a", v "b"]
              (var "a" `arr` (var "b" `arr` Type.apps' (var "RewriteCase") [var "a", var "b"]))
          )
        ]
    rewriteTerm =
      DataDeclaration
        (Unique "d577219dc862f148bbdbeb78ae977f6a7da22eb44a1b43d484cabd3e4d7e76a1")
        ()
        [v "a", v "b"]
        [ ( (),
            v "RewriteTerm.RewriteTerm",
            Type.foralls
              ()
              [v "a", v "b"]
              (var "a" `arr` (var "b" `arr` Type.apps' (var "RewriteTerm") [var "a", var "b"]))
          )
        ]
    rewriteType =
      DataDeclaration
        (Unique "f9ae4c4263c2f173deeb550dc1f798147c301ea3a6b306810988e4634834507b")
        ()
        [v "a", v "b"]
        [ ( (),
            v "RewriteSignature.RewriteSignature",
            Type.foralls
              ()
              [v "a", v "b"]
              ((var "a" `arr` (var "b" `arr` var "Unit")) `arr` Type.apps' (var "RewriteSignature") [var "a", var "b"])
          )
        ]
    rewrites =
      DataDeclaration
        (Unique "f64795bf31f7eb41e59b31379d6576a4abaca5b4c1bfc0b8c211e608906aff1a")
        ()
        [v "a"]
        [ ( (),
            v "Rewrites.Rewrites",
            Type.foralls
              ()
              [v "a"]
              (var "a" `arr` Type.apps' (var "Rewrites") [var "a"])
          )
        ]
    isTest =
      DataDeclaration
        (Unique "e6dca08b40458b03ca1660cfbdaecaa7279b42d18257898b5fd1c34596aac36f")
        ()
        []
        [((), v "IsTest.IsTest", var "IsTest")]
    isPropagated =
      DataDeclaration
        (Unique "b28d929d0a73d2c18eac86341a3bb9399f8550c11b5f35eabb2751e6803ccc20")
        ()
        []
        [((), v "IsPropagated.IsPropagated", var "IsPropagated")]
    fmode =
      DataDeclaration
        (Unique "3c11ba4f0a5d8fedd427b476cdd2d7673197d11e")
        ()
        []
        [ ((), v "io2.FileMode.Read", var "io2.FileMode"),
          ((), v "io2.FileMode.Write", var "io2.FileMode"),
          ((), v "io2.FileMode.Append", var "io2.FileMode"),
          ((), v "io2.FileMode.ReadWrite", var "io2.FileMode")
        ]
    bmode =
      DataDeclaration
        (Unique "7dd9560d3826c21e5e6a7e08f575b61adcddf849")
        ()
        []
        [ ((), v "io2.BufferMode.NoBuffering", var "io2.BufferMode"),
          ((), v "io2.BufferMode.LineBuffering", var "io2.BufferMode"),
          ((), v "io2.BufferMode.BlockBuffering", var "io2.BufferMode"),
          ( (),
            v "io2.BufferMode.SizedBlockBuffering",
            Type.nat () `arr` var "io2.BufferMode"
          )
        ]
    smode =
      DataDeclaration
        (Unique "453a764f73cb4c7371d9af23b2d5ed646bf9e57c")
        ()
        []
        [ ((), v "io2.SeekMode.AbsoluteSeek", var "io2.SeekMode"),
          ((), v "io2.SeekMode.RelativeSeek", var "io2.SeekMode"),
          ((), v "io2.SeekMode.SeekFromEnd", var "io2.SeekMode")
        ]
    ioerr =
      DataDeclaration
        (Unique "5915e25ac83205f7885395cc6c6c988bc5ec69a1")
        ()
        []
        [ ((), v "io2.IOError.AlreadyExists", var "io2.IOError"),
          ((), v "io2.IOError.NoSuchThing", var "io2.IOError"),
          ((), v "io2.IOError.ResourceBusy", var "io2.IOError"),
          ((), v "io2.IOError.ResourceExhausted", var "io2.IOError"),
          ((), v "io2.IOError.EOF", var "io2.IOError"),
          ((), v "io2.IOError.IllegalOperation", var "io2.IOError"),
          ((), v "io2.IOError.PermissionDenied", var "io2.IOError"),
          ((), v "io2.IOError.UserError", var "io2.IOError")
        ]
    failure =
      DataDeclaration
        (Unique "52ad89274a358b9c802792aa05915e25ac83205f7885395cc6c6c988bc5ec69a1")
        ()
        []
        [ ((), v "io2.Failure.Failure", Type.typeLink () `arr` (Type.text () `arr` (Type.any () `arr` var "io2.Failure")))
        ]

    tlsFailure =
      DataDeclaration
        (Unique "df5ba835130b227ab83d02d1feff5402455a732d613b51dee32230d2f2d067c6")
        ()
        []
        []

    ioFailure =
      DataDeclaration
        (Unique "009cb00e78cac9e47485cc3633c7a363939f63866ea07ab330346a2121d69a83")
        ()
        []
        []

    arrayFailure =
      DataDeclaration
        (Unique "8e877b3a45a3029904dbca9cbd8dda0ec0d147d67bd5b89027a90632c9e927fb")
        ()
        []
        []

    runtimeFailure =
      DataDeclaration
        (Unique "1061ebd9e1b8f99fafecdf02966898fd19151cc14759a7f192e12c5071fb8986")
        ()
        []
        []

    arithmeticFailure =
      DataDeclaration
        (Unique "57eebfd5958d4b07c460293760f241d3e14285740bca78e2149d166e951efc07")
        ()
        []
        []

    miscFailure =
      DataDeclaration
        (Unique "4bd8b3a187c5426d17c30f19efd2851fe6dcfceb302b1dec5df6dfb4214841d9")
        ()
        []
        []

    stmFailure =
      DataDeclaration
        (Unique "0dd9991d6c88424007bfa0b6e55a5211d1a9b6f473ed542dad3b6ecaf94c6941")
        ()
        []
        []

    threadKilledFailure =
      DataDeclaration
        (Unique "e7e479ebb757edcd5acff958b00aa228ac75b0c53638d44cf9d62fca045c33cf")
        ()
        []
        []

    cryptoFailure =
      DataDeclaration
        (Unique "09132bf0cc3f07db75be127d141da91fdd545adcff88866268dfd428e9879742")
        ()
        []
        []

    stdhnd =
      DataDeclaration
        (Unique "67bf7a8e517cbb1e9f42bc078e35498212d3be3c")
        ()
        []
        [ ((), v "io2.StdHandle.StdIn", var "io2.StdHandle"),
          ((), v "io2.StdHandle.StdOut", var "io2.StdHandle"),
          ((), v "io2.StdHandle.StdErr", var "io2.StdHandle")
        ]
    seqview =
      DataDeclaration
        Structural
        ()
        [v "a", v "b"]
        [ ( (),
            v "SeqView.VEmpty",
            Type.foralls
              ()
              [v "a", v "b"]
              (Type.apps' (var "SeqView") [var "a", var "b"])
          ),
          ( (),
            v "SeqView.VElem",
            let sv = Type.apps' (var "SeqView") [var "a", var "b"]
             in Type.foralls
                  ()
                  [v "a", v "b"]
                  (var "a" `arr` (var "b" `arr` sv))
          )
        ]
    tr =
      DataDeclaration
        (Unique "70621e539cd802b2ad53105697800930411a3ebc")
        ()
        []
        [ ((), v "Test.Result.Fail", Type.text () `arr` var "Test.Result"),
          ((), v "Test.Result.Ok", Type.text () `arr` var "Test.Result")
        ]
    doc =
      DataDeclaration
        (Unique "c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2ac3713831264adb47fb6bd1e058d5f004")
        ()
        []
        [ ((), v "Doc.Blob", Type.text () `arr` var "Doc"),
          ((), v "Doc.Link", Type.refId () linkRef `arr` var "Doc"),
          ((), v "Doc.Signature", Type.termLink () `arr` var "Doc"),
          ((), v "Doc.Source", Type.refId () linkRef `arr` var "Doc"),
          ((), v "Doc.Evaluate", Type.termLink () `arr` var "Doc"),
          ((), v "Doc.Join", Type.app () (Type.list ()) (var "Doc") `arr` var "Doc")
        ]
    link =
      DataDeclaration
        (Unique "a5803524366ead2d7f3780871d48771e8142a3b48802f34a96120e230939c46bd5e182fcbe1fa64e9bff9bf741f3c04")
        ()
        []
        [ ((), v "Link.Term", Type.termLink () `arr` var "Link"),
          ((), v "Link.Type", Type.typeLink () `arr` var "Link")
        ]
    map =
      DataDeclaration
        (Unique "s9drbo3urtmpecjn6ivkj5mn0vr11gfn")
        ()
        [v "k", v "v"]
        let forke = Type.foralls () [v "k", v "v"]
            k = var "k"
            e = var "v"
            mapke = Type.apps' (var "Map") [k, e]
         in [ ( (),
                v "Map.Bin",
                forke $ Type.nat () `arr` k `arr` e `arr` mapke `arr` mapke `arr` mapke
              ),
              ((), v "Map.Tip", forke mapke)
            ]
    set =
      DataDeclaration
        Structural
        ()
        [v "a"]
        let va = var "a"
            mapau = Type.apps' (var "Map") [va, var "Unit"]
            seta = Type.apps' (var "Set") [va]
         in [ ( (),
                v "Set.Set",
                Type.foralls () [v "a"] $ mapau `arr` seta
              )
            ]

    json =
      DataDeclaration
        (Unique "oml0j9g6bb2tij2s75k4v7n1nftj199i")
        ()
        []
        let json = var "Json"
            tup x y = Type.apps' (var "Tuple") [x, y]
            pair x y = tup x (tup y (var "Unit"))
         in [ ((), v "Json.Null", var "Json"),
              ((), v "Json.Boolean", Type.boolean () `arr` json),
              ( (),
                v "Json.Object",
                Type.app () (Type.list ()) (pair (Type.text ()) json)
                  `arr` json
              ),
              ((), v "Json.Number.Unparsed", Type.text () `arr` json),
              ((), v "Json.Text", Type.text () `arr` json),
              ( (),
                v "Json.Array",
                Type.app () (Type.list ()) json `arr` json
              )
            ]
    jsonParseError =
      DataDeclaration
        (Unique "u3j6g9j6daejijc5e0rcujjj3sd6j3gq")
        ()
        []
        let jpe = var "Json.ParseError"
         in [ ( (),
                v "Json.ParseError.ParseError",
                Type.text () `arr` Type.nat () `arr` Type.text () `arr` jpe
              )
            ]

    avro =
      DataDeclaration
        (Unique "qvcc7sqhpv38ttfjq2f2r8k88sl8euku")
        ()
        []
        let avro = var "avro.AvroValue"
            schema = var "avro.schema.deconflicted.ReadSchema"
            nat = Type.nat ()
            map x y = Type.apps' (var "Map") [x, y]
            list = Type.app () (Type.list ())
            text = Type.text ()
            bytes = Type.bytes ()
            int = Type.int ()
            float = Type.float ()
            boolean = Type.boolean ()
         in [ ((), v "avro.AvroValue.RecordValue", schema `arr` list avro `arr` avro),
              ((), v "avro.AvroValue.BytesValue", schema `arr` bytes `arr` avro),
              ((), v "avro.AvroValue.FixedValue", schema `arr` bytes `arr` avro),
              ((), v "avro.AvroValue.ArrayValue", list avro `arr` avro),
              ((), v "avro.AvroValue.MapValue", map text avro `arr` avro),
              ((), v "avro.AvroValue.NullValue", avro),
              ((), v "avro.AvroValue.StringValue", schema `arr` text `arr` avro),
              ((), v "avro.AvroValue.EnumValue", schema `arr` nat `arr` text `arr` avro),
              ((), v "avro.AvroValue.FloatValue", schema `arr` float `arr` avro),
              ((), v "avro.AvroValue.DoubleValue", schema `arr` float `arr` avro),
              ((), v "avro.AvroValue.IntValue", schema `arr` int `arr` avro),
              ((), v "avro.AvroValue.LongValue", schema `arr` int `arr` avro),
              ((), v "avro.AvroValue.BooleanValue", boolean `arr` avro),
              ((), v "avro.AvroValue.UnionValue", schema `arr` nat `arr` avro `arr` avro)
            ]

    avroLogicalStringType =
      DataDeclaration
        (Unique "h0pekchk286pkvo3kj97ci4v94h0vgh9")
        ()
        []
        let logicalString = var "avro.schema.LogicalStringType"
         in [((), v "avro.schema.LogicalStringType.UUID", logicalString)]

    avroLogicalBytesType =
      DataDeclaration
        (Unique "iqgd92f4icrrnaij4cggq39vhtq17inl")
        ()
        []
        let decimal = var "avro.schema.Decimal"
            logicalBytes = var "avro.schema.LogicalBytesType"
         in [((), v "avro.schema.LogicalBytesType.DecimalB", decimal `arr` logicalBytes)]

    avroLogicalFixedType =
      DataDeclaration
        (Unique "rnmp14qn1ugce77g9d88par2ns1eua3i")
        ()
        []
        let decimal = var "avro.schema.Decimal"
            logicalFixed = var "avro.schema.LogicalFixedType"
         in [ ((), v "avro.schema.LogicalFixedType.Duration", logicalFixed),
              ((), v "avro.schema.LogicalFixedType.DecimalF", decimal `arr` logicalFixed)
            ]

    avroDecimal =
      DataDeclaration
        (Unique "kar29s8peeugk9igfghjdrv7ngp0e8jl")
        ()
        []
        let decimal = var "avro.schema.Decimal"
            nat = Type.nat ()
         in [((), v "avro.schema.Decimal.Decimal", nat `arr` nat `arr` decimal)]

    avroLogicalIntType =
      DataDeclaration
        (Unique "kdfbu044nv0okj667o0vkjcdbu7fs576")
        ()
        []
        let logicalInt = var "avro.schema.LogicalIntType"
            avroDecimal = var "avro.schema.Decimal"
         in [ ((), v "avro.schema.LogicalIntType.Date", logicalInt),
              ((), v "avro.schema.LogicalIntType.TimeMillis", logicalInt),
              ((), v "avro.schema.LogicalIntType.DecimalI", avroDecimal `arr` logicalInt)
            ]

    avroLogicalLongType =
      DataDeclaration
        (Unique "baf0kq3njc0o87a1n97svg5qk4350tn8")
        ()
        []
        let logicalLong = var "avro.schema.LogicalLongType"
            avroDecimal = var "avro.schema.Decimal"
         in [ ((), v "avro.schema.LogicalLongType.TimeMicros", logicalLong),
              ((), v "avro.schema.LogicalLongType.TimestampMillis", logicalLong),
              ((), v "avro.schema.LogicalLongType.TimestampMicros", logicalLong),
              ((), v "avro.schema.LogicalLongType.LocalTimestampMillis", logicalLong),
              ((), v "avro.schema.LogicalLongType.LocalTimestampMicros", logicalLong),
              ((), v "avro.schema.LogicalLongType.DecimalL", avroDecimal `arr` logicalLong)
            ]

    avroDefaultValue =
      DataDeclaration
        (Unique "vg0ijfhcnpes0q3m64a972n8btdhp43q")
        ()
        []
        let defaultValue = var "avro.schema.DefaultValue"
            schema = var "avro.schema.Schema"
            bytes = Type.bytes ()
            list = Type.app () (Type.list ())
            int = Type.int ()
            boolean = Type.boolean ()
            map x y = Type.apps' (var "Map") [x, y]
            nat = Type.nat ()
            float = Type.float ()
            text = Type.text ()
         in [ ((), v "avro.schema.DefaultValue.DefaultBytes", schema `arr` bytes `arr` defaultValue),
              ((), v "avro.schema.DefaultValue.DefaultFixed", schema `arr` bytes `arr` defaultValue),
              ((), v "avro.schema.DefaultValue.DefaultArray", list defaultValue `arr` defaultValue),
              ((), v "avro.schema.DefaultValue.DefaultInt32", schema `arr` int `arr` defaultValue),
              ((), v "avro.schema.DefaultValue.DefaultInt64", schema `arr` int `arr` defaultValue),
              ((), v "avro.schema.DefaultValue.DefaultEnum", schema `arr` nat `arr` text `arr` defaultValue),
              ((), v "avro.schema.DefaultValue.DefaultBoolean", boolean `arr` defaultValue),
              ((), v "avro.schema.DefaultValue.DefaultNull", defaultValue),
              ((), v "avro.schema.DefaultValue.DefaultMap", map text defaultValue `arr` defaultValue),
              ((), v "avro.schema.DefaultValue.DefaultString", schema `arr` text `arr` defaultValue),
              ((), v "avro.schema.DefaultValue.DefaultFloat", schema `arr` float `arr` defaultValue),
              ((), v "avro.schema.DefaultValue.DefaultDouble", schema `arr` float `arr` defaultValue),
              ((), v "avro.schema.DefaultValue.DefaultRecord", schema `arr` map text defaultValue `arr` defaultValue),
              ((), v "avro.schema.DefaultValue.DefaultUnion", list schema `arr` schema `arr` defaultValue `arr` defaultValue)
            ]

    avroSchema =
      DataDeclaration
        (Unique "i3t59pro14podvneaqa6775kdt7j10h0")
        ()
        []
        let schema = var "avro.schema.Schema"
            fixed = var "avro.schema.AvroFixed"
            enum = var "avro.schema.AvroEnum"
            defaultValue = var "avro.schema.DefaultValue"
            logicalString = var "avro.schema.LogicalStringType"
            logicalInt = var "avro.schema.LogicalIntType"
            logicalLong = var "avro.schema.LogicalLongType"
            logicalBytes = var "avro.schema.LogicalBytesType"
            typeName = var "avro.schema.TypeName"
            avroRecord = var "avro.schema.AvroRecord"
            opt = Type.app' (var "Optional")
            map x y = Type.apps' (var "Map") [x, y]
            list = Type.app () (Type.list ())
            text = Type.text ()
         in [ ((), v "avro.schema.Schema.Fixed", fixed `arr` schema),
              ((), v "avro.schema.Schema.Enum", enum `arr` schema),
              ((), v "avro.schema.Schema.Map", schema `arr` map text defaultValue `arr` schema),
              ((), v "avro.schema.Schema.Int", opt logicalInt `arr` schema),
              ((), v "avro.schema.Schema.Union", list schema `arr` schema),
              ((), v "avro.schema.Schema.Long", opt logicalLong `arr` schema),
              ((), v "avro.schema.Schema.NamedType", typeName `arr` schema),
              ((), v "avro.schema.Schema.Bytes", opt logicalBytes `arr` schema),
              ((), v "avro.schema.Schema.Array", schema `arr` list defaultValue `arr` schema),
              ((), v "avro.schema.Schema.Null", schema),
              ((), v "avro.schema.Schema.Boolean", schema),
              ((), v "avro.schema.Schema.Float", schema),
              ((), v "avro.schema.Schema.Double", schema),
              ((), v "avro.schema.Schema.String", opt logicalString `arr` schema),
              ((), v "avro.schema.Schema.Record", avroRecord `arr` schema)
            ]

    avroReadFloat =
      DataDeclaration
        (Unique "j9mrc4pakmmu0onnf886qmvporqki488")
        ()
        []
        let readFloat = var "avro.schema.deconflicted.ReadFloat"
         in [ ((), v "avro.schema.deconflicted.ReadFloat.FloatFromInt32", readFloat),
              ((), v "avro.schema.deconflicted.ReadFloat.FloatFromInt64", readFloat),
              ((), v "avro.schema.deconflicted.ReadFloat.ReadFloat", readFloat)
            ]

    avroFixed =
      DataDeclaration
        (Unique "kh3gqke6sgjumbug1fmnlbq19gobs4b4")
        ()
        []
        let fixed = var "avro.schema.AvroFixed"
            typeName = var "avro.schema.TypeName"
            logicalFixed = var "avro.schema.LogicalFixedType"
            opt = Type.app' (var "Optional")
            list = Type.app () (Type.list ())
            text = Type.text ()
            nat = Type.nat ()
         in [((), v "avro.schema.AvroFixed.AvroFixed", typeName `arr` opt text `arr` list typeName `arr` nat `arr` opt logicalFixed `arr` fixed)]

    avroReadDouble =
      DataDeclaration
        (Unique "od9j87j4ga5nss8vtqjii34l4fajkji8")
        ()
        []
        let readDouble = var "avro.schema.deconflicted.ReadDouble"
         in [ ((), v "avro.schema.deconflicted.ReadDouble.DoubleFromInt32", readDouble),
              ((), v "avro.schema.deconflicted.ReadDouble.DoubleFromInt64", readDouble),
              ((), v "avro.schema.deconflicted.ReadDouble.DoubleFromFloat", readDouble),
              ((), v "avro.schema.deconflicted.ReadDouble.ReadDouble", readDouble)
            ]

    avroTypeName =
      DataDeclaration
        (Unique "pbiqt5r9j4dmctk9kafv1ta3okd3nt24")
        ()
        []
        let typeName = var "avro.schema.TypeName"
            list = Type.app () (Type.list ())
            text = Type.text ()
         in [((), v "avro.schema.TypeName.TypeName", text `arr` list text `arr` typeName)]

    avroReadRecord =
      DataDeclaration
        (Unique "iinf3n46okum42uqpgdcob58ngjrbile")
        ()
        []
        let readRecord = var "avro.schema.deconflicted.ReadRecord"
            typeName = var "avro.schema.TypeName"
            readField = var "avro.schema.deconflicted.ReadField"
            opt = Type.app' (var "Optional")
            list = Type.app () (Type.list ())
            text = Type.text ()
         in [((), v "avro.schema.deconflicted.ReadRecord.ReadRecord", typeName `arr` list typeName `arr` opt text `arr` list readField `arr` readRecord)]

    avroReadField =
      DataDeclaration
        (Unique "nrdvcs03lkqbm5j8f6qcg77eu9vqs1b1")
        ()
        []
        let readField = var "avro.schema.deconflicted.ReadField"
            fieldStatus = var "avro.schema.deconflicted.FieldStatus"
            readSchema = var "avro.schema.deconflicted.ReadSchema"
            order = var "avro.schema.Order"
            defaultValue = var "avro.schema.DefaultValue"
            opt = Type.app' (var "Optional")
            list = Type.app () (Type.list ())
            text = Type.text ()
         in [((), v "avro.schema.deconflicted.ReadField.ReadField", text `arr` list text `arr` opt text `arr` readSchema `arr` fieldStatus `arr` opt order `arr` opt defaultValue `arr` readField)]

    avroOrder =
      DataDeclaration
        (Unique "f2lv9b1fdffaouhepfu1g02s7dbi804e")
        ()
        []
        let avroOrder = var "avro.schema.Order"
         in [ ((), v "avro.schema.Order.Ascending", avroOrder),
              ((), v "avro.schema.Order.Descending", avroOrder),
              ((), v "avro.schema.Order.Ignore", avroOrder)
            ]

    avroFieldStatus =
      DataDeclaration
        (Unique "o0hrihuckkskb90jvob2mpiclv8ocj1a")
        ()
        []
        let fieldStatus = var "avro.schema.deconflicted.FieldStatus"
            defaultValue = var "avro.schema.DefaultValue"
            nat = Type.nat ()
         in [ ((), v "avro.schema.deconflicted.FieldStatus.AsIs", nat `arr` fieldStatus),
              ((), v "avro.schema.deconflicted.FieldStatus.Ignored", fieldStatus),
              ((), v "avro.schema.deconflicted.FieldStatus.Defaulted", nat `arr` defaultValue `arr` fieldStatus)
            ]

    avroField =
      DataDeclaration
        (Unique "hdtsj889pvvvhmn482d9ipnlchjl16ua")
        ()
        []
        let avroField = var "avro.schema.AvroField"
            avroSchema = var "avro.schema.Schema"
            avroOrder = var "avro.schema.Order"
            defaultValue = var "avro.schema.DefaultValue"
            opt = Type.app' (var "Optional")
            list = Type.app () (Type.list ())
            text = Type.text ()
         in [((), v "avro.schema.AvroField.AvroField", text `arr` opt text `arr` avroSchema `arr` list text `arr` opt avroOrder `arr` opt defaultValue `arr` avroField)]

    avroEnum =
      DataDeclaration
        (Unique "h3c1uc2gnhv0q2k0sqnsc28m3vvpdf0b")
        ()
        []
        let avroEnum = var "avro.schema.AvroEnum"
            typeName = var "avro.schema.TypeName"
            opt = Type.app' (var "Optional")
            list = Type.app () (Type.list ())
            text = Type.text ()
         in [((), v "avro.schema.AvroEnum.AvroEnum", typeName `arr` opt text `arr` list typeName `arr` list text `arr` opt text `arr` avroEnum)]

    avroRecord =
      DataDeclaration
        (Unique "caboag889sudpeepia4436ae50346077")
        ()
        []
        let avroRecord = var "avro.schema.AvroRecord"
            typeName = var "avro.schema.TypeName"
            avroField = var "avro.schema.AvroField"
            opt = Type.app' (var "Optional")
            list = Type.app () (Type.list ())
            text = Type.text ()
         in [((), v "avro.schema.AvroRecord.AvroRecord", typeName `arr` opt text `arr` list typeName `arr` list avroField `arr` avroRecord)]

    avroReadLong =
      DataDeclaration
        (Unique "bm61dmnipg0geplp9g3avrd64fshqofk")
        ()
        []
        let readLong = var "avro.schema.deconflicted.ReadLong"
         in [ ((), v "avro.schema.deconflicted.ReadLong.LongFromInt32", readLong),
              ((), v "avro.schema.deconflicted.ReadLong.ReadLong", readLong)
            ]

    avroReadSchema =
      DataDeclaration
        (Unique "h7uslccb5g625skvdq6mpp8hjvl645kt")
        ()
        []
        let schema = var "avro.schema.deconflicted.ReadSchema"
            logicalString = var "avro.schema.LogicalStringType"
            logicalBytes = var "avro.schema.LogicalBytesType"
            logicalInt = var "avro.schema.LogicalIntType"
            logicalLong = var "avro.schema.LogicalLongType"
            defaultValue = var "avro.schema.DefaultValue"
            readFloat = var "avro.schema.deconflicted.ReadFloat"
            avroFixed = var "avro.schema.AvroFixed"
            readDouble = var "avro.schema.deconflicted.ReadDouble"
            typeName = var "avro.schema.TypeName"
            readRecord = var "avro.schema.deconflicted.ReadRecord"
            avroEnum = var "avro.schema.AvroEnum"
            readLong = var "avro.schema.deconflicted.ReadLong"
            tup x y = Type.apps' (var "Tuple") [x, y]
            pair x y = tup x (tup y (var "Unit"))
            opt = Type.app' (var "Optional")
            map x y = Type.apps' (var "Map") [x, y]
            list = Type.app () (Type.list ())
            text = Type.text ()
            nat = Type.nat ()
         in [ ((), v "avro.schema.deconflicted.ReadSchema.Null", schema),
              ((), v "avro.schema.deconflicted.ReadSchema.Boolean", schema),
              ((), v "avro.schema.deconflicted.ReadSchema.String", opt logicalString `arr` schema),
              ((), v "avro.schema.deconflicted.ReadSchema.Float", readFloat `arr` schema),
              ((), v "avro.schema.deconflicted.ReadSchema.Fixed", avroFixed `arr` schema),
              ((), v "avro.schema.deconflicted.ReadSchema.Double", readDouble `arr` schema),
              ((), v "avro.schema.deconflicted.ReadSchema.Bytes", opt logicalBytes `arr` schema),
              ((), v "avro.schema.deconflicted.ReadSchema.NamedType", typeName `arr` schema),
              ((), v "avro.schema.deconflicted.ReadSchema.Int", opt logicalInt `arr` schema),
              ((), v "avro.schema.deconflicted.ReadSchema.Long", readLong `arr` opt logicalLong `arr` schema),
              ((), v "avro.schema.deconflicted.ReadSchema.Map", schema `arr` map text defaultValue `arr` schema),
              ((), v "avro.schema.deconflicted.ReadSchema.Record", readRecord `arr` schema),
              ((), v "avro.schema.deconflicted.ReadSchema.FreeUnion", nat `arr` schema `arr` schema),
              ((), v "avro.schema.deconflicted.ReadSchema.Enum", avroEnum `arr` schema),
              ((), v "avro.schema.deconflicted.ReadSchema.Union", list (pair nat schema) `arr` schema),
              ((), v "avro.schema.deconflicted.ReadSchema.Array", schema `arr` list defaultValue `arr` schema)
            ]

builtinEffectDecls :: [(Symbol, Reference.Id, DD.EffectDeclaration Symbol ())]
builtinEffectDecls =
  case hashDataDecls $ Map.fromList [(v "Exception", exception)] of
    Right a -> over _3 DD.EffectDeclaration <$> a
    Left e -> error $ "builtinEffectDecls: " <> show e
  where
    v = Var.named
    var name = Type.var () (v name)
    arr = Type.arrow'
    self t = Type.cleanupAbilityLists $ Type.effect () [var "Exception"] t
    exception =
      DataDeclaration
        Structural
        ()
        []
        [ ((), v "Exception.raise", Type.forAll () (v "x") (failureType () `arr` self (var "x")))
        ]

pattern UnitRef :: Reference
pattern UnitRef <- (unUnitRef -> True)

pattern PairRef :: Reference
pattern PairRef <- (unPairRef -> True)

pattern EitherRef :: Reference
pattern EitherRef <- ((==) eitherRef -> True)

pattern OptionalRef :: Reference
pattern OptionalRef <- (unOptionalRef -> True)

pattern OptionalNone' :: ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern OptionalNone' <- Term.Constructor' (ConstructorReference OptionalRef ((==) noneId -> True))

pattern OptionalSome' ::
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a ->
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern OptionalSome' d <- Term.App' (Term.Constructor' (ConstructorReference OptionalRef ((==) someId -> True))) d

pattern TupleType' :: (Var v) => [Type v a] -> Type v a
pattern TupleType' ts <- (unTupleType -> Just ts)

pattern TupleTerm' :: [Term2 vt at ap v a] -> Term2 vt at ap v a
pattern TupleTerm' xs <- (unTupleTerm -> Just xs)

pattern TuplePattern :: [Pattern.Pattern loc] -> Pattern.Pattern loc
pattern TuplePattern ps <- (unTuplePattern -> Just ps)

pattern EitherLeft' :: Term2 vt at ap v a -> Term2 vt at ap v a
pattern EitherLeft' tm <- (unLeftTerm -> Just tm)

pattern EitherRight' :: Term2 vt at ap v a -> Term2 vt at ap v a
pattern EitherRight' tm <- (unRightTerm -> Just tm)

pattern EitherLeftId :: ConstructorId
pattern EitherLeftId <- ((==) eitherLeftId -> True)

pattern EitherRightId :: ConstructorId
pattern EitherRightId <- ((==) eitherRightId -> True)

unLeftTerm,
  unRightTerm ::
    Term.Term2 vt at ap v a ->
    Maybe (Term.Term2 vt at ap v a)
unRightTerm t = case t of
  Term.App' (Term.Constructor' (ConstructorReference EitherRef EitherRightId)) tm ->
    Just tm
  _ -> Nothing
unLeftTerm t = case t of
  Term.App' (Term.Constructor' (ConstructorReference EitherRef EitherLeftId)) tm ->
    Just tm
  _ -> Nothing

-- some pattern synonyms to make pattern matching on some of these constants more pleasant
pattern DocRef :: Reference
pattern DocRef <- ((== docRef) -> True)

pattern DocJoin ::
  Seq (ABT.Term (Term.F typeVar typeAnn patternAnn) v a) ->
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern DocJoin segs <- Term.App' (Term.Constructor' (ConstructorReference DocRef DocJoinId)) (Term.List' segs)

pattern DocBlob :: Text -> ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern DocBlob txt <- Term.App' (Term.Constructor' (ConstructorReference DocRef DocBlobId)) (Term.Text' txt)

pattern DocLink ::
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a ->
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern DocLink link <- Term.App' (Term.Constructor' (ConstructorReference DocRef DocLinkId)) link

pattern DocSource ::
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a ->
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern DocSource link <- Term.App' (Term.Constructor' (ConstructorReference DocRef DocSourceId)) link

pattern DocSignature ::
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a ->
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern DocSignature link <- Term.App' (Term.Constructor' (ConstructorReference DocRef DocSignatureId)) link

pattern DocEvaluate ::
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a ->
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern DocEvaluate link <- Term.App' (Term.Constructor' (ConstructorReference DocRef DocEvaluateId)) link

pattern Doc :: ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern Doc <- Term.App' (Term.Constructor' (ConstructorReference DocRef _)) _

pattern DocSignatureId :: ConstructorId
pattern DocSignatureId <- ((== docSignatureId) -> True)

pattern DocBlobId :: ConstructorId
pattern DocBlobId <- ((== docBlobId) -> True)

pattern DocLinkId :: ConstructorId
pattern DocLinkId <- ((== docLinkId) -> True)

pattern DocSourceId :: ConstructorId
pattern DocSourceId <- ((== docSourceId) -> True)

pattern DocEvaluateId :: ConstructorId
pattern DocEvaluateId <- ((== docEvaluateId) -> True)

pattern DocJoinId :: ConstructorId
pattern DocJoinId <- ((== docJoinId) -> True)

pattern LinkTermId :: ConstructorId
pattern LinkTermId <- ((== linkTermId) -> True)

pattern LinkTypeId :: ConstructorId
pattern LinkTypeId <- ((== linkTypeId) -> True)

pattern LinkRef :: Reference
pattern LinkRef <- ((== linkRef) -> True)

pattern LinkTerm :: ABT.Term (Term.F typeVar typeAnn patternAnn) v a -> ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern LinkTerm tm <- Term.App' (Term.Constructor' (ConstructorReference LinkRef LinkTermId)) tm

pattern LinkType ::
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a ->
  ABT.Term (Term.F typeVar typeAnn patternAnn) v a
pattern LinkType ty <- Term.App' (Term.Constructor' (ConstructorReference LinkRef LinkTypeId)) ty

unitType,
  pairType,
  optionalType,
  testResultListType,
  eitherType,
  ioErrorType,
  fileModeType,
  filePathType,
  bufferModeType,
  seekModeType,
  stdHandleType,
  failureType,
  thunkArgType,
  exceptionType ::
    (Ord v) => a -> Type v a
unitType a = Type.ref a unitRef
-- used for the type of the argument to force a thunk
thunkArgType = unitType
pairType a = Type.ref a pairRef
testResultListType a = Type.app a (Type.list a) (Type.ref a testResultRef)
optionalType a = Type.ref a optionalRef
eitherType a = Type.ref a eitherRef
ioErrorType a = Type.ref a ioErrorRef
fileModeType a = Type.ref a fileModeRef
filePathType a = Type.ref a filePathRef
bufferModeType a = Type.ref a bufferModeRef
seekModeType a = Type.ref a seekModeRef
stdHandleType a = Type.ref a stdHandleRef
failureType a = Type.ref a failureRef
exceptionType a = Type.ref a exceptionRef

tlsSignedCertType :: (Var v) => a -> Type v a
tlsSignedCertType a = Type.ref a tlsSignedCertRef

unitTerm :: (Var v) => a -> Term2 vt at ap v a
unitTerm ann = Term.constructor ann (ConstructorReference unitRef 0)

tupleConsTerm ::
  (Ord v, Semigroup a) =>
  Term2 vt at ap v a ->
  Term2 vt at ap v a ->
  Term2 vt at ap v a
tupleConsTerm hd tl =
  Term.apps' (Term.constructor (ABT.annotation hd) (ConstructorReference pairRef 0)) [hd, tl]

tupleTerm :: (Var v, Monoid a) => [Term2 vt at ap v a] -> Term2 vt at ap v a
tupleTerm = foldr tupleConsTerm (unitTerm mempty)

-- delayed terms are just lambdas that take a single `()` arg
-- `force` calls the function
forceTerm :: (Var v) => a -> a -> Term v a -> Term v a
forceTerm a au e = Term.app a e (unitTerm au)

delayTerm :: (Var v) => a -> a -> Term v a -> Term v a
delayTerm spanAnn argAnn = Term.lam spanAnn (argAnn, Var.typed Var.Delay)

unTupleTerm ::
  Term.Term2 vt at ap v a ->
  Maybe [Term.Term2 vt at ap v a]
unTupleTerm t = case t of
  Term.Apps' (Term.Constructor' (ConstructorReference PairRef 0)) [fst, snd] ->
    (fst :) <$> unTupleTerm snd
  Term.Constructor' (ConstructorReference UnitRef 0) -> Just []
  _ -> Nothing

unTupleType :: (Var v) => Type v a -> Maybe [Type v a]
unTupleType t = case t of
  Type.Apps' (Type.Ref' PairRef) [fst, snd] -> (fst :) <$> unTupleType snd
  Type.Ref' UnitRef -> Just []
  _ -> Nothing

unTuplePattern :: Pattern.Pattern loc -> Maybe [Pattern.Pattern loc]
unTuplePattern p = case p of
  Pattern.Constructor _ (ConstructorReference PairRef 0) [fst, snd] -> (fst :) <$> unTuplePattern snd
  Pattern.Constructor _ (ConstructorReference UnitRef 0) [] -> Just []
  _ -> Nothing

unUnitRef, unPairRef, unOptionalRef :: Reference -> Bool
unUnitRef = (== unitRef)
unPairRef = (== pairRef)
unOptionalRef = (== optionalRef)
