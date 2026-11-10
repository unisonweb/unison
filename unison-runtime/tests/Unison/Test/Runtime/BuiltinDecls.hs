-- | The runtime's constants for the builtin declarations
-- (Unison.Runtime.Builtin.Decls) agree with their sources. If this fails,
-- run unison-runtime/scripts/gen-builtin-decls.sh.
module Unison.Test.Runtime.BuiltinDecls (test) where

import EasyTest
import Unison.Builtin qualified as B
import Unison.Builtin.Decls qualified as D
import Unison.DataDeclaration qualified as DD
import Unison.Reference qualified as R
import Unison.Runtime.Builtin.Decls qualified as C
import Unison.Var qualified as Var

test :: Test ()
test =
  scope "builtin-decls" . tests $
    [ scope "references" . tests $
        [ same "unitRef" C.unitRef D.unitRef,
          same "pairRef" C.pairRef D.pairRef,
          same "optionalRef" C.optionalRef D.optionalRef,
          same "eitherRef" C.eitherRef D.eitherRef,
          same "testResultRef" C.testResultRef D.testResultRef,
          same "linkRef" C.linkRef D.linkRef,
          same "docRef" C.docRef D.docRef,
          same "ioErrorRef" C.ioErrorRef D.ioErrorRef,
          same "stdHandleRef" C.stdHandleRef D.stdHandleRef,
          same "failureRef" C.failureRef D.failureRef,
          same "ioFailureRef" C.ioFailureRef D.ioFailureRef,
          same "tlsFailureRef" C.tlsFailureRef D.tlsFailureRef,
          same "arrayFailureRef" C.arrayFailureRef D.arrayFailureRef,
          same "cryptoFailureRef" C.cryptoFailureRef D.cryptoFailureRef,
          same "exceptionRef" C.exceptionRef D.exceptionRef,
          same "isPropagatedRef" C.isPropagatedRef D.isPropagatedRef,
          same "isTestRef" C.isTestRef D.isTestRef,
          same "runtimeFailureRef" C.runtimeFailureRef D.runtimeFailureRef,
          same "arithmeticFailureRef" C.arithmeticFailureRef D.arithmeticFailureRef,
          same "miscFailureRef" C.miscFailureRef D.miscFailureRef,
          same "stmFailureRef" C.stmFailureRef D.stmFailureRef,
          same "threadKilledFailureRef" C.threadKilledFailureRef D.threadKilledFailureRef,
          same "asyncCancelledFailureRef" C.asyncCancelledFailureRef D.asyncCancelledFailureRef,
          same "outOfBoundsRef" C.outOfBoundsRef D.outOfBoundsRef,
          same "fileModeRef" C.fileModeRef D.fileModeRef,
          same "bufferModeRef" C.bufferModeRef D.bufferModeRef,
          same "seekModeRef" C.seekModeRef D.seekModeRef,
          same "seqViewRef" C.seqViewRef D.seqViewRef,
          same "mapRef" C.mapRef D.mapRef,
          same "setRef" C.setRef D.setRef,
          same "jsonRef" C.jsonRef D.jsonRef,
          same "parseErrorRef" C.parseErrorRef D.parseErrorRef
        ],
      scope "constructor numbers" . tests $
        [ same "noneId" C.noneId D.noneId,
          same "someId" C.someId D.someId,
          same "okConstructorId" C.okConstructorId D.okConstructorId,
          same "failConstructorId" C.failConstructorId D.failConstructorId,
          same "eitherRightId" C.eitherRightId D.eitherRightId,
          same "eitherLeftId" C.eitherLeftId D.eitherLeftId,
          same "bufferModeNoBufferingId" C.bufferModeNoBufferingId D.bufferModeNoBufferingId,
          same "bufferModeLineBufferingId" C.bufferModeLineBufferingId D.bufferModeLineBufferingId,
          same "bufferModeBlockBufferingId" C.bufferModeBlockBufferingId D.bufferModeBlockBufferingId,
          same "bufferModeSizedBlockBufferingId" C.bufferModeSizedBlockBufferingId D.bufferModeSizedBlockBufferingId,
          same "seqViewEmpty" C.seqViewEmpty D.seqViewEmpty,
          same "seqViewElem" C.seqViewElem D.seqViewElem,
          same "mapTip" C.mapTip D.mapTip,
          same "mapBin" C.mapBin D.mapBin,
          same "setWrap" C.setWrap D.setWrap,
          same "jsonNull" C.jsonNull D.jsonNull,
          same "jsonBool" C.jsonBool D.jsonBool,
          same "jsonObj" C.jsonObj D.jsonObj,
          same "jsonNum" C.jsonNum D.jsonNum,
          same "jsonText" C.jsonText D.jsonText,
          same "jsonArr" C.jsonArr D.jsonArr,
          same "jsonParseError" C.jsonParseError D.jsonParseError,
          same "fileModeReadId" C.fileModeReadId D.fileModeReadId,
          same "fileModeWriteId" C.fileModeWriteId D.fileModeWriteId,
          same "fileModeAppendId" C.fileModeAppendId D.fileModeAppendId,
          same "fileModeReadWriteId" C.fileModeReadWriteId D.fileModeReadWriteId,
          same "seekModeAbsoluteId" C.seekModeAbsoluteId D.seekModeAbsoluteId,
          same "seekModeRelativeId" C.seekModeRelativeId D.seekModeRelativeId,
          same "seekModeEndId" C.seekModeEndId D.seekModeEndId,
          same "stdInId" C.stdInId D.stdInId,
          same "stdOutId" C.stdOutId D.stdOutId,
          same "stdErrId" C.stdErrId D.stdErrId,
          same "exceptionRaiseId" C.exceptionRaiseId D.exceptionRaiseId
        ],
      same "pairCtorRef" C.pairCtorRef D.pairCtorRef,
      same "unitCtorRef" C.unitCtorRef D.unitCtorRef,
      same "builtinTypeRefs" C.builtinTypeRefs [r | (_, r) <- B.builtinTypes],
      same "builtinDataDeclRefs" C.builtinDataDeclRefs [R.DerivedId r | (_, r, _) <- D.builtinDataDecls],
      same "builtinEffectDeclRefs" C.builtinEffectDeclRefs [R.DerivedId r | (_, r, _) <- D.builtinEffectDecls],
      same "builtinConstructorNames" C.builtinConstructorNames $
        [(R.DerivedId r, map Var.name (DD.constructorVars dd)) | (_, r, dd) <- D.builtinDataDecls]
          ++ [(R.DerivedId r, map Var.name (DD.constructorVars (DD.toDataDecl ed))) | (_, r, ed) <- D.builtinEffectDecls]
    ]
  where
    same :: (Eq a, Show a) => String -> a -> a -> Test ()
    same name generated source =
      scope name $
        if generated == source
          then ok
          else
            crash $
              "Unison.Runtime.Builtin.Decls is out of date: run unison-runtime/scripts/gen-builtin-decls.sh\n"
                ++ ("  generated: " ++ show generated ++ "\n")
                ++ ("  source:    " ++ show source)
