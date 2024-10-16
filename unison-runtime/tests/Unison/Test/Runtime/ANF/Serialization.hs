{-# LANGUAGE OverloadedStrings #-}

-- | Round trip tests for ANF serialization.
module Unison.Test.Runtime.ANF.Serialization (Unison.Test.Runtime.ANF.Serialization.test) where

import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Primitive.Array (Array)
import Data.Primitive.Array qualified as Array
import Data.Primitive.ByteArray (ByteArray)
import Data.Primitive.ByteArray qualified as ByteArray
import Data.Primitive.Types (Prim)
import Data.Serialize.Get (Get)
import Data.Serialize.Put (Put)
import Data.Text qualified as Text
import EasyTest qualified as EasyTest
import Hedgehog hiding (Rec, Test, test)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Unison.ConstructorReference
import Unison.ConstructorType qualified as CT
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.Referent qualified as Referent
import Unison.Runtime.ANF
import Unison.Runtime.ANF.Serialize
import Unison.Util.Bytes qualified as Util.Bytes
import Unison.Util.Text qualified as Util.Text

test :: EasyTest.Test ()
test =
  void . EasyTest.scope "anf.serialization" $ do
    success <-
      EasyTest.io $
        checkParallel $
          Group
            "roundtrip"
            [ ("value", valueRoundtrip)
            ]
    EasyTest.expect success

genWord64 :: Gen Word64
genWord64 = Gen.word64 (Range.linear 0 100)

genSmallText :: Gen Text
genSmallText = Gen.text (Range.linear 2 4) Gen.alphaNum

genUText :: Gen Util.Text.Text
genUText = Util.Text.pack . Text.unpack <$> genSmallText

genUBytes :: Gen Util.Bytes.Bytes
genUBytes = Util.Bytes.fromByteString <$> Gen.bytes (Range.linear 0 4)

-- This can generate invalid hashes, but that's not really an issue for testing serialization.
genHash :: Gen Hash
genHash = Hash.fromByteString <$> Gen.bytes (Range.singleton 32)

genReference :: Gen Reference.Reference
genReference =
  Gen.choice
    [ Reference.ReferenceBuiltin <$> genSmallText,
      Reference.ReferenceDerived <$> genRefId
    ]
  where
    genRefId :: Gen (Reference.Id' Hash)
    genRefId = Reference.Id <$> genHash <*> genWord64

genReferent :: Gen Referent.Referent
genReferent =
  Gen.choice
    [ Referent.Ref <$> genReference,
      Referent.Con <$> genConstructorReference <*> genConstructorType
    ]
  where
    genConstructorType = Gen.choice [pure CT.Data, pure CT.Effect]
    genConstructorReference = ConstructorReference <$> genReference <*> genWord64

genGroupRef :: Gen GroupRef
genGroupRef = GR <$> genReference <*> genWord64

genUBValue :: Gen UBValue
genUBValue =
  Gen.choice
    [ -- Unboxed values are no longer valid in ANF serialization.
      -- Left <$> genWord64,
      Right <$> genValue
    ]

genValList :: Gen ValList
genValList = Gen.list (Range.linear 0 4) genUBValue

genCont :: Gen Cont
genCont = do
  Gen.choice
    [ pure KE,
      Mark <$> genWord64 <*> Gen.list (Range.linear 0 4) genReference <*> Gen.map (Range.linear 0 4) ((,) <$> genReference <*> genValue) <*> genCont,
      Push <$> genWord64 <*> genWord64 <*> genGroupRef <*> genCont
    ]

genArray :: Range Int -> Gen a -> Gen (Array a)
genArray range gen =
  Array.arrayFromList <$> Gen.list range gen

genByteArray :: (Prim p) => Gen p -> Gen ByteArray
genByteArray genP = do
  ByteArray.byteArrayFromList <$> Gen.list (Range.linear 0 20) genP

genBLit :: Gen BLit
genBLit =
  Gen.choice
    [ Text <$> genUText,
      List <$> Gen.seq (Range.linear 0 4) genValue,
      TmLink <$> genReferent,
      TyLink <$> genReference,
      Bytes <$> genUBytes,
      Quote <$> genValue,
      -- Code is not yet included, generating valid ANF terms is complex.
      -- , Code <$> genCode
      BArr <$> genByteArray genWord64,
      Pos <$> genWord64,
      Neg <$> genWord64,
      Char <$> Gen.unicode,
      Float <$> Gen.double (Range.linearFrac 0 100),
      Arr <$> genArray (Range.linear 0 4) genValue
    ]

genValue :: Gen Value
genValue = Gen.sized \n -> do
  -- Limit amount of recursion to avoid infinitely deep values
  let gValList
        | n > 1 = Gen.small genValList
        | otherwise = pure []
  Gen.choice
    [ Partial <$> genGroupRef <*> gValList,
      Data <$> genReference <*> genWord64 <*> gValList,
      Cont <$> gValList <*> genCont,
      BLit <$> genBLit
    ]

valueRoundtrip :: Property
valueRoundtrip =
  getPutRoundtrip getValue putValue genValue

getPutRoundtrip :: (Eq a, Show a) => (Version -> Get a) -> (Version -> a -> Put) -> Gen a -> Property
getPutRoundtrip get put builder =
  property $ do
    v <- forAll builder
    version <- forAll versionToTest
    let bytes = runPutS (put version v)
    runGetS (get version) bytes === Right v
  where
    versionToTest = do
      Gen.choice
        [ Transfer <$> Gen.enum 4 valueVersion,
          Hash <$> Gen.enum 4 valueVersion
        ]
