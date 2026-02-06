{-# LANGUAGE OverloadedStrings #-}

-- | Round trip tests for ANF serialization.
module Unison.Test.Runtime.ANF.Serialization (Unison.Test.Runtime.ANF.Serialization.test) where

import Control.Monad.ST (ST, runST)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Primitive.Array (Array)
import Data.Primitive.Array qualified as Array
import Data.Primitive.ByteArray (ByteArray)
import Data.Primitive.ByteArray qualified as ByteArray
import Data.Primitive.Types (Prim)
import EasyTest qualified as EasyTest
import Hedgehog hiding (Rec, Test, test)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Numeric.Natural (Natural)
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Runtime.ANF
import Unison.Runtime.ANF.Serialize
import Unison.Runtime.Serialize.Get
import Unison.Test.Gen
import Unison.Util.Bytes qualified as Util.Bytes

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

genUBytes :: Gen Util.Bytes.Bytes
genUBytes = Util.Bytes.fromByteString <$> Gen.bytes (Range.linear 0 4)

genGroupRef :: Gen (GroupRef Reference)
genGroupRef = GR <$> genReference <*> genSmallWord64

genValList :: Gen (ValList Reference)
genValList = Gen.list (Range.linear 0 4) genValue

genCont :: Gen (Cont Reference)
genCont = do
  Gen.choice
    [ pure KE,
      Mark <$> genSmallWord64 <*> Gen.list (Range.linear 0 4) genReference <*> Gen.list (Range.linear 0 4) ((,) <$> genReference <*> genValue) <*> genCont,
      Push <$> genSmallWord64 <*> genSmallWord64 <*> genGroupRef <*> genCont
    ]

genArray :: Range Int -> Gen a -> Gen (Array a)
genArray range gen =
  Array.arrayFromList <$> Gen.list range gen

genByteArray :: (Prim p) => Gen p -> Gen ByteArray
genByteArray genP = do
  ByteArray.byteArrayFromList <$> Gen.list (Range.linear 0 20) genP

genBLit :: Gen (BLit Reference)
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
      BArr <$> genByteArray genSmallWord64,
      Pos <$> genSmallWord64,
      Neg <$> genSmallWord64,
      Char <$> Gen.unicode,
      Float <$> Gen.double (Range.linearFrac 0 100),
      Arr <$> genArray (Range.linear 0 4) genValue,
      BigInt <$> genInteger,
      BigNat <$> genNatural
    ]

-- | Generate arbitrary precision integers, including values larger than Int64
genInteger :: Gen Integer
genInteger =
  Gen.choice
    [ -- Small integers (fit in Int64)
      Gen.integral (Range.linearFrom 0 (-2 ^ (63 :: Int)) (2 ^ (63 :: Int) - 1)),
      -- Large positive integers (require arbitrary precision)
      Gen.integral (Range.linear (2 ^ (64 :: Int)) (2 ^ (256 :: Int))),
      -- Large negative integers
      Gen.integral (Range.linear (negate (2 ^ (256 :: Int))) (negate (2 ^ (64 :: Int))))
    ]

-- | Generate arbitrary precision naturals, including values larger than Word64
genNatural :: Gen Natural
genNatural =
  Gen.choice
    [ -- Small naturals (fit in Word64)
      Gen.integral (Range.linear 0 (2 ^ (64 :: Int) - 1)),
      -- Large naturals (require arbitrary precision)
      Gen.integral (Range.linear (2 ^ (64 :: Int)) (2 ^ (256 :: Int)))
    ]

genValue :: Gen (Value Reference)
genValue = Gen.sized \n -> do
  -- Limit amount of recursion to avoid infinitely deep values
  let gValList
        | n > 1 = Gen.small genValList
        | otherwise = pure []
  Gen.choice
    [ Partial <$> genGroupRef <*> gValList,
      Data <$> genReference <*> genSmallWord64 <*> gValList,
      Cont <$> gValList <*> genCont,
      BLit <$> genBLit
    ]

valueRoundtrip :: Property
valueRoundtrip =
  getPutRoundtrip (getValue . (,False)) putValue genValue

getPutRoundtrip ::
  (Eq a, Show a) =>
  (forall s. Version -> Get (ST s) a) ->
  (Version -> a -> Builder) ->
  Gen a ->
  Property
getPutRoundtrip get put builder =
  property $ do
    v <- forAll builder
    version <- forAll versionToTest
    let bytes = toStrict . toLazyByteString $ put version v
    runST (runGetCatch (get version) bytes) === Right v
  where
    versionToTest = do
      Gen.choice
        [ Transfer <$> Gen.enum 4 valueVersion,
          Hash <$> Gen.enum 4 valueVersion
        ]
