{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Util.Avro where

import Data.Avro
import Data.Avro.Deriving (deriveAvroFromByteString, r)
import Data.ByteString.Lazy
import Data.Tagged
import qualified Unison.Util.Avro2 as A2

deriveAvroFromByteString
  [r|
{
  "name": "A",
  "type": "record",
  "fields": [
    { "name": "name", "type": "string" },
    { "name": "age", "type": "int" }
  ]
}
|]

a :: A
a = A "one" 23

a2 :: A2.A
a2 = A2.A "two" (Just 93) (Just 555)

aToA2 :: Either String A2.A
aToA2 =
  readAvro (schema @A) (encodeValue a)

a2ToA :: Either String A
a2ToA =
  readAvro (schema @A2.A) (encodeValue a2)

readAvro ::
  forall clientType serverType.
  (FromAvro serverType, HasAvroSchema serverType) =>
  Tagged clientType Schema ->
  ByteString ->
  Either String serverType
readAvro (Tagged clientSchema) bytes = do
  readSchema <- deconflict clientSchema serverSchema
  decodeValueWithSchema readSchema bytes
  where
    (Tagged serverSchema) = schema @serverType
