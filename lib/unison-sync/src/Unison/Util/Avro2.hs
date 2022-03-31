{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Unison.Util.Avro2 where

import Data.Avro.Deriving (deriveAvroFromByteString, r)

deriveAvroFromByteString
  [r|
{
  "name": "A",
  "type": "record",
  "fields": [
    { "name": "name", "type": "string" },
    { "name": "age", "type": ["null","int"] },
    { "name": "ssn", "type": ["null","int"], "default": null }
  ]
}
|]

-- >>> aToA2
-- Left "No default found for deconflicted field ssn"
-- >>> a2ToA
-- Right (A {aName = "two", aAge = 93})

-- a :: A
-- a = A "one" 23

-- a2 :: A2
-- a2 = A2 "one" 23 -- (Just 555)

-- aToA2 :: Either String A2
-- aToA2 =
--   readAvro (schema @A) (encodeValue a)

-- a2ToA :: Either String A
-- a2ToA =
--   readAvro (schema @A2) (encodeValue a2)

-- readAvro ::
--   forall clientType serverType.
--   (FromAvro serverType, HasAvroSchema serverType) =>
--   Tagged clientType Schema ->
--   ByteString ->
--   Either String serverType
-- readAvro (Tagged clientSchema) bytes = do
--   readSchema <- deconflict clientSchema serverSchema
--   decodeValueWithSchema readSchema bytes
--   where
--     (Tagged serverSchema) = schema @serverType
