module Unison.Reference where

import Dict (Dict)
import Dict
import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder (Decoder)
import List
import String
import Unison.Hash (Hash)
import Unison.Hash as H

type Reference
  = Builtin String
  | Derived Hash

type alias Key = String

decode : Decoder Reference
decode = Decoder.union' <| \t ->
  if | t == "Builtin" -> Decoder.map Builtin Decoder.string
     | t == "Derived" -> Decoder.map Derived H.decode
     | otherwise -> Decoder.fail ("unknown tag while decoding a Reference: " ++ t)

encode : Encoder Reference
encode r = case r of
  Builtin b -> Encoder.tag' "Builtin" Encoder.string b
  Derived h -> Encoder.tag' "Derived" H.encode h

decodeMap : Decoder v -> Decoder (Dict Key v)
decodeMap v =
  Decoder.list (Decoder.tuple2 decode v)
  |> Decoder.map (\kvs -> Dict.fromList (List.map (\(k,v) -> (toKey k,v)) kvs))

toKey : Reference -> Key
toKey r = case r of
  Builtin s -> s
  Derived b -> "#" ++ b
