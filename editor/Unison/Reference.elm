module Unison.Reference where

import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder (Decoder)
import String
import Unison.Hash (Hash)
import Unison.Hash as H

data Reference
  = Builtin String
  | Derived Hash

decode : Decoder Reference
decode = Decoder.union' <| \t ->
  if | t == "Builtin" -> Decoder.map Builtin Decoder.string
     | t == "Derived" -> Decoder.map Derived H.decode
     | otherwise -> Decoder.fail ("unknown tag: " ++ t)

encode : Encoder Reference
encode r = case r of
  Builtin b -> Encoder.tag' "Builtin" Encoder.string b
  Derived h -> Encoder.tag' "Derived" H.encode h

toString : Reference -> String
toString r = case r of
  Builtin s -> "builtin:" ++ s
  Derived b -> "derived:" ++ b

fromString : String -> Maybe Reference
fromString s =
  if | String.startsWith "builtin:" s -> Just (Builtin (String.dropLeft (String.length "builtin:") s))
     | String.startsWith "derived:" s -> Just (Derived (String.dropLeft (String.length "derived:") s))
     | otherwise -> Nothing
