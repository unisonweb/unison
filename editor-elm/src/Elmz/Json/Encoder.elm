module Elmz.Json.Encoder where

import List
import Maybe
import Json.Encode as J
import Dict as M
import Set as S

type alias Encoder a = a -> J.Value

render : Encoder a -> a -> String
render ja a = J.encode 0 (ja a)

contramap : (a -> b) -> Encoder b -> Encoder a
contramap f j = j << f

unit : J.Value -> Encoder a
unit v _ = v

string : Encoder String
string = J.string

null : Encoder a
null _ = J.null

float : Encoder Float
float = J.float

int : Encoder Int
int n = J.float (toFloat n)

optional : Encoder a -> Encoder (Maybe a)
optional ja oa = Maybe.withDefault J.null (Maybe.map ja oa)

bool : Encoder Bool
bool = J.bool

list : Encoder a -> Encoder (List a)
list f vs = J.list (List.map f vs)

emptyArray : Encoder a
emptyArray _ = J.list []

set : Encoder comparable -> Encoder (S.Set comparable)
set a = list a << S.toList

dict : Encoder comparable -> Encoder v -> Encoder (M.Dict comparable v)
dict k v = list (tuple2 k v) << M.toList

object : Encoder v -> Encoder (M.Dict String v)
object v =
  M.map (\k val -> v val) >> M.toList >> J.object

object2 : (String, Encoder v1)
       -> (String, Encoder v2)
       -> Encoder (v1,v2)
object2 (k1,e) (k2,e2) (v1,v2) =
  J.object [(k1, e v1), (k2, e2 v2)]

object3 : (String, Encoder v1)
       -> (String, Encoder v2)
       -> (String, Encoder v3)
       -> Encoder (v1,v2,v3)
object3 (k1,e) (k2,e2) (k3,e3) (v1,v2,v3) =
  J.object [(k1, e v1), (k2, e2 v2), (k3, e3 v3)]

object4 : (String, Encoder v1)
       -> (String, Encoder v2)
       -> (String, Encoder v3)
       -> (String, Encoder v4)
       -> Encoder (v1,v2,v3,v4)
object4 (k1,e) (k2,e2) (k3,e3) (k4, e4) (v1,v2,v3,v4) =
  J.object [(k1, e v1), (k2, e2 v2), (k3, e3 v3), (k4, e4 v4)]

tuple1 : Encoder a -> Encoder a
tuple1 ea a = J.list [ea a]

tuple2 : Encoder a -> Encoder b -> Encoder (a,b)
tuple2 a b p = J.list [a (fst p), b (snd p)]

tuple3 : Encoder a -> Encoder b -> Encoder c -> Encoder (a,b,c)
tuple3 ja jb jc p = case p of
  (a,b,c) -> J.list [ja a, jb b, jc c]

tuple4 : Encoder a -> Encoder b -> Encoder c -> Encoder d -> Encoder (a,b,c,d)
tuple4 ja jb jc jd p = case p of
  (a,b,c,d) -> J.list [ja a, jb b, jc c, jd d]

tuple5 : Encoder a
      -> Encoder b
      -> Encoder c
      -> Encoder d
      -> Encoder e
      -> Encoder (a,b,c,d,e)
tuple5 ja jb jc jd je p = case p of
  (a,b,c,d,e) -> J.list [ja a, jb b, jc c, jd d, je e]

tag : String -> String -> String -> Encoder a -> Encoder a
tag tagKey contentsKey tagValue j a =
  J.object [(tagKey, J.string tagValue), (contentsKey, j a)]

tag' : String -> Encoder a -> Encoder a
tag' = tag "tag" "contents"

tagProduct : String -> Encoder a -> Encoder a
tagProduct t encoder a = J.list [ J.string t, encoder a ]

product0 : Encoder ()
product0 _ = J.list []
