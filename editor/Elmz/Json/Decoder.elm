module Elmz.Json.Decoder where

import Maybe (maybe)
import List as L
import String as S
import Either (..)
import Either as E
import Json as J
import Dict as M
import Set
import Set (Set)

type Msg = [String]
type Decoder a = J.Value -> Either Msg a

run : Decoder a -> J.Value -> Either String a
run p v = case p v of
  Left errs -> Left (S.join "\n" errs)
  Right a -> Right a

unit : a -> Decoder a
unit a _ = Right a

fail : String -> Decoder a
fail msg v = Left [msg]

scope : String -> Decoder a -> Decoder a
scope msg p v = case p v of
  Left stack -> Left (msg :: stack)
  a          -> a

bind : Decoder a -> (a -> Decoder b) -> Decoder b
bind p f v = case p v of
  Right a -> f a v
  Left e -> Left e

infixl 3 >>=
(>>=) : Decoder a -> (a -> Decoder b) -> Decoder b
a >>= f = bind a f

map : (a -> b) -> Decoder a -> Decoder b
map f p v = case p v of
  Left e -> Left e
  Right a -> Right (f a)

lift2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
lift2 f a b v = case (a v, b v) of
  (Left e, _) -> Left e
  (_, Left e) -> Left e
  (Right a, Right b) -> Right (f a b)

lift3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
lift3 f a b c v = case (a v, b v, c v) of
  (Left e, _, _) -> Left e
  (_, Left e, _) -> Left e
  (_, _, Left e) -> Left e
  (Right a, Right b, Right c) -> Right (f a b c)

apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply f a = lift2 (<|) f a

infixl 4 #
(#) : Decoder (a -> b) -> Decoder a -> Decoder b
f # a = apply f a

value : Decoder J.Value
value v = Right v

string : Decoder String
string = value >>= \v -> case v of
  J.String s -> unit s
  _ -> fail ("not a string: " ++ J.toString "" v)

int : Decoder Int
int = value >>= \v -> case v of
  J.Number v -> unit (floor v)
  _ -> fail ("not a number: " ++ J.toString "" v)

number : Decoder Float
number = value >>= \v -> case v of
  J.Number v -> unit v
  _ -> fail ("not a number: " ++ J.toString "" v)

object : Decoder v -> Decoder (M.Dict String v)
object v j = case j of
  J.Object dict -> traverseMap v dict
  _ -> Left ["not an object: " ++ J.toString "" j]

set : Decoder comparable -> Decoder (Set comparable)
set p = map Set.fromList (array p)

array : Decoder a -> Decoder [a]
array p v = case v of
  J.Array vs -> case E.partition (L.map p vs) of
    ([], results) -> Right results
    (h :: t, _) -> Left h
  _ -> Left ["not an array: " ++ J.toString "" v]

optional : Decoder a -> Decoder (Maybe a)
optional p v = case p v of
  Left err ->
    case v of J.Null -> Right Nothing
              _      -> Left err
  Right a -> Right (Just a)

key : String -> Decoder J.Value
key k v = case v of
  J.Object dict ->
    let msg = Left ["key not found: " ++ k ++ " in " ++ show dict]
    in maybe msg Right (M.get k dict)
  _ -> Left ["cannot lookup key " ++ k ++ " in: " ++ J.toString "" v]

index : Int -> Decoder J.Value
index ind v = case v of
  J.Array vs ->
    let msg = Left ["index not found: " ++ show ind ++ " in " ++ show vs]
    in maybe msg Right (safeIndex ind vs)
  _ -> Left ["cannot lookup index " ++ show ind ++ " in: " ++ J.toString "" v]

nest : Decoder J.Value -> Decoder a -> Decoder a
nest pv p v = case pv v of
  Left err -> Left err
  Right v -> p v

union : String -> String -> (String -> Decoder a) -> Decoder a
union tag contents f =
  nest (key tag) string >>= \t -> nest (key contents) (f t)

union' : (String -> Decoder a) -> Decoder a
union' = union "tag" "contents"

product2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
product2 f a b = lift2 f (nest (index 0) a) (nest (index 1) b)

product3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
product3 f a b c = lift3 f (nest (index 0) a) (nest (index 1) b) (nest (index 2) c)

tuple2 : Decoder a -> Decoder b -> Decoder (a,b)
tuple2 = product2 (,)

product5 : (a -> b -> c -> d -> e -> f)
        -> Decoder a
        -> Decoder b
        -> Decoder c
        -> Decoder d
        -> Decoder e
        -> Decoder f
product5 f a b c d e =
  unit f
    # nest (index 0) a
    # nest (index 1) b
    # nest (index 2) c
    # nest (index 3) d
    # nest (index 4) e

newtyped' : (a -> b) -> Decoder a -> Decoder b
newtyped' f p = union' (\_ -> map f p)

safeIndex : Int -> [a] -> Maybe a
safeIndex i xs = case drop i xs of
  [] -> Nothing
  h :: t -> Just h

-- private

traverseMap : (a -> Either e b) -> M.Dict comparable a -> Either e (M.Dict comparable b)
traverseMap f m =
  let strength (a, b) = either (Left << identity) (Right << (,) a) b
      eithers = M.map f m |> M.toList |> L.map strength
  in case E.partition eithers of
    (lefts, rights) -> if (isEmpty lefts)
                       then Right (M.fromList rights)
                       else Left (head lefts)
