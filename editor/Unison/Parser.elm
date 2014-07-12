module Unison.Parser where

import List as L
import String as S
import Either(..)
import Json as J
import Dict as M

type Msg = [String]
type Parser a = J.Value -> Either Msg a

run : Parser a -> J.Value -> Either String a
run p v = case p v of
  Left errs -> Left (S.join "\n" errs)
  Right a -> Right a

unit : a -> Parser a
unit a _ = Right a

fail : String -> Parser a
fail msg v = Left [msg]

scope : String -> Parser a -> Parser a
scope msg p v = case p v of
  Left stack -> Left (msg :: stack)
  a          -> a

bind : Parser a -> (a -> Parser b) -> Parser b
bind p f v = case p v of
  Right a -> f a v
  Left e -> Left e

infixl 3 >>=
(>>=) : Parser a -> (a -> Parser b) -> Parser b
a >>= f = bind a f

map : (a -> b) -> Parser a -> Parser b
map f p v = case p v of
  Left e -> Left e
  Right a -> Right (f a)

lift2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
lift2 f a b v = case (a v, b v) of
  (Left e, _) -> Left e
  (_, Left e) -> Left e
  (Right a, Right b) -> Right (f a b)

apply : Parser (a -> b) -> Parser a -> Parser b
apply f a = lift2 (<|) f a

infixl 4 #
(#) : Parser (a -> b) -> Parser a -> Parser b
f # a = apply f a

value : Parser J.Value
value v = Right v

string : Parser String
string = value >>= \v -> case v of
  J.String s -> unit s
  _ -> fail ("not a string: " ++ J.toString "" v)

int : Parser Int
int = value >>= \v -> case v of
  J.Number v -> unit (floor v)
  _ -> fail ("not a number: " ++ J.toString "" v)

number : Parser Float
number = value >>= \v -> case v of
  J.Number v -> unit v
  _ -> fail ("not a number: " ++ J.toString "" v)

array : Parser a -> Parser [a]
array p v = case v of
  J.Array vs -> case partition (L.map p vs) of
    ([], results) -> Right results
    (h :: t, _) -> Left h
  _ -> Left ["not an array: " ++ J.toString "" v]

optional : Parser a -> Parser (Maybe a)
optional p v = case p v of
  Left err ->
    case v of J.Null -> Right Nothing
              _      -> Left err
  Right a -> Right (Just a)

key : String -> Parser J.Value
key k v = case v of
  J.Object dict ->
    let msg = Left ["key not found: " ++ k ++ " in " ++ show dict]
    in maybe msg Right (M.get k dict)
  _ -> Left ["cannot lookup key " ++ k ++ " in: " ++ J.toString "" v]

index : Int -> Parser J.Value
index ind v = case v of
  J.Array vs ->
    let msg = Left ["index not found: " ++ show ind ++ " in " ++ show vs]
    in maybe msg Right (safeIndex ind vs)
  _ -> Left ["cannot lookup index " ++ show ind ++ " in: " ++ J.toString "" v]

nest : Parser J.Value -> Parser a -> Parser a
nest pv p v = case pv v of
  Left err -> Left err
  Right v -> p v

union : String -> String -> (String -> Parser a) -> Parser a
union tag contents f =
  nest (key tag) string >>= \t -> nest (key contents) (f t)

union' : (String -> Parser a) -> Parser a
union' = union "tag" "contents"

product2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
product2 f a b = lift2 f (nest (index 0) a) (nest (index 1) b)

tuple2 : Parser a -> Parser b -> Parser (a,b)
tuple2 = product2 (,)

product5 : (a -> b -> c -> d -> e -> f)
        -> Parser a
        -> Parser b
        -> Parser c
        -> Parser d
        -> Parser e
        -> Parser f
product5 f a b c d e =
  unit f
    # nest (index 0) a
    # nest (index 1) b
    # nest (index 2) c
    # nest (index 3) d
    # nest (index 4) e

newtyped' : (a -> b) -> Parser a -> Parser b
newtyped' f p = union' (\_ -> map f p)

safeIndex : Int -> [a] -> Maybe a
safeIndex i xs = case drop i xs of
  [] -> Nothing
  h :: t -> Just h
