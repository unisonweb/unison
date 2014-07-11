module Unison.Parser where

import Either(..)
import Json as J
import Dict as M

type Msg = [String]

-- type P a 
-- type P a = J.Value -> Either Msg a
-- type Parser a = { focus : Path, parser : P a } 
-- combinators like lift2 can merge common paths

type Parser a = J.Value -> Either Msg a

run : Parser a -> J.Value -> Either String a
run p v = case p v of
  Left stack -> Left (join "\n" stack)
  Right a -> Right a

unit : a -> Parser a
unit a _ = Right a

fail : String -> Parser a
fail msg _ = Left [msg]

scope : String -> Parser a -> Parser a
scope msg p v = case p v of 
  Left stack -> Left (msg :: stack)
  a          -> a

bind : (a -> Parser b) -> Parser a -> Parser b
bind f p v = case p v of
  Right a -> f a v
  Left e -> Left e 

infixl 3 >>= 
(>>=) : Parser a -> (a -> Parser b) -> Parser b
a >>= f = bind f a

map : (a -> b) -> Parser a -> Parser b
map f = bind (unit . f) 

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

infixl 4 #|
(#|) : (a -> b) -> Parser a -> Parser b
f #| a = map f a

key : String -> Parser J.Value
key k v = case v of 
  J.Object obj -> M.get k obj 
               |> maybe (Left ["not found"]) Right
  e -> Left ["not found: " ++ J.toString "" v]

string : Parser String
string v = case v of 
  J.String s -> Right s
  _ -> Left ["not a string: " ++ J.toString "" v]

int : Parser Int
int v = case v of
  J.Number v -> Right (floor v)
  _ -> Left ["not a number: " ++ J.toString "" v]
  
number : Parser Float
number v = case v of 
  J.Number v -> Right v
  _ -> Left ["not a number: " ++ J.toString "" v]
  
asString : Parser J.Value -> Parser String
asString p v = case v of 
  J.String s -> Right s
  _ -> Left ["not a string: " ++ J.toString "" v]

atKey : String -> Parser a -> Parser a  
atKey fld p = 
  let p2 v = case v of
    J.Object obj -> M.get fld obj 
                 |> maybe (Left ["not found"]) p   
    e -> Left ["not found: " ++ J.toString "" v]
  in scope fld p2

atIndex : Int -> Parser a -> Parser a  
atIndex ind p = 
  let p2 v = case v of
    J.Array vs -> 
      let sub = drop ind vs 
      in if isEmpty sub 
         then Left ["out of bounds: " ++ show ind ++ " " ++ J.toString "" v] 
         else p (head sub)
    e -> Left ["not found: " ++ J.toString "" v]
  in scope ("index: " ++ show ind) p2

union : String -> String -> (String -> Parser a) -> Parser a
union tag contents f = 
  asString (key tag) >>= \t -> atKey contents (f t)

union' : (String -> Parser a) -> Parser a
union' = union "tag" "contents"
