module Unison.Parser where

import List as L
import Either(..)
import Json as J
import Dict as M

type Msg = [String]

type P a = J.Value -> Either Msg a

data E = Key String | Index Int
type Path = [E]
  
type Parser a = { focus : Path, parse : P a }

safeIndex : Int -> [a] -> Maybe a
safeIndex i xs = case drop i xs of
  [] -> Nothing
  h :: t -> Just h

getPath : Path -> J.Value -> Either Msg J.Value
getPath p v =
  let 
    failure loc v = Left [
      "invalid path: " ++ show p,
      "remainder: " ++ show loc,
      "focus: " ++ J.toString "" v
    ]
    go loc v = case loc of
      [] -> Right v
      Key k :: t -> case v of
        J.Object obj -> case M.get k obj of
          Nothing -> failure loc v 
          Just v -> go t v 
        _ -> failure loc v 
      Index i :: t -> case v of
        J.Array vs -> case safeIndex i vs of
          Nothing -> failure loc v
          Just v -> go t v
        _ -> failure loc v
  in go p v 

run : Parser a -> J.Value -> Either Msg a
run p v = either Left p.parse (getPath p.focus v)

unit : a -> Parser a
unit a = { focus = [], parse = \v -> Right a }

value : Parser J.Value
value = { focus = [], parse = Right } 

fail : String -> Parser a
fail msg = { focus = [], parse = \v -> Left [msg] }

scope : String -> Parser a -> Parser a
scope msg p = 
  let focus = p.focus
      parse v = case p.parse v of 
        Left stack -> Left (msg :: stack)
        a          -> a
  in { focus = focus, parse = parse }

bind : (a -> Parser b) -> Parser a -> Parser b
bind f p = 
  let focus = p.focus
      parse v = case p.parse v of
        Right a -> run (f a) v
        Left e -> Left e
  in { focus = focus, parse = parse }

infixl 3 >>= 
(>>=) : Parser a -> (a -> Parser b) -> Parser b
a >>= f = bind f a

map : (a -> b) -> Parser a -> Parser b
map f p = 
  let parse v = case p.parse v of 
    Left e -> Left e
    Right a -> Right (f a)
  in { p | parse <- parse }

lift2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
lift2 f a b = 
  let common = a.focus `zip` b.focus
            |> concatMap (\p -> if fst p == snd p then [fst p] else []) 
      restL = drop (length common) a.focus 
      restR = drop (length common) b.focus
      parseL v = either Left a.parse (getPath restL v)
      parseR v = either Left b.parse (getPath restR v)
      parse v = case (parseL v, parseR v) of
        (Left e, _) -> Left e
        (_, Left e) -> Left e
        (Right a, Right b) -> Right (f a b) 
  in { focus = common, parse = parse }

apply : Parser (a -> b) -> Parser a -> Parser b
apply f a = lift2 (<|) f a 

infixl 4 #
(#) : Parser (a -> b) -> Parser a -> Parser b
f # a = apply f a

infixl 4 #|
(#|) : (a -> b) -> Parser a -> Parser b
f #| a = map f a

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
array p = 
  let parse v = case v of
    J.Array vs -> case partition (L.map (run p) vs) of
      ([], results) -> Right results
      (h :: t, _) -> Left h
    _ -> Left ["not an array: " ++ J.toString "" v]
  in { focus = [], parse = parse }

atKey : String -> Parser a -> Parser a
atKey k p = { p | focus <- Key k :: p.focus }

atIndex : Int -> Parser a -> Parser a  
atIndex ind p = { p | focus <- Index ind :: p.focus }

union : String -> String -> (String -> Parser a) -> Parser a
union tag contents f = 
  let parse v = case v of 
    J.Object obj -> case M.get tag obj of
      Nothing -> Left ["invalid key: " ++ tag]
      Just (J.String t) -> 
        maybe (Left ["invalid key: " ++ contents]) 
              (run (f t))
              (M.get contents obj)
      Just t -> Left ["invalid tag: " ++ J.toString "" t]
    _ -> Left ["union applied to non-object: " ++ J.toString "" v]
  in { focus = [], parse = parse }

union' : (String -> Parser a) -> Parser a
union' = union "tag" "contents"
