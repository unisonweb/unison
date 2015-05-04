module Elmz.Parser where

import String
import Regex

type alias Msg = List String
type alias Status = { message : Msg, committed : Bool }

type alias Parser a = { string : String, offset : Int } -> Result Status (a, Int)

parse : Parser a -> String -> Result String a
parse p s = case p { string = s, offset = 0 } of
  Err e -> Err (String.join "\n" e.message)
  Ok (a,_) -> Ok a

unit : a -> Parser a
unit a _ = Ok (a, 0)

fail : Parser a
fail s = Err { message = [], committed = False }

map : (a -> b) -> Parser a -> Parser b
map f p s = case p s of
  Ok (a, consumed) -> Ok (f a, consumed)
  Err e -> Err e

(<$>) : (a -> b) -> Parser a -> Parser b
(<$>) = map

-- note: implementation not stack safe
many : Parser a -> Parser (List a)
many a s = case a s of
  Err e -> if e.committed then Err e else Ok ([],0)
  Ok (hd,consumed) -> case many a { s | offset <- s.offset + consumed } of
    Err e -> Err e
    Ok (tl,consumedTl) -> Ok (hd :: tl, consumed + consumedTl)

some : Parser a -> Parser (List a)
some a = (::) <$> a <*> many a

andThen : Parser a -> (a -> Parser b) -> Parser b
andThen p f s = case p s of
  Err e -> Err e
  Ok (a, consumed) ->
    (if consumed > 0 then commit else identity) (f a) { s | offset <- s.offset + consumed }

commit : Parser a -> Parser a
commit p s = case p s of
  Err e -> Err { e | committed <- True }
  r -> r

attempt : Parser a -> Parser a
attempt p s = case p s of
  Err e -> Err { e | committed <- False }
  Ok a -> Ok a

scope : String -> Parser a -> Parser a
scope lbl p s = case p s of
  Err e -> Err { e | message <- lbl :: e.message }
  Ok a -> Ok a

label : String -> Parser a -> Parser a
label lbl p s = case p s of
  Err e -> Err { e | message <- [lbl] }
  Ok a -> Ok a

or : Parser a -> Parser a -> Parser a
or p p2 s = case p s of
  Ok a -> Ok a
  Err e -> if e.committed then Err e else p2 s

choice : List (Parser a) -> Parser a
choice = List.foldr or fail

ap : Parser (a -> b) -> Parser a -> Parser b
ap f a = f `andThen` \f -> map f a

satisfy : (Char -> Bool) -> Parser String
satisfy f s =
  let sub = String.slice s.offset ((String.length s.string) `max` (s.offset + 1)) s.string
  in if String.all f sub then Ok (sub, 1) else Err { message = [], committed = False }

symbol : Char -> Parser String
symbol c = satisfy ((==) c)

token : String -> Parser String
token t s =
  if String.startsWith t (String.dropLeft s.offset s.string)
  then Ok (t, String.length t)
  else Err { message = ["expected " ++ t], committed = False }

reset : { string : String, offset : Int } -> { string : String, offset : Int }
reset s = { string = String.dropLeft s.offset s.string, offset = 0 }

digits : Parser String
digits = regex "\\d+"

nonnegativeInt : Parser Int
nonnegativeInt = digits `andThen` \s -> case String.toInt s of
  Err e -> label e fail
  Ok n -> unit n

int : Parser Int
int = optional (symbol '-') `andThen` \sign -> case sign of
  Nothing -> nonnegativeInt
  Just _ -> map negate nonnegativeInt

float : Parser Float
float = regex "[+-]?\\d+\\.\\d+" `andThen` \s -> case String.toFloat s of
  Err e -> label e fail
  Ok n -> unit n

optional : Parser a -> Parser (Maybe a)
optional p = map Just p `or` unit Nothing

regex : String -> Parser String
regex r s = let compiled = Regex.regex r in case reset s of
  s -> case List.map .match (Regex.find (Regex.AtMost 1) compiled s.string) of
    [] -> Err { message = [], committed = False }
    hd :: _ -> Ok (hd, String.length hd)

infixl 4 <*>
infixl 4 <*
infixl 4 *>
infixl 4 <$
infixl 4 <$>

(<*>) : Parser (a -> b) -> Parser a -> Parser b
(<*>) = ap

(<*) : Parser a -> Parser b -> Parser a
a <* b = unit always <*> a <*> b

(<$) : a -> Parser b -> Parser a
a <$ p = map (always a) p

(*>) : Parser a -> Parser b -> Parser b
a *> b = unit (\_ b -> b) <*> a <*> b
