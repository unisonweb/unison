module Unison.SearchboxParser where

import Debug
import Elmz.Distance as Distance
import Elmz.Distance exposing (Distance)
import List
import Elmz.Parser as Parser
import Elmz.Parser exposing (Parser, (<*), (*>), (<*>), (<$>), (<$))
import Unison.Term as Term
import Unison.Term exposing (Term)
import String
import Set

parser : { literal : Term -> a
         , query : String -> a
         , combine : a -> String -> a }
      -> Parser a
parser env =
  let lit = Parser.map env.literal literal
      q = Parser.map env.query (until ' ')
      any = Parser.satisfy (always True)
  in Parser.choice
    [ env.combine <$> (lit <* space) <*> any
    , lit
    , env.combine <$> (q <* space) <*> any
    , q ]

space = Parser.satisfy ((==) ' ')

parse :
  { literal : Term -> a
  , query : String -> a
  , combine : a -> String -> a }
  -> String -> Result String a
parse env = Parser.parse (parser env)

parseTerm : String -> Result String Term
parseTerm = Parser.parse literal

operator : Parser String
operator =
  let ops = Set.fromList (String.toList "!@#$%^&*-+|\\;.></`~")
  in Parser.satisfy (\c -> Set.member c ops)

spaces : Parser ()
spaces = () <$ Parser.some (Parser.satisfy ((==) ' '))

literal : Parser Term
literal =
  Parser.choice [ blank, distance, float, int, string, openString ]

blank : Parser Term
blank = Parser.map (always Term.Blank) (Parser.symbol '_')

int : Parser Term
int = Parser.map (Term.Lit << Term.Number << toFloat) (Parser.attempt Parser.int)

float : Parser Term
float = Parser.map (Term.Lit << Term.Number) (Parser.attempt Parser.float)

string : Parser Term
string = (Parser.symbol quote *> (until quote) <* Parser.symbol quote)
      |> Parser.map (Term.Lit << Term.Text)

openString : Parser Term
openString =
  Parser.symbol quote `Parser.andThen` \_ -> until quote
  |> Parser.map (Term.Lit << Term.Text)

distance : Parser Term
distance =
  Parser.choice [ pixels, fraction ]
  |> Parser.map (Term.Lit << Term.Distance)

pixels : Parser Distance
pixels =
  let f n = Distance.Scale (toFloat n) Distance.Pixel
  in f <$> Parser.nonnegativeInt <* Parser.token "px"

fraction : Parser Distance
fraction = Parser.symbol '1'
        *> Parser.symbol '/'
        *> (Parser.nonnegativeInt <* Parser.symbol 'd')
        |> Parser.map (\denominator -> Distance.Fraction (1.0 / toFloat denominator))

quote = '"' -- "

until : Char -> Parser String
until c =
  Parser.map String.concat (Parser.many (Parser.satisfy ((/=) c)))

until1 : Char -> Parser String
until1 c =
  Parser.map String.concat (Parser.some (Parser.satisfy ((/=) c)))
