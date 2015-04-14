module Unison.SearchboxParser where

import Debug
import Elmz.Distance as Distance
import Elmz.Distance (Distance)
import List
import Parser
import Parser (Parser, (<*), (*>), (<*>), (<$>), (<$))
import Parser.Number
import Parser.Char
import Unison.Term as Term
import Unison.Term (Term)
import String
import Set

parser : { literal : Term -> a
         , query : String -> a
         , combine : a -> Char -> a }
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
  , combine : a -> Char -> a }
  -> String -> Result String a
parse env = Parser.parse (parser env)

parseTerm : String -> Result String Term
parseTerm = Parser.parse literal

operator : Parser Char
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
int = Parser.map (Term.Lit << Term.Number << toFloat) Parser.Number.integer

float : Parser Term
float = Parser.map (Term.Lit << Term.Number) Parser.Number.float

string : Parser Term
string = Parser.Char.between quote quote (until quote)
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
  in f <$> Parser.Number.natural <* Parser.token "px"

fraction : Parser Distance
fraction = Parser.symbol '1'
        *> Parser.symbol '/'
        *> (Parser.Number.natural <* Parser.symbol 'd')
        |> Parser.map (\denominator -> Distance.Fraction (1.0 / toFloat denominator))

quote = '"' -- "

until : Char -> Parser String
until c =
  Parser.map (String.concat << List.map String.fromChar)
             (Parser.many (Parser.satisfy ((/=) c)))

until1 : Char -> Parser String
until1 c =
  Parser.map (String.concat << List.map String.fromChar)
             (Parser.some (Parser.satisfy ((/=) c)))
