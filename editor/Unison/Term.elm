module Unison.Term where

import Array
import Array (Array)
import Dict
import Dict (Dict)
import Elmz.Distance (Distance)
import Elmz.Maybe as EM
import Json
import Maybe (isJust, maybe)
import Set
import Set (Set)
import String
import Text(..)
import Text
import Unison.Hash (Hash)
import Unison.Hash as H
import Unison.Jsonify (Jsonify)
import Unison.Jsonify as J
import Unison.Metadata (Metadata, Fixity)
import Unison.Metadata as Metadata
import Unison.Parser (Parser)
import Unison.Parser as P
import Unison.Path (..)
import Unison.Path as Path
import Unison.Type as T
import Unison.Var (I)
import Unison.Var as V
type Path = Path.Path -- to avoid conflict with Graphics.Collage.Path

data Literal
  = Number Float
  | Str String
  | Dist Distance
  | Style Text.Style
  | Vector (Array Term)
  | Builtin String

data Term
  = Var I
  | Lit Literal
  | Con Hash
  | Ref Hash
  | App Term Term
  | Ann Term T.Type
  | Lam I Term

{-| Returns the subterm at the given path, if the path is valid. -}
at : Path -> Term -> Maybe Term
at p e = case (p,e) of
  ([], e) -> Just e
  (Fn :: t, App f _) -> at t f
  (Arg :: t, App _ arg) -> at t arg
  (Body :: t, Lam _ body) -> at t body
  (Index i :: t, Lit (Vector es)) -> case Array.get i es of
    Just e -> at t e
    _ -> Nothing
  _ -> Nothing

{-| Returns `True` if the path points to a valid subterm -}
valid : Term -> Path -> Bool
valid e p = isJust (at p e)

{-| Move path to point to leftmost child, or return `p` unmodified
    if no such child exists. -}
down : Term -> Path -> Path
down e p =
  let apps e = case e of
        App f x -> apps f + 1
        _ -> 1
      go e = case e of
        App f x -> p `append` repeat (apps f) Fn
        Lit (Vector es) -> if Array.length es == 0 then p else p `snoc` Index 0
        Lam _ _ -> p `snoc` Body
        _ -> p
  in maybe p go (at p e)

{-| Move path to point to parent node in "logical" layout. -}
up : Path -> Path
up p =
  let go p = case p of
    [] -> []
    _ :: Arg :: tl -> reverse (Arg :: tl)
    Fn :: tl -> go tl
    Arg :: tl -> go tl
    _ :: tl -> reverse tl -- Index or Body
  in go (reverse p)

{-| Move the path to its immediate sibling to the right,
    or return `p` unmodified if no such sibling exists.  -}
siblingR : Term -> Path -> Path
siblingR e p =
  let p2 = increment (valid e) p
  in if decrement (valid e) p2 == p then p2
     else p

{-| Move the path to its immediate sibling to the right,
    or return `p` unmodified if no such sibling exists.  -}
siblingL : Term -> Path -> Path
siblingL e p =
  let p2 = decrement (valid e) p
  in if increment (valid e) p2 == p then p2
     else p

parseLiteral : Parser Literal
parseLiteral = P.union' <| \t ->
  if | t == "Number" -> P.map Number P.number
     | t == "String" -> P.map Str P.string
     | t == "Builtin" -> P.map Builtin P.string
     | t == "Vector" -> P.map (Vector << Array.fromList) (P.array parseTerm)

jsonifyLiteral l = case l of
  Number n -> J.tag' "Number" J.number n
  Str s -> J.tag' "String" J.string s
  Vector es -> J.tag' "Vector" (J.contramap Array.toList (J.array jsonifyTerm)) es
  Builtin s -> J.tag' "Builtin" J.string s

parseTerm : Parser Term
parseTerm = P.union' <| \t ->
  if | t == "Var" -> P.map Var V.parse
     | t == "Lit" -> P.map Lit parseLiteral
     | t == "Con" -> P.map Con H.parse
     | t == "Ref" -> P.map Ref H.parse
     | t == "App" -> P.lift2 App parseTerm parseTerm
     | t == "Ann" -> P.lift2 Ann parseTerm T.parseType
     | t == "Lam" -> P.lift2 Lam V.parse parseTerm

jsonifyTerm : Jsonify Term
jsonifyTerm e = case e of
  Var v -> J.tag' "Var" V.jsonify v
  Lit l -> J.tag' "Lit" jsonifyLiteral l
  Con h -> J.tag' "Con" H.jsonify h
  Ref h -> J.tag' "Ref" H.jsonify h
  App f x -> J.tag' "App" (J.array jsonifyTerm) [f, x]
  Ann e t -> J.tag' "Ann" (J.tuple2 jsonifyTerm T.jsonifyType) (e, t)
  Lam n body -> J.tag' "Lam" (J.tuple2 V.jsonify jsonifyTerm) (n, body)

