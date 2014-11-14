module Unison.Term where

import Array
import Array (Array)
import Dict
import Dict (Dict)
import Elmz.Distance as Distance
import Elmz.Maybe as EM
import Elmz.Layout (Layout)
import Elmz.Json.Encoder as Encoder
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Decoder as Decoder
import Elmz.Json.Decoder (Decoder)
import Json
import Maybe (isJust, maybe)
import Set
import Set (Set)
import String
import Text(..)
import Text
import Unison.Hash (Hash)
import Unison.Hash as H
import Unison.Metadata (Metadata, Fixity)
import Unison.Metadata as Metadata
import Unison.Path (..)
import Unison.Path as Path
import Unison.Type as T
import Unison.Var (I)
import Unison.Var as V
type Path = Path.Path -- to avoid conflict with Graphics.Collage.Path

data Literal
  = Number Float
  | Str String
  | Relative Distance.Relative
  | Absolute Distance.Absolute
  | Vector (Array Term)

data Term
  = Var I
  | Blank
  | Lit Literal
  | Con Hash
  | Ref Hash
  | Builtin String
  | App Term Term
  | Ann Term T.Type
  | Lam I Term
  | Embed (Layout { path : Path, selectable : Bool })

data ClosedTerm = ClosedTerm Term

close : Term -> Maybe ClosedTerm
close e = if unbound e == Set.empty then Just (ClosedTerm e) else Nothing

unclose : ClosedTerm -> Term
unclose (ClosedTerm e) = e

rename : I -> I -> Term -> Term
rename from to e = case e of
  Var i -> if i == from then Var to else e
  Lit l -> case l of
    Vector es -> Lit (Vector (Array.map (rename from to) es))
    _ -> e
  App f arg -> App (rename from to f) (rename from to arg)
  Ann e t -> Ann (rename from to e) t
  Lam n inner -> Lam n (rename from to inner)
  _ -> e

substitute : Term -> I -> ClosedTerm -> Term
substitute body v x =
  let freshx = fresh (unclose x)
      go body = case body of
        Var i -> if i == v then unclose x else body
        Lit l -> case l of
          Vector es -> Lit (Vector (Array.map (\body -> substitute body v x) es))
          _ -> body
        App f arg -> App (substitute f v x) (substitute arg v x)
        Ann e t -> Ann (substitute e v x) t
        Lam n inner -> if n == v then Lam n inner
                       else let inner' = substitute inner v x
                                n' = freshx `max` n
                            in if n' >= n then (Lam n' (substitute (rename n n' inner) v x))
                               else Lam n (substitute inner v x)
        _ -> body
  in go body

fresh : Term -> I
fresh e = case e of
  Lam n _ -> V.succ n
  Lit l -> case l of
    Vector es -> Array.map fresh es |> Array.foldl max V.z
    _ -> V.z
  App f arg -> max (fresh f) (fresh arg)
  Ann e _ -> fresh e
  _ -> V.z

{-| Returns the set of free variables in the given `Term`. -}
unbound : Term -> Set I
unbound e = case e of
  Var i -> Set.singleton i
  Lit l -> case l of
    Vector es -> Array.map unbound es |> Array.foldl Set.union Set.empty
    _ -> Set.empty
  App f arg -> Set.union (unbound f) (unbound arg)
  Ann e _ -> unbound e
  Lam n body -> Set.remove n (unbound body)
  _ -> Set.empty

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

decodeLiteral : Decoder Literal
decodeLiteral = Decoder.union' <| \t ->
  if | t == "Number" -> Decoder.map Number Decoder.number
     | t == "String" -> Decoder.map Str Decoder.string
     | t == "Vector" -> Decoder.map (Vector << Array.fromList) (Decoder.array decodeTerm)

encodeLiteral l = case l of
  Number n -> Encoder.tag' "Number" Encoder.number n
  Str s -> Encoder.tag' "String" Encoder.string s
  Vector es -> Encoder.tag' "Vector" (Encoder.contramap Array.toList (Encoder.array encodeTerm)) es

decodeTerm : Decoder Term
decodeTerm = Decoder.union' <| \t ->
  if | t == "Var" -> Decoder.map Var V.decode
     | t == "Lit" -> Decoder.map Lit decodeLiteral
     | t == "Con" -> Decoder.map Con H.decode
     | t == "Ref" -> Decoder.map Ref H.decode
     | t == "Builtin" -> Decoder.map Builtin Decoder.string
     | t == "App" -> Decoder.lift2 App decodeTerm decodeTerm
     | t == "Ann" -> Decoder.lift2 Ann decodeTerm T.decodeType
     | t == "Lam" -> Decoder.lift2 Lam V.decode decodeTerm
     | t == "Blank" -> Decoder.unit Blank

encodeTerm : Encoder Term
encodeTerm e = case e of
  Blank -> Encoder.tag' "Blank" Encoder.product0 ()
  Var v -> Encoder.tag' "Var" V.encode v
  Lit l -> Encoder.tag' "Lit" encodeLiteral l
  Con h -> Encoder.tag' "Con" H.encode h
  Ref h -> Encoder.tag' "Ref" H.encode h
  Builtin s -> Encoder.tag' "Builtin" Encoder.string s
  App f x -> Encoder.tag' "App" (Encoder.array encodeTerm) [f, x]
  Ann e t -> Encoder.tag' "Ann" (Encoder.tuple2 encodeTerm T.encodeType) (e, t)
  Lam n body -> Encoder.tag' "Lam" (Encoder.tuple2 V.encode encodeTerm) (n, body)
  Embed e -> Encoder.tag' "Embed" Encoder.product0 ()

