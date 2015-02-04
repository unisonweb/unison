module Unison.Term where

import Array
import Array (Array)
import Debug
import Dict
import Dict (Dict)
import Elmz.Distance as Distance
import Elmz.Maybe as EM
import Elmz.Layout (Layout)
import Elmz.Json.Encoder (Encoder)
import Elmz.Json.Encoder as Encoder
import Elmz.Json.Decoder (Decoder)
import Elmz.Json.Decoder as Decoder
import List
import List ((::))
import Maybe
import Set
import Set (Set)
import String
import Text
import Unison.Reference as R
import Unison.Hash (Hash)
import Unison.Hash as H
import Unison.Metadata (Metadata, Fixity)
import Unison.Metadata as Metadata
import Unison.Path (..)
import Unison.Path as Path
import Unison.Type as T
import Unison.Var (I)
import Unison.Var as V
type alias E = Path.E
type alias Path = Path.Path -- to avoid conflict with Graphics.Collage.Path

type Literal
  = Number Float
  | Str String
  | Distance Distance.Distance

type Term
  = Var I
  | Blank
  | Lit Literal
  | Ref R.Reference
  | App Term Term
  | Ann Term T.Type
  | Lam I Term
  | Vector (Array Term)
  | Embed (Layout { path : Path, selectable : Bool })

type ClosedTerm = ClosedTerm Term

close : Term -> Maybe ClosedTerm
close e = if unbound e == Set.empty then Just (ClosedTerm e) else Nothing

unclose : ClosedTerm -> Term
unclose (ClosedTerm e) = e

rename : I -> I -> Term -> Term
rename from to e = case e of
  Var i -> if i == from then Var to else e
  Vector es -> Vector (Array.map (rename from to) es)
  Lit l -> e
  App f arg -> App (rename from to f) (rename from to arg)
  Ann e t -> Ann (rename from to e) t
  Lam n inner -> Lam n (rename from to inner)
  _ -> e

substitute : Term -> I -> ClosedTerm -> Term
substitute body v x =
  let freshx = fresh (unclose x)
      go body = case body of
        Var i -> if i == v then unclose x else body
        Lit l -> body
        Vector es -> Vector (Array.map (\body -> substitute body v x) es)
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
  Lit l -> V.z
  Vector es -> Array.map fresh es |> Array.foldl max V.z
  App f arg -> max (fresh f) (fresh arg)
  Ann e _ -> fresh e
  _ -> V.z

{-| Returns the set of free variables in the given `Term`. -}
unbound : Term -> Set I
unbound e = case e of
  Var i -> Set.singleton i
  Vector es -> Array.map unbound es |> Array.foldl Set.union Set.empty
  Lit l -> Set.empty
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
  (Index i :: t, Vector es) -> case Array.get i es of
    Just e -> at t e
    _ -> Nothing
  _ -> Nothing

{-| Sets the given path to `e'`, if the path is valid. -}
set : Path -> Term -> Term -> Maybe Term
set p e e' = Debug.crash "todo: Term.set"

{-| Returns `True` if the path points to a valid subterm -}
valid : Term -> Path -> Bool
valid e p = case at p e of
  Nothing -> False
  Just _ -> True

{-| Move path to point to leftmost child, or return `p` unmodified
    if no such child exists. -}
down : Term -> Path -> Path
down e p =
  let apps e = case e of
        App f x -> apps f + 1
        _ -> 1
      go e = case e of
        App f x -> p `append` List.repeat (apps f) Fn
        Vector es -> if Array.length es == 0 then p else p `snoc` Index 0
        Lam _ _ -> p `snoc` Body
        _ -> p
  in Maybe.withDefault p (Maybe.map go (at p e))

{-| Move path to point to parent node in "logical" layout. -}
up : Path -> Path
up p =
  let go p = case p of
    [] -> []
    _ :: Arg :: tl -> List.reverse (Arg :: tl)
    Fn :: tl -> go tl
    Arg :: tl -> go tl
    _ :: tl -> List.reverse tl -- Index or Body
  in go (List.reverse p)

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

decodeDistance : Decoder (Distance.Distance)
decodeDistance = Decoder.union' <| \t ->
  if | t == "Pixel" -> Decoder.unit Distance.Pixel
     | t == "Scale" -> Decoder.product2 Distance.Scale Decoder.float decodeDistance
     | t == "Ceiling" -> Decoder.map Distance.Ceiling decodeDistance
     | t == "Floor" -> Decoder.map Distance.Floor decodeDistance
     | t == "Min" -> Decoder.product2 Distance.Min decodeDistance decodeDistance
     | t == "Max" -> Decoder.product2 Distance.Max decodeDistance decodeDistance

encodeDistance : Encoder Distance.Distance
encodeDistance e = case e of
  Distance.Pixel -> Encoder.tag' "Pixel" Encoder.product0 ()
  Distance.Scale k dist -> Encoder.tag' "Scale" (Encoder.tuple2 Encoder.float encodeDistance) (k,dist)
  Distance.Ceiling dist -> Encoder.tag' "Ceiling" encodeDistance dist
  Distance.Floor dist -> Encoder.tag' "Floor" encodeDistance dist
  Distance.Max dist1 dist2 -> Encoder.tag' "Max" (Encoder.tuple2 encodeDistance encodeDistance) (dist1, dist2)
  Distance.Min dist1 dist2 -> Encoder.tag' "Min" (Encoder.tuple2 encodeDistance encodeDistance) (dist1, dist2)

decodeLiteral : Decoder Literal
decodeLiteral = Decoder.union' <| \t ->
  if | t == "Number" -> Decoder.map Number Decoder.float
     | t == "String" -> Decoder.map Str Decoder.string
     | t == "Distance" -> Decoder.map Distance decodeDistance

encodeLiteral l = case l of
  Number n -> Encoder.tag' "Number" Encoder.float n
  Str s -> Encoder.tag' "String" Encoder.string s
  Distance d -> Encoder.tag' "Distance" encodeDistance d

decodeTerm : Decoder Term
decodeTerm = Decoder.union' <| \t ->
  if | t == "Var" -> Decoder.map Var V.decode
     | t == "Lit" -> Decoder.map Lit decodeLiteral
     | t == "Vector" -> Decoder.map Vector (Decoder.array decodeTerm)
     | t == "Ref" -> Decoder.map Ref R.decode
     | t == "App" -> Decoder.product2 App decodeTerm decodeTerm
     | t == "Ann" -> Decoder.product2 Ann decodeTerm T.decodeType
     | t == "Lam" -> Decoder.product2 Lam V.decode decodeTerm
     | t == "Blank" -> Decoder.unit Blank

encodeTerm : Encoder Term
encodeTerm e = case e of
  Blank -> Encoder.tag' "Blank" Encoder.product0 ()
  Var v -> Encoder.tag' "Var" V.encode v
  Lit l -> Encoder.tag' "Lit" encodeLiteral l
  Ref h -> Encoder.tag' "Ref" R.encode h
  App f x -> Encoder.tag' "App" (Encoder.list encodeTerm) [f, x]
  Ann e t -> Encoder.tag' "Ann" (Encoder.tuple2 encodeTerm T.encodeType) (e, t)
  Lam n body -> Encoder.tag' "Lam" (Encoder.tuple2 V.encode encodeTerm) (n, body)
  Embed e -> Encoder.tag' "Embed" Encoder.product0 ()

